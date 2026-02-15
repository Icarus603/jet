//! Await implementation for async/await support.
//!
//! This module provides the core `.await` functionality, allowing tasks to
//! suspend execution until a future completes. It handles:
//! - Suspending the current task
//! - Registering wakers for notification
//! - Resuming tasks when futures become ready
//!
//! # Architecture
//!
//! The await mechanism works as follows:
//! 1. When `.await` is called on a future, it polls the future
//! 2. If the future is ready, the value is returned immediately
//! 3. If the future is pending, the current task is suspended
//! 4. The future's waker is set up to resume the task when ready
//! 5. When woken, the task is rescheduled and the future is polled again
//!
//! # Safety
//!
//! Await points are where self-referential futures are most vulnerable.
//! The Pin guarantee ensures that the future is not moved while suspended.

use crate::future::{Context, Future, Poll, TaskHandle};
use crate::task::TaskId;
use std::marker::PhantomPinned;
use std::pin::Pin;
use std::task::{RawWaker, RawWakerVTable, Waker};

/// The result of an await operation.
pub type AwaitResult<T> = Result<T, AwaitError>;

/// Errors that can occur during await.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AwaitError {
    /// The task was cancelled while waiting.
    Cancelled,
    /// The awaited task panicked.
    Panicked,
    /// A timeout occurred.
    Timeout,
}

impl std::fmt::Display for AwaitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AwaitError::Cancelled => write!(f, "task was cancelled"),
            AwaitError::Panicked => write!(f, "awaited task panicked"),
            AwaitError::Timeout => write!(f, "await operation timed out"),
        }
    }
}

impl std::error::Error for AwaitError {}

/// Awaits a future, suspending the current task until it completes.
///
/// This is the core await implementation. It polls the future and handles
/// suspension/resumption of the current task.
///
/// # Safety
///
/// The future must be pinned before calling this function.
pub async fn await_future<F>(future: F) -> F::Output
where
    F: Future,
{
    AwaitableFuture {
        future: Some(future),
        _pinned: PhantomPinned,
    }
    .await
}

/// A future that wraps another future to provide await functionality.
struct AwaitableFuture<F> {
    future: Option<F>,
    _pinned: PhantomPinned,
}

impl<F: Future> Future for AwaitableFuture<F> {
    type Output = F::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // SAFETY: We're not moving the struct, just polling the inner future
        let this = unsafe { self.get_unchecked_mut() };

        if let Some(ref mut future) = this.future {
            // SAFETY: We're not moving the inner future
            unsafe {
                let pinned = Pin::new_unchecked(future);
                match pinned.poll(cx) {
                    Poll::Ready(result) => {
                        this.future = None;
                        Poll::Ready(result)
                    }
                    Poll::Pending => Poll::Pending,
                }
            }
        } else {
            panic!("AwaitableFuture polled after completion")
        }
    }
}

// Also implement std::future::Future for native async/await support
impl<F: Future> std::future::Future for AwaitableFuture<F> {
    type Output = F::Output;

    fn poll(
        self: Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        // SAFETY: We're not moving the struct, just polling the inner future
        let this = unsafe { self.get_unchecked_mut() };

        if let Some(ref mut future) = this.future {
            // SAFETY: We're not moving the inner future
            unsafe {
                let pinned = Pin::new_unchecked(future);
                // Convert our custom Poll to std::task::Poll
                match pinned.poll(&mut Context::new(&create_simple_waker(), 0)) {
                    Poll::Ready(result) => {
                        this.future = None;
                        std::task::Poll::Ready(result)
                    }
                    Poll::Pending => std::task::Poll::Pending,
                }
            }
        } else {
            panic!("AwaitableFuture polled after completion")
        }
    }
}

/// Awaits multiple futures concurrently, returning when all complete.
///
/// # Example
///
/// ```ignore
/// use jet_rt_sched::await_impl::await_all;
/// use jet_rt_sched::future::{Future, Poll, Context};
///
/// async fn example() {
///     let a = async { 1 };
///     let b = async { 2 };
///     let results = await_all(vec![a, b]).await;
///     assert_eq!(results, vec![1, 2]);
/// }
/// ```
pub async fn await_all<F, I>(futures: I) -> Vec<F::Output>
where
    F: Future + Unpin,
    I: IntoIterator<Item = F>,
{
    let futures: Vec<_> = futures.into_iter().map(Some).collect();
    let mut results = Vec::with_capacity(futures.len());
    results.resize_with(futures.len(), || None);

    AwaitAll {
        futures,
        results,
        _pinned: PhantomPinned,
    }
    .await
}

/// Future that awaits all futures in a collection.
struct AwaitAll<F: Future + Unpin> {
    futures: Vec<Option<F>>,
    results: Vec<Option<F::Output>>,
    _pinned: PhantomPinned,
}

impl<F: Future + Unpin> Future for AwaitAll<F> {
    type Output = Vec<F::Output>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        let mut all_done = true;

        for (i, fut_opt) in this.futures.iter_mut().enumerate() {
            if let Some(fut) = fut_opt {
                match Pin::new(fut).poll(cx) {
                    Poll::Ready(result) => {
                        this.results[i] = Some(result);
                        *fut_opt = None;
                    }
                    Poll::Pending => {
                        all_done = false;
                    }
                }
            }
        }

        if all_done {
            Poll::Ready(
                std::mem::take(&mut this.results)
                    .into_iter()
                    .map(|r| r.unwrap())
                    .collect(),
            )
        } else {
            Poll::Pending
        }
    }
}

// Also implement std::future::Future for native async/await support
impl<F: Future + Unpin> std::future::Future for AwaitAll<F> {
    type Output = Vec<F::Output>;

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        use std::task::Poll as StdPoll;

        // Create a dummy waker that delegates to the std waker
        let waker = cx.waker().clone();
        let mut our_cx = Context::new(&waker, 0);

        // SAFETY: We're not moving the struct
        let this = unsafe { self.get_unchecked_mut() };
        let mut all_done = true;

        for (i, fut_opt) in this.futures.iter_mut().enumerate() {
            if let Some(fut) = fut_opt {
                match Pin::new(fut).poll(&mut our_cx) {
                    Poll::Ready(result) => {
                        this.results[i] = Some(result);
                        *fut_opt = None;
                    }
                    Poll::Pending => {
                        all_done = false;
                    }
                }
            }
        }

        if all_done {
            StdPoll::Ready(
                std::mem::take(&mut this.results)
                    .into_iter()
                    .map(|r| r.unwrap())
                    .collect(),
            )
        } else {
            StdPoll::Pending
        }
    }
}

/// Awaits multiple futures concurrently, returning when any completes.
///
/// Returns the result of the first future to complete, along with the
/// remaining futures.
///
/// # Example
///
/// ```ignore
/// use jet_rt_sched::await_impl::await_any;
///
/// async fn example() {
///     let a = async { 1 };
///     let b = async { 2 };
///     let (result, _remaining) = await_any(vec![a, b]).await;
///     // result will be either 1 or 2
/// }
/// ```
pub async fn await_any<F, I>(futures: I) -> (F::Output, usize, Vec<F>)
where
    F: Future + Unpin,
    I: IntoIterator<Item = F>,
{
    AwaitAny {
        futures: futures.into_iter().collect(),
        _pinned: PhantomPinned,
    }
    .await
}

/// Future that awaits any future in a collection.
struct AwaitAny<F: Future + Unpin> {
    futures: Vec<F>,
    _pinned: PhantomPinned,
}

impl<F: Future + Unpin> Future for AwaitAny<F> {
    type Output = (F::Output, usize, Vec<F>);

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };

        for (i, fut) in this.futures.iter_mut().enumerate() {
            match Pin::new(fut).poll(cx) {
                Poll::Ready(result) => {
                    // Remove the completed future and return remaining
                    let mut remaining = std::mem::take(&mut this.futures);
                    remaining.remove(i);
                    return Poll::Ready((result, i, remaining));
                }
                Poll::Pending => {}
            }
        }

        Poll::Pending
    }
}

// Also implement std::future::Future for native async/await support
impl<F: Future + Unpin> std::future::Future for AwaitAny<F> {
    type Output = (F::Output, usize, Vec<F>);

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        use std::task::Poll as StdPoll;

        // Create a dummy waker that delegates to the std waker
        let waker = cx.waker().clone();
        let mut our_cx = Context::new(&waker, 0);

        // SAFETY: We're not moving the struct
        let this = unsafe { self.get_unchecked_mut() };

        for (i, fut) in this.futures.iter_mut().enumerate() {
            match Pin::new(fut).poll(&mut our_cx) {
                Poll::Ready(result) => {
                    // Remove the completed future and return remaining
                    let mut remaining = std::mem::take(&mut this.futures);
                    remaining.remove(i);
                    return StdPoll::Ready((result, i, remaining));
                }
                Poll::Pending => {}
            }
        }

        StdPoll::Pending
    }
}

/// Awaits a task handle, returning its result.
///
/// This is used to await spawned tasks.
pub async fn await_task<T>(handle: TaskHandle<T>) -> Result<T, AwaitError> {
    TaskAwaiter {
        handle,
        _pinned: PhantomPinned,
    }
    .await
}

/// Future that awaits a task handle.
struct TaskAwaiter<T> {
    handle: TaskHandle<T>,
    _pinned: PhantomPinned,
}

impl<T> Future for TaskAwaiter<T> {
    type Output = Result<T, AwaitError>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };

        // Check if the task is complete
        if let Some(result) = this.handle.try_get() {
            return Poll::Ready(Ok(result));
        }

        // Register for wakeup when the task completes
        this.handle.register_waker(cx.waker());

        // Check again in case it completed between try_get and register_waker
        if let Some(result) = this.handle.try_get() {
            return Poll::Ready(Ok(result));
        }

        Poll::Pending
    }
}

// Also implement std::future::Future for native async/await support
impl<T> std::future::Future for TaskAwaiter<T> {
    type Output = Result<T, AwaitError>;

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        use std::task::Poll as StdPoll;

        let this = unsafe { self.get_unchecked_mut() };

        // Check if the task is complete
        if let Some(result) = this.handle.try_get() {
            return StdPoll::Ready(Ok(result));
        }

        // Register for wakeup when the task completes
        this.handle.register_waker(cx.waker());

        // Check again in case it completed between try_get and register_waker
        if let Some(result) = this.handle.try_get() {
            return StdPoll::Ready(Ok(result));
        }

        StdPoll::Pending
    }
}

/// Awaits a future with a timeout.
///
/// Returns `Ok(result)` if the future completes in time, or `Err(AwaitError::Timeout)`
/// if the timeout expires.
pub async fn await_timeout<F>(
    future: F,
    timeout: std::time::Duration,
) -> Result<F::Output, AwaitError>
where
    F: Future,
{
    let deadline = std::time::Instant::now() + timeout;

    TimeoutAwaiter {
        future: Some(future),
        deadline,
        _pinned: PhantomPinned,
    }
    .await
}

/// Future that awaits with a timeout.
struct TimeoutAwaiter<F> {
    future: Option<F>,
    deadline: std::time::Instant,
    _pinned: PhantomPinned,
}

impl<F: Future> Future for TimeoutAwaiter<F> {
    type Output = Result<F::Output, AwaitError>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };

        // Check for timeout
        if std::time::Instant::now() >= this.deadline {
            return Poll::Ready(Err(AwaitError::Timeout));
        }

        // Poll the inner future
        if let Some(ref mut future) = this.future {
            unsafe {
                let pinned = Pin::new_unchecked(future);
                match pinned.poll(cx) {
                    Poll::Ready(result) => {
                        this.future = None;
                        Poll::Ready(Ok(result))
                    }
                    Poll::Pending => {
                        // Register for wakeup and continue waiting
                        cx.waker().wake_by_ref();
                        Poll::Pending
                    }
                }
            }
        } else {
            panic!("TimeoutAwaiter polled after completion")
        }
    }
}

// Also implement std::future::Future for native async/await support
impl<F: Future> std::future::Future for TimeoutAwaiter<F> {
    type Output = Result<F::Output, AwaitError>;

    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        use std::task::Poll as StdPoll;

        let this = unsafe { self.get_unchecked_mut() };

        // Check for timeout
        if std::time::Instant::now() >= this.deadline {
            return StdPoll::Ready(Err(AwaitError::Timeout));
        }

        // Poll the inner future
        if let Some(ref mut future) = this.future {
            unsafe {
                let pinned = Pin::new_unchecked(future);
                // Create a waker that delegates to the std waker
                let waker = cx.waker().clone();
                let mut our_cx = Context::new(&waker, 0);
                match pinned.poll(&mut our_cx) {
                    Poll::Ready(result) => {
                        this.future = None;
                        StdPoll::Ready(Ok(result))
                    }
                    Poll::Pending => {
                        // Register for wakeup and continue waiting
                        cx.waker().wake_by_ref();
                        StdPoll::Pending
                    }
                }
            }
        } else {
            panic!("TimeoutAwaiter polled after completion")
        }
    }
}

/// Creates a waker that will resume the current task when invoked.
///
/// This is used by futures to register for notification when they become ready.
pub fn create_await_waker(_task_id: TaskId) -> Waker {
    // In a full implementation, this would create a waker that properly
    // reschedules the task when wake() is called
    create_simple_waker()
}

/// Creates a simple waker that does nothing.
fn create_simple_waker() -> Waker {
    static VTABLE: RawWakerVTable = RawWakerVTable::new(
        |_| RawWaker::new(std::ptr::null(), &VTABLE),
        |_| {},
        |_| {},
        |_| {},
    );

    unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &VTABLE)) }
}

/// A future that can be used to selectively await based on a condition.
///
/// This is similar to `select!` in other async runtimes.
pub struct Select<F1, F2> {
    future1: F1,
    future2: F2,
    _pinned: PhantomPinned,
}

impl<F1, F2> Select<F1, F2> {
    /// Creates a new select future.
    pub fn new(future1: F1, future2: F2) -> Self {
        Self {
            future1,
            future2,
            _pinned: PhantomPinned,
        }
    }
}

/// The result of a select operation.
pub enum SelectResult<A, B> {
    /// The first future completed.
    First(A),
    /// The second future completed.
    Second(B),
}

impl<F1, F2> Future for Select<F1, F2>
where
    F1: Future + Unpin,
    F2: Future + Unpin,
{
    type Output = SelectResult<F1::Output, F2::Output>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };

        // Try polling the first future
        match Pin::new(&mut this.future1).poll(cx) {
            Poll::Ready(result) => return Poll::Ready(SelectResult::First(result)),
            Poll::Pending => {}
        }

        // Try polling the second future
        match Pin::new(&mut this.future2).poll(cx) {
            Poll::Ready(result) => Poll::Ready(SelectResult::Second(result)),
            Poll::Pending => Poll::Pending,
        }
    }
}

/// A future that completes when a condition becomes true.
///
/// This is useful for waiting on state changes.
pub struct WaitUntil<F> {
    condition: F,
    _pinned: PhantomPinned,
}

impl<F> WaitUntil<F> {
    /// Creates a new WaitUntil future.
    pub fn new(condition: F) -> Self {
        Self {
            condition,
            _pinned: PhantomPinned,
        }
    }
}

impl<F> Future for WaitUntil<F>
where
    F: FnMut() -> bool + Unpin,
{
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };

        if (this.condition)() {
            Poll::Ready(())
        } else {
            // Re-register for polling
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::future::AsyncFn;

    fn dummy_context<'a>() -> Context<'a> {
        // Use a static waker to avoid lifetime issues
        static STATIC_VTABLE: RawWakerVTable = RawWakerVTable::new(
            |_| RawWaker::new(std::ptr::null(), &STATIC_VTABLE),
            |_| {},
            |_| {},
            |_| {},
        );
        static STATIC_WAKER: std::sync::OnceLock<Waker> = std::sync::OnceLock::new();
        let waker = STATIC_WAKER.get_or_init(|| unsafe {
            Waker::from_raw(RawWaker::new(std::ptr::null(), &STATIC_VTABLE))
        });
        Context::new(waker, 1)
    }

    #[test]
    fn test_await_error_display() {
        assert_eq!(format!("{}", AwaitError::Cancelled), "task was cancelled");
        assert_eq!(format!("{}", AwaitError::Panicked), "awaited task panicked");
        assert_eq!(
            format!("{}", AwaitError::Timeout),
            "await operation timed out"
        );
    }

    #[test]
    fn test_await_all() {
        // Use the same closure type by creating a helper function
        fn make_async_fn(n: i32) -> AsyncFn<impl FnOnce() -> i32, i32> {
            AsyncFn::new(move || n)
        }

        let futures: Vec<AsyncFn<_, i32>> =
            vec![make_async_fn(1), make_async_fn(2), make_async_fn(3)];

        // Use Box::pin since AwaitAll contains PhantomPinned
        let mut await_all = Box::pin(AwaitAll {
            futures: futures.into_iter().map(Some).collect(),
            results: vec![None, None, None],
            _pinned: PhantomPinned,
        });
        let mut cx = dummy_context();

        loop {
            match await_all.as_mut().poll(&mut cx) {
                Poll::Ready(results) => {
                    assert_eq!(results, vec![1, 2, 3]);
                    break;
                }
                Poll::Pending => {}
            }
        }
    }

    #[test]
    fn test_await_any() {
        // Use the same closure type by creating a helper function
        fn make_async_fn(n: i32) -> AsyncFn<impl FnOnce() -> i32, i32> {
            AsyncFn::new(move || n)
        }

        let futures: Vec<AsyncFn<_, i32>> = vec![make_async_fn(1), make_async_fn(2)];

        // Use Box::pin since AwaitAny contains PhantomPinned
        let mut await_any = Box::pin(AwaitAny {
            futures,
            _pinned: PhantomPinned,
        });
        let mut cx = dummy_context();

        loop {
            match await_any.as_mut().poll(&mut cx) {
                Poll::Ready((result, index, remaining)) => {
                    // One of the futures should have completed
                    assert!([1, 2].contains(&result));
                    assert!(index < 2);
                    assert_eq!(remaining.len(), 1);
                    break;
                }
                Poll::Pending => {}
            }
        }
    }

    #[test]
    fn test_select() {
        fn make_async_str(s: &'static str) -> AsyncFn<impl FnOnce() -> &'static str, &'static str> {
            AsyncFn::new(move || s)
        }

        let future1 = make_async_str("first");
        let future2 = make_async_str("second");

        // Use Box::pin since Select contains PhantomPinned
        let mut select = Box::pin(Select::new(future1, future2));
        let mut cx = dummy_context();

        loop {
            match select.as_mut().poll(&mut cx) {
                Poll::Ready(result) => {
                    match result {
                        SelectResult::First(val) => assert_eq!(val, "first"),
                        SelectResult::Second(val) => assert_eq!(val, "second"),
                    }
                    break;
                }
                Poll::Pending => {}
            }
        }
    }

    #[test]
    fn test_wait_until() {
        let mut counter = 0;
        let condition = || {
            counter += 1;
            counter >= 3
        };

        // Use Box::pin since WaitUntil contains PhantomPinned
        let mut wait = Box::pin(WaitUntil::new(condition));
        let mut cx = dummy_context();

        loop {
            match wait.as_mut().poll(&mut cx) {
                Poll::Ready(()) => {
                    assert!(counter >= 3);
                    break;
                }
                Poll::Pending => {}
            }
        }
    }

    #[test]
    fn test_timeout_awaiter_success() {
        let future = AsyncFn::new(|| 42);
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);

        // SAFETY: TimeoutAwaiter contains PhantomPinned, so we need to pin it on the heap
        let mut timeout = Box::pin(TimeoutAwaiter {
            future: Some(future),
            deadline,
            _pinned: PhantomPinned,
        });
        let mut cx = dummy_context();

        loop {
            match timeout.as_mut().poll(&mut cx) {
                Poll::Ready(Ok(result)) => {
                    assert_eq!(result, 42);
                    break;
                }
                Poll::Ready(Err(_)) => panic!("Should not timeout"),
                Poll::Pending => {}
            }
        }
    }

    #[test]
    fn test_timeout_awaiter_expired() {
        // Create a future that never completes
        struct NeverCompletes;
        impl Future for NeverCompletes {
            type Output = i32;
            fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
                Poll::Pending
            }
        }

        let deadline = std::time::Instant::now() - std::time::Duration::from_millis(1);
        // SAFETY: TimeoutAwaiter contains PhantomPinned, so we need to pin it on the heap
        let mut timeout = Box::pin(TimeoutAwaiter {
            future: Some(NeverCompletes),
            deadline,
            _pinned: PhantomPinned,
        });
        let mut cx = dummy_context();

        match timeout.as_mut().poll(&mut cx) {
            Poll::Ready(Err(AwaitError::Timeout)) => {}
            _ => panic!("Expected timeout error"),
        }
    }
}
