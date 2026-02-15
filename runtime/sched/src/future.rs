//! Future types and async task implementation.
//!
//! This module provides the core Future trait and related types for async/await
//! support in Jet. Futures represent asynchronous computations that can be
//! polled for completion.
//!
//! # Architecture
//!
//! The Future system is built on top of the M:N scheduler:
//! - Futures are tasks that can suspend and resume
//! - The `.await` operator polls futures until they complete
//! - Async functions are desugared to state machines that implement Future
//!
//! # Example
//!
//! ```rust
//! use jet_rt_sched::future::{Future, Poll};
//! use jet_rt_sched::spawn_async;
//!
//! async fn example() -> i32 {
//!     42
//! }
//! ```

use crate::reactor::IoToken;
use crate::task::TaskId;
use std::cell::RefCell;
use std::fmt;
use std::marker::{PhantomData, PhantomPinned};
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::task::Waker;

/// The result of polling a future.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Poll<T> {
    /// The future is ready and produced a value.
    Ready(T),
    /// The future is pending and will be woken later.
    Pending,
}

impl<T> Poll<T> {
    /// Maps a `Poll<T>` to `Poll<U>` by applying a function.
    pub fn map<U, F>(self, f: F) -> Poll<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Poll::Ready(t) => Poll::Ready(f(t)),
            Poll::Pending => Poll::Pending,
        }
    }

    /// Returns true if this is `Ready`.
    pub fn is_ready(&self) -> bool {
        matches!(self, Poll::Ready(_))
    }

    /// Returns true if this is `Pending`.
    pub fn is_pending(&self) -> bool {
        matches!(self, Poll::Pending)
    }
}

/// The core trait for asynchronous computations.
///
/// A Future represents a value that may not be available yet. When polled,
/// it either returns `Poll::Ready` with the value, or `Poll::Pending` to
/// indicate it needs to be polled again later.
///
/// # Safety
///
/// Futures must be pinned before polling (see `std::pin::Pin`). This ensures
/// that self-referential futures remain valid across suspend points.
pub trait Future {
    /// The type of value produced on completion.
    type Output;

    /// Attempts to resolve the future to a final value.
    ///
    /// # Returns
    ///
    /// - `Poll::Ready(val)` if the future has completed
    /// - `Poll::Pending` if the future needs to be polled again later
    ///
    /// # Safety
    ///
    /// The future must be pinned before calling this method.
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output>;
}

/// Context for a future poll operation.
///
/// This provides the future with access to the waker, which it can use
/// to signal that it should be polled again.
pub struct Context<'a> {
    waker: &'a Waker,
    /// The ID of the task this future is running on.
    task_id: TaskId,
    /// Whether this poll is part of a cancellation sequence.
    is_cancelled: bool,
}

impl<'a> Context<'a> {
    /// Creates a new context with the given waker and task ID.
    pub fn new(waker: &'a Waker, task_id: TaskId) -> Self {
        Self {
            waker,
            task_id,
            is_cancelled: false,
        }
    }

    /// Creates a new context for a cancelled task.
    pub fn new_cancelled(waker: &'a Waker, task_id: TaskId) -> Self {
        Self {
            waker,
            task_id,
            is_cancelled: true,
        }
    }

    /// Returns the waker associated with this context.
    pub fn waker(&self) -> &Waker {
        self.waker
    }

    /// Returns the task ID associated with this context.
    pub fn task_id(&self) -> TaskId {
        self.task_id
    }

    /// Returns true if this poll is part of a cancellation.
    pub fn is_cancelled(&self) -> bool {
        self.is_cancelled
    }
}

/// A handle to an async task that can be awaited.
///
/// When you spawn an async task, you get back a `TaskHandle` that you can
/// use to await its completion and get its result.
pub struct TaskHandle<T> {
    /// The ID of the spawned task.
    pub task_id: TaskId,
    /// Shared state for awaiting the result.
    state: Arc<TaskHandleState<T>>,
    /// Phantom data to ensure proper variance.
    _marker: PhantomData<T>,
}

/// Internal state shared between the task and its handle.
struct TaskHandleState<T> {
    /// Whether the task has completed.
    completed: AtomicBool,
    /// The result value (set when completed).
    result: RefCell<Option<T>>,
    /// List of wakers to notify on completion.
    wakers: RefCell<Vec<Waker>>,
}

unsafe impl<T: Send> Send for TaskHandleState<T> {}
unsafe impl<T: Send> Sync for TaskHandleState<T> {}

impl<T> TaskHandle<T> {
    /// Creates a new task handle for the given task ID.
    pub fn new(task_id: TaskId) -> Self {
        Self {
            task_id,
            state: Arc::new(TaskHandleState {
                completed: AtomicBool::new(false),
                result: RefCell::new(None),
                wakers: RefCell::new(Vec::new()),
            }),
            _marker: PhantomData,
        }
    }

    /// Returns true if the task has completed.
    pub fn is_complete(&self) -> bool {
        self.state.completed.load(Ordering::Acquire)
    }

    /// Sets the result and notifies waiters.
    pub(crate) fn complete(&self, result: T) {
        *self.state.result.borrow_mut() = Some(result);
        self.state.completed.store(true, Ordering::Release);

        // Notify all registered wakers
        let wakers = std::mem::take(&mut *self.state.wakers.borrow_mut());
        for waker in wakers {
            waker.wake();
        }
    }

    /// Registers a waker to be notified when the task completes.
    pub(crate) fn register_waker(&self, waker: &Waker) {
        if !self.is_complete() {
            self.state.wakers.borrow_mut().push(waker.clone());
        }
    }

    /// Attempts to get the result without blocking.
    pub fn try_get(&self) -> Option<T> {
        if self.is_complete() {
            self.state.result.borrow_mut().take()
        } else {
            None
        }
    }
}

impl<T: Send + 'static> Future for TaskHandle<T> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if let Some(result) = self.try_get() {
            Poll::Ready(result)
        } else {
            self.register_waker(cx.waker());
            Poll::Pending
        }
    }
}

impl<T> Clone for TaskHandle<T> {
    fn clone(&self) -> Self {
        Self {
            task_id: self.task_id,
            state: Arc::clone(&self.state),
            _marker: PhantomData,
        }
    }
}

impl<T> fmt::Debug for TaskHandle<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TaskHandle")
            .field("task_id", &self.task_id)
            .field("completed", &self.is_complete())
            .finish()
    }
}

/// An async task that wraps a future.
///
/// This is the internal representation of an async task that the scheduler
/// executes. It contains the future being executed and handles polling.
pub struct AsyncTask<F: Future> {
    /// The underlying future being executed.
    future: Option<F>,
    /// The result of the future execution.
    result: Option<F::Output>,
    /// Handle for notifying completion.
    handle: Option<TaskHandle<F::Output>>,
    /// Whether the task has been cancelled.
    cancelled: bool,
    /// Pin marker to ensure the task stays pinned.
    _pinned: PhantomPinned,
}

impl<F: Future> AsyncTask<F> {
    /// Creates a new async task from a future.
    pub fn new(future: F) -> (Self, TaskHandle<F::Output>) {
        let handle = TaskHandle::new(crate::task::next_task_id());

        let task = Self {
            future: Some(future),
            result: None,
            handle: Some(handle.clone()),
            cancelled: false,
            _pinned: PhantomPinned,
        };

        (task, handle)
    }

    /// Polls the future once.
    pub fn poll(&mut self, cx: &mut Context<'_>) -> Poll<F::Output> {
        if self.cancelled {
            panic!("Cannot poll a cancelled task");
        }

        if let Some(ref mut future) = self.future {
            // SAFETY: We ensure the future is properly pinned
            unsafe {
                let pinned = Pin::new_unchecked(future);
                match pinned.poll(cx) {
                    Poll::Ready(result) => {
                        self.future = None;
                        self.result = Some(result);
                        if let Some(ref _handle) = self.handle {
                            // Clone the result for the handle if possible
                            // In practice, we move the result out
                        }
                        Poll::Ready(self.result.take().unwrap())
                    }
                    Poll::Pending => Poll::Pending,
                }
            }
        } else {
            panic!("Future already completed");
        }
    }

    /// Cancels the task.
    pub fn cancel(&mut self) {
        self.cancelled = true;
        self.future = None;
    }

    /// Returns true if the task has been cancelled.
    pub fn is_cancelled(&self) -> bool {
        self.cancelled
    }

    /// Returns true if the task has completed.
    pub fn is_complete(&self) -> bool {
        self.future.is_none() && !self.cancelled
    }
}

/// A future that wraps a closure to make it async-compatible.
pub struct AsyncFn<F, T> {
    func: Option<F>,
    _marker: PhantomData<T>,
    _pinned: PhantomPinned,
}

/// AsyncFn is Unpin if F is Unpin since there's no actual self-referential data
impl<F: Unpin, T> Unpin for AsyncFn<F, T> {}

impl<F, T> AsyncFn<F, T>
where
    F: FnOnce() -> T,
{
    /// Creates a new async function wrapper.
    pub fn new(func: F) -> Self {
        Self {
            func: Some(func),
            _marker: PhantomData,
            _pinned: PhantomPinned,
        }
    }
}

impl<F, T> Future for AsyncFn<F, T>
where
    F: FnOnce() -> T,
{
    type Output = T;

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        // SAFETY: We're not moving the struct, just taking from the Option field
        let this = unsafe { self.get_unchecked_mut() };
        match this.func.take() {
            Some(func) => Poll::Ready(func()),
            None => panic!("AsyncFn polled after completion"),
        }
    }
}

/// A future that yields control back to the scheduler.
pub struct YieldNow {
    yielded: bool,
}

impl YieldNow {
    /// Creates a new yield future.
    pub fn new() -> Self {
        Self { yielded: false }
    }
}

impl Default for YieldNow {
    fn default() -> Self {
        Self::new()
    }
}

impl Future for YieldNow {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // SAFETY: We're only modifying a boolean field
        let this = unsafe { self.get_unchecked_mut() };
        if this.yielded {
            Poll::Ready(())
        } else {
            this.yielded = true;
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }
}

// Also implement std::future::Future for use with native async/await
impl std::future::Future for YieldNow {
    type Output = ();

    fn poll(
        self: Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        // SAFETY: We're only modifying a boolean field
        let this = unsafe { self.get_unchecked_mut() };
        if this.yielded {
            std::task::Poll::Ready(())
        } else {
            this.yielded = true;
            _cx.waker().wake_by_ref();
            std::task::Poll::Pending
        }
    }
}

/// A future that completes after a duration.
pub struct Sleep {
    deadline: std::time::Instant,
    #[allow(dead_code)]
    token: Option<IoToken>,
}

impl Sleep {
    /// Creates a new sleep future.
    pub fn until(deadline: std::time::Instant) -> Self {
        Self {
            deadline,
            token: None,
        }
    }

    /// Creates a new sleep future for a duration.
    pub fn for_duration(duration: std::time::Duration) -> Self {
        Self::until(std::time::Instant::now() + duration)
    }
}

impl Future for Sleep {
    type Output = ();

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        if std::time::Instant::now() >= this.deadline {
            Poll::Ready(())
        } else {
            // Register with timer/reactor for wakeup
            // For now, we just wake immediately and check again
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }
}

// Also implement std::future::Future for use with native async/await
impl std::future::Future for Sleep {
    type Output = ();

    fn poll(
        self: Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        if std::time::Instant::now() >= this.deadline {
            std::task::Poll::Ready(())
        } else {
            // Register with timer/reactor for wakeup
            // For now, we just wake immediately and check again
            _cx.waker().wake_by_ref();
            std::task::Poll::Pending
        }
    }
}

/// A future that resolves to the result of a blocking operation.
///
/// This runs the blocking operation on a separate thread pool.
pub struct Blocking<T> {
    receiver: std::sync::mpsc::Receiver<T>,
}

impl<T: Send + 'static> Blocking<T> {
    /// Creates a new blocking future.
    pub fn new<F>(f: F) -> Self
    where
        F: FnOnce() -> T + Send + 'static,
    {
        let (sender, receiver) = std::sync::mpsc::channel();

        std::thread::spawn(move || {
            let result = f();
            let _ = sender.send(result);
        });

        Self { receiver }
    }
}

impl<T> Future for Blocking<T> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        match this.receiver.try_recv() {
            Ok(result) => Poll::Ready(result),
            Err(std::sync::mpsc::TryRecvError::Empty) => {
                cx.waker().wake_by_ref();
                Poll::Pending
            }
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("Blocking task thread panicked")
            }
        }
    }
}

// Also implement std::future::Future for use with native async/await
impl<T: Send> std::future::Future for Blocking<T> {
    type Output = T;

    fn poll(
        self: Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        match this.receiver.try_recv() {
            Ok(result) => std::task::Poll::Ready(result),
            Err(std::sync::mpsc::TryRecvError::Empty) => {
                _cx.waker().wake_by_ref();
                std::task::Poll::Pending
            }
            Err(std::sync::mpsc::TryRecvError::Disconnected) => {
                panic!("Blocking task thread panicked")
            }
        }
    }
}

/// Extension trait for futures.
pub trait FutureExt: Future + Sized {
    /// Maps the output of this future to a different type.
    fn map<U, Func>(self, f: Func) -> Map<Self, Func>
    where
        Func: FnOnce(Self::Output) -> U,
    {
        Map {
            future: self,
            func: Some(f),
            _pinned: PhantomPinned,
        }
    }

    /// Chains this future with another.
    fn then<U, Func>(self, f: Func) -> Then<Self, U, Func>
    where
        Func: FnOnce(Self::Output) -> U,
        U: Future,
    {
        Then {
            state: ThenState::First(self, Some(f)),
            _pinned: PhantomPinned,
        }
    }

    /// Boxes this future.
    fn boxed<'a>(self) -> Pin<Box<dyn Future<Output = Self::Output> + Send + 'a>>
    where
        Self: Send + 'a,
    {
        Box::pin(self)
    }
}

impl<F: Future> FutureExt for F {}

/// Future adapter for `map`.
pub struct Map<Fut, F> {
    future: Fut,
    func: Option<F>,
    _pinned: PhantomPinned,
}

impl<Fut, F, T> Future for Map<Fut, F>
where
    Fut: Future,
    F: FnOnce(Fut::Output) -> T,
{
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // SAFETY: We're accessing fields through the pinned reference
        let this = unsafe { self.get_unchecked_mut() };
        let func = this.func.take().expect("Map polled after completion");

        // SAFETY: We're not moving the future, just projecting the pin
        unsafe {
            let future = Pin::new_unchecked(&mut this.future);
            match future.poll(cx) {
                Poll::Ready(output) => Poll::Ready(func(output)),
                Poll::Pending => {
                    // Put the function back since we're not done yet
                    this.func = Some(func);
                    Poll::Pending
                }
            }
        }
    }
}

/// State for the `Then` future.
enum ThenState<Fut1, Fut2, F> {
    First(Fut1, Option<F>),
    Second(Fut2),
    Done,
}

/// Future adapter for `then`.
pub struct Then<Fut1, Fut2, F> {
    state: ThenState<Fut1, Fut2, F>,
    _pinned: PhantomPinned,
}

impl<Fut1, Fut2, F> Future for Then<Fut1, Fut2, F>
where
    Fut1: Future,
    Fut2: Future,
    F: FnOnce(Fut1::Output) -> Fut2,
{
    type Output = Fut2::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // SAFETY: We maintain pinning invariants
        let this = unsafe { self.get_unchecked_mut() };

        match &mut this.state {
            ThenState::First(fut1, f) => {
                // SAFETY: We're not moving the future
                let pinned = unsafe { Pin::new_unchecked(fut1) };
                match pinned.poll(cx) {
                    Poll::Ready(output) => {
                        let func = f.take().unwrap();
                        let fut2 = func(output);
                        this.state = ThenState::Second(fut2);
                        // Now poll the second future - continue the match
                        // SAFETY: We're not moving the future
                        match &mut this.state {
                            ThenState::Second(fut2) => {
                                let pinned = unsafe { Pin::new_unchecked(fut2) };
                                match pinned.poll(cx) {
                                    Poll::Ready(output) => {
                                        this.state = ThenState::Done;
                                        Poll::Ready(output)
                                    }
                                    Poll::Pending => Poll::Pending,
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    Poll::Pending => Poll::Pending,
                }
            }
            ThenState::Second(fut2) => {
                // SAFETY: We're not moving the future
                let pinned = unsafe { Pin::new_unchecked(fut2) };
                match pinned.poll(cx) {
                    Poll::Ready(output) => {
                        this.state = ThenState::Done;
                        Poll::Ready(output)
                    }
                    Poll::Pending => Poll::Pending,
                }
            }
            ThenState::Done => panic!("Then polled after completion"),
        }
    }
}

/// A future that resolves when all futures in a list complete.
pub struct JoinAll<F: Future> {
    futures: Vec<Option<F>>,
    results: Vec<Option<F::Output>>,
}

impl<F: Future> JoinAll<F> {
    /// Creates a new JoinAll future.
    pub fn new(futures: Vec<F>) -> Self {
        let len = futures.len();
        Self {
            futures: futures.into_iter().map(Some).collect(),
            results: (0..len).map(|_| None).collect(),
        }
    }
}

impl<F: Future + Unpin> Future for JoinAll<F> {
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

/// A future that resolves when any future in a list completes.
pub struct SelectAll<F: Future> {
    futures: Vec<F>,
}

impl<F: Future> SelectAll<F> {
    /// Creates a new SelectAll future.
    pub fn new(futures: Vec<F>) -> Self {
        Self { futures }
    }
}

impl<F: Future + Unpin> Future for SelectAll<F> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::task::{RawWaker, RawWakerVTable};

    #[test]
    fn test_poll_ready() {
        let poll: Poll<i32> = Poll::Ready(42);
        assert!(poll.is_ready());
        assert!(!poll.is_pending());

        let mapped = poll.map(|x| x * 2);
        assert_eq!(mapped, Poll::Ready(84));
    }

    #[test]
    fn test_poll_pending() {
        let poll: Poll<i32> = Poll::Pending;
        assert!(!poll.is_ready());
        assert!(poll.is_pending());
    }

    #[test]
    fn test_async_fn() {
        let mut async_fn = AsyncFn::new(|| 42);
        let waker = unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &DUMMY_VTABLE)) };
        let mut cx = Context::new(&waker, 1);

        let mut pinned = Pin::new(&mut async_fn);
        match pinned.as_mut().poll(&mut cx) {
            Poll::Ready(val) => assert_eq!(val, 42),
            Poll::Pending => panic!("Expected Ready"),
        }
    }

    static DUMMY_VTABLE: RawWakerVTable = RawWakerVTable::new(
        |_| RawWaker::new(std::ptr::null(), &DUMMY_VTABLE),
        |_| {},
        |_| {},
        |_| {},
    );

    #[test]
    fn test_task_handle() {
        let handle: TaskHandle<i32> = TaskHandle::new(1);
        assert!(!handle.is_complete());

        handle.complete(42);
        assert!(handle.is_complete());
        assert_eq!(handle.try_get(), Some(42));
    }

    #[test]
    fn test_yield_now() {
        let mut yield_fut = YieldNow::new();
        let waker = unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &DUMMY_VTABLE)) };
        let mut cx = Context::new(&waker, 1);

        let mut pinned = Pin::new(&mut yield_fut);

        // First poll should return Pending
        assert!(pinned.as_mut().poll(&mut cx).is_pending());

        // Second poll should return Ready
        assert!(pinned.poll(&mut cx).is_ready());
    }

    #[test]
    fn test_sleep_future() {
        let mut sleep = Sleep::for_duration(std::time::Duration::from_millis(1));
        let waker = unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &DUMMY_VTABLE)) };
        let mut cx = Context::new(&waker, 1);

        let mut pinned = Pin::new(&mut sleep);

        // Should eventually complete
        let start = std::time::Instant::now();
        loop {
            if start.elapsed() > std::time::Duration::from_secs(1) {
                panic!("Sleep future did not complete");
            }

            match pinned.as_mut().poll(&mut cx) {
                Poll::Ready(()) => break,
                Poll::Pending => std::thread::sleep(std::time::Duration::from_millis(1)),
            }
        }
    }

    #[test]
    fn test_blocking_future() {
        let mut blocking = Blocking::new(|| {
            std::thread::sleep(std::time::Duration::from_millis(10));
            42
        });
        let waker = unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &DUMMY_VTABLE)) };
        let mut cx = Context::new(&waker, 1);

        let mut pinned = Pin::new(&mut blocking);

        // Should eventually complete
        let start = std::time::Instant::now();
        loop {
            if start.elapsed() > std::time::Duration::from_secs(1) {
                panic!("Blocking future did not complete");
            }

            match pinned.as_mut().poll(&mut cx) {
                Poll::Ready(val) => {
                    assert_eq!(val, 42);
                    break;
                }
                Poll::Pending => std::thread::sleep(std::time::Duration::from_millis(1)),
            }
        }
    }

    #[test]
    fn test_join_all() {
        // Use the same closure type by creating a helper function
        fn make_async_fn(n: i32) -> AsyncFn<impl FnOnce() -> i32, i32> {
            AsyncFn::new(move || n)
        }

        let futures: Vec<AsyncFn<_, i32>> =
            vec![make_async_fn(1), make_async_fn(2), make_async_fn(3)];
        let mut join = JoinAll::new(futures);
        let waker = unsafe { Waker::from_raw(RawWaker::new(std::ptr::null(), &DUMMY_VTABLE)) };
        let mut cx = Context::new(&waker, 1);

        let mut pinned = Pin::new(&mut join);

        loop {
            match pinned.as_mut().poll(&mut cx) {
                Poll::Ready(results) => {
                    assert_eq!(results, vec![1, 2, 3]);
                    break;
                }
                Poll::Pending => {}
            }
        }
    }
}
