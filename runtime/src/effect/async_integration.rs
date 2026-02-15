//! Async/await integration for effect handlers.
//!
//! This module provides integration between the effect system and the
//! async runtime. It allows effect handlers to work correctly in async
//! contexts, including:
//!
//! - Suspending tasks when effects are performed
//! - Resuming tasks when handlers complete
//! - Coordinating with the scheduler for async effect handling
//!
//! # Async Effect Handling
//!
//! When an effect is performed in an async context, the task may need
//! to be suspended until the effect is handled. This module provides
//! the mechanisms to coordinate this suspension and resumption.

use std::any::Any;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::task::{Context as TaskContext, Poll, Waker};

use jet_rt_sched::{Task, TaskId};

use super::continuation::{ContinuationHandle, ContinuationId};
use super::dispatch::{OperationId, PerformResult};
use super::handler::{EffectHandler, HandlerResult};
use super::stack::EffectStack;
use super::EffectTypeId;
use super::EffectValue;

/// A future that represents an async effect operation.
///
/// This future completes when the effect is handled and the continuation
/// is resumed with a value.
pub struct AsyncEffectFuture {
    /// The ID of the continuation waiting for this effect.
    continuation_id: ContinuationId,
    /// Shared state for the async operation.
    state: Arc<Mutex<AsyncEffectState>>,
}

/// Shared state for an async effect operation.
struct AsyncEffectState {
    /// Whether the effect has completed.
    completed: bool,
    /// The result value (if completed).
    result: Option<Box<dyn Any + Send>>,
    /// The waker to notify when completed.
    waker: Option<Waker>,
}

impl AsyncEffectFuture {
    /// Creates a new async effect future.
    fn new(continuation_id: ContinuationId) -> Self {
        Self {
            continuation_id,
            state: Arc::new(Mutex::new(AsyncEffectState {
                completed: false,
                result: None,
                waker: None,
            })),
        }
    }

    /// Completes the async effect with a value.
    pub fn complete(&self, value: Box<dyn Any + Send>) {
        let mut state = self.state.lock().unwrap();
        state.completed = true;
        state.result = Some(value);
        if let Some(waker) = state.waker.take() {
            waker.wake();
        }
    }

    /// Returns the continuation ID for this future.
    pub fn continuation_id(&self) -> ContinuationId {
        self.continuation_id
    }
}

impl Future for AsyncEffectFuture {
    type Output = Box<dyn Any + Send>;

    fn poll(self: Pin<&mut Self>, cx: &mut TaskContext<'_>) -> Poll<Self::Output> {
        let mut state = self.state.lock().unwrap();

        if state.completed {
            Poll::Ready(state.result.take().expect("Result should be present"))
        } else {
            state.waker = Some(cx.waker().clone());
            Poll::Pending
        }
    }
}

/// An async effect handler that can suspend and resume.
///
/// This trait extends `EffectHandler` with async capabilities, allowing
/// handlers to perform async operations before resuming continuations.
pub trait AsyncEffectHandler: EffectHandler {
    /// Handles an effect operation asynchronously.
    ///
    /// Returns a future that completes when the effect is handled.
    fn handle_async(
        &mut self,
        operation: OperationId,
        args: Vec<EffectValue>,
        k: ContinuationHandle,
    ) -> Pin<Box<dyn Future<Output = HandlerResult> + Send + '_>>;

    /// Returns true if this handler should be invoked asynchronously.
    fn is_async(&self) -> bool {
        true
    }
}

/// A handle to an async effect operation in progress.
#[derive(Debug)]
pub struct AsyncEffectHandle {
    /// The task ID of the async handler.
    pub task_id: TaskId,
    /// The continuation ID waiting for completion.
    pub continuation_id: ContinuationId,
    /// Whether the operation has completed.
    completed: AtomicU64,
}

impl AsyncEffectHandle {
    /// Creates a new async effect handle.
    pub fn new(task_id: TaskId, continuation_id: ContinuationId) -> Self {
        Self {
            task_id,
            continuation_id,
            completed: AtomicU64::new(0),
        }
    }

    /// Marks the operation as completed.
    pub fn mark_completed(&self) {
        self.completed.store(1, Ordering::SeqCst);
    }

    /// Checks if the operation has completed.
    pub fn is_completed(&self) -> bool {
        self.completed.load(Ordering::SeqCst) != 0
    }
}

/// Registry for pending async effect operations.
pub struct AsyncEffectRegistry {
    /// Pending async operations.
    pending: Mutex<HashMap<ContinuationId, AsyncEffectHandle>>,
    /// Completed async operations waiting for their continuations to be resumed.
    completed: Mutex<HashMap<ContinuationId, Box<dyn Any + Send>>>,
}

impl AsyncEffectRegistry {
    /// Creates a new async effect registry.
    pub fn new() -> Self {
        Self {
            pending: Mutex::new(HashMap::new()),
            completed: Mutex::new(HashMap::new()),
        }
    }

    /// Registers a new async effect operation.
    pub fn register(&self, continuation_id: ContinuationId, handle: AsyncEffectHandle) {
        let mut pending = self.pending.lock().unwrap();
        pending.insert(continuation_id, handle);
    }

    /// Marks an async effect as completed.
    pub fn complete(&self, continuation_id: ContinuationId, result: Box<dyn Any + Send>) {
        // Remove from pending
        let mut pending = self.pending.lock().unwrap();
        if let Some(handle) = pending.remove(&continuation_id) {
            handle.mark_completed();
        }
        drop(pending);

        // Add to completed
        let mut completed = self.completed.lock().unwrap();
        completed.insert(continuation_id, result);
    }

    /// Takes a completed result.
    pub fn take_result(&self, continuation_id: ContinuationId) -> Option<Box<dyn Any + Send>> {
        let mut completed = self.completed.lock().unwrap();
        completed.remove(&continuation_id)
    }

    /// Checks if an operation is pending.
    pub fn is_pending(&self, continuation_id: ContinuationId) -> bool {
        let pending = self.pending.lock().unwrap();
        pending.contains_key(&continuation_id)
    }

    /// Returns the number of pending operations.
    pub fn pending_count(&self) -> usize {
        let pending = self.pending.lock().unwrap();
        pending.len()
    }
}

impl Default for AsyncEffectRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Performs an effect operation in an async context.
///
/// This function is used when an effect is performed from within an async
/// block. It properly suspends the current async task and sets up the
/// continuation to be resumed when the effect is handled.
///
/// # Safety
///
/// Must be called from within an async task context with a valid effect stack.
pub unsafe fn perform_effect_async(
    stack: &mut EffectStack,
    _effect_id: EffectTypeId,
    _operation: OperationId,
    _args: Vec<EffectValue>,
) -> AsyncEffectFuture {
    // Mark the current frame as async
    stack.set_async_context(true);

    // Create the future
    let continuation_id = ContinuationId(0); // Would be generated uniquely
                                             // Perform the effect (this will suspend the task)
                                             // In a real implementation, this would integrate with the scheduler
                                             // to properly suspend and resume the task

    AsyncEffectFuture::new(continuation_id)
}

/// Resumes a continuation asynchronously.
///
/// This is used by async handlers to resume continuations after async
/// operations complete.
///
/// # Safety
///
/// The continuation must be valid and not already consumed.
pub unsafe fn resume_continuation_async(
    continuation: ContinuationHandle,
    value: Box<dyn Any + Send>,
    registry: &AsyncEffectRegistry,
) {
    // Mark the operation as completed in the registry
    registry.complete(continuation.id(), value);

    // In a real implementation, this would also notify the scheduler
    // to resume the suspended task
}

/// A wrapper for running effectful code in an async context.
pub struct AsyncEffectContext {
    /// The effect stack for this async context.
    stack: EffectStack,
    /// Registry for async operations.
    registry: Arc<AsyncEffectRegistry>,
}

impl AsyncEffectContext {
    /// Creates a new async effect context for a task.
    ///
    /// # Safety
    ///
    /// The task pointer must remain valid.
    pub unsafe fn new(task: NonNull<Task>) -> Self {
        let mut stack = EffectStack::new(task);
        stack.set_async_context(true);

        Self {
            stack,
            registry: Arc::new(AsyncEffectRegistry::new()),
        }
    }

    /// Performs an effect operation.
    pub fn perform(
        &mut self,
        effect_id: EffectTypeId,
        operation: OperationId,
        args: Vec<EffectValue>,
    ) -> PerformResult {
        unsafe { self.stack.perform(effect_id, operation, args) }
    }

    /// Performs an effect operation asynchronously.
    pub async fn perform_async(
        &mut self,
        effect_id: EffectTypeId,
        operation: OperationId,
        args: Vec<EffectValue>,
    ) -> Box<dyn Any + Send> {
        let future = unsafe { perform_effect_async(&mut self.stack, effect_id, operation, args) };
        future.await
    }

    /// Returns a reference to the effect stack.
    pub fn stack(&self) -> &EffectStack {
        &self.stack
    }

    /// Returns a mutable reference to the effect stack.
    pub fn stack_mut(&mut self) -> &mut EffectStack {
        &mut self.stack
    }

    /// Returns a reference to the async registry.
    pub fn registry(&self) -> &Arc<AsyncEffectRegistry> {
        &self.registry
    }
}

/// A future that wraps an effectful computation.
pub struct EffectfulFuture<F> {
    /// The computation to run.
    computation: Option<F>,
    /// The effect stack for this computation.
    #[allow(dead_code)]
    stack: Option<EffectStack>,
}

impl<F, T> EffectfulFuture<F>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    /// Creates a new effectful future.
    ///
    /// # Safety
    ///
    /// Must be called from within a task context.
    pub unsafe fn new(computation: F, stack: EffectStack) -> Self {
        Self {
            computation: Some(computation),
            stack: Some(stack),
        }
    }
}

impl<F, T> Future for EffectfulFuture<F>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    type Output = T;

    fn poll(self: Pin<&mut Self>, _cx: &mut TaskContext<'_>) -> Poll<Self::Output> {
        // In a real implementation, this would:
        // 1. Set up the effect stack
        // 2. Run the computation
        // 3. Handle any async effects
        // 4. Return the result

        let this = unsafe { self.get_unchecked_mut() };
        if let Some(computation) = this.computation.take() {
            let result = computation();
            Poll::Ready(result)
        } else {
            Poll::Pending
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_async_effect_state() {
        let state = Arc::new(Mutex::new(AsyncEffectState {
            completed: false,
            result: None,
            waker: None,
        }));

        // Initially not completed
        {
            let s = state.lock().unwrap();
            assert!(!s.completed);
        }

        // Complete
        {
            let mut s = state.lock().unwrap();
            s.completed = true;
            s.result = Some(Box::new(42i32));
        }

        {
            let s = state.lock().unwrap();
            assert!(s.completed);
            assert!(s.result.is_some());
        }
    }

    #[test]
    fn test_async_effect_handle() {
        let handle = AsyncEffectHandle::new(1u64, ContinuationId(1));

        assert!(!handle.is_completed());
        assert_eq!(handle.task_id, 1u64);
        assert_eq!(handle.continuation_id, ContinuationId(1));

        handle.mark_completed();
        assert!(handle.is_completed());
    }

    #[test]
    fn test_async_effect_registry() {
        let registry = AsyncEffectRegistry::new();

        assert_eq!(registry.pending_count(), 0);

        // Register an operation
        let handle = AsyncEffectHandle::new(1u64, ContinuationId(1));
        registry.register(ContinuationId(1), handle);

        assert_eq!(registry.pending_count(), 1);
        assert!(registry.is_pending(ContinuationId(1)));

        // Complete the operation
        registry.complete(ContinuationId(1), Box::new(42i32));

        assert!(!registry.is_pending(ContinuationId(1)));
        assert_eq!(registry.pending_count(), 0);

        // Take the result
        let result = registry.take_result(ContinuationId(1));
        assert!(result.is_some());
        assert_eq!(*result.unwrap().downcast::<i32>().unwrap(), 42);
    }

    // Note: Full async integration tests require the scheduler
    // and are in the integration tests.
}
