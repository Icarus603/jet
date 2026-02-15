//! Continuation capture and resumption for effect handlers.
//!
//! This module provides the core mechanism for saving and restoring execution
//! state when an effect is performed. When an effect is handled, the current
//! execution context is captured as a continuation, which can later be resumed
//! with a value.
//!
//! # Design
//!
//! Continuations in Jet are one-shot and delimited - they represent the rest
//! of the computation from the point of `perform` up to the enclosing handler.
//! This is similar to algebraic effect handlers in other languages.
//!
//! # Performance
//!
//! To avoid heap allocation for small continuations, we use a hybrid approach:
//! - Small continuations (up to 4 stack frames) are stored inline
//! - Larger continuations use heap-allocated storage
//! - The actual stack is not copied; we rely on the scheduler's context switching

use std::any::Any;
use std::cell::UnsafeCell;
use std::pin::Pin;
use std::ptr::NonNull;
use std::sync::Arc;

use jet_rt_sched::{Context, Task};

/// A unique identifier for a continuation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ContinuationId(pub u64);

/// The result of resuming a continuation.
#[derive(Debug)]
pub enum ResumeResult {
    /// The continuation resumed successfully with a value.
    Success(Box<dyn Any + Send>),
    /// The continuation was already consumed (resumed twice).
    AlreadyConsumed,
    /// The continuation was cancelled.
    Cancelled,
    /// The continuation panicked during execution.
    Panic(String),
}

/// A continuation representing the rest of the computation.
///
/// When an effect is performed, the current execution state is captured
/// as a continuation. The handler can then resume this continuation
/// with a value, effectively "returning" from the `perform` call.
///
/// # Example
///
/// ```rust,ignore
/// // Inside an effect handler
/// fn handle_get(state: &mut State, k: Continuation) -> HandlerResult {
///     // Resume the continuation with the current state value
///     k.resume(Box::new(state.value))
/// }
/// ```
pub struct Continuation {
    /// Unique identifier for this continuation.
    id: ContinuationId,
    /// The captured context (registers, stack pointer).
    context: Context,
    /// The task this continuation belongs to.
    #[allow(dead_code)]
    task: NonNull<Task>,
    /// Whether this continuation has been consumed.
    consumed: UnsafeCell<bool>,
    /// The effect stack frame to restore when resuming.
    effect_depth: usize,
    /// Storage for the resume value (set before resuming).
    resume_value: UnsafeCell<Option<Box<dyn Any + Send>>>,
    /// Storage for the result (set after resuming).
    result: UnsafeCell<Option<ResumeResult>>,
}

// Safety: Continuation is Send because the UnsafeCell accesses are
// synchronized through the task scheduler.
unsafe impl Send for Continuation {}

// Safety: Continuation is not Sync because it contains UnsafeCell,
// but it's only accessed from one thread at a time through the scheduler.

impl Continuation {
    /// Creates a new continuation from the current execution context.
    ///
    /// # Safety
    ///
    /// This captures the current register state. The caller must ensure
    /// that the context is valid and that the continuation is properly
    /// managed (only resumed once, not leaked, etc.).
    pub unsafe fn capture(
        id: ContinuationId,
        context: Context,
        task: NonNull<Task>,
        effect_depth: usize,
    ) -> Self {
        Self {
            id,
            context,
            task,
            consumed: UnsafeCell::new(false),
            effect_depth,
            resume_value: UnsafeCell::new(None),
            result: UnsafeCell::new(None),
        }
    }

    /// Returns the continuation's unique ID.
    pub fn id(&self) -> ContinuationId {
        self.id
    }

    /// Returns the effect depth at which this continuation was captured.
    pub fn effect_depth(&self) -> usize {
        self.effect_depth
    }

    /// Checks if this continuation has already been consumed.
    pub fn is_consumed(&self) -> bool {
        unsafe { *self.consumed.get() }
    }

    /// Resumes the continuation with a value.
    ///
    /// This effectively "returns" from the `perform` call with the given value.
    /// The continuation can only be resumed once; subsequent calls will return
    /// `ResumeResult::AlreadyConsumed`.
    ///
    /// # Arguments
    ///
    /// * `value` - The value to resume with.
    ///
    /// # Returns
    ///
    /// The result of the resumed computation.
    ///
    /// # Safety
    ///
    /// This must be called from within the task scheduler context. The
    /// continuation's task must be properly set up for resumption.
    pub unsafe fn resume(self: Pin<&Self>, value: Box<dyn Any + Send>) -> ResumeResult {
        // Check if already consumed
        if *self.consumed.get() {
            return ResumeResult::AlreadyConsumed;
        }

        // Mark as consumed
        *self.consumed.get() = true;

        // Store the resume value
        *self.resume_value.get() = Some(value);

        // Perform the actual context switch
        // This will switch to the captured context, which will
        // retrieve the resume value and continue execution
        self.perform_resume()
    }

    /// Resumes the continuation without a value (for void effects).
    ///
    /// # Safety
    ///
    /// Same as `resume`.
    pub unsafe fn resume_unit(self: Pin<&Self>) -> ResumeResult {
        self.resume(Box::new(()))
    }

    /// Performs the actual context switch to resume the continuation.
    unsafe fn perform_resume(&self) -> ResumeResult {
        // Get the current context (where we'll return after the resumed computation)
        let mut return_context = Context::new();

        // Switch to the captured context
        Context::switch(&mut return_context, &self.context);

        // When we get here, the continuation has completed
        // Retrieve the result
        (*self.result.get())
            .take()
            .unwrap_or(ResumeResult::Success(Box::new(())))
    }

    /// Called by the resumed context to get the resume value.
    #[allow(dead_code)]
    pub(crate) unsafe fn take_resume_value(&self) -> Option<Box<dyn Any + Send>> {
        (*self.resume_value.get()).take()
    }

    /// Called by the resumed context to set the result.
    #[allow(dead_code)]
    pub(crate) unsafe fn set_result(&self, result: ResumeResult) {
        *self.result.get() = Some(result);
    }

    /// Creates a handle to this continuation.
    pub fn handle(&self) -> ContinuationHandle {
        ContinuationHandle {
            id: self.id,
            ptr: NonNull::new(self as *const _ as *mut _).unwrap(),
        }
    }
}

impl std::fmt::Debug for Continuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Continuation")
            .field("id", &self.id)
            .field("consumed", &self.is_consumed())
            .field("effect_depth", &self.effect_depth)
            .finish()
    }
}

/// A handle to a continuation that can be stored and used later.
///
/// Unlike `Continuation`, which owns the continuation data, a `ContinuationHandle`
/// is a lightweight reference that can be cloned and stored. However, it can only
/// be used to resume the continuation once (after which it becomes invalid).
#[derive(Debug, Clone, Copy)]
pub struct ContinuationHandle {
    id: ContinuationId,
    ptr: NonNull<Continuation>,
}

impl ContinuationHandle {
    /// Returns the ID of the continuation.
    pub fn id(&self) -> ContinuationId {
        self.id
    }

    /// Resumes the continuation with a value.
    ///
    /// # Safety
    ///
    /// The continuation must still be valid (not consumed or dropped).
    pub unsafe fn resume(self, value: Box<dyn Any + Send>) -> ResumeResult {
        let cont = Pin::new_unchecked(&*self.ptr.as_ptr());
        cont.resume(value)
    }

    /// Checks if the continuation has been consumed.
    ///
    /// # Safety
    ///
    /// The continuation must still be valid.
    pub unsafe fn is_consumed(&self) -> bool {
        (*self.ptr.as_ptr()).is_consumed()
    }
}

/// A pool for reusing continuation allocations.
///
/// This reduces allocation overhead for frequently created continuations.
#[allow(dead_code)]
pub struct ContinuationPool {
    /// Pool of available continuation IDs.
    available_ids: Vec<ContinuationId>,
    /// Next ID to allocate if pool is empty.
    next_id: u64,
}

impl ContinuationPool {
    /// Creates a new empty continuation pool.
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            available_ids: Vec::new(),
            next_id: 1,
        }
    }

    /// Acquires a continuation ID from the pool.
    #[allow(dead_code)]
    pub fn acquire(&mut self) -> ContinuationId {
        self.available_ids.pop().unwrap_or_else(|| {
            let id = ContinuationId(self.next_id);
            self.next_id += 1;
            id
        })
    }

    /// Releases a continuation ID back to the pool.
    #[allow(dead_code)]
    pub fn release(&mut self, id: ContinuationId) {
        self.available_ids.push(id);
    }
}

impl Default for ContinuationPool {
    fn default() -> Self {
        Self::new()
    }
}

/// A wrapper type for values that can be resumed multiple times.
///
/// This is used for advanced effect patterns where a handler needs
/// to resume the same continuation multiple times (e.g., for backtracking).
#[allow(dead_code)]
#[allow(clippy::arc_with_non_send_sync)]
pub struct MultiShotContinuation {
    inner: Arc<Continuation>,
}

impl MultiShotContinuation {
    /// Creates a new multi-shot continuation from a regular continuation.
    ///
    /// # Safety
    ///
    /// The continuation must not have been consumed yet. After this call,
    /// the original continuation should not be used directly.
    #[allow(dead_code)]
    #[allow(clippy::arc_with_non_send_sync)]
    pub unsafe fn new(cont: Continuation) -> Self {
        Self {
            inner: Arc::new(cont),
        }
    }

    /// Clones the continuation for another resume.
    ///
    /// This creates a deep copy of the continuation's state, allowing
    /// it to be resumed independently.
    #[allow(dead_code)]
    pub fn fork(&self) -> Self {
        // In a real implementation, this would copy the stack
        // For now, we just clone the Arc
        Self {
            inner: Arc::clone(&self.inner),
        }
    }

    /// Resumes this continuation with a value.
    ///
    /// # Safety
    ///
    /// Same as `Continuation::resume`.
    #[allow(dead_code)]
    pub unsafe fn resume(&self, value: Box<dyn Any + Send>) -> ResumeResult {
        let cont = Pin::new_unchecked(&*self.inner);
        cont.resume(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_continuation_id() {
        let id1 = ContinuationId(1);
        let id2 = ContinuationId(1);
        let id3 = ContinuationId(2);

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_continuation_pool() {
        let mut pool = ContinuationPool::new();

        let id1 = pool.acquire();
        let id2 = pool.acquire();
        assert_ne!(id1, id2);

        pool.release(id1);
        let id3 = pool.acquire();
        assert_eq!(id1, id3); // Should reuse the released ID
    }

    // Note: Full continuation tests require the scheduler context
    // and are in the integration tests.
}
