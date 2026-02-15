//! Effect stack management.
//!
//! This module implements the effect stack, which tracks active handlers
//! and continuations for each task. The stack supports nested handlers
//! and efficient lookup of handlers for effect dispatch.
//!
//! # Stack Structure
//!
//! EffectStack contains:
//! - HandlerTable (maps effects to handlers)
//!   - Handler A (effect_id = 1)
//!   - Handler B (effect_id = 2)
//!   - Handler C (effect_id = 3)
//! - ContinuationTable (active continuations)
//!   - Continuation 1
//!   - Continuation 2
//! - StackFrames (nested handler scopes)
//!   - Frame 0 (depth 0)
//!   - Frame 1 (depth 1)

use std::collections::HashMap;
use std::ptr::NonNull;

use jet_rt_sched::{Context, Task};

use super::continuation::{Continuation, ContinuationId};
use super::dispatch::{EffectDispatch, OperationId, PerformResult};
use super::handler::{HandlerId, HandlerTable};
use super::EffectTypeId;
use super::EffectValue;

/// A unique identifier for a stack frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackFrameId(pub u64);

/// A frame in the effect stack representing a handler scope.
#[derive(Debug)]
pub struct EffectFrame {
    /// The frame ID.
    pub id: StackFrameId,
    /// The depth of this frame (0 = root).
    pub depth: usize,
    /// The handler IDs active in this frame.
    pub handlers: Vec<HandlerId>,
    /// The parent frame (if any).
    pub parent: Option<StackFrameId>,
    /// Whether this frame is for an async context.
    pub is_async: bool,
    /// The task this frame belongs to.
    pub task: NonNull<Task>,
}

impl EffectFrame {
    /// Creates a new effect frame.
    pub fn new(id: StackFrameId, depth: usize, task: NonNull<Task>) -> Self {
        Self {
            id,
            depth,
            handlers: Vec::new(),
            parent: None,
            is_async: false,
            task,
        }
    }

    /// Adds a handler to this frame.
    pub fn add_handler(&mut self, handler_id: HandlerId) {
        self.handlers.push(handler_id);
    }

    /// Sets the async flag for this frame.
    pub fn set_async(&mut self, is_async: bool) {
        self.is_async = is_async;
    }
}

/// The effect stack for a task.
///
/// Each task has its own effect stack that tracks active handlers
/// and continuations. The stack is used to dispatch effect operations
/// to their handlers.
pub struct EffectStack {
    /// The handler table for this stack.
    handler_table: HandlerTable,
    /// Active continuations.
    continuations: HashMap<ContinuationId, Continuation>,
    /// Stack frames for nested scopes.
    frames: Vec<EffectFrame>,
    /// The current frame index.
    current_frame: usize,
    /// The task this stack belongs to.
    task: NonNull<Task>,
    /// The dispatch mechanism.
    dispatch: EffectDispatch,
    /// Counter for generating frame IDs.
    next_frame_id: u64,
    /// The saved context for resuming after an effect.
    resume_context: Option<Context>,
}

// Safety: EffectStack is Send because all its fields are Send
// and it's only accessed from one thread at a time through the scheduler.
unsafe impl Send for EffectStack {}

impl EffectStack {
    /// Creates a new effect stack for a task.
    ///
    /// # Safety
    ///
    /// The task pointer must remain valid for the lifetime of this stack.
    pub unsafe fn new(task: NonNull<Task>) -> Self {
        let root_frame = EffectFrame::new(StackFrameId(0), 0, task);

        Self {
            handler_table: HandlerTable::new(),
            continuations: HashMap::new(),
            frames: vec![root_frame],
            current_frame: 0,
            task,
            dispatch: EffectDispatch::new(),
            next_frame_id: 1,
            resume_context: None,
        }
    }

    /// Creates a new effect stack with default settings.
    ///
    /// This is useful for testing when you don't have a real task.
    pub fn new_test() -> Self {
        // Create a dummy task pointer - only for testing!
        let dummy_task = NonNull::dangling();
        unsafe { Self::new(dummy_task) }
    }

    /// Returns a reference to the handler table.
    pub fn handler_table(&self) -> &HandlerTable {
        &self.handler_table
    }

    /// Returns a mutable reference to the handler table.
    pub fn handler_table_mut(&mut self) -> &mut HandlerTable {
        &mut self.handler_table
    }

    /// Pushes a new frame onto the stack.
    pub fn push_frame(&mut self) -> StackFrameId {
        let id = StackFrameId(self.next_frame_id);
        self.next_frame_id += 1;

        let depth = self.frames.len();
        let parent = Some(self.frames[self.current_frame].id);

        let frame = EffectFrame {
            id,
            depth,
            handlers: Vec::new(),
            parent,
            is_async: false,
            task: self.task,
        };

        self.frames.push(frame);
        self.current_frame = depth;

        id
    }

    /// Pops the current frame from the stack.
    ///
    /// Returns `None` if trying to pop the root frame.
    pub fn pop_frame(&mut self) -> Option<EffectFrame> {
        if self.current_frame == 0 {
            return None;
        }

        let frame = self.frames.pop()?;
        self.current_frame -= 1;

        // Remove handlers that were in this frame
        let current_depth = self.frames[self.current_frame].depth;
        self.handler_table.remove_below_depth(current_depth + 1);

        Some(frame)
    }

    /// Returns the current frame.
    pub fn current_frame(&self) -> &EffectFrame {
        &self.frames[self.current_frame]
    }

    /// Returns the current frame (mutable).
    pub fn current_frame_mut(&mut self) -> &mut EffectFrame {
        &mut self.frames[self.current_frame]
    }

    /// Returns the current stack depth.
    pub fn depth(&self) -> usize {
        self.frames.len() - 1
    }

    /// Performs an effect operation.
    ///
    /// This is the main entry point for effect operations. It looks up
    /// the handler and dispatches the operation.
    ///
    /// # Safety
    ///
    /// Must be called from within the task context.
    pub unsafe fn perform(
        &mut self,
        effect_id: EffectTypeId,
        operation: OperationId,
        args: Vec<EffectValue>,
    ) -> PerformResult {
        // Temporarily take the dispatch to avoid borrowing issues
        let dispatch = std::ptr::addr_of!(self.dispatch);
        unsafe { (*dispatch).dispatch(self, effect_id, operation, args) }
    }

    /// Captures the current continuation.
    ///
    /// # Safety
    ///
    /// This captures the current register state. The caller must ensure
    /// that the context is valid.
    pub unsafe fn capture_continuation(&self, id: ContinuationId) -> Continuation {
        // Get current context
        let context = Context::new();
        // Note: In a real implementation, we'd capture the current context here
        // For now, we create a placeholder

        Continuation::capture(id, context, self.task, self.depth())
    }

    /// Registers a continuation with the stack.
    pub fn register_continuation(&mut self, id: ContinuationId, cont: Continuation) {
        self.continuations.insert(id, cont);
    }

    /// Takes a continuation from the stack.
    pub fn take_continuation(&mut self, id: ContinuationId) -> Option<Continuation> {
        self.continuations.remove(&id)
    }

    /// Returns a reference to a continuation.
    pub fn get_continuation(&self, id: ContinuationId) -> Option<&Continuation> {
        self.continuations.get(&id)
    }

    /// Returns the task this stack belongs to.
    pub fn task(&self) -> NonNull<Task> {
        self.task
    }

    /// Sets the resume context.
    pub fn set_resume_context(&mut self, context: Context) {
        self.resume_context = Some(context);
    }

    /// Takes the resume context.
    pub fn take_resume_context(&mut self) -> Option<Context> {
        self.resume_context.take()
    }

    /// Returns statistics for this stack.
    pub fn stats(&self) -> StackStats {
        StackStats {
            num_handlers: self.handler_table.len(),
            num_continuations: self.continuations.len(),
            num_frames: self.frames.len(),
            current_depth: self.depth(),
        }
    }

    /// Clears all continuations (used for cleanup).
    pub fn clear_continuations(&mut self) {
        self.continuations.clear();
    }

    /// Checks if we're in an async context.
    pub fn is_async_context(&self) -> bool {
        self.frames.iter().any(|f| f.is_async)
    }

    /// Sets the async flag on the current frame.
    pub fn set_async_context(&mut self, is_async: bool) {
        if let Some(frame) = self.frames.get_mut(self.current_frame) {
            frame.set_async(is_async);
        }
    }
}

/// Statistics for an effect stack.
#[derive(Debug, Clone, Copy)]
pub struct StackStats {
    /// Number of registered handlers.
    pub num_handlers: usize,
    /// Number of active continuations.
    pub num_continuations: usize,
    /// Number of stack frames.
    pub num_frames: usize,
    /// Current stack depth.
    pub current_depth: usize,
}

impl std::fmt::Debug for EffectStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EffectStack")
            .field("num_handlers", &self.handler_table.len())
            .field("num_continuations", &self.continuations.len())
            .field("num_frames", &self.frames.len())
            .field("current_frame", &self.current_frame)
            .field("depth", &self.depth())
            .finish()
    }
}

/// A guard that automatically pops a frame when dropped.
///
/// Note: Currently only used in tests for managing frame lifecycle.
#[cfg(test)]
#[allow(dead_code)]
pub struct FrameGuard<'a> {
    stack: &'a mut EffectStack,
    frame_id: StackFrameId,
    completed: bool,
}

#[cfg(test)]
#[allow(dead_code)]
impl<'a> FrameGuard<'a> {
    /// Creates a new frame guard.
    pub fn new(stack: &'a mut EffectStack, frame_id: StackFrameId) -> Self {
        Self {
            stack,
            frame_id,
            completed: false,
        }
    }

    /// Marks the frame as completed successfully.
    pub fn mark_completed(&mut self) {
        self.completed = true;
    }

    /// Returns the frame ID.
    pub fn frame_id(&self) -> StackFrameId {
        self.frame_id
    }
}

#[cfg(test)]
impl<'a> Drop for FrameGuard<'a> {
    fn drop(&mut self) {
        // Pop frames until we reach the guarded frame
        while self.stack.current_frame().id != self.frame_id {
            if self.stack.pop_frame().is_none() {
                break;
            }
        }
        // Pop the guarded frame itself
        if self.stack.current_frame().id == self.frame_id {
            self.stack.pop_frame();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stack_frame_id() {
        let id1 = StackFrameId(1);
        let id2 = StackFrameId(1);
        let id3 = StackFrameId(2);

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_effect_frame() {
        let task = NonNull::dangling();
        let mut frame = EffectFrame::new(StackFrameId(1), 0, task);

        assert_eq!(frame.depth, 0);
        assert!(frame.handlers.is_empty());

        frame.add_handler(HandlerId(1));
        frame.add_handler(HandlerId(2));

        assert_eq!(frame.handlers.len(), 2);
        assert!(!frame.is_async);

        frame.set_async(true);
        assert!(frame.is_async);
    }

    #[test]
    fn test_effect_stack_frames() {
        let mut stack = EffectStack::new_test();

        assert_eq!(stack.depth(), 0);
        assert_eq!(stack.frames.len(), 1); // Root frame

        // Push a new frame
        let frame1 = stack.push_frame();
        assert_eq!(stack.depth(), 1);
        assert_eq!(stack.current_frame().id, frame1);

        // Push another frame
        let frame2 = stack.push_frame();
        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.current_frame().id, frame2);

        // Pop frames
        let popped = stack.pop_frame().unwrap();
        assert_eq!(popped.id, frame2);
        assert_eq!(stack.depth(), 1);

        // Pop again
        let popped = stack.pop_frame().unwrap();
        assert_eq!(popped.id, frame1);
        assert_eq!(stack.depth(), 0);

        // Can't pop root frame
        assert!(stack.pop_frame().is_none());
    }

    #[test]
    fn test_effect_stack_continuations() {
        let mut stack = EffectStack::new_test();

        // Register a continuation
        let id = ContinuationId(1);
        let task = NonNull::dangling();
        let cont = unsafe { Continuation::capture(id, Context::new(), task, 0) };

        stack.register_continuation(id, cont);
        assert_eq!(stack.continuations.len(), 1);

        // Get the continuation
        assert!(stack.get_continuation(id).is_some());

        // Take the continuation
        let taken = stack.take_continuation(id);
        assert!(taken.is_some());
        assert!(stack.get_continuation(id).is_none());

        // Clear all continuations
        stack.register_continuation(ContinuationId(2), unsafe {
            Continuation::capture(ContinuationId(2), Context::new(), task, 0)
        });
        stack.clear_continuations();
        assert!(stack.continuations.is_empty());
    }

    #[test]
    fn test_stack_stats() {
        let mut stack = EffectStack::new_test();

        let stats = stack.stats();
        assert_eq!(stats.num_handlers, 0);
        assert_eq!(stats.num_continuations, 0);
        assert_eq!(stats.num_frames, 1); // Root frame
        assert_eq!(stats.current_depth, 0);

        // Add a frame
        stack.push_frame();
        let stats = stack.stats();
        assert_eq!(stats.num_frames, 2);
        assert_eq!(stats.current_depth, 1);
    }

    #[test]
    fn test_frame_guard() {
        let mut stack = EffectStack::new_test();

        let initial_depth = stack.depth();

        {
            let frame_id = stack.push_frame();
            // Check depth before creating guard to avoid borrow issues
            assert_eq!(stack.depth(), initial_depth + 1);
            let _guard = FrameGuard::new(&mut stack, frame_id);
            // Guard drops here
        }

        // Frame should be popped
        assert_eq!(stack.depth(), initial_depth);
    }

    #[test]
    fn test_async_context() {
        let mut stack = EffectStack::new_test();

        assert!(!stack.is_async_context());

        // Set async on root frame
        stack.set_async_context(true);
        assert!(stack.is_async_context());

        // Push a non-async frame
        stack.push_frame();
        stack.set_async_context(false);

        // Should still be async because parent is async
        assert!(stack.is_async_context());
    }
}
