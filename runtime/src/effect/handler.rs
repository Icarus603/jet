//! Effect handler implementation.
//!
//! This module defines the `EffectHandler` trait and related types for
//! implementing effect handlers. Handlers intercept effect operations and
//! define how to handle them.
//!
//! # Example Handler
//!
//! ```rust,ignore
//! // State effect handler
//! struct StateHandler<T> {
//!     state: T,
//! }
//!
//! impl<T: Clone + 'static> EffectHandler for StateHandler<T> {
//!     fn handle(
//!         &mut self,
//!         operation: OperationId,
//!         args: Vec<EffectValue>,
//!         k: ContinuationHandle,
//!     ) -> HandlerResult {
//!         match operation.0 {
//!             0 => { // Get
//!                 HandlerResult::Resume {
//!                     value: Box::new(self.state.clone()),
//!                     continuation: k,
//!                 }
//!             }
//!             1 => { // Put
//!                 self.state = *args[0].downcast::<T>().unwrap();
//!                 HandlerResult::ResumeUnit { continuation: k }
//!             }
//!             _ => HandlerResult::Unhandled,
//!         }
//!     }
//! }
//! ```

use std::any::Any;
use std::collections::HashMap;
use std::fmt;

#[cfg(test)]
use std::ptr::NonNull;

use super::continuation::ContinuationHandle;
use super::dispatch::{EffectId, OperationId};
use super::EffectValue;

/// A unique identifier for a handler instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HandlerId(pub u64);

/// The result of handling an effect operation.
#[derive(Debug)]
pub enum HandlerResult {
    /// Resume the continuation with a value.
    Resume {
        /// The value to resume with.
        value: Box<dyn Any + Send>,
        /// The continuation to resume.
        continuation: ContinuationHandle,
    },
    /// Resume the continuation with unit value.
    ResumeUnit {
        /// The continuation to resume.
        continuation: ContinuationHandle,
    },
    /// Resume with a modified continuation (for advanced handlers).
    ResumeWith {
        /// The value to resume with.
        value: Box<dyn Any + Send>,
        /// A transformation to apply to the continuation.
        transform: ContinuationTransform,
    },
    /// The effect was not handled by this handler.
    Unhandled,
    /// The handler wants to abort with an error.
    Abort {
        /// The error message.
        error: String,
    },
    /// The handler will resume later (async handling).
    Suspend,
}

/// A transformation to apply to a continuation before resuming.
pub enum ContinuationTransform {
    /// No transformation.
    Identity,
    /// Wrap the continuation with a function.
    Wrap(Box<dyn FnOnce(ContinuationHandle) -> ContinuationHandle + Send>),
    /// Replace the continuation entirely.
    Replace(ContinuationHandle),
}

impl std::fmt::Debug for ContinuationTransform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identity => f.debug_struct("Identity").finish(),
            Self::Wrap(_) => f.debug_struct("Wrap").finish_non_exhaustive(),
            Self::Replace(handle) => f.debug_struct("Replace").field("handle", handle).finish(),
        }
    }
}

/// Specifies how a handler should resume after handling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResumeKind {
    /// Resume once (linear continuation).
    Once,
    /// Resume multiple times (multi-shot continuation).
    MultiShot,
    /// Never resume (aborting handler).
    Never,
}

/// A trait for effect handlers.
///
/// Implementors of this trait can be registered with an effect stack
/// to handle specific effect operations.
///
/// # Safety
///
/// Handlers must properly manage continuations. A continuation should
/// only be resumed once (unless `ResumeKind::MultiShot` is specified).
pub trait EffectHandler: Send + fmt::Debug {
    /// Returns the ID of the effect this handler handles.
    fn effect_id(&self) -> EffectId;

    /// Returns the resume kind for this handler.
    fn resume_kind(&self) -> ResumeKind {
        ResumeKind::Once
    }

    /// Handles an effect operation.
    ///
    /// # Arguments
    ///
    /// * `operation` - The operation being performed.
    /// * `args` - The arguments to the operation.
    /// * `k` - The continuation to resume after handling.
    ///
    /// # Returns
    ///
    /// A `HandlerResult` indicating how to proceed.
    fn handle(
        &mut self,
        operation: OperationId,
        args: Vec<EffectValue>,
        k: ContinuationHandle,
    ) -> HandlerResult;

    /// Called when the handler scope is exited normally.
    ///
    /// This gives the handler a chance to clean up resources.
    fn on_scope_exit(&mut self) {}

    /// Called when the handler is aborted due to an error.
    ///
    /// This gives the handler a chance to clean up resources on error paths.
    fn on_abort(&mut self, _error: &str) {}
}

/// A function-based effect handler for simple cases.
///
/// Note: Currently only used in tests for creating simple handlers.
#[cfg(test)]
pub struct FnHandler {
    effect_id: EffectId,
    handler_fn:
        Box<dyn FnMut(OperationId, Vec<EffectValue>, ContinuationHandle) -> HandlerResult + Send>,
    resume_kind: ResumeKind,
}

#[cfg(test)]
impl FnHandler {
    /// Creates a new function-based handler.
    pub fn new<F>(effect_id: EffectId, handler_fn: F) -> Self
    where
        F: FnMut(OperationId, Vec<EffectValue>, ContinuationHandle) -> HandlerResult
            + Send
            + 'static,
    {
        Self {
            effect_id,
            handler_fn: Box::new(handler_fn),
            resume_kind: ResumeKind::Once,
        }
    }

    /// Sets the resume kind for this handler.
    #[allow(dead_code)]
    pub fn with_resume_kind(mut self, kind: ResumeKind) -> Self {
        self.resume_kind = kind;
        self
    }
}

#[cfg(test)]
impl EffectHandler for FnHandler {
    fn effect_id(&self) -> EffectId {
        self.effect_id
    }

    fn resume_kind(&self) -> ResumeKind {
        self.resume_kind
    }

    fn handle(
        &mut self,
        operation: OperationId,
        args: Vec<EffectValue>,
        k: ContinuationHandle,
    ) -> HandlerResult {
        (self.handler_fn)(operation, args, k)
    }
}

#[cfg(test)]
impl fmt::Debug for FnHandler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FnHandler")
            .field("effect_id", &self.effect_id)
            .field("resume_kind", &self.resume_kind)
            .finish_non_exhaustive()
    }
}

/// An entry in the handler table.
pub struct HandlerEntry {
    /// The handler ID.
    pub id: HandlerId,
    /// The handler instance.
    pub handler: Box<dyn EffectHandler>,
    /// The effect ID this handler handles.
    pub effect_id: EffectId,
    /// The stack depth at which this handler was installed.
    pub depth: usize,
}

impl fmt::Debug for HandlerEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HandlerEntry")
            .field("id", &self.id)
            .field("effect_id", &self.effect_id)
            .field("depth", &self.depth)
            .field("handler", &self.handler)
            .finish()
    }
}

/// A table mapping effect IDs to their handlers.
pub struct HandlerTable {
    /// The handlers in registration order.
    handlers: Vec<HandlerEntry>,
    /// Quick lookup from effect ID to handler index.
    effect_to_handler: HashMap<EffectId, usize>,
    /// Counter for generating handler IDs.
    next_id: u64,
}

impl HandlerTable {
    /// Creates a new empty handler table.
    pub fn new() -> Self {
        Self {
            handlers: Vec::new(),
            effect_to_handler: HashMap::new(),
            next_id: 1,
        }
    }

    /// Registers a new handler.
    ///
    /// Returns the handler ID and the index in the handler stack.
    pub fn register(
        &mut self,
        handler: Box<dyn EffectHandler>,
        depth: usize,
    ) -> (HandlerId, usize) {
        let id = HandlerId(self.next_id);
        self.next_id += 1;

        let effect_id = handler.effect_id();
        let entry = HandlerEntry {
            id,
            handler,
            effect_id,
            depth,
        };

        let index = self.handlers.len();
        self.handlers.push(entry);
        self.effect_to_handler.insert(effect_id, index);

        (id, index)
    }

    /// Looks up a handler by effect ID.
    pub fn lookup(&self, effect_id: EffectId) -> Option<(&HandlerEntry, usize)> {
        self.effect_to_handler
            .get(&effect_id)
            .map(|&idx| (&self.handlers[idx], idx))
    }

    /// Looks up a handler by effect ID (mutable).
    pub fn lookup_mut(&mut self, effect_id: EffectId) -> Option<(&mut HandlerEntry, usize)> {
        if let Some(&idx) = self.effect_to_handler.get(&effect_id) {
            Some((&mut self.handlers[idx], idx))
        } else {
            None
        }
    }

    /// Removes handlers at or below a given depth.
    ///
    /// Returns the number of handlers removed.
    pub fn remove_below_depth(&mut self, depth: usize) -> Vec<HandlerEntry> {
        let mut removed = Vec::new();
        let mut new_handlers = Vec::new();

        for entry in self.handlers.drain(..) {
            if entry.depth < depth {
                new_handlers.push(entry);
            } else {
                // Remove from lookup
                self.effect_to_handler.remove(&entry.effect_id);
                removed.push(entry);
            }
        }

        self.handlers = new_handlers;
        removed
    }

    /// Returns the number of registered handlers.
    pub fn len(&self) -> usize {
        self.handlers.len()
    }

    /// Returns true if no handlers are registered.
    pub fn is_empty(&self) -> bool {
        self.handlers.is_empty()
    }

    /// Returns the current handler depth (max depth of all handlers).
    pub fn current_depth(&self) -> usize {
        self.handlers.iter().map(|h| h.depth).max().unwrap_or(0)
    }

    /// Gets a handler entry by index.
    pub fn get(&self, index: usize) -> Option<&HandlerEntry> {
        self.handlers.get(index)
    }

    /// Gets a mutable handler entry by index.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut HandlerEntry> {
        self.handlers.get_mut(index)
    }
}

impl Default for HandlerTable {
    fn default() -> Self {
        Self::new()
    }
}

/// A scope guard that automatically removes handlers when dropped.
///
/// Note: Currently only used in tests for managing handler scope lifecycle.
#[cfg(test)]
#[allow(dead_code)]
pub struct HandlerScopeGuard {
    /// The handler table to modify on drop.
    table: NonNull<HandlerTable>,
    /// The depth below which handlers should be removed.
    depth: usize,
    /// Whether the scope completed successfully.
    completed: bool,
}

#[cfg(test)]
#[allow(dead_code)]
impl HandlerScopeGuard {
    /// Creates a new scope guard.
    ///
    /// # Safety
    ///
    /// The table pointer must remain valid for the lifetime of this guard.
    pub unsafe fn new(table: NonNull<HandlerTable>, depth: usize) -> Self {
        Self {
            table,
            depth,
            completed: false,
        }
    }

    /// Marks the scope as completed successfully.
    pub fn mark_completed(&mut self) {
        self.completed = true;
    }
}

#[cfg(test)]
impl Drop for HandlerScopeGuard {
    fn drop(&mut self) {
        unsafe {
            let table = &mut *self.table.as_ptr();
            let removed = table.remove_below_depth(self.depth);

            // Notify handlers of scope exit
            for mut entry in removed {
                if self.completed {
                    entry.handler.on_scope_exit();
                } else {
                    entry.handler.on_abort("scope dropped without completion");
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handler_id() {
        let id1 = HandlerId(1);
        let id2 = HandlerId(1);
        let id3 = HandlerId(2);

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_handler_table() {
        let mut table = HandlerTable::new();
        assert!(table.is_empty());

        // Create a simple handler
        let handler = FnHandler::new(EffectId(1), |_op, _args, k| HandlerResult::ResumeUnit {
            continuation: k,
        });

        let (id, idx) = table.register(Box::new(handler), 0);
        assert_eq!(table.len(), 1);
        assert_eq!(idx, 0);

        // Look up the handler
        let (entry, found_idx) = table.lookup(EffectId(1)).unwrap();
        assert_eq!(entry.id, id);
        assert_eq!(found_idx, 0);

        // Look up non-existent handler
        assert!(table.lookup(EffectId(999)).is_none());
    }

    #[test]
    fn test_handler_table_depth() {
        let mut table = HandlerTable::new();

        let handler1 = FnHandler::new(EffectId(1), |_op, _args, k| HandlerResult::ResumeUnit {
            continuation: k,
        });
        let handler2 = FnHandler::new(EffectId(2), |_op, _args, k| HandlerResult::ResumeUnit {
            continuation: k,
        });

        table.register(Box::new(handler1), 0);
        table.register(Box::new(handler2), 1);

        assert_eq!(table.current_depth(), 1);

        // Remove handlers at depth 1 and below
        let removed = table.remove_below_depth(1);
        assert_eq!(removed.len(), 1);
        assert_eq!(table.len(), 1);

        // Effect 2 should be removed
        assert!(table.lookup(EffectId(2)).is_none());
        // Effect 1 should still be there
        assert!(table.lookup(EffectId(1)).is_some());
    }

    #[test]
    fn test_handler_result() {
        // Test that HandlerResult variants can be created and matched
        let result = HandlerResult::Unhandled;
        match result {
            HandlerResult::Unhandled => {
                // Expected
            }
            _ => panic!("Expected Unhandled"),
        }

        let result = HandlerResult::Suspend;
        match result {
            HandlerResult::Suspend => {
                // Expected
            }
            _ => panic!("Expected Suspend"),
        }
    }
}
