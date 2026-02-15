//! Jet Runtime Effect System
//!
//! This module implements the runtime support for Jet's effect system.
//! It provides:
//!
//! - **Effect handler dispatch**: Routing effect operations to their handlers
//! - **Continuation capture/resumption**: Saving and restoring execution state
//! - **Effect stacks**: Managing nested handlers and their scopes
//! - **Async integration**: Supporting effects in async/await contexts
//!
//! # Architecture
//!
//! The effect runtime is tightly integrated with the task scheduler to support
//! suspending and resuming tasks when effects are performed. Each task has an
//! associated effect stack that tracks active handlers.
//!
//! ```text
//! Task
//!   └── EffectStack
//!         ├── Handler (Effect A)
//!         │     └── Continuation (saved state)
//!         ├── Handler (Effect B)
//!         │     └── Continuation
//!         └── Handler (Effect C)
//!               └── Continuation
//! ```

use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU64, Ordering};

pub mod async_integration;
pub mod continuation;
pub mod dispatch;
pub mod handler;
pub mod stack;

pub use async_integration::{
    AsyncEffectContext, AsyncEffectFuture, AsyncEffectHandle, AsyncEffectHandler,
    AsyncEffectRegistry, EffectfulFuture,
};
pub use continuation::{Continuation, ContinuationHandle, ResumeResult};
pub use dispatch::{EffectDispatch, EffectId, OperationId, PerformResult};
pub use handler::{EffectHandler, HandlerId, HandlerResult, HandlerTable, ResumeKind};
pub use stack::{EffectFrame, EffectStack, StackFrameId};

/// Unique identifier for effect types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EffectTypeId(pub u64);

impl EffectTypeId {
    /// Creates a new effect type ID.
    pub fn new(id: u64) -> Self {
        Self(id)
    }
}

/// A value passed to or returned from an effect operation.
pub type EffectValue = Box<dyn Any + Send>;

thread_local! {
    /// Thread-local storage for the current effect stack.
    static CURRENT_EFFECT_STACK: RefCell<Option<NonNull<EffectStack>>> = const { RefCell::new(None) };
}

/// Sets the current effect stack for this thread.
///
/// # Safety
///
/// The pointer must remain valid until `clear_current_effect_stack` is called
/// or the stack is replaced.
pub unsafe fn set_current_effect_stack(stack: NonNull<EffectStack>) {
    CURRENT_EFFECT_STACK.with(|s| {
        *s.borrow_mut() = Some(stack);
    });
}

/// Clears the current effect stack for this thread.
pub fn clear_current_effect_stack() {
    CURRENT_EFFECT_STACK.with(|s| {
        *s.borrow_mut() = None;
    });
}

/// Gets the current effect stack if one is set.
pub fn current_effect_stack() -> Option<NonNull<EffectStack>> {
    CURRENT_EFFECT_STACK.with(|s| *s.borrow())
}

/// Performs an effect operation in the current context.
///
/// This is the core operation of the effect system. It:
/// 1. Looks up the handler for the effect in the current stack
/// 2. Captures the current continuation
/// 3. Dispatches to the handler
/// 4. Returns the result (or suspends if the handler doesn't resume immediately)
///
/// # Safety
///
/// Must be called from within a task context with a valid effect stack.
///
/// # Example
///
/// ```rust,ignore
/// // In generated code for: perform Get()
/// let result: i32 = perform_effect(
///     EffectTypeId(1),  // State effect
///     OperationId(0),   // Get operation
///     vec![],           // No arguments
/// ).unwrap();
/// ```
pub unsafe fn perform_effect(
    effect_id: EffectTypeId,
    operation: OperationId,
    args: Vec<EffectValue>,
) -> PerformResult {
    match current_effect_stack() {
        Some(stack_ptr) => {
            let stack = &mut *stack_ptr.as_ptr();
            stack.perform(effect_id, operation, args)
        }
        None => PerformResult::NoHandler {
            effect_id,
            operation,
        },
    }
}

/// The global effect runtime managing effect type registration.
pub struct EffectRuntime {
    /// Registered effect types.
    effect_types: HashMap<EffectTypeId, RegisteredEffect>,
    /// Counter for generating effect IDs.
    next_effect_id: AtomicU64,
}

/// Information about a registered effect type.
#[derive(Debug, Clone)]
pub struct RegisteredEffect {
    /// The effect type ID.
    pub id: EffectTypeId,
    /// The name of the effect.
    pub name: String,
    /// Number of operations for this effect.
    pub operation_count: u32,
}

impl EffectRuntime {
    /// Creates a new effect runtime.
    pub fn new() -> Self {
        Self {
            effect_types: HashMap::new(),
            next_effect_id: AtomicU64::new(1),
        }
    }

    /// Registers a new effect type.
    pub fn register_effect(
        &mut self,
        name: impl Into<String>,
        operation_count: u32,
    ) -> EffectTypeId {
        let id = EffectTypeId(self.next_effect_id.fetch_add(1, Ordering::SeqCst));
        let effect = RegisteredEffect {
            id,
            name: name.into(),
            operation_count,
        };
        self.effect_types.insert(id, effect);
        id
    }

    /// Looks up an effect by ID.
    pub fn get_effect(&self, id: EffectTypeId) -> Option<&RegisteredEffect> {
        self.effect_types.get(&id)
    }

    /// Returns the number of registered effects.
    pub fn effect_count(&self) -> usize {
        self.effect_types.len()
    }
}

impl Default for EffectRuntime {
    fn default() -> Self {
        Self::new()
    }
}

/// A trait for types that can be used as effect arguments or results.
///
/// This is automatically implemented for types that are `Any + Send + 'static`.
pub trait EffectData: Any + Send {
    /// Converts this value into a boxed `Any`.
    fn into_any(self: Box<Self>) -> Box<dyn Any + Send>;
    /// Clones this value into a boxed `Any`.
    fn clone_any(&self) -> Option<Box<dyn Any + Send>>;
}

impl<T: Any + Send + Clone + 'static> EffectData for T {
    fn into_any(self: Box<Self>) -> Box<dyn Any + Send> {
        self
    }

    fn clone_any(&self) -> Option<Box<dyn Any + Send>> {
        Some(Box::new(self.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_effect_type_id() {
        let id1 = EffectTypeId::new(1);
        let id2 = EffectTypeId::new(1);
        let id3 = EffectTypeId::new(2);

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_effect_runtime() {
        let mut runtime = EffectRuntime::new();

        let state_effect = runtime.register_effect("State", 3); // get, put, modify
        let io_effect = runtime.register_effect("IO", 2); // read, write

        assert_eq!(runtime.effect_count(), 2);

        let state = runtime.get_effect(state_effect).unwrap();
        assert_eq!(state.name, "State");
        assert_eq!(state.operation_count, 3);

        let io = runtime.get_effect(io_effect).unwrap();
        assert_eq!(io.name, "IO");
    }

    #[test]
    fn test_effect_stack_thread_local() {
        clear_current_effect_stack();
        assert!(current_effect_stack().is_none());

        // Create a dummy stack (we won't use it, just test the TLS)
        let mut stack = EffectStack::new_test();
        let stack_ptr = NonNull::new(&mut stack).unwrap();

        unsafe {
            set_current_effect_stack(stack_ptr);
            assert!(current_effect_stack().is_some());
        }

        clear_current_effect_stack();
        assert!(current_effect_stack().is_none());
    }
}
