//! Effect dispatch mechanism.
//!
//! This module implements the core dispatch logic for effect operations.
//! When a `perform` is executed, the dispatcher:
//!
//! 1. Looks up the handler for the effect in the current stack
//! 2. Captures the continuation (rest of the computation)
//! 3. Invokes the handler
//! 4. Returns the result or suspends the task
//!
//! # Performance
//!
//! Dispatch uses a fast path for common cases:
//! - Inline caching for recently used handlers
//! - Direct dispatch for builtin effects
//! - Avoids allocation for simple handlers that resume immediately

use std::any::Any;
use std::cell::Cell;
use std::pin::Pin;
use std::sync::atomic::{AtomicU64, Ordering};

use super::continuation::{ContinuationHandle, ContinuationId, ResumeResult};
use super::handler::HandlerResult;
use super::stack::EffectStack;
use super::EffectTypeId;
use super::EffectValue;

/// A unique identifier for an effect instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EffectId(pub u64);

/// A unique identifier for an operation within an effect.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OperationId(pub u32);

/// The result of performing an effect operation.
#[derive(Debug)]
pub enum PerformResult {
    /// The effect was handled and returned a value immediately.
    Immediate(Box<dyn Any + Send>),
    /// The effect was handled but the task was suspended.
    Suspended,
    /// No handler was found for the effect.
    NoHandler {
        /// The effect ID.
        effect_id: EffectTypeId,
        /// The operation ID.
        operation: OperationId,
    },
    /// The handler aborted with an error.
    Aborted {
        /// The error message.
        error: String,
    },
    /// The effect was delegated to an outer handler.
    Delegated {
        /// The continuation for when the effect completes.
        continuation: ContinuationHandle,
    },
}

/// Inline cache entry for fast handler lookup.
#[derive(Debug, Clone, Copy)]
struct InlineCacheEntry {
    /// The effect ID this cache entry is for.
    effect_id: EffectTypeId,
    /// The handler index in the handler table.
    handler_index: usize,
    /// Version counter for cache invalidation.
    #[allow(dead_code)]
    version: u64,
}

/// An inline cache for effect handler lookups.
///
/// This provides a fast path for common effect operations by caching
/// the most recently used handler lookups.
pub struct InlineCache {
    /// The cached entries (small fixed-size array for cache efficiency).
    entries: [Cell<Option<InlineCacheEntry>>; 4],
    /// Global version counter for cache invalidation.
    global_version: Cell<u64>,
}

impl InlineCache {
    /// Creates a new empty inline cache.
    pub fn new() -> Self {
        Self {
            entries: [
                Cell::new(None),
                Cell::new(None),
                Cell::new(None),
                Cell::new(None),
            ],
            global_version: Cell::new(0),
        }
    }

    /// Looks up an effect in the cache.
    fn lookup(&self, effect_id: EffectTypeId) -> Option<usize> {
        for entry in &self.entries {
            if let Some(cached) = entry.get() {
                if cached.effect_id == effect_id {
                    return Some(cached.handler_index);
                }
            }
        }
        None
    }

    /// Updates the cache with a new entry.
    fn update(&self, effect_id: EffectTypeId, handler_index: usize) {
        let version = self.global_version.get();
        let new_entry = InlineCacheEntry {
            effect_id,
            handler_index,
            version,
        };

        // Simple LRU: shift entries and put new one at front
        for i in (1..self.entries.len()).rev() {
            self.entries[i].set(self.entries[i - 1].get());
        }
        self.entries[0].set(Some(new_entry));
    }

    /// Invalidates the entire cache.
    pub fn invalidate(&self) {
        self.global_version.set(self.global_version.get() + 1);
        for entry in &self.entries {
            entry.set(None);
        }
    }

    /// Invalidates cache entries for a specific effect.
    pub fn invalidate_effect(&self, effect_id: EffectTypeId) {
        for entry in &self.entries {
            if let Some(cached) = entry.get() {
                if cached.effect_id == effect_id {
                    entry.set(None);
                }
            }
        }
    }
}

impl Default for InlineCache {
    fn default() -> Self {
        Self::new()
    }
}

/// The effect dispatch mechanism.
///
/// This struct handles the routing of effect operations to their handlers.
/// It maintains the inline cache and coordinates with the effect stack.
pub struct EffectDispatch {
    /// The inline cache for fast lookups.
    cache: InlineCache,
    /// Counter for generating continuation IDs.
    next_continuation_id: AtomicU64,
    /// Statistics for dispatch operations.
    stats: DispatchStats,
}

/// Statistics for effect dispatch.
#[derive(Debug, Default)]
pub struct DispatchStats {
    /// Number of cache hits.
    pub cache_hits: AtomicU64,
    /// Number of cache misses.
    pub cache_misses: AtomicU64,
    /// Number of effects dispatched.
    pub effects_dispatched: AtomicU64,
    /// Number of continuations created.
    pub continuations_created: AtomicU64,
}

impl EffectDispatch {
    /// Creates a new effect dispatch.
    pub fn new() -> Self {
        Self {
            cache: InlineCache::new(),
            next_continuation_id: AtomicU64::new(1),
            stats: DispatchStats::default(),
        }
    }

    /// Dispatches an effect operation to its handler.
    ///
    /// This is the core dispatch function that:
    /// 1. Looks up the handler (using cache if available)
    /// 2. Captures the continuation
    /// 3. Invokes the handler
    /// 4. Processes the handler result
    ///
    /// # Safety
    ///
    /// Must be called from within a task context.
    pub unsafe fn dispatch(
        &self,
        stack: &mut EffectStack,
        effect_id: EffectTypeId,
        operation: OperationId,
        args: Vec<EffectValue>,
    ) -> PerformResult {
        self.stats
            .effects_dispatched
            .fetch_add(1, Ordering::Relaxed);

        // Fast path: check inline cache
        if let Some(handler_idx) = self.cache.lookup(effect_id) {
            if let Some(entry) = stack.handler_table().get(handler_idx) {
                if entry.effect_id.0 == effect_id.0 {
                    self.stats.cache_hits.fetch_add(1, Ordering::Relaxed);
                    return self.invoke_handler(stack, handler_idx, operation, args);
                }
            }
            // Cache stale, invalidate
            self.cache.invalidate_effect(effect_id);
        }

        self.stats.cache_misses.fetch_add(1, Ordering::Relaxed);

        // Slow path: look up in handler table
        if let Some((_entry, idx)) = stack.handler_table().lookup(EffectId(effect_id.0)) {
            // Update cache
            self.cache.update(effect_id, idx);
            return self.invoke_handler(stack, idx, operation, args);
        }

        // No handler found
        PerformResult::NoHandler {
            effect_id,
            operation,
        }
    }

    /// Invokes a handler for an effect operation.
    unsafe fn invoke_handler(
        &self,
        stack: &mut EffectStack,
        handler_idx: usize,
        operation: OperationId,
        args: Vec<EffectValue>,
    ) -> PerformResult {
        // Capture continuation before invoking handler
        let cont_id = ContinuationId(self.next_continuation_id.fetch_add(1, Ordering::SeqCst));
        self.stats
            .continuations_created
            .fetch_add(1, Ordering::Relaxed);

        // Create the continuation
        let continuation = stack.capture_continuation(cont_id);
        let handle = continuation.handle();

        // Store continuation in stack
        stack.register_continuation(cont_id, continuation);

        // Get mutable reference to handler
        let handler_entry = stack
            .handler_table_mut()
            .get_mut(handler_idx)
            .expect("handler index should be valid");

        // Invoke the handler
        let result = handler_entry.handler.handle(operation, args, handle);

        // Process handler result
        self.process_handler_result(stack, result)
    }

    /// Processes the result from a handler.
    fn process_handler_result(
        &self,
        stack: &mut EffectStack,
        result: HandlerResult,
    ) -> PerformResult {
        match result {
            HandlerResult::Resume {
                value,
                continuation,
            } => {
                // Handler wants to resume immediately
                // Remove the continuation from the stack since we're using it
                if let Some(cont) = stack.take_continuation(continuation.id()) {
                    // Resume the continuation
                    unsafe {
                        let cont = Pin::new(&cont);
                        match cont.resume(value) {
                            ResumeResult::Success(val) => PerformResult::Immediate(val),
                            ResumeResult::AlreadyConsumed => PerformResult::Aborted {
                                error: "Continuation already consumed".to_string(),
                            },
                            ResumeResult::Cancelled => PerformResult::Suspended,
                            ResumeResult::Panic(msg) => PerformResult::Aborted { error: msg },
                        }
                    }
                } else {
                    PerformResult::Aborted {
                        error: "Continuation not found".to_string(),
                    }
                }
            }
            HandlerResult::ResumeUnit { continuation } => {
                // Resume with unit value
                self.process_handler_result(
                    stack,
                    HandlerResult::Resume {
                        value: Box::new(()),
                        continuation,
                    },
                )
            }
            HandlerResult::ResumeWith { value, transform } => {
                // Apply transformation and resume
                // For now, just resume directly (transformation would modify continuation)
                drop(transform);
                PerformResult::Immediate(value)
            }
            HandlerResult::Unhandled => {
                // Handler didn't handle this operation
                // This shouldn't happen if dispatch is correct
                PerformResult::Aborted {
                    error: "Handler returned Unhandled".to_string(),
                }
            }
            HandlerResult::Abort { error } => PerformResult::Aborted { error },
            HandlerResult::Suspend => PerformResult::Suspended,
        }
    }

    /// Returns the dispatch statistics.
    pub fn stats(&self) -> &DispatchStats {
        &self.stats
    }

    /// Invalidates the dispatch cache.
    pub fn invalidate_cache(&self) {
        self.cache.invalidate();
    }
}

impl Default for EffectDispatch {
    fn default() -> Self {
        Self::new()
    }
}

/// A trait for types that can intercept and transform effect operations.
///
/// This is used for advanced effect patterns like logging, tracing,
/// or effect composition.
///
/// Note: This trait is currently unused but kept for future effect system
/// enhancements like tracing and logging interceptors.
#[allow(dead_code)]
pub trait EffectInterceptor: Send {
    /// Called before an effect is dispatched.
    ///
    /// Returns `None` to allow normal dispatch, or `Some(result)` to
    /// intercept and return a different result.
    fn before_perform(
        &mut self,
        effect_id: EffectTypeId,
        operation: OperationId,
        args: &[EffectValue],
    ) -> Option<PerformResult>;

    /// Called after an effect is handled.
    fn after_perform(
        &mut self,
        effect_id: EffectTypeId,
        operation: OperationId,
        result: &PerformResult,
    );
}

/// A chain of effect interceptors.
///
/// Note: This struct is currently unused but kept for future effect system
/// enhancements like tracing and logging interceptors.
#[allow(dead_code)]
pub struct InterceptorChain {
    interceptors: Vec<Box<dyn EffectInterceptor>>,
}

impl InterceptorChain {
    /// Creates a new empty interceptor chain.
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            interceptors: Vec::new(),
        }
    }

    /// Adds an interceptor to the chain.
    #[allow(dead_code)]
    pub fn add(&mut self, interceptor: Box<dyn EffectInterceptor>) {
        self.interceptors.push(interceptor);
    }

    /// Runs the before_perform hooks.
    ///
    /// Returns the first non-None result, or None if all pass.
    #[allow(dead_code)]
    pub fn before_perform(
        &mut self,
        effect_id: EffectTypeId,
        operation: OperationId,
        args: &[EffectValue],
    ) -> Option<PerformResult> {
        for interceptor in &mut self.interceptors {
            if let Some(result) = interceptor.before_perform(effect_id, operation, args) {
                return Some(result);
            }
        }
        None
    }

    /// Runs the after_perform hooks.
    #[allow(dead_code)]
    pub fn after_perform(
        &mut self,
        effect_id: EffectTypeId,
        operation: OperationId,
        result: &PerformResult,
    ) {
        for interceptor in &mut self.interceptors {
            interceptor.after_perform(effect_id, operation, result);
        }
    }
}

impl Default for InterceptorChain {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_effect_id() {
        let id1 = EffectId(1);
        let id2 = EffectId(1);
        let id3 = EffectId(2);

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_operation_id() {
        let id1 = OperationId(0);
        let id2 = OperationId(0);
        let id3 = OperationId(1);

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_inline_cache() {
        let cache = InlineCache::new();

        // Initially empty
        assert!(cache.lookup(EffectTypeId(1)).is_none());

        // Add entry
        cache.update(EffectTypeId(1), 42);
        assert_eq!(cache.lookup(EffectTypeId(1)), Some(42));

        // Add another entry
        cache.update(EffectTypeId(2), 100);
        assert_eq!(cache.lookup(EffectTypeId(1)), Some(42));
        assert_eq!(cache.lookup(EffectTypeId(2)), Some(100));

        // Invalidate specific effect
        cache.invalidate_effect(EffectTypeId(1));
        assert!(cache.lookup(EffectTypeId(1)).is_none());
        assert_eq!(cache.lookup(EffectTypeId(2)), Some(100));

        // Full invalidate
        cache.update(EffectTypeId(3), 200);
        cache.invalidate();
        assert!(cache.lookup(EffectTypeId(2)).is_none());
        assert!(cache.lookup(EffectTypeId(3)).is_none());
    }

    #[test]
    fn test_perform_result() {
        let result = PerformResult::Immediate(Box::new(42i32));
        match result {
            PerformResult::Immediate(val) => {
                assert_eq!(*val.downcast::<i32>().unwrap(), 42);
            }
            _ => panic!("Expected Immediate"),
        }

        let result = PerformResult::NoHandler {
            effect_id: EffectTypeId(1),
            operation: OperationId(0),
        };
        match result {
            PerformResult::NoHandler {
                effect_id,
                operation,
            } => {
                assert_eq!(effect_id.0, 1);
                assert_eq!(operation.0, 0);
            }
            _ => panic!("Expected NoHandler"),
        }
    }
}
