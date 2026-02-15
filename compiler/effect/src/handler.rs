//! Handler tracking for effect checking.
//!
//! This module tracks active effect handlers in scope and manages
//! the relationship between perform operations and their handlers.

use std::collections::HashMap;

use jet_diagnostics::Span;

use crate::effect::{DefId, EffectHandler, EffectId, EffectInstance};
use crate::error::{EffectError, EffectResult};

/// Tracks the handler stack during effect checking.
#[derive(Debug, Clone, Default)]
pub struct HandlerStack {
    /// Stack of handler scopes. Each scope can have multiple handlers.
    scopes: Vec<HandlerScope>,
    /// Maps effect IDs to their handler scope index and handler index.
    effect_to_handler: HashMap<EffectId, (usize, usize)>,
    /// Counter for generating unique handler IDs.
    next_handler_id: u32,
}

/// A scope containing active handlers.
#[derive(Debug, Clone, Default)]
pub struct HandlerScope {
    /// Unique identifier for this scope.
    pub id: ScopeId,
    /// Handlers active in this scope.
    pub handlers: Vec<ActiveHandler>,
    /// The span where this scope was created.
    pub span: Span,
    /// Whether this is a concurrent scope.
    pub is_concurrent: bool,
}

/// A unique identifier for a handler scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ScopeId(pub u32);

/// An active handler in the stack.
#[derive(Debug, Clone)]
pub struct ActiveHandler {
    /// Unique identifier for this handler.
    pub id: HandlerId,
    /// The effect being handled.
    pub effect_id: EffectId,
    /// The name of the effect.
    pub effect_name: String,
    /// The span where the handler was defined.
    pub span: Span,
    /// Operations this handler handles.
    pub operations: Vec<String>,
    /// Whether this handler is exhaustive.
    pub is_exhaustive: bool,
    /// Whether this handler resumes after handling.
    pub resumes: bool,
    /// Definition ID of the handler function.
    pub handler_def_id: DefId,
}

/// A unique identifier for a handler.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct HandlerId(pub u32);

impl HandlerStack {
    /// Creates a new empty handler stack.
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            effect_to_handler: HashMap::new(),
            next_handler_id: 0,
        }
    }

    /// Pushes a new scope onto the stack.
    pub fn push_scope(&mut self, span: Span) -> ScopeId {
        let id = ScopeId(self.next_handler_id);
        self.next_handler_id += 1;

        self.scopes.push(HandlerScope {
            id,
            handlers: Vec::new(),
            span,
            is_concurrent: false,
        });

        id
    }

    /// Pushes a concurrent scope onto the stack.
    pub fn push_concurrent_scope(&mut self, span: Span) -> ScopeId {
        let id = self.push_scope(span);
        if let Some(scope) = self.scopes.last_mut() {
            scope.is_concurrent = true;
        }
        id
    }

    /// Pops the current scope from the stack.
    pub fn pop_scope(&mut self) -> Option<HandlerScope> {
        let scope = self.scopes.pop()?;

        // Remove mappings for handlers in this scope
        for handler in &scope.handlers {
            self.effect_to_handler.remove(&handler.effect_id);
        }

        Some(scope)
    }

    /// Returns the current scope ID if any.
    pub fn current_scope(&self) -> Option<ScopeId> {
        self.scopes.last().map(|s| s.id)
    }

    /// Returns true if we're currently in a concurrent scope.
    pub fn in_concurrent_scope(&self) -> bool {
        self.scopes.iter().any(|s| s.is_concurrent)
    }

    /// Adds a handler to the current scope.
    pub fn add_handler(
        &mut self,
        handler: EffectHandler,
        span: Span,
        handler_def_id: DefId,
    ) -> EffectResult<HandlerId> {
        // Check for duplicate handlers in the current scope
        if let Some(current_scope) = self.scopes.last() {
            if let Some(existing) = current_scope
                .handlers
                .iter()
                .find(|h| h.effect_id == handler.effect)
            {
                return Err(EffectError::DuplicateHandler {
                    effect_name: handler.name.clone(),
                    first_handler_span: existing.span,
                    duplicate_span: span,
                });
            }
        }

        let handler_id = HandlerId(self.next_handler_id);
        self.next_handler_id += 1;

        let active_handler = ActiveHandler {
            id: handler_id,
            effect_id: handler.effect,
            effect_name: handler.name.clone(),
            span,
            operations: handler
                .operations
                .iter()
                .map(|op| op.name.clone())
                .collect(),
            is_exhaustive: handler.is_exhaustive,
            resumes: handler.operations.iter().any(|op| op.resumes),
            handler_def_id,
        };

        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.handlers.push(active_handler);
        } else {
            // Create a root scope if none exists
            self.push_scope(span);
            self.scopes
                .last_mut()
                .unwrap()
                .handlers
                .push(active_handler);
        }

        // Map effect to this handler
        let scope_idx = self.scopes.len() - 1;
        let handler_idx = self.scopes[scope_idx].handlers.len() - 1;
        self.effect_to_handler
            .insert(handler.effect, (scope_idx, handler_idx));

        Ok(handler_id)
    }

    /// Finds a handler for a given effect.
    pub fn find_handler(&self, effect: EffectId) -> Option<&ActiveHandler> {
        self.effect_to_handler
            .get(&effect)
            .and_then(|(scope_idx, handler_idx)| {
                self.scopes
                    .get(*scope_idx)
                    .and_then(|s| s.handlers.get(*handler_idx))
            })
    }

    /// Finds a handler for an effect instance.
    pub fn find_handler_for_instance(&self, effect: &EffectInstance) -> Option<&ActiveHandler> {
        self.find_handler(effect.id)
    }

    /// Returns true if the given effect has a handler in the current scope.
    pub fn is_handled(&self, effect: EffectId) -> bool {
        self.effect_to_handler.contains_key(&effect)
    }

    /// Returns true if the given effect instance has a handler.
    pub fn is_handled_instance(&self, effect: &EffectInstance) -> bool {
        self.is_handled(effect.id)
    }

    /// Gets all active handlers in the current scope.
    pub fn current_handlers(&self) -> Vec<&ActiveHandler> {
        self.scopes
            .last()
            .map(|s| s.handlers.iter().collect())
            .unwrap_or_default()
    }

    /// Gets all active handlers in all scopes.
    pub fn all_handlers(&self) -> Vec<&ActiveHandler> {
        self.scopes.iter().flat_map(|s| s.handlers.iter()).collect()
    }

    /// Returns the depth of the handler stack.
    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
    }

    /// Checks if a resume operation is valid in the current context.
    pub fn can_resume(&self) -> bool {
        // Can only resume inside a handler
        self.scopes
            .last()
            .map(|s| s.handlers.iter().any(|h| h.resumes))
            .unwrap_or(false)
    }

    /// Gets the innermost handler that allows resuming.
    pub fn get_resumable_handler(&self) -> Option<&ActiveHandler> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|s| s.handlers.iter())
            .find(|h| h.resumes)
    }

    /// Returns the set of effects that are currently handled.
    pub fn handled_effects(&self) -> Vec<EffectId> {
        self.effect_to_handler.keys().copied().collect()
    }

    /// Creates a snapshot of the current handler stack state.
    pub fn snapshot(&self) -> HandlerSnapshot {
        HandlerSnapshot {
            scope_count: self.scopes.len(),
            handled_effects: self.handled_effects(),
        }
    }

    /// Restores the handler stack to a previous snapshot.
    /// This removes scopes that were added after the snapshot.
    pub fn restore(&mut self, snapshot: &HandlerSnapshot) {
        while self.scopes.len() > snapshot.scope_count {
            self.pop_scope();
        }
    }
}

/// A snapshot of the handler stack at a point in time.
#[derive(Debug, Clone)]
pub struct HandlerSnapshot {
    /// The number of scopes at the time of the snapshot.
    pub scope_count: usize,
    /// The effects that were handled at the time of the snapshot.
    pub handled_effects: Vec<EffectId>,
}

/// Tracks perform operations that need to be matched with handlers.
#[derive(Debug, Clone, Default)]
pub struct PerformTracker {
    /// Perform operations that haven't been matched with handlers yet.
    pending: Vec<PendingPerform>,
    /// Matched perform-handler pairs.
    matched: Vec<MatchedPerform>,
}

/// A perform operation waiting to be matched with a handler.
#[derive(Debug, Clone)]
pub struct PendingPerform {
    /// The effect being performed.
    pub effect: EffectInstance,
    /// The operation name.
    pub operation: String,
    /// Where the perform happened.
    pub span: Span,
    /// The scope depth at the time of perform.
    pub scope_depth: usize,
}

/// A matched perform-handler pair.
#[derive(Debug, Clone)]
pub struct MatchedPerform {
    /// The perform operation.
    pub perform: PendingPerform,
    /// The handler that matched.
    pub handler_id: HandlerId,
    /// The scope where the handler was found.
    pub handler_scope: ScopeId,
}

impl PerformTracker {
    /// Creates a new perform tracker.
    pub fn new() -> Self {
        Self {
            pending: Vec::new(),
            matched: Vec::new(),
        }
    }

    /// Records a perform operation.
    pub fn record_perform(
        &mut self,
        effect: EffectInstance,
        operation: String,
        span: Span,
        scope_depth: usize,
    ) {
        self.pending.push(PendingPerform {
            effect,
            operation,
            span,
            scope_depth,
        });
    }

    /// Tries to match pending performs with handlers from the stack.
    pub fn match_with_handlers(&mut self, handler_stack: &HandlerStack) {
        let mut still_pending = Vec::new();

        for perform in self.pending.drain(..) {
            if let Some(handler) = handler_stack.find_handler_for_instance(&perform.effect) {
                self.matched.push(MatchedPerform {
                    perform,
                    handler_id: handler.id,
                    handler_scope: handler_stack
                        .current_scope()
                        .expect("Handler found but no current scope"),
                });
            } else {
                still_pending.push(perform);
            }
        }

        self.pending = still_pending;
    }

    /// Gets all pending (unmatched) performs.
    pub fn pending_performs(&self) -> &[PendingPerform] {
        &self.pending
    }

    /// Gets all matched performs.
    pub fn matched_performs(&self) -> &[MatchedPerform] {
        &self.matched
    }

    /// Takes all pending performs (clears them).
    pub fn take_pending(&mut self) -> Vec<PendingPerform> {
        std::mem::take(&mut self.pending)
    }

    /// Clears all matched performs.
    pub fn clear_matched(&mut self) {
        self.matched.clear();
    }

    /// Returns true if there are no pending performs.
    pub fn all_matched(&self) -> bool {
        self.pending.is_empty()
    }
}

/// Context for verifying effect handling within a function.
#[derive(Debug, Clone)]
pub struct HandlerContext {
    /// The handler stack.
    pub stack: HandlerStack,
    /// The perform tracker.
    pub tracker: PerformTracker,
    /// Whether we're currently inside a handler body.
    pub in_handler_body: bool,
    /// The current handler if in_handler_body is true.
    pub current_handler: Option<HandlerId>,
}

impl HandlerContext {
    /// Creates a new handler context.
    pub fn new() -> Self {
        Self {
            stack: HandlerStack::new(),
            tracker: PerformTracker::new(),
            in_handler_body: false,
            current_handler: None,
        }
    }

    /// Enters a handler scope.
    pub fn enter_scope(&mut self, span: Span) -> ScopeId {
        self.stack.push_scope(span)
    }

    /// Leaves the current handler scope.
    pub fn leave_scope(&mut self) -> Option<HandlerScope> {
        // Match any pending performs before leaving
        self.tracker.match_with_handlers(&self.stack);
        self.stack.pop_scope()
    }

    /// Adds a handler to the current scope.
    pub fn add_handler(
        &mut self,
        handler: EffectHandler,
        span: Span,
        handler_def_id: DefId,
    ) -> EffectResult<HandlerId> {
        self.stack.add_handler(handler, span, handler_def_id)
    }

    /// Records a perform operation.
    pub fn record_perform(&mut self, effect: EffectInstance, operation: String, span: Span) {
        let depth = self.stack.scope_depth();
        self.tracker.record_perform(effect, operation, span, depth);
        self.tracker.match_with_handlers(&self.stack);
    }

    /// Checks if an effect is handled in the current context.
    pub fn is_handled(&self, effect: &EffectInstance) -> bool {
        self.stack.is_handled_instance(effect)
    }

    /// Gets unhandled performs that escape this context.
    pub fn unhandled_performs(&self) -> Vec<PendingPerform> {
        self.tracker.pending_performs().to_vec()
    }

    /// Enters a handler body context.
    pub fn enter_handler_body(&mut self, handler_id: HandlerId) {
        self.in_handler_body = true;
        self.current_handler = Some(handler_id);
    }

    /// Leaves a handler body context.
    pub fn leave_handler_body(&mut self) {
        self.in_handler_body = false;
        self.current_handler = None;
    }
}

impl Default for HandlerContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::effect::EffectInstance;

    fn test_span() -> Span {
        Span::new(0, 10)
    }

    fn create_test_handler(effect_id: EffectId, name: &str) -> EffectHandler {
        EffectHandler {
            effect: effect_id,
            name: name.to_string(),
            operations: vec![],
            is_exhaustive: true,
        }
    }

    #[test]
    fn test_handler_stack_push_pop() {
        let mut stack = HandlerStack::new();
        assert_eq!(stack.scope_depth(), 0);

        let scope1 = stack.push_scope(test_span());
        assert_eq!(stack.scope_depth(), 1);

        let scope2 = stack.push_scope(test_span());
        assert_eq!(stack.scope_depth(), 2);
        assert_ne!(scope1, scope2);

        stack.pop_scope();
        assert_eq!(stack.scope_depth(), 1);

        stack.pop_scope();
        assert_eq!(stack.scope_depth(), 0);
    }

    #[test]
    fn test_handler_stack_add_handler() {
        let mut stack = HandlerStack::new();
        stack.push_scope(test_span());

        let handler = create_test_handler(EffectId(1), "State");
        let id = stack.add_handler(handler, test_span(), DefId(1)).unwrap();

        assert!(stack.is_handled(EffectId(1)));
        assert!(!stack.is_handled(EffectId(2)));

        let found = stack.find_handler(EffectId(1)).unwrap();
        assert_eq!(found.id, id);
        assert_eq!(found.effect_name, "State");
    }

    #[test]
    fn test_handler_stack_duplicate() {
        let mut stack = HandlerStack::new();
        stack.push_scope(test_span());

        let handler1 = create_test_handler(EffectId(1), "State");
        stack
            .add_handler(handler1, Span::new(0, 5), DefId(1))
            .unwrap();

        let handler2 = create_test_handler(EffectId(1), "State");
        let result = stack.add_handler(handler2, Span::new(10, 15), DefId(2));

        assert!(result.is_err());
        match result.unwrap_err() {
            EffectError::DuplicateHandler { effect_name, .. } => {
                assert_eq!(effect_name, "State");
            }
            _ => panic!("Expected DuplicateHandler error"),
        }
    }

    #[test]
    fn test_handler_stack_find_in_parent_scope() {
        let mut stack = HandlerStack::new();
        stack.push_scope(test_span());

        let handler = create_test_handler(EffectId(1), "State");
        stack.add_handler(handler, test_span(), DefId(1)).unwrap();

        // Push a new scope - should still find handler from parent
        stack.push_scope(test_span());
        assert!(stack.is_handled(EffectId(1)));

        let found = stack.find_handler(EffectId(1));
        assert!(found.is_some());
    }

    #[test]
    fn test_handler_snapshot() {
        let mut stack = HandlerStack::new();
        stack.push_scope(test_span());

        let handler = create_test_handler(EffectId(1), "State");
        stack.add_handler(handler, test_span(), DefId(1)).unwrap();

        let snapshot = stack.snapshot();
        assert_eq!(snapshot.scope_count, 1);
        assert_eq!(snapshot.handled_effects, vec![EffectId(1)]);

        // Add more scopes
        stack.push_scope(test_span());
        assert_eq!(stack.scope_depth(), 2);

        // Restore
        stack.restore(&snapshot);
        assert_eq!(stack.scope_depth(), 1);
    }

    #[test]
    fn test_perform_tracker() {
        let mut tracker = PerformTracker::new();
        let effect = EffectInstance::new(EffectId(1), "State");

        tracker.record_perform(effect.clone(), "get".to_string(), test_span(), 0);
        assert_eq!(tracker.pending_performs().len(), 1);
        assert!(tracker.matched_performs().is_empty());

        // Create a handler stack with a handler for this effect
        let mut stack = HandlerStack::new();
        stack.push_scope(test_span());
        let handler = create_test_handler(EffectId(1), "State");
        stack.add_handler(handler, test_span(), DefId(1)).unwrap();

        // Match performs
        tracker.match_with_handlers(&stack);
        assert!(tracker.pending_performs().is_empty());
        assert_eq!(tracker.matched_performs().len(), 1);
    }

    #[test]
    fn test_handler_context() {
        let mut ctx = HandlerContext::new();

        ctx.enter_scope(test_span());
        assert_eq!(ctx.stack.scope_depth(), 1);

        let handler = create_test_handler(EffectId(1), "State");
        ctx.add_handler(handler, test_span(), DefId(1)).unwrap();

        let effect = EffectInstance::new(EffectId(1), "State");
        ctx.record_perform(effect, "get".to_string(), test_span());

        assert!(ctx.is_handled(&EffectInstance::new(EffectId(1), "State")));
        assert!(ctx.unhandled_performs().is_empty());

        ctx.leave_scope();
        assert_eq!(ctx.stack.scope_depth(), 0);
    }

    #[test]
    fn test_concurrent_scope() {
        let mut stack = HandlerStack::new();
        assert!(!stack.in_concurrent_scope());

        stack.push_scope(test_span());
        assert!(!stack.in_concurrent_scope());

        stack.push_concurrent_scope(test_span());
        assert!(stack.in_concurrent_scope());

        stack.pop_scope();
        assert!(!stack.in_concurrent_scope());
    }
}
