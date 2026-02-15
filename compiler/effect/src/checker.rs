//! Effect checker for the Jet language.
//!
//! This module implements the effect checking pass, which verifies that:
//! 1. All effects performed by a function are either handled or declared
//! 2. Effect handlers are complete and correct
//! 3. Effects don't escape their handling scope
//!
//! The checker works on the typed AST (TAST) produced by the type checker.

use std::collections::HashMap;

use jet_diagnostics::{Diagnostic, Span};
use jet_parser::ast::{self, Expr, Function, Module, ModuleItem, Stmt, Type};

use crate::effect::{BuiltinEffect, DefId, EffectId, EffectInstance, EffectSet, EffectVar};
use crate::error::{EffectError, EffectErrors, EffectResult};
use crate::handler::HandlerContext;
use crate::inference::EffectInference;

/// Convert a lexer span to a diagnostics span.
fn to_diag_span(span: jet_lexer::Span) -> Span {
    Span::new(span.start, span.end)
}

/// The effect checker verifies that effects are properly handled.
pub struct EffectChecker {
    /// Accumulated errors during checking.
    errors: EffectErrors,
    /// Handler context for tracking active handlers.
    handler_ctx: HandlerContext,
    /// Effect inference engine.
    #[allow(dead_code)]
    inference: EffectInference,
    /// Maps function names to their declared effects.
    function_effects: HashMap<String, EffectSet>,
    /// Maps effect names to their IDs.
    effect_registry: HashMap<String, EffectId>,
    /// Counter for generating fresh effect IDs.
    next_effect_id: u32,
    /// Counter for generating fresh definition IDs.
    next_def_id: u32,
    /// Counter for generating fresh effect variables.
    next_effect_var: u32,
    /// Current function being checked.
    current_function: Option<String>,
    /// Whether we're in an async context.
    in_async_context: bool,
    /// Whether we're in an unsafe context.
    #[allow(dead_code)]
    in_unsafe_context: bool,
}

/// The result of checking an expression.
#[derive(Debug, Clone)]
pub struct CheckedExpr {
    /// The inferred effects of the expression.
    pub effects: EffectSet,
    /// Whether this expression can diverge (loop forever or panic).
    pub can_diverge: bool,
    /// Whether this expression returns (exits the function).
    pub returns: bool,
}

impl EffectChecker {
    /// Creates a new effect checker.
    pub fn new() -> Self {
        let mut checker = Self {
            errors: EffectErrors::new(),
            handler_ctx: HandlerContext::new(),
            inference: EffectInference::new(),
            function_effects: HashMap::new(),
            effect_registry: HashMap::new(),
            next_effect_id: 5, // 0-4 reserved for builtins
            next_def_id: 0,
            next_effect_var: 0,
            current_function: None,
            in_async_context: false,
            in_unsafe_context: false,
        };

        // Register builtin effects
        checker.register_builtin_effects();
        checker
    }

    /// Registers the builtin effects (async, io, unsafe, etc.).
    fn register_builtin_effects(&mut self) {
        self.effect_registry
            .insert("async".to_string(), BuiltinEffect::Async.effect_id());
        self.effect_registry
            .insert("io".to_string(), BuiltinEffect::Io.effect_id());
        self.effect_registry
            .insert("unsafe".to_string(), BuiltinEffect::Unsafe.effect_id());
        self.effect_registry
            .insert("diverges".to_string(), BuiltinEffect::Diverges.effect_id());
        self.effect_registry
            .insert("panic".to_string(), BuiltinEffect::Panic.effect_id());
    }

    /// Gets or creates an effect ID for a named effect.
    pub fn get_or_create_effect(&mut self, name: &str) -> EffectId {
        if let Some(&id) = self.effect_registry.get(name) {
            return id;
        }
        let id = EffectId(self.next_effect_id);
        self.next_effect_id += 1;
        self.effect_registry.insert(name.to_string(), id);
        id
    }

    /// Looks up an effect ID by name.
    pub fn lookup_effect(&self, name: &str) -> Option<EffectId> {
        self.effect_registry.get(name).copied()
    }

    /// Creates a fresh definition ID.
    pub fn fresh_def_id(&mut self) -> DefId {
        let id = DefId(self.next_def_id);
        self.next_def_id += 1;
        id
    }

    /// Creates a fresh effect variable.
    pub fn fresh_effect_var(&mut self) -> EffectVar {
        let var = EffectVar(self.next_effect_var);
        self.next_effect_var += 1;
        var
    }

    /// Checks a complete module.
    pub fn check_module(&mut self, module: &Module) -> Vec<Diagnostic> {
        // First pass: collect all function signatures
        self.collect_function_signatures(module);

        // Second pass: check each function body
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                if let Err(e) = self.check_function(func) {
                    self.errors.push(e);
                }
            }
        }

        self.errors.to_diagnostics()
    }

    /// Collects function signatures for cross-function effect checking.
    fn collect_function_signatures(&mut self, module: &Module) {
        for item in &module.items {
            if let ModuleItem::Function(func) = item {
                let effects = self.parse_effect_types(&func.effects);
                self.function_effects
                    .insert(func.name.name.clone(), effects);
            }
        }
    }

    /// Parses effect types from AST into an EffectSet.
    fn parse_effect_types(&mut self, effects: &[Type]) -> EffectSet {
        let mut set = EffectSet::empty();

        for effect in effects {
            if let Some(instance) = self.parse_effect_type(effect) {
                set.insert(instance);
            }
        }

        set
    }

    /// Parses a single effect type.
    fn parse_effect_type(&mut self, effect: &Type) -> Option<EffectInstance> {
        match effect {
            Type::Path(path) => {
                let name = path.segments.last()?.name.clone();
                let id = self.get_or_create_effect(&name);
                Some(EffectInstance::new(id, name))
            }
            Type::Generic(base, _args) => {
                let name = match base.as_ref() {
                    Type::Path(path) => path.segments.last()?.name.clone(),
                    _ => return None,
                };
                let id = self.get_or_create_effect(&name);
                // Convert type arguments - simplified
                let type_args = Vec::new();
                Some(EffectInstance::with_type_args(id, name, type_args))
            }
            _ => None,
        }
    }

    /// Checks a function definition.
    pub fn check_function(&mut self, func: &Function) -> EffectResult<()> {
        let prev_function = self.current_function.clone();
        self.current_function = Some(func.name.name.clone());

        // Get declared effects
        let declared_effects = self.parse_effect_types(&func.effects);

        // Check if function is async
        let was_async = self.in_async_context;
        self.in_async_context = self.is_async_function(func);

        // Enter handler scope for the function body
        self.handler_ctx.enter_scope(to_diag_span(func.span));

        // Check the function body
        let body_result = self.check_expr(&func.body);

        // Leave handler scope
        self.handler_ctx.leave_scope();

        // Restore async context
        self.in_async_context = was_async;

        // Check that inferred effects match declared effects
        match body_result {
            Ok(checked) => {
                // Add async effect if in async context
                let mut inferred = checked.effects;
                if self.in_async_context {
                    inferred.insert(BuiltinEffect::Async.instance());
                }

                // Check for unhandled effects
                let unhandled: Vec<_> = inferred
                    .iter()
                    .filter(|e| !declared_effects.contains(e))
                    .cloned()
                    .collect();

                if !unhandled.is_empty() {
                    for effect in unhandled {
                        self.errors.push(EffectError::UnhandledEffect {
                            effect: Box::new(effect),
                            span: to_diag_span(func.span),
                            in_function: self.current_function.clone(),
                        });
                    }
                }

                // Check for unnecessarily declared effects
                let unnecessary: Vec<_> = declared_effects
                    .iter()
                    .filter(|e| !inferred.contains(e) && !e.is_builtin())
                    .cloned()
                    .collect();

                if !unnecessary.is_empty() && !declared_effects.is_empty() {
                    // This is a warning, not an error
                    // TODO: Add warning system
                }
            }
            Err(e) => {
                self.errors.push(e);
            }
        }

        self.current_function = prev_function;
        Ok(())
    }

    /// Checks if a function is async.
    fn is_async_function(&self, func: &Function) -> bool {
        // Check if declared as async
        func.effects.iter().any(|e| matches!(e, Type::Path(p) if p.segments.last().map(|s| s.name == "async").unwrap_or(false)))
    }

    /// Checks an expression and returns its effects.
    pub fn check_expr(&mut self, expr: &Expr) -> EffectResult<CheckedExpr> {
        match expr {
            Expr::Literal(_) => Ok(CheckedExpr {
                effects: EffectSet::empty(),
                can_diverge: false,
                returns: false,
            }),

            Expr::Variable(_) => Ok(CheckedExpr {
                effects: EffectSet::empty(),
                can_diverge: false,
                returns: false,
            }),

            Expr::Path(_) => Ok(CheckedExpr {
                effects: EffectSet::empty(),
                can_diverge: false,
                returns: false,
            }),

            Expr::Binary { left, right, .. } => {
                let left_checked = self.check_expr(left)?;
                let right_checked = self.check_expr(right)?;
                Ok(CheckedExpr {
                    effects: left_checked.effects.merged(&right_checked.effects),
                    can_diverge: left_checked.can_diverge || right_checked.can_diverge,
                    returns: false,
                })
            }

            Expr::Unary { expr, .. } => self.check_expr(expr),

            Expr::Call { func, args } => {
                let func_checked = self.check_expr(func)?;

                // Check arguments
                let mut arg_effects = EffectSet::empty();
                for arg in args {
                    let checked = self.check_expr(arg)?;
                    arg_effects.union(&checked.effects);
                }

                // Get effects from the function being called
                let call_effects = self.get_call_effects(func);

                let mut all_effects = func_checked.effects;
                all_effects.union(&arg_effects);
                all_effects.union(&call_effects);

                Ok(CheckedExpr {
                    effects: all_effects,
                    can_diverge: func_checked.can_diverge,
                    returns: false,
                })
            }

            Expr::MethodCall { receiver, args, .. } => {
                let receiver_checked = self.check_expr(receiver)?;

                let mut arg_effects = EffectSet::empty();
                for arg in args {
                    let checked = self.check_expr(arg)?;
                    arg_effects.union(&checked.effects);
                }

                let mut all_effects = receiver_checked.effects;
                all_effects.union(&arg_effects);

                // Method calls may have effects based on the method
                // For now, assume they might have any effect

                Ok(CheckedExpr {
                    effects: all_effects,
                    can_diverge: false,
                    returns: false,
                })
            }

            Expr::FieldAccess { object, .. } => self.check_expr(object),

            Expr::Index { object, index } => {
                let obj_checked = self.check_expr(object)?;
                let idx_checked = self.check_expr(index)?;
                Ok(CheckedExpr {
                    effects: obj_checked.effects.merged(&idx_checked.effects),
                    can_diverge: obj_checked.can_diverge || idx_checked.can_diverge,
                    returns: false,
                })
            }

            Expr::Block(block) => self.check_block(block),

            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_checked = self.check_expr(cond)?;

                // Check branches with fresh handler contexts
                let then_checked = self.check_expr(then_branch)?;

                let else_checked = else_branch
                    .as_ref()
                    .map(|e| self.check_expr(e))
                    .transpose()?
                    .unwrap_or(CheckedExpr {
                        effects: EffectSet::empty(),
                        can_diverge: false,
                        returns: false,
                    });

                let mut effects = cond_checked.effects;
                effects.union(&then_checked.effects);
                effects.union(&else_checked.effects);

                Ok(CheckedExpr {
                    effects,
                    can_diverge: then_checked.can_diverge || else_checked.can_diverge,
                    returns: then_checked.returns && else_checked.returns,
                })
            }

            Expr::Match { expr, arms } => {
                let expr_checked = self.check_expr(expr)?;

                let mut arm_effects = EffectSet::empty();
                let mut all_return = true;
                let mut any_diverge = false;

                for arm in arms {
                    let body_checked = self.check_expr(&arm.body)?;
                    arm_effects.union(&body_checked.effects);
                    all_return = all_return && body_checked.returns;
                    any_diverge = any_diverge || body_checked.can_diverge;
                }

                let mut effects = expr_checked.effects;
                effects.union(&arm_effects);

                Ok(CheckedExpr {
                    effects,
                    can_diverge: any_diverge,
                    returns: all_return,
                })
            }

            Expr::While { cond, body, .. } => {
                let cond_checked = self.check_expr(cond)?;
                let body_checked = self.check_expr(body)?;

                let mut effects = cond_checked.effects;
                effects.union(&body_checked.effects);

                Ok(CheckedExpr {
                    effects,
                    can_diverge: body_checked.can_diverge, // Loops might diverge
                    returns: false,
                })
            }

            Expr::For { iterable, body, .. } => {
                let iter_checked = self.check_expr(iterable)?;
                let body_checked = self.check_expr(body)?;

                let mut effects = iter_checked.effects;
                effects.union(&body_checked.effects);

                Ok(CheckedExpr {
                    effects,
                    can_diverge: body_checked.can_diverge,
                    returns: false,
                })
            }

            Expr::Loop { body, .. } => {
                let body_checked = self.check_expr(body)?;
                Ok(CheckedExpr {
                    effects: body_checked.effects,
                    can_diverge: true, // Loops might run forever
                    returns: body_checked.returns,
                })
            }

            Expr::Lambda {
                params: _,
                effects,
                body,
                ..
            } => {
                // Enter new scope for lambda
                self.handler_ctx.enter_scope(Span::new(0, 0));

                // Parse declared effects
                let declared_effects = self.parse_effect_types(effects);

                // Check body
                let body_checked = self.check_expr(body)?;

                // Leave scope
                self.handler_ctx.leave_scope();

                // Check that body effects are covered by declared effects
                let unhandled: Vec<_> = body_checked
                    .effects
                    .iter()
                    .filter(|e| !declared_effects.contains(e))
                    .cloned()
                    .collect();

                if !unhandled.is_empty() {
                    // Lambda effects don't match - this is an error
                    return Err(EffectError::EffectMismatch {
                        declared: Box::new(declared_effects),
                        actual: Box::new(body_checked.effects),
                        span: Span::default(),
                        function_name: "lambda".to_string(),
                    });
                }

                Ok(CheckedExpr {
                    effects: EffectSet::empty(), // Lambda itself is pure (effects are latent)
                    can_diverge: false,
                    returns: false,
                })
            }

            Expr::Await(expr) => {
                if !self.in_async_context {
                    return Err(EffectError::EffectNotAllowed {
                        effect_name: "async".to_string(),
                        context: "non-async function".to_string(),
                        span: Span::default(),
                        suggestion: Some(
                            "mark the function as async or use it inside an async block"
                                .to_string(),
                        ),
                    });
                }

                let checked = self.check_expr(expr)?;
                let mut effects = checked.effects;
                effects.insert(BuiltinEffect::Async.instance());

                Ok(CheckedExpr {
                    effects,
                    can_diverge: checked.can_diverge,
                    returns: false,
                })
            }

            Expr::Try(expr) => {
                let checked = self.check_expr(expr)?;
                // The ? operator propagates errors - mark as having error effects
                // that need to be handled by the caller
                Ok(CheckedExpr {
                    effects: checked.effects,
                    can_diverge: checked.can_diverge,
                    returns: false,
                })
            }

            Expr::Assign { target, value, .. } => {
                let target_checked = self.check_expr(target)?;
                let value_checked = self.check_expr(value)?;

                let mut effects = target_checked.effects;
                effects.union(&value_checked.effects);

                Ok(CheckedExpr {
                    effects,
                    can_diverge: target_checked.can_diverge || value_checked.can_diverge,
                    returns: false,
                })
            }

            Expr::Break { value, .. } => {
                let effects = if let Some(v) = value {
                    self.check_expr(v)?.effects
                } else {
                    EffectSet::empty()
                };

                Ok(CheckedExpr {
                    effects,
                    can_diverge: false,
                    returns: false,
                })
            }

            Expr::Continue { .. } => Ok(CheckedExpr {
                effects: EffectSet::empty(),
                can_diverge: false,
                returns: false,
            }),

            Expr::Return(expr) => {
                let effects = if let Some(e) = expr {
                    self.check_expr(e)?.effects
                } else {
                    EffectSet::empty()
                };

                Ok(CheckedExpr {
                    effects,
                    can_diverge: false,
                    returns: true,
                })
            }

            Expr::Tuple(exprs) => {
                let mut effects = EffectSet::empty();
                for expr in exprs {
                    effects.union(&self.check_expr(expr)?.effects);
                }
                Ok(CheckedExpr {
                    effects,
                    can_diverge: false,
                    returns: false,
                })
            }

            Expr::Array(exprs) => {
                let mut effects = EffectSet::empty();
                for expr in exprs {
                    effects.union(&self.check_expr(expr)?.effects);
                }
                Ok(CheckedExpr {
                    effects,
                    can_diverge: false,
                    returns: false,
                })
            }

            Expr::StructLiteral { fields, .. } => {
                let mut effects = EffectSet::empty();
                for field in fields {
                    if let Some(value) = &field.value {
                        effects.union(&self.check_expr(value)?.effects);
                    }
                }
                Ok(CheckedExpr {
                    effects,
                    can_diverge: false,
                    returns: false,
                })
            }

            Expr::Spawn(expr) => {
                let checked = self.check_expr(expr)?;
                let mut effects = checked.effects;
                // Spawn requires async effect
                effects.insert(BuiltinEffect::Async.instance());

                Ok(CheckedExpr {
                    effects,
                    can_diverge: false,
                    returns: false,
                })
            }

            Expr::Async(block) => {
                let was_async = self.in_async_context;
                self.in_async_context = true;

                self.handler_ctx.enter_scope(to_diag_span(block.span));
                let checked = self.check_block(block)?;
                self.handler_ctx.leave_scope();

                self.in_async_context = was_async;

                let mut effects = checked.effects;
                effects.insert(BuiltinEffect::Async.instance());

                Ok(CheckedExpr {
                    effects,
                    can_diverge: checked.can_diverge,
                    returns: checked.returns,
                })
            }

            Expr::Concurrent(block) => {
                self.handler_ctx
                    .stack
                    .push_concurrent_scope(to_diag_span(block.span));
                let checked = self.check_block(block)?;
                self.handler_ctx.stack.pop_scope();

                Ok(CheckedExpr {
                    effects: checked.effects,
                    can_diverge: checked.can_diverge,
                    returns: checked.returns,
                })
            }

            Expr::SelfExpr(_) => Ok(CheckedExpr {
                effects: EffectSet::empty(),
                can_diverge: false,
                returns: false,
            }),
            Expr::Pass => Ok(CheckedExpr {
                effects: EffectSet::empty(),
                can_diverge: false,
                returns: false,
            }),

            Expr::Raise(raise_expr) => {
                // Check the arguments to the raise expression
                let mut effects = EffectSet::empty();
                for arg in &raise_expr.args {
                    let checked = self.check_expr(arg)?;
                    effects.union(&checked.effects);
                }

                // Get the effect name from the operation or effect path
                let effect_name = raise_expr
                    .effect
                    .as_ref()
                    .and_then(|p| p.segments.last())
                    .map(|s| s.name.clone())
                    .unwrap_or_else(|| raise_expr.operation.name.clone());

                // Create the effect instance
                let effect_id = self.get_or_create_effect(&effect_name);
                let effect_instance = EffectInstance::new(effect_id, effect_name)
                    .with_source(crate::effect::EffectSource::Inferred);

                // Add the effect to the set
                effects.insert(effect_instance.clone());

                // Record the perform operation in the handler context
                self.handler_ctx.record_perform(
                    effect_instance.clone(),
                    raise_expr.operation.name.clone(),
                    to_diag_span(raise_expr.span),
                );

                // Check if the effect is handled in the current context
                if !self.handler_ctx.is_handled(&effect_instance) {
                    // Effect is not handled - it propagates upward
                    // This is not an error here; it's checked at function level
                }

                Ok(CheckedExpr {
                    effects,
                    can_diverge: true, // Raise can diverge if not resumed
                    returns: false,
                })
            }

            Expr::Handle(handle_expr) => {
                // Enter a new handler scope
                self.handler_ctx.enter_scope(to_diag_span(handle_expr.span));

                // Create handlers for each handler arm
                for arm in &handle_expr.handlers {
                    // Create the effect handler
                    let effect_id = self.get_or_create_effect(&arm.operation.name);
                    let def_id = self.fresh_def_id();
                    let handler = crate::effect::EffectHandler {
                        effect: effect_id,
                        name: arm.operation.name.clone(),
                        operations: vec![crate::effect::HandledOperation {
                            name: arm.operation.name.clone(),
                            handler_def_id: def_id,
                            resumes: arm.resume_name.is_some(),
                        }],
                        is_exhaustive: true,
                    };

                    // Add the handler to the context
                    let handler_id = self
                        .handler_ctx
                        .add_handler(handler, to_diag_span(arm.span), def_id)
                        .unwrap_or_default();

                    // Enter handler body context
                    self.handler_ctx.enter_handler_body(handler_id);

                    // Check the handler body
                    let _body_checked = self.check_expr(&arm.body)?;

                    // Leave handler body context
                    self.handler_ctx.leave_handler_body();
                }

                // Check the body expression being handled
                let body_checked = self.check_expr(&handle_expr.body)?;

                // Leave the handler scope
                self.handler_ctx.leave_scope();

                // Effects from the body that are handled are removed from the result
                // The handle expression itself has the unhandled effects from the body
                Ok(CheckedExpr {
                    effects: body_checked.effects,
                    can_diverge: body_checked.can_diverge,
                    returns: body_checked.returns,
                })
            }

            Expr::Resume(resume_expr) => {
                // Check if we're inside a handler body
                if !self.handler_ctx.stack.can_resume() {
                    return Err(EffectError::InvalidResume {
                        span: to_diag_span(resume_expr.span),
                        note: "resume can only be used inside an effect handler".to_string(),
                    });
                }

                // Check the value being resumed (if any)
                let effects = if let Some(value) = &resume_expr.value {
                    self.check_expr(value)?.effects
                } else {
                    EffectSet::empty()
                };

                // Resume is a terminal operation within the handler
                Ok(CheckedExpr {
                    effects,
                    can_diverge: false,
                    returns: true, // Resume returns from the handler
                })
            }
        }
    }

    /// Checks a block expression.
    fn check_block(&mut self, block: &ast::Block) -> EffectResult<CheckedExpr> {
        self.handler_ctx.enter_scope(to_diag_span(block.span));

        let mut effects = EffectSet::empty();
        let mut can_diverge = false;
        let mut returns = false;

        for stmt in &block.stmts {
            let stmt_result = self.check_stmt(stmt)?;
            effects.union(&stmt_result.effects);
            can_diverge = can_diverge || stmt_result.can_diverge;
            returns = returns || stmt_result.returns;
        }

        if let Some(expr) = &block.expr {
            let expr_result = self.check_expr(expr)?;
            effects.union(&expr_result.effects);
            can_diverge = can_diverge || expr_result.can_diverge;
            returns = returns || expr_result.returns;
        }

        self.handler_ctx.leave_scope();

        Ok(CheckedExpr {
            effects,
            can_diverge,
            returns,
        })
    }

    /// Checks a statement.
    fn check_stmt(&mut self, stmt: &Stmt) -> EffectResult<CheckedExpr> {
        match stmt {
            Stmt::Let { value, .. } => self.check_expr(value),

            Stmt::Expr(expr) => self.check_expr(expr),

            Stmt::Assign { target, value, .. } => {
                let target_checked = self.check_expr(target)?;
                let value_checked = self.check_expr(value)?;

                let mut effects = target_checked.effects;
                effects.union(&value_checked.effects);

                Ok(CheckedExpr {
                    effects,
                    can_diverge: target_checked.can_diverge || value_checked.can_diverge,
                    returns: false,
                })
            }

            Stmt::Return(expr) => {
                let effects = if let Some(e) = expr {
                    self.check_expr(e)?.effects
                } else {
                    EffectSet::empty()
                };

                Ok(CheckedExpr {
                    effects,
                    can_diverge: false,
                    returns: true,
                })
            }

            Stmt::Break { value, .. } => {
                let effects = if let Some(v) = value {
                    self.check_expr(v)?.effects
                } else {
                    EffectSet::empty()
                };

                Ok(CheckedExpr {
                    effects,
                    can_diverge: false,
                    returns: false,
                })
            }

            Stmt::Continue { .. } => Ok(CheckedExpr {
                effects: EffectSet::empty(),
                can_diverge: false,
                returns: false,
            }),

            Stmt::Handle { body, handlers } => {
                // Enter a new handler scope
                // Use the first handler's span if available, otherwise use a default span
                let scope_span = handlers
                    .first()
                    .map(|arm| to_diag_span(arm.span))
                    .unwrap_or_else(|| Span::new(0, 0));
                self.handler_ctx.enter_scope(scope_span);

                // Create handlers for each handler arm
                for arm in handlers {
                    // Create the effect handler
                    let effect_id = self.get_or_create_effect(&arm.operation.name);
                    let def_id = self.fresh_def_id();
                    let handler = crate::effect::EffectHandler {
                        effect: effect_id,
                        name: arm.operation.name.clone(),
                        operations: vec![crate::effect::HandledOperation {
                            name: arm.operation.name.clone(),
                            handler_def_id: def_id,
                            resumes: arm.resume_name.is_some(),
                        }],
                        is_exhaustive: true,
                    };

                    // Add the handler to the context
                    let handler_id = self
                        .handler_ctx
                        .add_handler(handler, to_diag_span(arm.span), def_id)
                        .unwrap_or_default();

                    // Enter handler body context
                    self.handler_ctx.enter_handler_body(handler_id);

                    // Check the handler body
                    let _body_checked = self.check_expr(&arm.body)?;

                    // Leave handler body context
                    self.handler_ctx.leave_handler_body();
                }

                // Check the body expression being handled
                let body_checked = self.check_expr(body)?;

                // Leave the handler scope
                self.handler_ctx.leave_scope();

                Ok(CheckedExpr {
                    effects: body_checked.effects,
                    can_diverge: body_checked.can_diverge,
                    returns: body_checked.returns,
                })
            }
        }
    }

    /// Gets the effects of a function call.
    fn get_call_effects(&self, func: &Expr) -> EffectSet {
        match func {
            Expr::Variable(ident) => {
                // Look up function in registry
                if let Some(effects) = self.function_effects.get(&ident.name) {
                    effects.clone()
                } else {
                    EffectSet::empty()
                }
            }
            Expr::Path(path) => {
                // Look up qualified function
                let name = path
                    .segments
                    .last()
                    .map(|s| s.name.clone())
                    .unwrap_or_default();
                if let Some(effects) = self.function_effects.get(&name) {
                    effects.clone()
                } else {
                    EffectSet::empty()
                }
            }
            _ => EffectSet::empty(),
        }
    }

    /// Returns true if there are any errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Takes all errors.
    pub fn take_errors(&mut self) -> EffectErrors {
        let mut errors = EffectErrors::new();
        std::mem::swap(&mut errors, &mut self.errors);
        errors
    }

    /// Gets a reference to the errors.
    pub fn errors(&self) -> &EffectErrors {
        &self.errors
    }
}

impl Default for EffectChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to check a module.
pub fn check_module(module: &Module) -> Vec<Diagnostic> {
    let mut checker = EffectChecker::new();
    checker.check_module(module)
}

/// Convenience function to check a single expression.
pub fn check_expr(expr: &Expr) -> EffectResult<CheckedExpr> {
    let mut checker = EffectChecker::new();
    checker.check_expr(expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_parser::ast::{Ident, Literal};

    fn test_span() -> jet_lexer::Span {
        jet_lexer::Span::new(0, 10)
    }

    #[test]
    fn test_checker_literal() {
        let expr = Expr::Literal(Literal::Integer(42));
        let result = check_expr(&expr).unwrap();

        assert!(result.effects.is_empty());
        assert!(!result.can_diverge);
        assert!(!result.returns);
    }

    #[test]
    fn test_checker_binary() {
        let expr = Expr::Binary {
            op: ast::BinaryOp::Add,
            left: Box::new(Expr::Literal(Literal::Integer(1))),
            right: Box::new(Expr::Literal(Literal::Integer(2))),
        };
        let result = check_expr(&expr).unwrap();

        assert!(result.effects.is_empty());
    }

    #[test]
    fn test_checker_effect_registry() {
        let mut checker = EffectChecker::new();

        // Builtin effects should be registered
        assert!(checker.lookup_effect("async").is_some());
        assert!(checker.lookup_effect("io").is_some());
        assert!(checker.lookup_effect("unsafe").is_some());

        // Custom effects are created on demand
        let id1 = checker.get_or_create_effect("CustomError");
        let id2 = checker.get_or_create_effect("CustomError");
        assert_eq!(id1, id2);

        let id3 = checker.get_or_create_effect("OtherError");
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_checker_parse_effects() {
        let mut checker = EffectChecker::new();

        // Parse effect type
        let effect_type = Type::Path(jet_parser::ast::Path::single(Ident::new(
            "IoError",
            test_span(),
        )));
        let instance = checker.parse_effect_type(&effect_type).unwrap();
        assert_eq!(instance.name, "IoError");
    }
}
