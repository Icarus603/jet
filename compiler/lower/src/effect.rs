//! Effect lowering from AST to IR.
//!
//! This module provides functions for lowering effect operations (perform, handle, resume)
//! to Jet IR instructions.

use crate::context::{HandlerContext, LoweringContext};
use crate::expr::infer_expr_type;

use crate::expr::lower_expr;
use jet_ir::{BlockId, Instruction, Terminator, Ty, ValueId};
use jet_parser::ast;

/// Information about a handler case.
#[derive(Debug, Clone)]
pub struct HandlerCase {
    /// The operation name being handled.
    pub operation: String,
    /// The block containing the handler body.
    pub handler_block: BlockId,
    /// The parameter value for the operation argument.
    pub param_value: ValueId,
}

/// Lowers a handle expression (effect handler).
///
/// A handle expression creates a handler context that intercepts `perform` operations
/// within its body and dispatches them to the appropriate handler cases.
pub fn lower_handle(
    ctx: &mut LoweringContext,
    body: &ast::Expr,
    handlers: &[ast::HandlerArm],
) -> ValueId {
    // Create blocks for the handler structure
    let resume_block = ctx.create_block("resume");
    let exit_block = ctx.create_block("handle_exit");

    // Create handler blocks for each operation
    let mut handler_cases = Vec::new();
    for (i, handler) in handlers.iter().enumerate() {
        let handler_block = ctx.create_block(format!("handler_{}", i));

        // Create parameter for the handler
        let param_value = ctx.new_value();

        handler_cases.push(HandlerCase {
            operation: handler.operation.name.clone(),
            handler_block,
            param_value,
        });
    }

    // Build handler context
    let handler_map: std::collections::HashMap<String, BlockId> = handler_cases
        .iter()
        .map(|case| (case.operation.clone(), case.handler_block))
        .collect();

    let handler_ctx = HandlerContext {
        resume_block,
        handlers: handler_map,
    };

    // Push handler context and lower the body
    let old_handler = ctx.push_handler_context(handler_ctx);

    // Lower the body in a new scope
    ctx.enter_scope();
    let body_value = lower_expr(ctx, body);
    ctx.exit_scope();

    // Pop handler context
    ctx.pop_handler_context(old_handler);

    // If body didn't terminate, branch to exit
    if let Some(current) = ctx.get_current_block() {
        if !current.is_terminated() {
            ctx.terminate(Terminator::Branch(exit_block));
        }
    }

    // Lower each handler case
    for (i, handler) in handlers.iter().enumerate() {
        let case = &handler_cases[i];
        ctx.set_current_block(case.handler_block);

        ctx.enter_scope();

        // Bind the operation parameters
        for (j, param) in handler.params.iter().enumerate() {
            let param_val = ctx.new_value();
            // Extract parameter from the operation payload
            ctx.emit(Instruction::ExtractField {
                result: param_val,
                aggregate: case.param_value,
                field_index: j,
            });
            bind_handler_pattern(ctx, param, param_val);
        }

        // Bind resume function if named
        if let Some(ref resume_name) = handler.resume_name {
            let resume_val = ctx.new_value();
            ctx.bind_variable(
                resume_name.name.clone(),
                resume_val,
                Ty::Ptr(Box::new(Ty::Void)),
                false,
            );
        }

        // Lower the handler body
        let handler_result = lower_expr(ctx, &handler.body);

        // If not terminated, resume with the result
        if let Some(current) = ctx.get_current_block() {
            if !current.is_terminated() {
                ctx.emit(Instruction::Resume {
                    value: handler_result,
                });
                ctx.terminate(Terminator::Branch(resume_block));
            }
        }

        ctx.exit_scope();
    }

    // Resume block - continues after effect handling
    ctx.set_current_block(resume_block);
    let resume_result = ctx.new_value();
    // The resume result comes from the handler that resumed
    let resume_ty = infer_expr_type(ctx, body);
    ctx.emit(Instruction::Phi {
        result: resume_result,
        incoming: handler_cases
            .iter()
            .map(|case| (case.handler_block, body_value))
            .collect(),
        ty: resume_ty,
    });

    // Exit block
    ctx.set_current_block(exit_block);

    // Return the final result
    resume_result
}

/// Lowers a perform expression (effect operation).
///
/// A perform expression invokes an effect operation, which may be handled
/// by an enclosing handler. If no handler is found, this is an error.
pub fn lower_perform(ctx: &mut LoweringContext, operation: &str, args: &[ast::Expr]) -> ValueId {
    // Check if we have an active handler context
    if let Some(handler_ctx) = ctx.handler_context().cloned() {
        // Look up the handler for this operation
        if let Some(&handler_block) = handler_ctx.handlers.get(operation) {
            // Lower the arguments
            let arg_values: Vec<ValueId> = args.iter().map(|arg| lower_expr(ctx, arg)).collect();

            // Create the result value
            let result = ctx.new_value();

            // Emit perform instruction
            // This represents the effect operation that will be handled
            ctx.emit(Instruction::Call {
                result,
                func: format!("perform_{}", operation),
                args: arg_values.clone(),
                ty: Ty::I64, // Placeholder for handled effect result
            });

            // Branch to the handler block
            ctx.terminate(Terminator::Branch(handler_block));

            // Continue in the resume block after the handler returns
            ctx.set_current_block(handler_ctx.resume_block);

            result
        } else {
            // No handler found for this operation - this is an unhandled effect
            panic!("Unhandled effect operation: {}", operation);
        }
    } else {
        // No handler context - this is an unhandled effect
        panic!("Effect operation outside of handler context: {}", operation);
    }
}

/// Lowers a resume expression.
///
/// A resume expression continues execution from a handler with a value.
/// It can only appear within a handler body.
pub fn lower_resume(ctx: &mut LoweringContext, value: &ast::Expr) -> ValueId {
    let resume_val = lower_expr(ctx, value);

    // Emit resume instruction
    ctx.emit(Instruction::Resume { value: resume_val });

    // Resume doesn't produce a value in the normal sense,
    // but we return the value for completeness
    resume_val
}

/// Lowers a raise expression (error handling).
///
/// A raise expression throws an error that can be caught by error handlers.
pub fn lower_raise(ctx: &mut LoweringContext, error: &ast::Expr) -> ValueId {
    let error_val = lower_expr(ctx, error);

    // Create error handling blocks
    let error_block = ctx.create_block("error_handler");
    let continue_block = ctx.create_block("after_error");

    // For now, lower to a call to the error handler
    let result = ctx.new_value();
    ctx.emit(Instruction::Call {
        result,
        func: "raise".to_string(),
        args: vec![error_val],
        ty: Ty::Void,
    });

    // Branch to error handling
    ctx.terminate(Terminator::CondBranch {
        cond: result,
        then_block: error_block,
        else_block: continue_block,
    });

    // Continue in the normal path
    ctx.set_current_block(continue_block);

    result
}

/// Binds a handler pattern to a value.
fn bind_handler_pattern(ctx: &mut LoweringContext, pattern: &ast::Pattern, value: ValueId) {
    match pattern {
        ast::Pattern::Ident { name, mutable } => {
            ctx.bind_variable(name.name.clone(), value, Ty::I64, *mutable);
        }
        ast::Pattern::Tuple(patterns) => {
            for (i, pat) in patterns.iter().enumerate() {
                let elem_val = ctx.new_value();
                ctx.emit(Instruction::ExtractField {
                    result: elem_val,
                    aggregate: value,
                    field_index: i,
                });
                bind_handler_pattern(ctx, pat, elem_val);
            }
        }
        ast::Pattern::Wildcard(_) => {
            // Nothing to bind
        }
        _ => {
            // Other patterns - simplified handling
        }
    }
}

/// Lowers a try-catch style expression.
///
/// This handles errors that may be raised during execution.
pub fn lower_try_catch(
    ctx: &mut LoweringContext,
    try_body: &ast::Expr,
    catch_patterns: &[(ast::Pattern, ast::Expr)],
) -> ValueId {
    let try_block = ctx.create_block("try_body");
    let catch_block = ctx.create_block("catch_handler");
    let merge_block = ctx.create_block("try_merge");

    // Branch to try body
    ctx.terminate(Terminator::Branch(try_block));

    // Lower try body
    ctx.set_current_block(try_block);
    ctx.enter_scope();

    // Set up error handling context
    // In a real implementation, this would register the catch block as an error handler

    let try_result = lower_expr(ctx, try_body);
    ctx.exit_scope();

    let try_terminated = ctx
        .get_current_block()
        .map(|b| b.is_terminated())
        .unwrap_or(false);
    if !try_terminated {
        ctx.terminate(Terminator::Branch(merge_block));
    }

    // Lower catch handlers
    ctx.set_current_block(catch_block);

    let mut catch_results = Vec::new();

    for (pattern, handler_body) in catch_patterns {
        ctx.enter_scope();

        // Bind the error pattern
        let error_val = ctx.new_value();
        bind_handler_pattern(ctx, pattern, error_val);

        let catch_result = lower_expr(ctx, handler_body);
        catch_results.push((ctx.current_block().unwrap_or(catch_block), catch_result));

        ctx.exit_scope();

        if let Some(current) = ctx.get_current_block() {
            if !current.is_terminated() {
                ctx.terminate(Terminator::Branch(merge_block));
            }
        }
    }

    // Merge block with phi
    ctx.set_current_block(merge_block);
    let result = ctx.new_value();

    let mut incoming = vec![(try_block, try_result)];
    incoming.extend(catch_results);

    let phi_ty = infer_expr_type(ctx, try_body);
    ctx.emit(Instruction::Phi {
        result,
        incoming,
        ty: phi_ty,
    });

    result
}

/// Lowers an effect block (explicit effect marking).
pub fn lower_effect_block(
    ctx: &mut LoweringContext,
    effects: &[ast::Type],
    body: &ast::Expr,
) -> ValueId {
    // Track the effects that this block may perform
    let _effect_names: Vec<String> = effects
        .iter()
        .filter_map(|ty| {
            if let ast::Type::Path(path) = ty {
                path.segments.first().map(|s| s.name.clone())
            } else {
                None
            }
        })
        .collect();

    // Lower the body normally
    // The effects are tracked for type checking and validation
    lower_expr(ctx, body)
}

/// Extracts the operation name from a pattern.
///
/// This is used to identify which operation a handler case handles.
#[allow(dead_code)]
fn get_operation_name(pattern: &ast::Pattern) -> String {
    match pattern {
        ast::Pattern::Ident { name, .. } => name.name.clone(),
        ast::Pattern::Enum { variant, .. } => variant.name.clone(),
        ast::Pattern::Struct { path, .. } => path
            .segments
            .last()
            .map(|s| s.name.clone())
            .unwrap_or_else(|| "unknown".to_string()),
        _ => "unknown".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::{Ident, Literal, Pattern};

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, Span::new(0, 0))
    }

    // Note: HandlerArm is not in the parser's ast module yet
    // These tests would need to be updated when the AST is extended

    #[test]
    fn test_get_operation_name() {
        let ident_pattern = Pattern::Ident {
            mutable: false,
            name: make_ident("Get"),
        };
        assert_eq!(get_operation_name(&ident_pattern), "Get");

        let wildcard = Pattern::Wildcard(Span::new(0, 0));
        assert_eq!(get_operation_name(&wildcard), "unknown");
    }

    #[test]
    fn test_lower_resume() {
        let tcx = jet_typeck::TypeContext::new();
        let mut ctx = LoweringContext::new("test", &tcx);

        // Add a function
        let func = jet_ir::Function::new("test_func", vec![], Ty::Void);
        ctx.module.add_function(func);
        ctx.start_function(0);

        let entry = ctx.create_block("entry");
        ctx.set_current_block(entry);

        // Set up handler context
        let resume_block = ctx.create_block("resume");
        let handler_ctx = HandlerContext {
            resume_block,
            handlers: std::collections::HashMap::new(),
        };
        ctx.push_handler_context(handler_ctx);

        // resume 42
        let _result = lower_resume(&mut ctx, &ast::Expr::Literal(Literal::Integer(42)));

        let block = ctx.get_current_block().unwrap();
        assert!(block
            .instructions
            .iter()
            .any(|i| matches!(i, Instruction::Resume { .. })));
    }
}
