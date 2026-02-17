//! Closure conversion for lambda expressions.
//!
//! This module implements closure conversion, which transforms lambda expressions
//! into heap-allocated closure structures containing:
//! 1. A pointer to the function code
//! 2. An environment struct containing captured variables
//!
//! The closure conversion process:
//! 1. Identify captured variables (free variables in the lambda body)
//! 2. Create an environment struct type
//! 3. Generate a top-level function that takes the environment as first parameter
//! 4. Create a closure value containing the function pointer and environment

use crate::context::LoweringContext;
use crate::ty::lower_type;
use jet_ir::{ConstantValue, Function, Instruction, Param, Terminator, Ty, ValueId};
use jet_parser::ast;
use std::collections::HashSet;

/// Information about a closure.
#[derive(Debug, Clone)]
pub struct ClosureInfo {
    /// The name of the generated function.
    pub function_name: String,
    /// The captured variables (name, type, is_mutable).
    pub captures: Vec<(String, Ty, bool)>,
    /// The environment struct type.
    pub env_type: Ty,
}

/// Converts a lambda expression to a closure.
///
/// Returns the value ID of the closure struct.
pub fn convert_lambda(
    ctx: &mut LoweringContext,
    params: &[ast::Param],
    return_type: Option<&ast::Type>,
    effects: &[ast::Type],
    body: &ast::Expr,
) -> ValueId {
    // Identify captured variables
    let captures = identify_captures(ctx, body);

    // Generate a unique name for the closure function
    let module_name = ctx.module.name.clone();
    let unique_id = ctx.next_value_id();
    let function_name = format!("__closure_{}_{}", module_name, unique_id.0);

    // Create environment struct type
    let env_field_types: Vec<Ty> = captures.iter().map(|(_, ty, _)| ty.clone()).collect();
    let env_type = Ty::Struct(env_field_types);

    // Create the closure function (this adds it to the module and returns the function)
    let _closure_func = create_closure_function(
        ctx,
        &function_name,
        params,
        return_type,
        effects,
        body,
        &captures,
        &env_type,
    );

    // Find the function index (it was added by create_closure_function)
    let func_index = ctx
        .module
        .functions
        .iter()
        .position(|f| f.name == function_name)
        .expect("Closure function should have been added to module");

    // Create the closure value (function pointer + environment)
    create_closure_value(ctx, func_index, &captures, &env_type)
}

/// Identifies variables captured by a lambda expression.
fn identify_captures(ctx: &LoweringContext, body: &ast::Expr) -> Vec<(String, Ty, bool)> {
    let mut free_vars = HashSet::new();
    collect_free_variables(body, &mut free_vars);

    // Filter to only variables that exist in outer scopes
    let mut captures = Vec::new();
    for var_name in free_vars {
        if let Some(ty) = ctx.lookup_variable_type(&var_name) {
            let is_mut = ctx.is_variable_mutable(&var_name).unwrap_or(false);
            captures.push((var_name, ty.clone(), is_mut));
        }
    }

    captures
}

/// Collects free variables from an expression.
fn collect_free_variables(expr: &ast::Expr, free_vars: &mut HashSet<String>) {
    match expr {
        ast::Expr::Variable(ident) => {
            free_vars.insert(ident.name.clone());
        }
        ast::Expr::Binary { left, right, .. } => {
            collect_free_variables(left, free_vars);
            collect_free_variables(right, free_vars);
        }
        ast::Expr::Unary { expr, .. } => {
            collect_free_variables(expr, free_vars);
        }
        ast::Expr::Call { func, args } => {
            collect_free_variables(func, free_vars);
            for arg in args {
                collect_free_variables(arg, free_vars);
            }
        }
        ast::Expr::MethodCall { receiver, args, .. } => {
            collect_free_variables(receiver, free_vars);
            for arg in args {
                collect_free_variables(arg, free_vars);
            }
        }
        ast::Expr::FieldAccess { object, .. } => {
            collect_free_variables(object, free_vars);
        }
        ast::Expr::Index { object, index } => {
            collect_free_variables(object, free_vars);
            collect_free_variables(index, free_vars);
        }
        ast::Expr::Block(block) => {
            collect_free_variables_from_block(block, free_vars);
        }
        ast::Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_free_variables(cond, free_vars);
            collect_free_variables(then_branch, free_vars);
            if let Some(else_expr) = else_branch {
                collect_free_variables(else_expr, free_vars);
            }
        }
        ast::Expr::Match { expr, arms } => {
            collect_free_variables(expr, free_vars);
            for arm in arms {
                collect_free_variables(&arm.body, free_vars);
                if let Some(ref guard) = arm.guard {
                    collect_free_variables(guard, free_vars);
                }
            }
        }
        ast::Expr::While { cond, body, .. } => {
            collect_free_variables(cond, free_vars);
            collect_free_variables(body, free_vars);
        }
        ast::Expr::For { iterable, body, .. } => {
            collect_free_variables(iterable, free_vars);
            collect_free_variables(body, free_vars);
        }
        ast::Expr::Loop { body, .. } => {
            collect_free_variables(body, free_vars);
        }
        ast::Expr::Tuple(elements) => {
            for elem in elements {
                collect_free_variables(elem, free_vars);
            }
        }
        ast::Expr::Array(elements) => {
            for elem in elements {
                collect_free_variables(elem, free_vars);
            }
        }
        ast::Expr::Assign { target, value, .. } => {
            collect_free_variables(target, free_vars);
            collect_free_variables(value, free_vars);
        }
        ast::Expr::Return(Some(expr)) => {
            collect_free_variables(expr, free_vars);
        }
        ast::Expr::Return(None) => {}
        ast::Expr::Break {
            value: Some(expr), ..
        } => {
            collect_free_variables(expr, free_vars);
        }
        ast::Expr::Break { value: None, .. } => {}
        ast::Expr::Await(expr) => {
            collect_free_variables(expr, free_vars);
        }
        ast::Expr::Try(expr) => {
            collect_free_variables(expr, free_vars);
        }
        ast::Expr::StructLiteral { fields, .. } => {
            for field in fields {
                if let Some(ref value) = field.value {
                    collect_free_variables(value, free_vars);
                }
            }
        }
        // Lambda expressions don't capture from their own body
        ast::Expr::Lambda { .. } => {
            // Nested lambdas are handled separately
        }
        // Literals, paths, self, pass don't contain free variables
        _ => {}
    }
}

/// Collects free variables from a block.
fn collect_free_variables_from_block(block: &ast::Block, free_vars: &mut HashSet<String>) {
    for stmt in &block.stmts {
        collect_free_variables_from_stmt(stmt, free_vars);
    }
    if let Some(ref expr) = block.expr {
        collect_free_variables(expr, free_vars);
    }
}

/// Collects free variables from a statement.
fn collect_free_variables_from_stmt(stmt: &ast::Stmt, free_vars: &mut HashSet<String>) {
    match stmt {
        ast::Stmt::Expr(expr) => {
            collect_free_variables(expr, free_vars);
        }
        ast::Stmt::Let { value, .. } => {
            collect_free_variables(value, free_vars);
        }
        ast::Stmt::Assign { target, value, .. } => {
            collect_free_variables(target, free_vars);
            collect_free_variables(value, free_vars);
        }
        ast::Stmt::Return(Some(expr)) => {
            collect_free_variables(expr, free_vars);
        }
        ast::Stmt::Return(None) => {}
        ast::Stmt::Break {
            value: Some(expr), ..
        } => {
            collect_free_variables(expr, free_vars);
        }
        ast::Stmt::Break { value: None, .. } => {}
        _ => {}
    }
}

/// Creates the closure function.
#[allow(clippy::too_many_arguments)]
fn create_closure_function(
    ctx: &mut LoweringContext,
    name: &str,
    params: &[ast::Param],
    return_type: Option<&ast::Type>,
    effects: &[ast::Type],
    body: &ast::Expr,
    captures: &[(String, Ty, bool)],
    env_type: &Ty,
) -> Function {
    // Create function parameters: env pointer + original params
    let mut func_params = vec![Param::new(
        "__env",
        Ty::Ptr(Box::new(env_type.clone())),
        ValueId::new(0),
    )];

    for (i, param) in params.iter().enumerate() {
        let param_ty = lower_type(&param.ty);
        let param_name = get_pattern_name(&param.pattern);
        func_params.push(Param::new(
            param_name,
            param_ty,
            ValueId::new((i + 1) as u32),
        ));
    }

    // Determine return type
    let ret_ty = return_type.map(lower_type).unwrap_or(Ty::Void);

    // Create the function
    let mut func = Function::new(name, func_params.clone(), ret_ty.clone());

    // Add effects
    for effect in effects {
        if let Some(ir_effect) = lower_effect(effect) {
            func.effects.push(ir_effect);
        }
    }

    // Add function to module temporarily so we can lower its body
    let func_index = ctx.module.functions.len();
    ctx.module.add_function(func.clone());

    // Save current state
    let saved_function = ctx.get_current_function_index();
    let saved_block = ctx.current_block();
    let saved_value_counter = ctx.get_value_counter();
    let saved_block_counter = ctx.get_block_counter();
    let saved_scope_stack = ctx.clone_scope_stack();
    let saved_loop_targets = ctx.clone_loop_targets();

    // Set up context for lowering the closure function body
    ctx.set_current_function_index(Some(func_index));
    ctx.set_value_counter(1); // Start at 1 because __env is value 0
    ctx.set_block_counter(0);
    ctx.enter_scope();
    ctx.clear_loop_targets();

    // Create entry block
    let entry_block_id = ctx.new_block_id();
    let entry_block = jet_ir::BasicBlock::with_name(entry_block_id, "entry");
    ctx.module.functions[func_index].add_block(entry_block);
    ctx.set_current_block(entry_block_id);

    // Load captured variables from environment and bind them
    for (i, (capture_name, capture_ty, is_mut)) in captures.iter().enumerate() {
        // Get pointer to the captured field in the environment
        let field_ptr = ctx.new_value();
        ctx.emit(Instruction::GetFieldPtr {
            result: field_ptr,
            ptr: ValueId::new(0), // __env parameter
            field_index: i,
            struct_ty: env_type.clone(),
        });

        // Load the captured value
        let loaded_val = ctx.new_value();
        ctx.emit(Instruction::Load {
            result: loaded_val,
            ptr: field_ptr,
            ty: capture_ty.clone(),
        });

        // Allocate space for the captured variable and store the loaded value
        let alloc = ctx.new_value();
        ctx.emit(Instruction::Alloc {
            result: alloc,
            ty: capture_ty.clone(),
        });
        ctx.emit(Instruction::Store {
            ptr: alloc,
            value: loaded_val,
        });

        // Bind the captured variable name to its allocation
        ctx.bind_variable(capture_name.clone(), alloc, capture_ty.clone(), *is_mut);
    }

    // Bind lambda parameters
    for (i, param) in params.iter().enumerate() {
        let param_ty = lower_type(&param.ty);
        let param_name = get_pattern_name(&param.pattern);
        let param_value = ValueId::new((i + 1) as u32);
        let is_mut = is_pattern_mutable(&param.pattern);

        // Allocate space for the parameter and store the value
        let alloc = ctx.new_value();
        ctx.emit(Instruction::Alloc {
            result: alloc,
            ty: param_ty.clone(),
        });
        ctx.emit(Instruction::Store {
            ptr: alloc,
            value: param_value,
        });

        ctx.bind_variable(param_name.to_string(), alloc, param_ty, is_mut);
    }

    // Lower the body expression
    let body_value = crate::expr::lower_expr(ctx, body);

    // Add return terminator if not already terminated
    let current_block = ctx.get_current_block();
    if current_block.map(|b| !b.is_terminated()).unwrap_or(false) {
        let ret_value = if ret_ty.is_void() {
            None
        } else {
            Some(body_value)
        };
        ctx.terminate(Terminator::Return(ret_value));
    }

    // Restore original state
    ctx.set_current_function_index(saved_function);
    ctx.set_current_block_opt(saved_block);
    ctx.set_value_counter(saved_value_counter);
    ctx.set_block_counter(saved_block_counter);
    ctx.set_scope_stack(saved_scope_stack);
    ctx.set_loop_targets(saved_loop_targets);

    // Return the completed function
    ctx.module.functions[func_index].clone()
}

/// Checks if a pattern is mutable.
fn is_pattern_mutable(pattern: &ast::Pattern) -> bool {
    match pattern {
        ast::Pattern::Ident { mutable, .. } => *mutable,
        ast::Pattern::Mut(_) => true,
        _ => false,
    }
}

/// Creates a closure value (function pointer + environment).
fn create_closure_value(
    ctx: &mut LoweringContext,
    func_index: usize,
    captures: &[(String, Ty, bool)],
    env_type: &Ty,
) -> ValueId {
    // Allocate environment on the heap
    let env_ptr = ctx.new_value();
    ctx.emit(Instruction::Alloc {
        result: env_ptr,
        ty: Ty::Ptr(Box::new(env_type.clone())),
    });

    // Store captured values in environment
    for (i, (name, ty, _)) in captures.iter().enumerate() {
        if let Some(captured_val) = ctx.lookup_variable(name) {
            let field_ptr = ctx.new_value();
            ctx.emit(Instruction::GetFieldPtr {
                result: field_ptr,
                ptr: env_ptr,
                field_index: i,
                struct_ty: env_type.clone(),
            });

            // Cast if necessary
            let val_to_store = if *ty != Ty::Ptr(Box::new(ty.clone())) {
                let casted = ctx.new_value();
                ctx.emit(Instruction::BitCast {
                    result: casted,
                    value: captured_val,
                    ty: ty.clone(),
                });
                casted
            } else {
                captured_val
            };

            ctx.emit(Instruction::Store {
                ptr: field_ptr,
                value: val_to_store,
            });
        }
    }

    // Create closure struct: { function_ptr, env_ptr }
    let func_ptr = ctx.new_value();
    ctx.emit(Instruction::Const {
        result: func_ptr,
        value: ConstantValue::Int(func_index as i64, Ty::Ptr(Box::new(Ty::Void))),
    });

    let closure_val = ctx.new_value();
    let closure_ty = Ty::Struct(vec![
        Ty::Ptr(Box::new(Ty::Void)),         // function pointer
        Ty::Ptr(Box::new(env_type.clone())), // environment pointer
    ]);

    ctx.emit(Instruction::StructAgg {
        result: closure_val,
        fields: vec![func_ptr, env_ptr],
        ty: closure_ty,
    });

    closure_val
}

/// Gets the name from a pattern.
fn get_pattern_name(pattern: &ast::Pattern) -> &str {
    match pattern {
        ast::Pattern::Ident { name, .. } => &name.name,
        ast::Pattern::Wildcard(_) => "_",
        ast::Pattern::Mut(inner) => get_pattern_name(inner),
        ast::Pattern::Ref { pattern, .. } => get_pattern_name(pattern),
        _ => "_",
    }
}

/// Lowers an effect type to an IR effect.
fn lower_effect(effect: &ast::Type) -> Option<jet_ir::Effect> {
    match effect {
        ast::Type::Path(path) if !path.segments.is_empty() => {
            let name = &path.segments[0].name;
            match name.as_str() {
                "async" => Some(jet_ir::Effect::Async),
                "io" => Some(jet_ir::Effect::IO),
                "alloc" => Some(jet_ir::Effect::Alloc),
                "diverges" => Some(jet_ir::Effect::Diverges),
                _ => Some(jet_ir::Effect::Raise(Ty::Named(name.clone()))),
            }
        }
        _ => None,
    }
}

/// Lowers a unit value.
#[allow(dead_code)]
fn lower_unit(ctx: &mut LoweringContext) -> ValueId {
    let result = ctx.new_value();
    let value = ConstantValue::Zero(Ty::Void);
    ctx.emit(Instruction::Const { result, value });
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_ir::Ty;
    use jet_lexer::Span;
    use jet_parser::ast::{Ident, Path, Pattern, Type};

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, Span::new(0, 0))
    }

    fn make_path(name: &str) -> Path {
        Path::new(vec![make_ident(name)], Span::new(0, 0))
    }

    #[test]
    fn test_collect_free_variables() {
        let mut free_vars = HashSet::new();

        // x + y
        let expr = ast::Expr::Binary {
            op: ast::BinaryOp::Add,
            left: Box::new(ast::Expr::Variable(make_ident("x"))),
            right: Box::new(ast::Expr::Variable(make_ident("y"))),
        };

        collect_free_variables(&expr, &mut free_vars);

        assert!(free_vars.contains("x"));
        assert!(free_vars.contains("y"));
    }

    #[test]
    fn test_closure_function_creation() {
        let mut ctx = LoweringContext::new("test");

        // Create a simple lambda: |x| -> x + y
        let _params = [ast::Param {
            pattern: Pattern::Ident {
                mutable: false,
                name: make_ident("x"),
            },
            ty: Type::Path(make_path("i64")),
        }];

        let body = ast::Expr::Binary {
            op: ast::BinaryOp::Add,
            left: Box::new(ast::Expr::Variable(make_ident("x"))),
            right: Box::new(ast::Expr::Variable(make_ident("y"))),
        };

        // Bind y in outer scope
        ctx.enter_scope();
        let y_val = ctx.new_value();
        ctx.bind_variable("y".to_string(), y_val, Ty::I64, false);

        let captures = identify_captures(&ctx, &body);
        assert_eq!(captures.len(), 1);
        assert_eq!(captures[0].0, "y");
    }
}
