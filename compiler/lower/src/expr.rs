//! Expression lowering from AST to IR.
//!
//! This module provides functions for lowering AST expressions to Jet IR instructions.

use crate::closure::convert_lambda;
use crate::context::{LoopTarget, LoweringContext};
use crate::pattern::compile_match;
use crate::stmt::{bind_pattern_ptr, lower_block};
use crate::ty::{
    bool_literal_type, char_literal_type, float_literal_type, int_literal_type, lower_type,
    string_literal_type, unit_type,
};
use jet_ir::{
    BinaryOp as IrBinaryOp, ConstantValue, Instruction, Terminator, Ty, UnaryOp as IrUnaryOp,
    ValueId,
};
use jet_parser::ast;
use jet_parser::ast::{BinaryOp, Literal, UnaryOp};

/// Infers the type of an expression.
pub fn infer_expr_type(ctx: &mut LoweringContext, expr: &ast::Expr) -> Ty {
    match expr {
        ast::Expr::Literal(lit) => match lit {
            ast::Literal::Integer(_) => int_literal_type(),
            ast::Literal::Float(_) => float_literal_type(),
            ast::Literal::Bool(_) => bool_literal_type(),
            ast::Literal::String(_) => string_literal_type(),
            ast::Literal::Char(_) => char_literal_type(),
            ast::Literal::Unit => unit_type(),
        },
        ast::Expr::StructLiteral { path, fields } => {
            let struct_name = if path.segments.is_empty() {
                "Unknown".to_string()
            } else {
                path.segments[0].name.clone()
            };
            if let Some(info) = ctx.lookup_struct(&struct_name) {
                Ty::Struct(info.field_types.clone())
            } else {
                Ty::Struct(vec![Ty::I64; fields.len()])
            }
        }
        ast::Expr::Variable(ident) => ctx
            .lookup_variable_type(&ident.name)
            .cloned()
            .unwrap_or(Ty::I64),
        ast::Expr::Path(path) if path.segments.len() == 1 => {
            let name = &path.segments[0].name;
            ctx.lookup_variable_type(name).cloned().unwrap_or(Ty::I64)
        }
        ast::Expr::Call { func, .. } => {
            // Helper to check function name
            let check_name = |name: &str| -> Option<Ty> {
                if name.starts_with("print") || name == "assert" || name == "panic" {
                    Some(Ty::Void)
                } else {
                    ctx.module
                        .get_function(name)
                        .map(|func| func.return_ty.clone())
                }
            };

            // Function call - look up return type
            if let ast::Expr::Variable(ident) = func.as_ref() {
                check_name(&ident.name).unwrap_or(Ty::I64)
            } else if let ast::Expr::Path(path) = func.as_ref() {
                if let Some(segment) = path.segments.last() {
                    check_name(&segment.name).unwrap_or(Ty::I64)
                } else {
                    Ty::I64
                }
            } else {
                Ty::I64
            }
        }
        ast::Expr::FieldAccess { object, field: _ } => {
            // Recurse to find struct type, then look up field
            // Use a simplified approach: just assume I64 if we can't easily resolve
            if let ast::Expr::Variable(ident) = object.as_ref() {
                if let Some(_struct_name) =
                    ctx.lookup_variable_type(&ident.name).and_then(|t| match t {
                        Ty::Struct(_) => Option::<String>::None,
                        _ => Option::<String>::None,
                    })
                {
                    Ty::I64
                } else {
                    Ty::I64
                }
            } else {
                Ty::I64
            }
        }
        ast::Expr::Block(block) => {
            if let Some(last) = block.stmts.last() {
                match last {
                    ast::Stmt::Expr(expr) => infer_expr_type(ctx, expr),
                    _ => Ty::Void,
                }
            } else {
                Ty::Void
            }
        }
        ast::Expr::If { then_branch, .. } => infer_expr_type(ctx, then_branch),
        ast::Expr::Assign { .. } => Ty::Void,
        ast::Expr::While { .. } | ast::Expr::For { .. } | ast::Expr::Loop { .. } => Ty::Void,
        ast::Expr::Break { .. } | ast::Expr::Continue { .. } | ast::Expr::Return(_) => Ty::Void,
        _ => Ty::I64,
    }
}

/// Loads a value from a pointer if the type suggests it should be loaded (primitives).
/// Structs are kept as pointers (l-values) for optimization, unless explicitly loaded.
fn maybe_load(ctx: &mut LoweringContext, ptr: ValueId, ty: Ty) -> ValueId {
    if matches!(ty, Ty::Struct(_) | Ty::Array(_, _)) {
        ptr
    } else {
        let loaded = ctx.new_value();
        ctx.emit(Instruction::Load {
            result: loaded,
            ptr,
            ty: ty.clone(),
        });
        loaded
    }
}

/// Lowers an expression to an l-value (pointer).
fn lower_lvalue(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    match expr {
        ast::Expr::Variable(ident) => {
            if let Some(ptr) = ctx.lookup_variable(&ident.name) {
                ptr
            } else {
                // External or missing variable - create placeholder
                let result = ctx.new_value();
                let value = ConstantValue::Zero(Ty::Ptr(Box::new(Ty::Void)));
                ctx.emit(Instruction::Const { result, value });
                result
            }
        }
        ast::Expr::Path(path) if path.segments.len() == 1 => {
            let name = &path.segments[0].name;
            if let Some(ptr) = ctx.lookup_variable(name) {
                ptr
            } else {
                let result = ctx.new_value();
                let value = ConstantValue::Zero(Ty::Ptr(Box::new(Ty::Void)));
                ctx.emit(Instruction::Const { result, value });
                result
            }
        }
        ast::Expr::FieldAccess { object, field } => {
            let obj_ptr = lower_lvalue(ctx, object);
            let field_ptr = ctx.new_value();

            // Try to resolve field index
            let field_index = if let ast::Expr::Variable(ident) = object.as_ref() {
                ctx.get_field_index(&ident.name, &field.name)
            } else {
                None
            }
            .unwrap_or(0);

            let struct_ty = infer_expr_type(ctx, object);
            ctx.emit(Instruction::GetFieldPtr {
                result: field_ptr,
                ptr: obj_ptr,
                field_index,
                struct_ty,
            });
            field_ptr
        }
        ast::Expr::Index { object, index } => {
            let obj_ptr = lower_lvalue(ctx, object);
            let idx = lower_expr(ctx, index);
            let elem_ptr = ctx.new_value();
            let array_ty = infer_expr_type(ctx, object);
            let elem_ty = if let Ty::Array(elem, _) = array_ty {
                *elem
            } else {
                Ty::I64 // Fallback
            };

            ctx.emit(Instruction::GetElementPtr {
                result: elem_ptr,
                ptr: obj_ptr,
                index: idx,
                elem_ty,
            });
            elem_ptr
        }
        _ => {
            // R-value spill: allocate temp slot and store value
            let val = lower_expr(ctx, expr);
            let ty = infer_expr_type(ctx, expr);
            let ptr = ctx.new_value();
            ctx.emit(Instruction::Alloc { result: ptr, ty });
            ctx.emit(Instruction::Store { ptr, value: val });
            ptr
        }
    }
}

pub fn lower_expr(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    match expr {
        ast::Expr::Literal(lit) => lower_literal(ctx, lit),
        ast::Expr::Variable(ident) => lower_variable(ctx, ident),
        ast::Expr::Path(path) => lower_path(ctx, path),
        ast::Expr::Binary { op, left, right } => lower_binary(ctx, *op, left, right),
        ast::Expr::Unary { op, expr } => lower_unary(ctx, *op, expr),
        ast::Expr::Call { func, args } => lower_call(ctx, func, args),
        ast::Expr::MethodCall {
            receiver,
            method,
            args,
        } => lower_method_call(ctx, receiver, method, args),
        ast::Expr::FieldAccess { object, field } => lower_field_access(ctx, object, field),
        ast::Expr::Index { object, index } => lower_index(ctx, object, index),
        ast::Expr::Block(block) => lower_block_expr(ctx, block),
        ast::Expr::If {
            cond,
            then_branch,
            else_branch,
        } => lower_if(ctx, cond, then_branch, else_branch.as_deref()),
        ast::Expr::Match { expr, arms } => {
            // Use the new pattern matching compilation
            let scrutinee = lower_expr(ctx, expr);
            let scrutinee_ty = infer_expr_type(ctx, expr);
            compile_match(ctx, scrutinee, scrutinee_ty, arms)
        }
        ast::Expr::While { label, cond, body } => lower_while(ctx, label.as_ref(), cond, body),
        ast::Expr::For {
            label,
            pattern,
            iterable,
            body,
        } => lower_for(ctx, label.as_ref(), pattern, iterable, body),
        ast::Expr::Loop { label, body } => lower_loop(ctx, label.as_ref(), body),
        ast::Expr::Lambda {
            params,
            return_type,
            effects,
            body,
        } => {
            // Use closure conversion for lambdas
            let effects_vec = effects.clone();
            convert_lambda(ctx, params, return_type.as_ref(), &effects_vec, body)
        }
        ast::Expr::Tuple(elements) => lower_tuple(ctx, elements),
        ast::Expr::Array(elements) => lower_array(ctx, elements),
        ast::Expr::Assign { target, op, value } => lower_assign(ctx, target, *op, value),
        ast::Expr::Break { label, value } => lower_break(ctx, label.as_ref(), value.as_deref()),
        ast::Expr::Continue { label } => lower_continue(ctx, label.as_ref()),
        ast::Expr::Return(value) => lower_return(ctx, value.as_deref()),
        ast::Expr::Await(expr) => lower_await(ctx, expr),
        ast::Expr::Try(expr) => lower_try(ctx, expr),
        ast::Expr::StructLiteral { path, fields } => lower_struct_literal(ctx, path, fields),
        ast::Expr::SelfExpr(_) => lower_self(ctx),
        ast::Expr::Spawn(expr) => lower_spawn(ctx, expr),
        ast::Expr::Async(block) => lower_async_block(ctx, block),
        ast::Expr::Concurrent(block) => lower_concurrent_block(ctx, block),
        ast::Expr::Pass => lower_pass(ctx),
        ast::Expr::Raise(raise_expr) => {
            // Build a call expression for the effect operation
            let op_ident =
                ast::Ident::new(raise_expr.operation.name.clone(), raise_expr.operation.span);
            let effect_call = ast::Expr::Call {
                func: Box::new(ast::Expr::Variable(op_ident)),
                args: raise_expr.args.clone(),
            };
            crate::effect::lower_raise(ctx, &effect_call)
        }
        ast::Expr::Handle(handle_expr) => {
            crate::effect::lower_handle(ctx, &handle_expr.body, &handle_expr.handlers)
        }
        ast::Expr::Resume(resume_expr) => {
            if let Some(ref value) = resume_expr.value {
                crate::effect::lower_resume(ctx, value)
            } else {
                lower_unit(ctx)
            }
        }
    }
}

/// Lowers a literal expression.
fn lower_literal(ctx: &mut LoweringContext, lit: &Literal) -> ValueId {
    let (value, _ty) = match lit {
        Literal::Integer(n) => (
            ConstantValue::Int(*n, int_literal_type()),
            int_literal_type(),
        ),
        Literal::Float(f) => (
            ConstantValue::Float(*f, float_literal_type()),
            float_literal_type(),
        ),
        Literal::Bool(b) => (ConstantValue::Bool(*b), bool_literal_type()),
        Literal::String(s) => (ConstantValue::String(s.clone()), string_literal_type()),
        Literal::Char(c) => (
            ConstantValue::Int(*c as i64, char_literal_type()),
            char_literal_type(),
        ),
        Literal::Unit => return lower_unit(ctx),
    };

    let result = ctx.new_value();
    ctx.emit(Instruction::Const { result, value });
    result
}

/// Lowers a unit value.
fn lower_unit(ctx: &mut LoweringContext) -> ValueId {
    let result = ctx.new_value();
    let value = ConstantValue::Zero(unit_type());
    ctx.emit(Instruction::Const { result, value });
    result
}

/// Lowers a variable reference.
/// Returns the value of the variable (loaded if necessary).
fn lower_variable(ctx: &mut LoweringContext, ident: &ast::Ident) -> ValueId {
    let ptr = if let Some(p) = ctx.lookup_variable(&ident.name) {
        p
    } else {
        // Fallback for missing/extern
        let result = ctx.new_value();
        let value = ConstantValue::Zero(Ty::Ptr(Box::new(Ty::Void)));
        ctx.emit(Instruction::Const { result, value });
        result
    };

    let ty = ctx
        .lookup_variable_type(&ident.name)
        .cloned()
        .unwrap_or(Ty::I64);
    maybe_load(ctx, ptr, ty)
}

/// Lowers a path expression.
fn lower_path(ctx: &mut LoweringContext, path: &ast::Path) -> ValueId {
    if path.segments.is_empty() {
        return lower_unit(ctx);
    }

    let name = &path.segments[0].name;
    if let Some(ptr) = ctx.lookup_variable(name) {
        let ty = ctx.lookup_variable_type(name).cloned().unwrap_or(Ty::I64);
        let loaded = ctx.new_value();
        ctx.emit(Instruction::Load {
            result: loaded,
            ptr,
            ty,
        });
        loaded
    } else {
        // External reference
        let result = ctx.new_value();
        let value = ConstantValue::Zero(Ty::I64);
        ctx.emit(Instruction::Const { result, value });
        result
    }
}

/// Lowers a binary operation.
fn lower_binary(
    ctx: &mut LoweringContext,
    op: BinaryOp,
    left: &ast::Expr,
    right: &ast::Expr,
) -> ValueId {
    let lhs = lower_expr(ctx, left);
    let rhs = lower_expr(ctx, right);
    let result = ctx.new_value();

    let ir_op = match op {
        BinaryOp::Add => IrBinaryOp::Add,
        BinaryOp::Sub => IrBinaryOp::Sub,
        BinaryOp::Mul => IrBinaryOp::Mul,
        BinaryOp::Div => IrBinaryOp::Div,
        BinaryOp::Mod => IrBinaryOp::Rem,
        BinaryOp::BitAnd => IrBinaryOp::And,
        BinaryOp::BitOr => IrBinaryOp::Or,
        BinaryOp::BitXor => IrBinaryOp::Xor,
        BinaryOp::Shl => IrBinaryOp::Shl,
        BinaryOp::Shr => IrBinaryOp::Shr,
        BinaryOp::Eq => IrBinaryOp::Eq,
        BinaryOp::Ne => IrBinaryOp::Ne,
        BinaryOp::Lt => IrBinaryOp::Lt,
        BinaryOp::Gt => IrBinaryOp::Gt,
        BinaryOp::Le => IrBinaryOp::Le,
        BinaryOp::Ge => IrBinaryOp::Ge,
        BinaryOp::And | BinaryOp::Or => {
            // Short-circuiting logical operators need special handling
            return lower_logical_op(ctx, op, left, right);
        }
        _ => {
            // Unsupported operators return unit
            return lower_unit(ctx);
        }
    };

    ctx.emit(Instruction::Binary {
        result,
        op: ir_op,
        lhs,
        rhs,
    });

    result
}

/// Lowers short-circuiting logical operators (and/or).
fn lower_logical_op(
    ctx: &mut LoweringContext,
    op: BinaryOp,
    left: &ast::Expr,
    right: &ast::Expr,
) -> ValueId {
    let lhs = lower_expr(ctx, left);

    let eval_right_block = ctx.create_block("eval_right");
    let merge_block = ctx.create_block("merge");

    let (short_circuit_value, cond_block) = match op {
        BinaryOp::And => {
            // If lhs is false, result is false (short-circuit)
            // If lhs is true, evaluate rhs
            ctx.terminate(Terminator::CondBranch {
                cond: lhs,
                then_block: eval_right_block,
                else_block: merge_block,
            });
            (false, eval_right_block)
        }
        BinaryOp::Or => {
            // If lhs is true, result is true (short-circuit)
            // If lhs is false, evaluate rhs
            ctx.terminate(Terminator::CondBranch {
                cond: lhs,
                then_block: merge_block,
                else_block: eval_right_block,
            });
            (true, eval_right_block)
        }
        _ => unreachable!(),
    };

    // Evaluate right operand
    ctx.set_current_block(eval_right_block);
    let rhs = lower_expr(ctx, right);
    ctx.terminate(Terminator::Branch(merge_block));

    // Merge block with phi
    ctx.set_current_block(merge_block);
    let result = ctx.new_value();

    let short_circuit_const = ctx.new_value();
    ctx.emit(Instruction::Const {
        result: short_circuit_const,
        value: ConstantValue::Bool(short_circuit_value),
    });

    ctx.emit(Instruction::Phi {
        result,
        incoming: vec![(eval_right_block, rhs), (cond_block, short_circuit_const)],
        ty: Ty::Bool,
    });

    result
}

/// Lowers a unary operation.
fn lower_unary(ctx: &mut LoweringContext, op: UnaryOp, expr: &ast::Expr) -> ValueId {
    let operand = lower_expr(ctx, expr);
    let result = ctx.new_value();

    match op {
        UnaryOp::Neg => {
            ctx.emit(Instruction::Unary {
                result,
                op: IrUnaryOp::Neg,
                operand,
            });
        }
        UnaryOp::Not | UnaryOp::BitNot => {
            ctx.emit(Instruction::Unary {
                result,
                op: IrUnaryOp::Not,
                operand,
            });
        }
        UnaryOp::Deref => {
            // Infer type of pointee
            let ptr_ty = infer_expr_type(ctx, expr);
            let val_ty = if let Ty::Ptr(inner) = ptr_ty {
                *inner
            } else {
                Ty::I64 // Fallback
            };

            ctx.emit(Instruction::Load {
                result,
                ptr: operand,
                ty: val_ty,
            });
        }
        UnaryOp::Ref | UnaryOp::RefMut => {
            // Reference creation - for now just return the operand
            // In a real implementation, this would create an alloca and store
            return operand;
        }
    }

    result
}

/// Lowers a function call.
fn lower_call(ctx: &mut LoweringContext, func: &ast::Expr, args: &[ast::Expr]) -> ValueId {
    let arg_values: Vec<ValueId> = args.iter().map(|arg| lower_expr(ctx, arg)).collect();

    let result = ctx.new_value();

    // Determine return type
    let check_name = |name: &str| -> Option<Ty> {
        if name.starts_with("print") || name == "assert" || name == "panic" {
            Some(Ty::Void)
        } else {
            ctx.module
                .get_function(name)
                .map(|func| func.return_ty.clone())
        }
    };

    // Get function name if it's a direct call
    let func_name = match func {
        ast::Expr::Variable(ident) => ident.name.clone(),
        ast::Expr::Path(path) if !path.segments.is_empty() => path
            .segments
            .iter()
            .map(|s| &s.name)
            .cloned()
            .collect::<Vec<_>>()
            .join("::"),
        _ => "indirect".to_string(),
    };

    let call_ty = if func_name == "indirect" {
        // Try to infer from function pointer type
        let ptr_ty = infer_expr_type(ctx, func);
        match ptr_ty {
            Ty::Function(_, ret) => *ret,
            _ => Ty::I64,
        }
    } else {
        check_name(&func_name).unwrap_or(Ty::I64)
    };

    if func_name == "indirect" {
        // Indirect call through function pointer
        let func_ptr = lower_expr(ctx, func);
        ctx.emit(Instruction::CallIndirect {
            result,
            ptr: func_ptr,
            args: arg_values,
            ty: call_ty,
        });
    } else {
        // Direct function call
        ctx.emit(Instruction::Call {
            result,
            func: func_name,
            args: arg_values,
            ty: call_ty,
        });
    }

    result
}

/// Lowers a method call.
fn lower_method_call(
    ctx: &mut LoweringContext,
    receiver: &ast::Expr,
    method: &ast::Ident,
    args: &[ast::Expr],
) -> ValueId {
    // For now, desugar to a function call with receiver as first argument
    let mut arg_values = vec![lower_expr(ctx, receiver)];
    arg_values.extend(args.iter().map(|arg| lower_expr(ctx, arg)));

    let result = ctx.new_value();
    let func_name = method.name.to_string();

    // Simplistic return type for methods
    let ret_ty = Ty::I64;

    ctx.emit(Instruction::Call {
        result,
        func: func_name,
        args: arg_values,
        ty: ret_ty,
    });

    result
}

/// Lowers a field access.
/// Lowers a field access.
/// Lowers a field access.
fn lower_field_access(
    ctx: &mut LoweringContext,
    object: &ast::Expr,
    field: &ast::Ident,
) -> ValueId {
    // Get pointer to the field via l-value lowering handling
    let field_ptr = ctx.new_value();

    // Helper to get l-value of object (handling recursion)
    let obj_ptr = lower_lvalue(ctx, object);

    let field_index = if let ast::Expr::Variable(ident) = object {
        ctx.get_field_index(&ident.name, &field.name)
    } else {
        None
    }
    .unwrap_or(0);

    // Infer struct type
    let struct_ty = infer_expr_type(ctx, object);

    ctx.emit(Instruction::GetFieldPtr {
        result: field_ptr,
        ptr: obj_ptr,
        field_index,
        struct_ty: struct_ty.clone(),
    });

    // Infer field type
    let ty = if let Ty::Struct(fields) = &struct_ty {
        fields.get(field_index).cloned().unwrap_or(Ty::I64)
    } else {
        Ty::I64
    };

    maybe_load(ctx, field_ptr, ty)
}

/// Lowers an index operation.
/// Lowers an index operation.
/// Lowers an index operation.
fn lower_index(ctx: &mut LoweringContext, object: &ast::Expr, index: &ast::Expr) -> ValueId {
    let obj_ptr = lower_lvalue(ctx, object);
    let idx = lower_expr(ctx, index);

    // Infer array type
    let array_ty = infer_expr_type(ctx, object);
    let elem_ty = if let Ty::Array(elem, _) = array_ty {
        *elem
    } else {
        Ty::I64 // Fallback
    };

    let elem_ptr = ctx.new_value();
    ctx.emit(Instruction::GetElementPtr {
        result: elem_ptr,
        ptr: obj_ptr,
        index: idx,
        elem_ty: elem_ty.clone(),
    });

    maybe_load(ctx, elem_ptr, elem_ty)
}

/// Lowers a block expression.
fn lower_block_expr(ctx: &mut LoweringContext, block: &ast::Block) -> ValueId {
    lower_block(ctx, block)
}

/// Lowers an if expression.
#[allow(unused_variables)]
fn lower_if(
    ctx: &mut LoweringContext,
    cond: &ast::Expr,
    then_branch: &ast::Expr,
    else_branch: Option<&ast::Expr>,
) -> ValueId {
    let cond_val = lower_expr(ctx, cond);

    let then_block = ctx.create_block("then");
    let else_block = ctx.create_block("else");
    // Reserve a block ID for merge block but don't create it yet
    let merge_block_id = ctx.create_block_id();

    ctx.terminate(Terminator::CondBranch {
        cond: cond_val,
        then_block,
        else_block,
    });

    // Then branch
    ctx.set_current_block(then_block);
    ctx.enter_scope();
    let then_val = lower_expr(ctx, then_branch);
    ctx.exit_scope();
    let then_end_block = ctx.current_block();
    let then_terminated = ctx
        .get_current_block()
        .map(|b| b.is_terminated())
        .unwrap_or(true);

    // Else branch
    ctx.set_current_block(else_block);
    let else_val = if let Some(else_expr) = else_branch {
        ctx.enter_scope();
        let val = lower_expr(ctx, else_expr);
        ctx.exit_scope();
        val
    } else {
        lower_unit(ctx)
    };
    let else_end_block = ctx.current_block();
    let else_terminated = ctx
        .get_current_block()
        .map(|b| b.is_terminated())
        .unwrap_or(true);

    // Now create the merge block after all nested blocks have been created
    let merge_block = jet_ir::BasicBlock::with_name(merge_block_id, "merge");
    ctx.add_block(merge_block);

    // Add branch terminators to the end of each branch that didn't terminate
    // This must be done BEFORE we set_current_block to merge_block_id
    // because we need to add the terminator to the end of the branch blocks
    if !then_terminated {
        if let Some(end_block) = then_end_block {
            ctx.set_current_block(end_block);
            ctx.terminate(Terminator::Branch(merge_block_id));
        }
    }
    if !else_terminated {
        if let Some(end_block) = else_end_block {
            ctx.set_current_block(end_block);
            ctx.terminate(Terminator::Branch(merge_block_id));
        }
    }

    // Handle cases where one or both branches have terminated
    match (then_terminated, else_terminated) {
        (true, true) => {
            // Both branches terminated - merge block is unreachable
            ctx.set_current_block(merge_block_id);

            // Find current function return type
            let ret_ty = ctx
                .current_function()
                .map(|f| f.return_ty.clone())
                .unwrap_or(Ty::Void);

            if ret_ty == Ty::Void {
                ctx.terminate(Terminator::Return(None));
            } else {
                let dummy = ctx.new_value();
                ctx.emit(Instruction::Const {
                    result: dummy,
                    value: ConstantValue::Zero(ret_ty),
                });
                ctx.terminate(Terminator::Return(Some(dummy)));
            }

            // Return dummy value for expression
            lower_unit(ctx)
        }
        (true, false) => {
            // Then branch terminated, else branch continues
            // No phi needed - value comes only from else branch
            ctx.set_current_block(merge_block_id);
            else_val
        }
        (false, true) => {
            // Else branch terminated, then branch continues
            // No phi needed - value comes only from then branch
            ctx.set_current_block(merge_block_id);
            then_val
        }
        (false, false) => {
            // Both branches continue - need phi
            ctx.set_current_block(merge_block_id);
            let result = ctx.new_value();

            let phi_ty = infer_expr_type(ctx, then_branch);
            // The phi predecessors are the blocks that branch to merge_block_id
            // which are then_end_block and else_end_block (after we added terminators above)
            let then_pred = then_end_block.unwrap_or(then_block);
            let else_pred = else_end_block.unwrap_or(else_block);
            ctx.emit(Instruction::Phi {
                result,
                incoming: vec![(then_pred, then_val), (else_pred, else_val)],
                ty: phi_ty,
            });

            result
        }
    }
}

/// Lowers a while loop.
fn lower_while(
    ctx: &mut LoweringContext,
    label: Option<&ast::Ident>,
    cond: &ast::Expr,
    body: &ast::Expr,
) -> ValueId {
    let cond_block = ctx.create_block("while_cond");
    let body_block = ctx.create_block("while_body");
    let exit_block = ctx.create_block("while_exit");

    let label_str = label.map(|l| l.name.clone());

    // Push loop target for break/continue
    ctx.push_loop_target(LoopTarget {
        label: label_str.clone(),
        break_block: exit_block,
        continue_block: cond_block,
        break_value: None,
    });

    // Branch to condition
    ctx.terminate(Terminator::Branch(cond_block));

    // Condition block
    ctx.set_current_block(cond_block);
    let cond_val = lower_expr(ctx, cond);
    ctx.terminate(Terminator::CondBranch {
        cond: cond_val,
        then_block: body_block,
        else_block: exit_block,
    });

    // Body block
    ctx.set_current_block(body_block);
    ctx.enter_scope();
    lower_expr(ctx, body);
    ctx.exit_scope();
    ctx.terminate(Terminator::Branch(cond_block));

    // Pop loop target
    ctx.pop_loop_target();

    // Exit block
    ctx.set_current_block(exit_block);
    lower_unit(ctx)
}

/// Lowers a for loop.
fn lower_for(
    ctx: &mut LoweringContext,
    label: Option<&ast::Ident>,
    pattern: &ast::Pattern,
    iterable: &ast::Expr,
    body: &ast::Expr,
) -> ValueId {
    let cond_block = ctx.create_block("for_cond");
    let body_block = ctx.create_block("for_body");
    let exit_block = ctx.create_block("for_exit");

    let label_str = label.map(|l| l.name.clone());

    // Initialize iterator (simplified)
    let _iter_val = lower_expr(ctx, iterable);

    // Push loop target
    ctx.push_loop_target(LoopTarget {
        label: label_str,
        break_block: exit_block,
        continue_block: cond_block,
        break_value: None,
    });

    // Branch to condition
    ctx.terminate(Terminator::Branch(cond_block));

    // Condition block (check if iterator has more)
    ctx.set_current_block(cond_block);
    let cond_val = ctx.new_value();
    ctx.emit(Instruction::Const {
        result: cond_val,
        value: ConstantValue::Bool(true), // Simplified
    });
    ctx.terminate(Terminator::CondBranch {
        cond: cond_val,
        then_block: body_block,
        else_block: exit_block,
    });

    // Body block
    ctx.set_current_block(body_block);
    ctx.enter_scope();

    // Bind pattern variable — allocate and initialize to 0
    // Allocate space for loop variable
    let loop_var_ptr = ctx.new_value();

    // Determine loop variable type (simplified - assume I64 for now)
    // In a real implementation, we'd infer from iterable
    let loop_var_ty = Ty::I64;

    ctx.emit(Instruction::Alloc {
        result: loop_var_ptr,
        ty: loop_var_ty.clone(),
    });

    // Initialize with 0 (or some default)
    let init_val = ctx.new_value();
    ctx.emit(Instruction::Const {
        result: init_val,
        value: ConstantValue::Int(0, Ty::I64),
    });

    // Store
    ctx.emit(Instruction::Store {
        ptr: loop_var_ptr,
        value: init_val,
    });

    // Bind pattern
    bind_pattern_ptr(ctx, pattern, loop_var_ptr, loop_var_ty);

    lower_expr(ctx, body);
    ctx.exit_scope();
    if !ctx
        .get_current_block()
        .map(|b| b.is_terminated())
        .unwrap_or(true)
    {
        ctx.terminate(Terminator::Branch(cond_block));
    }

    // Pop loop target
    ctx.pop_loop_target();

    // Exit block
    ctx.set_current_block(exit_block);
    lower_unit(ctx)
}

/// Lowers an infinite loop.
fn lower_loop(ctx: &mut LoweringContext, label: Option<&ast::Ident>, body: &ast::Expr) -> ValueId {
    let body_block = ctx.create_block("loop_body");
    let exit_block = ctx.create_block("loop_exit");

    let label_str = label.map(|l| l.name.clone());

    // Push loop target
    ctx.push_loop_target(LoopTarget {
        label: label_str,
        break_block: exit_block,
        continue_block: body_block,
        break_value: None,
    });

    // Branch to body
    ctx.terminate(Terminator::Branch(body_block));

    // Body block
    ctx.set_current_block(body_block);
    ctx.enter_scope();
    lower_expr(ctx, body);
    ctx.exit_scope();
    ctx.terminate(Terminator::Branch(body_block));

    // Pop loop target
    ctx.pop_loop_target();

    // Exit block (unreachable unless break)
    ctx.set_current_block(exit_block);
    lower_unit(ctx)
}

/// Lowers a lambda expression.
#[allow(dead_code)]
fn lower_lambda(
    ctx: &mut LoweringContext,
    _params: &[ast::Param],
    return_type: Option<&ast::Type>,
    _body: &ast::Expr,
) -> ValueId {
    // For now, create a closure structure
    // In a real implementation, this would create a new function

    let result = ctx.new_value();

    // Collect captured variables (simplified)
    let captures: Vec<ValueId> = Vec::new();

    // Create struct aggregate for closure
    let ret_ty = return_type.map(lower_type).unwrap_or(Ty::Void);
    let closure_ty = Ty::Struct(vec![Ty::Ptr(Box::new(Ty::Void)), ret_ty]);

    ctx.emit(Instruction::StructAgg {
        result,
        fields: captures,
        ty: closure_ty,
    });

    result
}

/// Lowers a tuple expression.
fn lower_tuple(ctx: &mut LoweringContext, elements: &[ast::Expr]) -> ValueId {
    let values: Vec<ValueId> = elements.iter().map(|e| lower_expr(ctx, e)).collect();
    let result = ctx.new_value();

    let elem_types: Vec<Ty> = values.iter().map(|_| Ty::I64).collect(); // Simplified
    let tuple_ty = Ty::Struct(elem_types);

    ctx.emit(Instruction::StructAgg {
        result,
        fields: values,
        ty: tuple_ty,
    });

    result
}

/// Lowers an array expression.
fn lower_array(ctx: &mut LoweringContext, elements: &[ast::Expr]) -> ValueId {
    let values: Vec<ValueId> = elements.iter().map(|e| lower_expr(ctx, e)).collect();
    let result = ctx.new_value();

    let array_ty = Ty::Array(Box::new(Ty::I64), values.len()); // Simplified

    ctx.emit(Instruction::ArrayAgg {
        result,
        elements: values,
        ty: array_ty,
    });

    result
}

/// Lowers an assignment expression.
fn lower_assign(
    ctx: &mut LoweringContext,
    target: &ast::Expr,
    op: ast::AssignOp,
    value: &ast::Expr,
) -> ValueId {
    let val = lower_expr(ctx, value);

    let ptr = lower_lvalue(ctx, target);

    // Handle compound assignment operators
    let final_val = if op != ast::AssignOp::Assign {
        let loaded = ctx.new_value();
        // Infer target type for load
        let target_ty = infer_expr_type(ctx, target);
        ctx.emit(Instruction::Load {
            result: loaded,
            ptr,
            ty: target_ty,
        });

        let computed = ctx.new_value();
        let bin_op = match op {
            ast::AssignOp::AddAssign => IrBinaryOp::Add,
            ast::AssignOp::SubAssign => IrBinaryOp::Sub,
            ast::AssignOp::MulAssign => IrBinaryOp::Mul,
            ast::AssignOp::DivAssign => IrBinaryOp::Div,
            ast::AssignOp::ModAssign => IrBinaryOp::Rem,
            ast::AssignOp::BitAndAssign => IrBinaryOp::And,
            ast::AssignOp::BitOrAssign => IrBinaryOp::Or,
            ast::AssignOp::BitXorAssign => IrBinaryOp::Xor,
            ast::AssignOp::ShlAssign => IrBinaryOp::Shl,
            ast::AssignOp::ShrAssign => IrBinaryOp::Shr,
            ast::AssignOp::Assign => unreachable!(),
        };

        ctx.emit(Instruction::Binary {
            result: computed,
            op: bin_op,
            lhs: loaded,
            rhs: val,
        });
        computed
    } else {
        val
    };

    ctx.emit(Instruction::Store {
        ptr,
        value: final_val,
    });

    // Assignment returns unit
    lower_unit(ctx)
}

/// Lowers a break expression.
fn lower_break(
    ctx: &mut LoweringContext,
    label: Option<&ast::Ident>,
    value: Option<&ast::Expr>,
) -> ValueId {
    let label_str = label.map(|l| l.name.as_str());
    let target = ctx
        .find_loop_target(label_str)
        .expect("No matching loop for break")
        .clone();

    let break_val = value.map(|v| lower_expr(ctx, v));

    ctx.terminate(Terminator::Branch(target.break_block));

    // Return the break value or unit
    break_val.unwrap_or_else(|| lower_unit(ctx))
}

/// Lowers a continue expression.
fn lower_continue(ctx: &mut LoweringContext, label: Option<&ast::Ident>) -> ValueId {
    let label_str = label.map(|l| l.name.as_str());
    let target = ctx
        .find_loop_target(label_str)
        .expect("No matching loop for continue")
        .clone();

    ctx.terminate(Terminator::Branch(target.continue_block));

    lower_unit(ctx)
}

/// Lowers a return expression.
fn lower_return(ctx: &mut LoweringContext, value: Option<&ast::Expr>) -> ValueId {
    let ret_val = value.map(|v| lower_expr(ctx, v));

    ctx.terminate(Terminator::Return(ret_val));

    // Return value is unreachable, but we need to return something
    lower_unit(ctx)
}

/// Lowers an await expression.
fn lower_await(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    let future = lower_expr(ctx, expr);
    let result = ctx.new_value();

    ctx.emit(Instruction::Await { result, future });

    result
}

/// Lowers a try expression (? operator).
fn lower_try(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    // For now, just lower the inner expression
    // In a real implementation, this would generate error handling code
    lower_expr(ctx, expr)
}

/// Lowers a struct literal.
fn lower_struct_literal(
    ctx: &mut LoweringContext,
    path: &ast::Path,
    fields: &[ast::FieldInit],
) -> ValueId {
    let field_values: Vec<ValueId> = fields
        .iter()
        .map(|f| lower_expr(ctx, f.value.as_ref().expect("Field value required")))
        .collect();

    // Get struct name and look up type info
    let struct_name = if path.segments.is_empty() {
        "Unknown".to_string()
    } else {
        path.segments[0].name.clone()
    };

    // Determine struct type
    let struct_ty = if let Some(info) = ctx.lookup_struct(&struct_name) {
        Ty::Struct(info.field_types.clone())
    } else {
        // Infer from field values
        Ty::Struct(vec![Ty::I64; field_values.len()])
    };

    let struct_val = ctx.new_value();
    ctx.emit(Instruction::StructAgg {
        result: struct_val,
        fields: field_values,
        ty: struct_ty,
    });

    // Return value directly (StructAgg result)
    struct_val
}

/// Lowers a self expression.
fn lower_self(ctx: &mut LoweringContext) -> ValueId {
    // Look up 'self' in scope
    if let Some(ptr) = ctx.lookup_variable("self") {
        // Load the value like we do for regular variables
        let ty = ctx.lookup_variable_type("self").cloned().unwrap_or(Ty::I64);
        maybe_load(ctx, ptr, ty)
    } else {
        // Create placeholder
        let result = ctx.new_value();
        let value = ConstantValue::Zero(Ty::Ptr(Box::new(Ty::Void)));
        ctx.emit(Instruction::Const { result, value });
        result
    }
}

/// Lowers an async block.
fn lower_async_block(ctx: &mut LoweringContext, block: &ast::Block) -> ValueId {
    // For now, just lower the block
    // In a real implementation, this would create an async wrapper
    lower_block(ctx, block)
}

/// Lowers a concurrent block.
fn lower_concurrent_block(ctx: &mut LoweringContext, block: &ast::Block) -> ValueId {
    // For now, just lower the block sequentially
    // In a real implementation, this would spawn concurrent tasks
    lower_block(ctx, block)
}

/// Lowers a pass expression (no-op).
fn lower_pass(ctx: &mut LoweringContext) -> ValueId {
    // Pass is a no-op, returns unit
    lower_unit(ctx)
}

/// Lowers a spawn expression.
fn lower_spawn(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    let spawned = lower_expr(ctx, expr);
    let result = ctx.new_value();

    // Spawn instruction
    ctx.emit(Instruction::Call {
        result,
        func: "spawn".to_string(), // Placeholder for runtime spawn
        args: vec![spawned],
        ty: Ty::I64, // Placeholder handle type
    });

    result
}

/// Lowers a typed expression to IR.
///
/// This is a stub implementation that will be expanded to properly handle
/// typed expressions. For now, it creates a placeholder value.
pub fn lower_typed_expr(
    _ctx: &mut LoweringContext,
    expr: &jet_typeck::TypedExpr,
) -> ValueId {
    // TODO: Implement full typed expression lowering
    // For now, just create a placeholder based on the expression type
    let result = _ctx.new_value();
    let ty = crate::ty::lower_typeck_type(expr.ty, _ctx.type_context());

    // Emit a placeholder constant
    _ctx.emit(Instruction::Const {
        result,
        value: ConstantValue::Zero(ty),
    });

    result
}
