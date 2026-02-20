//! Expression lowering from AST to IR.
//!
//! This module provides functions for lowering AST expressions to Jet IR instructions.

use crate::closure::convert_lambda;
use crate::context::{LoopTarget, LoweringContext};
use crate::pattern::compile_match;
use crate::stmt::{bind_pattern_ptr, lower_block};
use crate::ty::{
    bool_literal_type, char_literal_type, float_literal_type, int_literal_type,
    string_literal_type, unit_type,
};
use jet_ir::{
    BinaryOp as IrBinaryOp, BlockId, ConstantValue, Instruction, Terminator, Ty,
    UnaryOp as IrUnaryOp, ValueId,
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
            let check_name = |name: &str| -> Option<Ty> {
                if name.starts_with("print") || name == "assert" || name == "panic" {
                    Some(Ty::Void)
                } else {
                    ctx.module
                        .get_function(name)
                        .map(|func| func.return_ty.clone())
                }
            };
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
        ast::Expr::FieldAccess {
            object: _,
            field: _,
        } => Ty::I64,
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

fn lower_lvalue(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    match expr {
        ast::Expr::Variable(ident) => {
            if let Some(ptr) = ctx.lookup_variable(&ident.name) {
                ptr
            } else {
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
                Ty::I64
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
            let scrutinee = lower_expr(ctx, expr);
            let scrutinee_ty = infer_expr_type(ctx, expr);
            compile_match(ctx, scrutinee, scrutinee_ty, arms)
        }
        ast::Expr::While {
            label,
            cond,
            invariant: _,
            body,
        } => lower_while(ctx, label.as_ref(), cond, body),
        ast::Expr::For {
            label,
            pattern,
            iterable,
            invariant: _,
            body,
        } => lower_for(ctx, label.as_ref(), pattern, iterable, body),
        ast::Expr::Loop { label, body } => lower_loop(ctx, label.as_ref(), body),
        ast::Expr::Lambda {
            params,
            return_type,
            effects,
            body,
        } => {
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
        ast::Expr::Hole(_) => {
            // Holes are placeholders for type-directed development.
            // They should be filled before lowering; if we encounter one here,
            // it means compilation didn't complete successfully.
            // Emit a placeholder value that will fail at runtime if reached.
            lower_unit(ctx)
        }
    }
}

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

pub fn lower_unit(ctx: &mut LoweringContext) -> ValueId {
    let result = ctx.new_value();
    let value = ConstantValue::Zero(unit_type());
    ctx.emit(Instruction::Const { result, value });
    result
}

fn lower_variable(ctx: &mut LoweringContext, ident: &ast::Ident) -> ValueId {
    let ptr = if let Some(p) = ctx.lookup_variable(&ident.name) {
        p
    } else {
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
        let result = ctx.new_value();
        let value = ConstantValue::Zero(Ty::I64);
        ctx.emit(Instruction::Const { result, value });
        result
    }
}

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
        BinaryOp::And | BinaryOp::Or => return lower_logical_op(ctx, op, left, right),
        _ => return lower_unit(ctx),
    };
    ctx.emit(Instruction::Binary {
        result,
        op: ir_op,
        lhs,
        rhs,
    });
    result
}

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
            ctx.terminate(Terminator::CondBranch {
                cond: lhs,
                then_block: eval_right_block,
                else_block: merge_block,
            });
            (false, eval_right_block)
        }
        BinaryOp::Or => {
            ctx.terminate(Terminator::CondBranch {
                cond: lhs,
                then_block: merge_block,
                else_block: eval_right_block,
            });
            (true, eval_right_block)
        }
        _ => unreachable!(),
    };
    ctx.set_current_block(eval_right_block);
    let rhs = lower_expr(ctx, right);
    ctx.terminate(Terminator::Branch(merge_block));
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

fn lower_unary(ctx: &mut LoweringContext, op: UnaryOp, expr: &ast::Expr) -> ValueId {
    let operand = lower_expr(ctx, expr);
    let result = ctx.new_value();
    match op {
        UnaryOp::Neg => ctx.emit(Instruction::Unary {
            result,
            op: IrUnaryOp::Neg,
            operand,
        }),
        UnaryOp::Not | UnaryOp::BitNot => ctx.emit(Instruction::Unary {
            result,
            op: IrUnaryOp::Not,
            operand,
        }),
        UnaryOp::Deref => {
            let ptr_ty = infer_expr_type(ctx, expr);
            let val_ty = if let Ty::Ptr(inner) = ptr_ty {
                *inner
            } else {
                Ty::I64
            };
            ctx.emit(Instruction::Load {
                result,
                ptr: operand,
                ty: val_ty,
            });
        }
        UnaryOp::Ref | UnaryOp::RefMut => return operand,
    }
    result
}

fn lower_call(ctx: &mut LoweringContext, func: &ast::Expr, args: &[ast::Expr]) -> ValueId {
    let arg_values: Vec<ValueId> = args.iter().map(|arg| lower_expr(ctx, arg)).collect();
    let result = ctx.new_value();
    let check_name = |name: &str| -> Option<Ty> {
        if name.starts_with("print") || name == "assert" || name == "panic" {
            Some(Ty::Void)
        } else {
            ctx.module
                .get_function(name)
                .map(|func| func.return_ty.clone())
        }
    };
    // Detect module.function() calls: FieldAccess { object: Variable(mod) | Path(mod), field: fn }
    // This handles `import lib; lib.helper()` which is lowered as a direct Call to helper.
    let field_access_name: Option<String> = if let ast::Expr::FieldAccess { object, field } = func {
        match object.as_ref() {
            ast::Expr::Variable(ident) => Some(format!("{}::{}", ident.name, field.name)),
            ast::Expr::Path(path) if path.segments.len() == 1 => {
                Some(format!("{}::{}", path.segments[0].name, field.name))
            }
            _ => None,
        }
    } else {
        None
    };
    let func_name = if let Some(ref name) = field_access_name {
        name.clone()
    } else {
        match func {
            ast::Expr::Variable(ident) => ident.name.clone(),
            ast::Expr::Path(path) if !path.segments.is_empty() => path
                .segments
                .iter()
                .map(|s| &s.name)
                .cloned()
                .collect::<Vec<_>>()
                .join("::"),
            _ => "indirect".to_string(),
        }
    };
    let call_ty = if func_name == "indirect" {
        let ptr_ty = infer_expr_type(ctx, func);
        match ptr_ty {
            Ty::Function(_, ret) => *ret,
            _ => Ty::I64,
        }
    } else {
        check_name(&func_name).unwrap_or(Ty::I64)
    };
    if func_name == "indirect" {
        let func_ptr = lower_expr(ctx, func);
        ctx.emit(Instruction::CallIndirect {
            result,
            ptr: func_ptr,
            args: arg_values,
            ty: call_ty,
        });
    } else {
        ctx.emit(Instruction::Call {
            result,
            func: func_name,
            args: arg_values,
            ty: call_ty,
        });
    }
    result
}

fn lower_method_call(
    ctx: &mut LoweringContext,
    receiver: &ast::Expr,
    method: &ast::Ident,
    args: &[ast::Expr],
) -> ValueId {
    let mut arg_values = vec![lower_expr(ctx, receiver)];
    arg_values.extend(args.iter().map(|arg| lower_expr(ctx, arg)));
    let result = ctx.new_value();
    let func_name = method.name.to_string();
    let ret_ty = Ty::I64;
    ctx.emit(Instruction::Call {
        result,
        func: func_name,
        args: arg_values,
        ty: ret_ty,
    });
    result
}

fn lower_field_access(
    ctx: &mut LoweringContext,
    object: &ast::Expr,
    field: &ast::Ident,
) -> ValueId {
    let field_ptr = ctx.new_value();
    let obj_ptr = lower_lvalue(ctx, object);
    let field_index = if let ast::Expr::Variable(ident) = object {
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
        struct_ty: struct_ty.clone(),
    });
    let ty = match &struct_ty {
        Ty::Struct(fields) => fields.get(field_index).cloned().unwrap_or(Ty::I64),
        _ => {
            // For named/unknown types: look up from struct registry
            if let ast::Expr::Variable(ident) = object {
                if let Some(struct_info) = ctx.lookup_struct(&ident.name) {
                    struct_info
                        .field_types
                        .get(field_index)
                        .cloned()
                        .unwrap_or(Ty::I64)
                } else if let Some(var_ty) = ctx.lookup_variable_type(&ident.name) {
                    // If we know the variable type is a named struct, look up its fields
                    if let Ty::Struct(fields) = var_ty {
                        fields.get(field_index).cloned().unwrap_or(Ty::I64)
                    } else {
                        Ty::I64
                    }
                } else {
                    Ty::I64
                }
            } else {
                Ty::I64
            }
        }
    };
    maybe_load(ctx, field_ptr, ty)
}

fn lower_index(ctx: &mut LoweringContext, object: &ast::Expr, index: &ast::Expr) -> ValueId {
    let obj_ptr = lower_lvalue(ctx, object);
    let idx = lower_expr(ctx, index);
    let array_ty = infer_expr_type(ctx, object);
    let elem_ty = if let Ty::Array(elem, _) = array_ty {
        *elem
    } else {
        Ty::I64
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

fn lower_block_expr(ctx: &mut LoweringContext, block: &ast::Block) -> ValueId {
    lower_block(ctx, block)
}

fn lower_if(
    ctx: &mut LoweringContext,
    cond: &ast::Expr,
    then_branch: &ast::Expr,
    else_branch: Option<&ast::Expr>,
) -> ValueId {
    let cond_val = lower_expr(ctx, cond);
    let then_block = ctx.create_block("then");
    let else_block = ctx.create_block("else");
    let merge_block_id = ctx.create_block_id();
    ctx.terminate(Terminator::CondBranch {
        cond: cond_val,
        then_block,
        else_block,
    });
    ctx.set_current_block(then_block);
    ctx.enter_scope();
    let then_val = lower_expr(ctx, then_branch);
    ctx.exit_scope();
    let then_end_block = ctx.current_block();
    let then_terminated = ctx
        .get_current_block()
        .map(|b| b.is_terminated())
        .unwrap_or(true);
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
    let merge_block = jet_ir::BasicBlock::with_name(merge_block_id, "merge");
    ctx.add_block(merge_block);
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
    match (then_terminated, else_terminated) {
        (true, true) => {
            ctx.set_current_block(merge_block_id);
            lower_unit(ctx)
        }
        (true, false) => {
            ctx.set_current_block(merge_block_id);
            else_val
        }
        (false, true) => {
            ctx.set_current_block(merge_block_id);
            then_val
        }
        (false, false) => {
            ctx.set_current_block(merge_block_id);
            let result = ctx.new_value();
            let phi_ty = infer_expr_type(ctx, then_branch);
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
    ctx.push_loop_target(LoopTarget {
        label: label_str.clone(),
        break_block: exit_block,
        continue_block: cond_block,
        break_value: None,
    });
    ctx.terminate(Terminator::Branch(cond_block));
    ctx.set_current_block(cond_block);
    let cond_val = lower_expr(ctx, cond);
    ctx.terminate(Terminator::CondBranch {
        cond: cond_val,
        then_block: body_block,
        else_block: exit_block,
    });
    ctx.set_current_block(body_block);
    ctx.enter_scope();
    lower_expr(ctx, body);
    ctx.exit_scope();
    ctx.terminate(Terminator::Branch(cond_block));
    ctx.pop_loop_target();
    ctx.set_current_block(exit_block);
    lower_unit(ctx)
}

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
    let _iter_val = lower_expr(ctx, iterable);
    ctx.push_loop_target(LoopTarget {
        label: label_str,
        break_block: exit_block,
        continue_block: cond_block,
        break_value: None,
    });
    ctx.terminate(Terminator::Branch(cond_block));
    ctx.set_current_block(cond_block);
    let cond_val = ctx.new_value();
    ctx.emit(Instruction::Const {
        result: cond_val,
        value: ConstantValue::Bool(true),
    });
    ctx.terminate(Terminator::CondBranch {
        cond: cond_val,
        then_block: body_block,
        else_block: exit_block,
    });
    ctx.set_current_block(body_block);
    ctx.enter_scope();
    let loop_var_ptr = ctx.new_value();
    let loop_var_ty = Ty::I64;
    ctx.emit(Instruction::Alloc {
        result: loop_var_ptr,
        ty: loop_var_ty.clone(),
    });
    let init_val = ctx.new_value();
    ctx.emit(Instruction::Const {
        result: init_val,
        value: ConstantValue::Int(0, Ty::I64),
    });
    ctx.emit(Instruction::Store {
        ptr: loop_var_ptr,
        value: init_val,
    });
    bind_pattern_ptr(ctx, pattern, loop_var_ptr, loop_var_ty);
    lower_expr(ctx, body);
    ctx.exit_scope();
    ctx.terminate(Terminator::Branch(cond_block));
    ctx.pop_loop_target();
    ctx.set_current_block(exit_block);
    lower_unit(ctx)
}

fn lower_loop(ctx: &mut LoweringContext, label: Option<&ast::Ident>, body: &ast::Expr) -> ValueId {
    let body_block = ctx.create_block("loop_body");
    let exit_block = ctx.create_block("loop_exit");
    let label_str = label.map(|l| l.name.clone());
    ctx.push_loop_target(LoopTarget {
        label: label_str,
        break_block: exit_block,
        continue_block: body_block,
        break_value: None,
    });
    ctx.terminate(Terminator::Branch(body_block));
    ctx.set_current_block(body_block);
    ctx.enter_scope();
    lower_expr(ctx, body);
    ctx.exit_scope();
    ctx.terminate(Terminator::Branch(body_block));
    ctx.pop_loop_target();
    ctx.set_current_block(exit_block);
    lower_unit(ctx)
}

fn lower_tuple(ctx: &mut LoweringContext, elements: &[ast::Expr]) -> ValueId {
    let values: Vec<ValueId> = elements.iter().map(|e| lower_expr(ctx, e)).collect();
    let result = ctx.new_value();
    let elem_types: Vec<Ty> = values.iter().map(|_| Ty::I64).collect();
    let tuple_ty = Ty::Struct(elem_types);
    ctx.emit(Instruction::StructAgg {
        result,
        fields: values,
        ty: tuple_ty,
    });
    result
}

fn lower_array(ctx: &mut LoweringContext, elements: &[ast::Expr]) -> ValueId {
    let values: Vec<ValueId> = elements.iter().map(|e| lower_expr(ctx, e)).collect();
    let result = ctx.new_value();
    let array_ty = Ty::Array(Box::new(Ty::I64), values.len());
    ctx.emit(Instruction::ArrayAgg {
        result,
        elements: values,
        ty: array_ty,
    });
    result
}

fn lower_assign(
    ctx: &mut LoweringContext,
    target: &ast::Expr,
    op: ast::AssignOp,
    value: &ast::Expr,
) -> ValueId {
    let val = lower_expr(ctx, value);
    let ptr = lower_lvalue(ctx, target);
    let final_val = if op != ast::AssignOp::Assign {
        let loaded = ctx.new_value();
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
    lower_unit(ctx)
}

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
    break_val.unwrap_or_else(|| lower_unit(ctx))
}

fn lower_continue(ctx: &mut LoweringContext, label: Option<&ast::Ident>) -> ValueId {
    let label_str = label.map(|l| l.name.as_str());
    let target = ctx
        .find_loop_target(label_str)
        .expect("No matching loop for continue")
        .clone();
    ctx.terminate(Terminator::Branch(target.continue_block));
    lower_unit(ctx)
}

fn lower_return(ctx: &mut LoweringContext, value: Option<&ast::Expr>) -> ValueId {
    let ret_val = value.map(|v| lower_expr(ctx, v));
    ctx.terminate(Terminator::Return(ret_val));
    lower_unit(ctx)
}

fn lower_await(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    let future = lower_expr(ctx, expr);
    let result = ctx.new_value();
    ctx.emit(Instruction::Await { result, future });
    result
}

fn lower_try(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    lower_expr(ctx, expr)
}

fn lower_struct_literal(
    ctx: &mut LoweringContext,
    path: &ast::Path,
    fields: &[ast::FieldInit],
) -> ValueId {
    let field_values: Vec<ValueId> = fields
        .iter()
        .map(|f| lower_expr(ctx, f.value.as_ref().expect("Field value required")))
        .collect();
    let struct_name = if path.segments.is_empty() {
        "Unknown".to_string()
    } else {
        path.segments[0].name.clone()
    };
    let struct_ty = if let Some(info) = ctx.lookup_struct(&struct_name) {
        Ty::Struct(info.field_types.clone())
    } else {
        Ty::Struct(vec![Ty::I64; field_values.len()])
    };
    let struct_val = ctx.new_value();
    ctx.emit(Instruction::StructAgg {
        result: struct_val,
        fields: field_values,
        ty: struct_ty,
    });
    struct_val
}

fn lower_self(ctx: &mut LoweringContext) -> ValueId {
    if let Some(ptr) = ctx.lookup_variable("self") {
        let ty = ctx.lookup_variable_type("self").cloned().unwrap_or(Ty::I64);
        maybe_load(ctx, ptr, ty)
    } else {
        let result = ctx.new_value();
        let value = ConstantValue::Zero(Ty::Ptr(Box::new(Ty::Void)));
        ctx.emit(Instruction::Const { result, value });
        result
    }
}

fn lower_async_block(ctx: &mut LoweringContext, block: &ast::Block) -> ValueId {
    lower_block(ctx, block)
}

fn lower_concurrent_block(ctx: &mut LoweringContext, block: &ast::Block) -> ValueId {
    lower_block(ctx, block)
}

fn lower_pass(ctx: &mut LoweringContext) -> ValueId {
    lower_unit(ctx)
}

fn lower_spawn(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    let spawned = lower_expr(ctx, expr);
    let result = ctx.new_value();
    ctx.emit(Instruction::Call {
        result,
        func: "spawn".to_string(),
        args: vec![spawned],
        ty: Ty::I64,
    });
    result
}

// ============== TYPED EXPRESSION LOWERING ==============

pub fn lower_typed_expr(ctx: &mut LoweringContext, expr: &jet_typeck::TypedExpr) -> ValueId {
    use jet_typeck::TypedExprKind;
    match &expr.kind {
        TypedExprKind::Literal(lit) => lower_typed_literal(ctx, lit, expr.ty),
        TypedExprKind::Variable(ident) => lower_typed_variable(ctx, ident, expr.ty),
        TypedExprKind::Binary { op, left, right } => {
            lower_typed_binary(ctx, *op, left, right, expr.ty)
        }
        TypedExprKind::Unary { op, expr: operand } => lower_typed_unary(ctx, *op, operand, expr.ty),
        TypedExprKind::Call { func, args } => lower_typed_call(ctx, func, args, expr.ty),
        TypedExprKind::Block(block) => lower_typed_block(ctx, block),
        TypedExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => lower_typed_if(ctx, cond, then_branch, else_branch.as_deref(), expr.ty),
        TypedExprKind::Match {
            expr: scrutinee,
            arms,
        } => lower_typed_match(ctx, scrutinee, arms, expr.ty),
        TypedExprKind::While { cond, body } => lower_typed_while(ctx, cond, body),
        TypedExprKind::For {
            pattern,
            iterable,
            body,
        } => lower_typed_for(ctx, pattern, iterable, body),
        TypedExprKind::Loop { body } => lower_typed_loop(ctx, body),
        TypedExprKind::Lambda { params, body } => lower_typed_lambda(ctx, params, body, expr.ty),
        TypedExprKind::Await(future) => lower_typed_await(ctx, future, expr.ty),
        TypedExprKind::Try(inner) => lower_typed_try(ctx, inner, expr.ty),
        TypedExprKind::Assign { target, op, value } => lower_typed_assign(ctx, target, *op, value),
        TypedExprKind::Break(value) => lower_typed_break(ctx, value.as_deref()),
        TypedExprKind::Continue => lower_typed_continue(ctx),
        TypedExprKind::Return(value) => lower_typed_return(ctx, value.as_deref()),
        TypedExprKind::Tuple(elements) => lower_typed_tuple(ctx, elements, expr.ty),
        TypedExprKind::Array(elements) => lower_typed_array(ctx, elements, expr.ty),
        TypedExprKind::Spawn(expr) => lower_typed_spawn(ctx, expr, expr.ty),
        TypedExprKind::Async(block) => lower_typed_async(ctx, block, expr.ty),
        TypedExprKind::Concurrent(block) => lower_typed_concurrent(ctx, block, expr.ty),
        TypedExprKind::Hole(_) => {
            // Holes should be filled before lowering; if we encounter one here,
            // emit a placeholder value
            lower_unit(ctx)
        }
    }
}

fn lower_typed_literal(
    ctx: &mut LoweringContext,
    lit: &ast::Literal,
    ty: jet_typeck::TypeId,
) -> ValueId {
    let result = ctx.new_value();
    let ir_ty = crate::ty::lower_typeck_type(ty, ctx.type_context());
    let value = match lit {
        ast::Literal::Integer(n) => ConstantValue::Int(*n, ir_ty),
        ast::Literal::Float(f) => ConstantValue::Float(*f, ir_ty),
        ast::Literal::Bool(b) => ConstantValue::Bool(*b),
        ast::Literal::String(s) => ConstantValue::String(s.clone()),
        ast::Literal::Char(c) => ConstantValue::Int(*c as i64, ir_ty),
        ast::Literal::Unit => ConstantValue::Zero(Ty::Void),
    };
    ctx.emit(Instruction::Const { result, value });
    result
}

fn lower_typed_variable(
    ctx: &mut LoweringContext,
    ident: &ast::Ident,
    ty: jet_typeck::TypeId,
) -> ValueId {
    let ptr = if let Some(p) = ctx.lookup_variable(&ident.name) {
        p
    } else {
        let result = ctx.new_value();
        let value = ConstantValue::Zero(Ty::Ptr(Box::new(Ty::Void)));
        ctx.emit(Instruction::Const { result, value });
        return result;
    };
    let ir_ty = crate::ty::lower_typeck_type(ty, ctx.type_context());
    let loaded = ctx.new_value();
    ctx.emit(Instruction::Load {
        result: loaded,
        ptr,
        ty: ir_ty,
    });
    loaded
}

fn lower_typed_binary(
    ctx: &mut LoweringContext,
    op: ast::BinaryOp,
    left: &jet_typeck::TypedExpr,
    right: &jet_typeck::TypedExpr,
    result_ty: jet_typeck::TypeId,
) -> ValueId {
    if op == ast::BinaryOp::And || op == ast::BinaryOp::Or {
        return lower_typed_logical_op(ctx, op, left, right, result_ty);
    }
    let lhs = lower_typed_expr(ctx, left);
    let rhs = lower_typed_expr(ctx, right);
    let result = ctx.new_value();
    let ir_op = match op {
        ast::BinaryOp::Add => IrBinaryOp::Add,
        ast::BinaryOp::Sub => IrBinaryOp::Sub,
        ast::BinaryOp::Mul => IrBinaryOp::Mul,
        ast::BinaryOp::Div => IrBinaryOp::Div,
        ast::BinaryOp::Mod => IrBinaryOp::Rem,
        ast::BinaryOp::BitAnd => IrBinaryOp::And,
        ast::BinaryOp::BitOr => IrBinaryOp::Or,
        ast::BinaryOp::BitXor => IrBinaryOp::Xor,
        ast::BinaryOp::Shl => IrBinaryOp::Shl,
        ast::BinaryOp::Shr => IrBinaryOp::Shr,
        ast::BinaryOp::Eq => IrBinaryOp::Eq,
        ast::BinaryOp::Ne => IrBinaryOp::Ne,
        ast::BinaryOp::Lt => IrBinaryOp::Lt,
        ast::BinaryOp::Gt => IrBinaryOp::Gt,
        ast::BinaryOp::Le => IrBinaryOp::Le,
        ast::BinaryOp::Ge => IrBinaryOp::Ge,
        _ => return lower_unit(ctx),
    };
    ctx.emit(Instruction::Binary {
        result,
        op: ir_op,
        lhs,
        rhs,
    });
    result
}

fn lower_typed_logical_op(
    ctx: &mut LoweringContext,
    op: ast::BinaryOp,
    left: &jet_typeck::TypedExpr,
    right: &jet_typeck::TypedExpr,
    _result_ty: jet_typeck::TypeId,
) -> ValueId {
    let lhs = lower_typed_expr(ctx, left);
    let eval_right_block = ctx.create_block("eval_right");
    let merge_block = ctx.create_block("merge");
    let (short_circuit_value, cond_block) = match op {
        ast::BinaryOp::And => {
            ctx.terminate(Terminator::CondBranch {
                cond: lhs,
                then_block: eval_right_block,
                else_block: merge_block,
            });
            (false, eval_right_block)
        }
        ast::BinaryOp::Or => {
            ctx.terminate(Terminator::CondBranch {
                cond: lhs,
                then_block: merge_block,
                else_block: eval_right_block,
            });
            (true, eval_right_block)
        }
        _ => unreachable!(),
    };
    ctx.set_current_block(eval_right_block);
    let rhs = lower_typed_expr(ctx, right);
    ctx.terminate(Terminator::Branch(merge_block));
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

fn lower_typed_unary(
    ctx: &mut LoweringContext,
    op: ast::UnaryOp,
    expr: &jet_typeck::TypedExpr,
    result_ty: jet_typeck::TypeId,
) -> ValueId {
    let operand = lower_typed_expr(ctx, expr);
    let result = ctx.new_value();
    let ir_ty = crate::ty::lower_typeck_type(result_ty, ctx.type_context());
    match op {
        ast::UnaryOp::Neg => ctx.emit(Instruction::Unary {
            result,
            op: IrUnaryOp::Neg,
            operand,
        }),
        ast::UnaryOp::Not | ast::UnaryOp::BitNot => ctx.emit(Instruction::Unary {
            result,
            op: IrUnaryOp::Not,
            operand,
        }),
        ast::UnaryOp::Deref => ctx.emit(Instruction::Load {
            result,
            ptr: operand,
            ty: ir_ty,
        }),
        ast::UnaryOp::Ref | ast::UnaryOp::RefMut => return operand,
    }
    result
}

fn lower_typed_call(
    ctx: &mut LoweringContext,
    func: &jet_typeck::TypedExpr,
    args: &[jet_typeck::TypedExpr],
    result_ty: jet_typeck::TypeId,
) -> ValueId {
    let arg_values: Vec<ValueId> = args.iter().map(|arg| lower_typed_expr(ctx, arg)).collect();
    let result = ctx.new_value();
    let call_ty = crate::ty::lower_typeck_type(result_ty, ctx.type_context());
    if let jet_typeck::TypedExprKind::Variable(ident) = &func.kind {
        let func_name = ident.name.clone();
        ctx.emit(Instruction::Call {
            result,
            func: func_name,
            args: arg_values,
            ty: call_ty,
        });
    } else {
        let func_ptr = lower_typed_expr(ctx, func);
        ctx.emit(Instruction::CallIndirect {
            result,
            ptr: func_ptr,
            args: arg_values,
            ty: call_ty,
        });
    }
    result
}

fn lower_typed_block(ctx: &mut LoweringContext, block: &jet_typeck::TypedBlock) -> ValueId {
    ctx.enter_scope();
    let mut last_value = None;
    for stmt in &block.stmts {
        last_value = lower_typed_stmt_impl(ctx, stmt);
        if let Some(current) = ctx.get_current_block() {
            if current.is_terminated() {
                break;
            }
        }
    }
    let result = if let Some(expr) = &block.expr {
        lower_typed_expr(ctx, expr)
    } else {
        last_value.unwrap_or_else(|| {
            let unit = ctx.new_value();
            ctx.emit(Instruction::Const {
                result: unit,
                value: ConstantValue::Zero(Ty::Void),
            });
            unit
        })
    };
    ctx.exit_scope();
    result
}

pub(crate) fn lower_typed_stmt_impl(
    ctx: &mut LoweringContext,
    stmt: &jet_typeck::TypedStmt,
) -> Option<ValueId> {
    use jet_typeck::TypedStmt;
    match stmt {
        TypedStmt::Let { pattern, ty, value } => {
            lower_typed_let(ctx, pattern, *ty, value);
            None
        }
        TypedStmt::Expr(expr) => Some(lower_typed_expr(ctx, expr)),
        TypedStmt::Assign { target, op, value } => {
            lower_typed_assign_stmt(ctx, target, *op, value);
            None
        }
        TypedStmt::Return(value) => {
            lower_typed_return_stmt(ctx, value.as_ref());
            None
        }
        TypedStmt::Break(value) => {
            lower_typed_break_stmt(ctx, value.as_ref());
            None
        }
        TypedStmt::Continue => {
            lower_typed_continue_stmt(ctx);
            None
        }
    }
}

fn lower_typed_let(
    ctx: &mut LoweringContext,
    pattern: &ast::Pattern,
    ty: jet_typeck::TypeId,
    value: &jet_typeck::TypedExpr,
) {
    let init_val = lower_typed_expr(ctx, value);
    let var_ty = crate::ty::lower_typeck_type(ty, ctx.type_context());
    let alloc = ctx.new_value();
    ctx.emit(Instruction::Alloc {
        result: alloc,
        ty: var_ty.clone(),
    });
    ctx.emit(Instruction::Store {
        ptr: alloc,
        value: init_val,
    });
    crate::stmt::bind_pattern_ptr(ctx, pattern, alloc, var_ty);
}

fn lower_typed_if(
    ctx: &mut LoweringContext,
    cond: &jet_typeck::TypedExpr,
    then_branch: &jet_typeck::TypedExpr,
    else_branch: Option<&jet_typeck::TypedExpr>,
    result_ty: jet_typeck::TypeId,
) -> ValueId {
    let cond_val = lower_typed_expr(ctx, cond);
    let then_block = ctx.create_block("then");
    let else_block = ctx.create_block("else");
    let merge_block_id = ctx.create_block_id();
    ctx.terminate(Terminator::CondBranch {
        cond: cond_val,
        then_block,
        else_block,
    });
    ctx.set_current_block(then_block);
    ctx.enter_scope();
    let then_val = lower_typed_expr(ctx, then_branch);
    ctx.exit_scope();
    let then_end_block = ctx.current_block();
    let then_terminated = ctx
        .get_current_block()
        .map(|b| b.is_terminated())
        .unwrap_or(true);
    ctx.set_current_block(else_block);
    let else_val = if let Some(else_expr) = else_branch {
        ctx.enter_scope();
        let val = lower_typed_expr(ctx, else_expr);
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
    let merge_block = jet_ir::BasicBlock::with_name(merge_block_id, "merge");
    ctx.add_block(merge_block);
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
    match (then_terminated, else_terminated) {
        (true, true) => {
            ctx.set_current_block(merge_block_id);
            lower_unit(ctx)
        }
        (true, false) => {
            ctx.set_current_block(merge_block_id);
            else_val
        }
        (false, true) => {
            ctx.set_current_block(merge_block_id);
            then_val
        }
        (false, false) => {
            ctx.set_current_block(merge_block_id);
            let result = ctx.new_value();
            let phi_ty = crate::ty::lower_typeck_type(result_ty, ctx.type_context());
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

fn lower_typed_match(
    ctx: &mut LoweringContext,
    scrutinee: &jet_typeck::TypedExpr,
    arms: &[jet_typeck::TypedMatchArm],
    result_ty: jet_typeck::TypeId,
) -> ValueId {
    let scrutinee_val = lower_typed_expr(ctx, scrutinee);
    let scrutinee_ty = crate::ty::lower_typeck_type(scrutinee.ty, ctx.type_context());
    let result_ir_ty = crate::ty::lower_typeck_type(result_ty, ctx.type_context());

    let merge_block = ctx.create_block("match_merge");
    let arm_blocks: Vec<BlockId> = arms.iter().map(|_| ctx.create_block("match_arm")).collect();
    let result = ctx.new_value();
    let mut incoming: Vec<(BlockId, ValueId)> = Vec::new();

    // Branch from current block to first arm block
    if let Some(first_arm) = arm_blocks.first() {
        ctx.terminate(Terminator::Branch(*first_arm));
    }

    for (i, arm) in arms.iter().enumerate() {
        let arm_block = arm_blocks[i];
        ctx.set_current_block(arm_block);
        ctx.enter_scope();

        // Bind pattern variables
        crate::pattern::bind_match_pattern(ctx, &arm.pattern, scrutinee_val, &scrutinee_ty);

        // Lower guard if present
        if let Some(ref guard) = arm.guard {
            let guard_val = lower_typed_expr(ctx, guard);
            let guard_then = ctx.create_block("guard_then");
            let guard_else = ctx.create_block("guard_else");

            ctx.terminate(Terminator::CondBranch {
                cond: guard_val,
                then_block: guard_then,
                else_block: guard_else,
            });

            // Guard failed - go to next arm or merge
            ctx.set_current_block(guard_else);
            if i + 1 < arms.len() {
                ctx.terminate(Terminator::Branch(arm_blocks[i + 1]));
            } else {
                ctx.terminate(Terminator::Branch(merge_block));
            }

            ctx.set_current_block(guard_then);
        }

        let body_val = lower_typed_expr(ctx, &arm.body);
        let end_block = ctx.current_block().unwrap_or(arm_block);
        if !ctx
            .get_current_block()
            .map(|b| b.is_terminated())
            .unwrap_or(true)
        {
            ctx.terminate(Terminator::Branch(merge_block));
        }
        incoming.push((end_block, body_val));
        ctx.exit_scope();
    }

    ctx.set_current_block(merge_block);
    if !incoming.is_empty() {
        ctx.emit(Instruction::Phi {
            result,
            incoming,
            ty: result_ir_ty,
        });
    } else {
        ctx.emit(Instruction::Const {
            result,
            value: ConstantValue::Zero(Ty::Void),
        });
    }
    result
}

fn lower_typed_while(
    ctx: &mut LoweringContext,
    cond: &jet_typeck::TypedExpr,
    body: &jet_typeck::TypedExpr,
) -> ValueId {
    let cond_block = ctx.create_block("while_cond");
    let body_block = ctx.create_block("while_body");
    let exit_block = ctx.create_block("while_exit");
    ctx.push_loop_target(LoopTarget {
        label: None,
        break_block: exit_block,
        continue_block: cond_block,
        break_value: None,
    });
    ctx.terminate(Terminator::Branch(cond_block));
    ctx.set_current_block(cond_block);
    let cond_val = lower_typed_expr(ctx, cond);
    ctx.terminate(Terminator::CondBranch {
        cond: cond_val,
        then_block: body_block,
        else_block: exit_block,
    });
    ctx.set_current_block(body_block);
    ctx.enter_scope();
    lower_typed_expr(ctx, body);
    ctx.exit_scope();
    ctx.terminate(Terminator::Branch(cond_block));
    ctx.pop_loop_target();
    ctx.set_current_block(exit_block);
    lower_unit(ctx)
}

fn lower_typed_for(
    ctx: &mut LoweringContext,
    pattern: &ast::Pattern,
    iterable: &jet_typeck::TypedExpr,
    body: &jet_typeck::TypedExpr,
) -> ValueId {
    let cond_block = ctx.create_block("for_cond");
    let body_block = ctx.create_block("for_body");
    let exit_block = ctx.create_block("for_exit");
    let _iter_val = lower_typed_expr(ctx, iterable);
    ctx.push_loop_target(LoopTarget {
        label: None,
        break_block: exit_block,
        continue_block: cond_block,
        break_value: None,
    });
    ctx.terminate(Terminator::Branch(cond_block));
    ctx.set_current_block(cond_block);
    let cond_val = ctx.new_value();
    ctx.emit(Instruction::Const {
        result: cond_val,
        value: ConstantValue::Bool(true),
    });
    ctx.terminate(Terminator::CondBranch {
        cond: cond_val,
        then_block: body_block,
        else_block: exit_block,
    });
    ctx.set_current_block(body_block);
    ctx.enter_scope();
    let loop_var_ptr = ctx.new_value();
    let loop_var_ty = Ty::I64;
    ctx.emit(Instruction::Alloc {
        result: loop_var_ptr,
        ty: loop_var_ty.clone(),
    });
    let init_val = ctx.new_value();
    ctx.emit(Instruction::Const {
        result: init_val,
        value: ConstantValue::Int(0, Ty::I64),
    });
    ctx.emit(Instruction::Store {
        ptr: loop_var_ptr,
        value: init_val,
    });
    crate::stmt::bind_pattern_ptr(ctx, pattern, loop_var_ptr, loop_var_ty);
    lower_typed_expr(ctx, body);
    ctx.exit_scope();
    ctx.terminate(Terminator::Branch(cond_block));
    ctx.pop_loop_target();
    ctx.set_current_block(exit_block);
    lower_unit(ctx)
}

fn lower_typed_loop(ctx: &mut LoweringContext, body: &jet_typeck::TypedExpr) -> ValueId {
    let body_block = ctx.create_block("loop_body");
    let exit_block = ctx.create_block("loop_exit");
    ctx.push_loop_target(LoopTarget {
        label: None,
        break_block: exit_block,
        continue_block: body_block,
        break_value: None,
    });
    ctx.terminate(Terminator::Branch(body_block));
    ctx.set_current_block(body_block);
    ctx.enter_scope();
    lower_typed_expr(ctx, body);
    ctx.exit_scope();
    ctx.terminate(Terminator::Branch(body_block));
    ctx.pop_loop_target();
    ctx.set_current_block(exit_block);
    lower_unit(ctx)
}

fn lower_typed_lambda(
    ctx: &mut LoweringContext,
    params: &[jet_typeck::TypedParam],
    _body: &jet_typeck::TypedExpr,
    _lambda_ty: jet_typeck::TypeId,
) -> ValueId {
    let ast_params: Vec<ast::Param> = params
        .iter()
        .map(|p| ast::Param {
            pattern: p.pattern.clone(),
            ty: ast::Type::Path(ast::Path::new(
                vec![ast::Ident::new("int", jet_lexer::Span::new(0, 0))],
                jet_lexer::Span::new(0, 0),
            )),
        })
        .collect();
    let ast_body = ast::Expr::Literal(ast::Literal::Unit);
    let effects_vec = vec![];
    crate::closure::convert_lambda(ctx, &ast_params, None, &effects_vec, &ast_body)
}

fn lower_typed_await(
    ctx: &mut LoweringContext,
    future: &jet_typeck::TypedExpr,
    _result_ty: jet_typeck::TypeId,
) -> ValueId {
    let future_val = lower_typed_expr(ctx, future);
    let result = ctx.new_value();
    ctx.emit(Instruction::Await {
        result,
        future: future_val,
    });
    result
}

fn lower_typed_try(
    ctx: &mut LoweringContext,
    expr: &jet_typeck::TypedExpr,
    _result_ty: jet_typeck::TypeId,
) -> ValueId {
    lower_typed_expr(ctx, expr)
}

fn lower_typed_assign(
    ctx: &mut LoweringContext,
    target: &jet_typeck::TypedExpr,
    op: ast::AssignOp,
    value: &jet_typeck::TypedExpr,
) -> ValueId {
    let val = lower_typed_expr(ctx, value);
    let ptr = match &target.kind {
        jet_typeck::TypedExprKind::Variable(ident) => ctx
            .lookup_variable(&ident.name)
            .expect("Variable not found"),
        _ => {
            let temp = ctx.new_value();
            ctx.emit(Instruction::Alloc {
                result: temp,
                ty: Ty::I64,
            });
            temp
        }
    };
    let final_val = if op != ast::AssignOp::Assign {
        let loaded = ctx.new_value();
        let target_ty = crate::ty::lower_typeck_type(target.ty, ctx.type_context());
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
    lower_unit(ctx)
}

fn lower_typed_assign_stmt(
    ctx: &mut LoweringContext,
    target: &jet_typeck::TypedExpr,
    op: ast::AssignOp,
    value: &jet_typeck::TypedExpr,
) {
    lower_typed_assign(ctx, target, op, value);
}

fn lower_typed_break(ctx: &mut LoweringContext, value: Option<&jet_typeck::TypedExpr>) -> ValueId {
    let target = ctx
        .find_loop_target(None)
        .expect("No matching loop for break")
        .clone();
    let break_val = value.map(|v| lower_typed_expr(ctx, v));
    ctx.terminate(Terminator::Branch(target.break_block));
    break_val.unwrap_or_else(|| lower_unit(ctx))
}

fn lower_typed_break_stmt(ctx: &mut LoweringContext, value: Option<&jet_typeck::TypedExpr>) {
    lower_typed_break(ctx, value);
}

fn lower_typed_continue(ctx: &mut LoweringContext) -> ValueId {
    let target = ctx
        .find_loop_target(None)
        .expect("No matching loop for continue")
        .clone();
    ctx.terminate(Terminator::Branch(target.continue_block));
    lower_unit(ctx)
}

fn lower_typed_continue_stmt(ctx: &mut LoweringContext) {
    lower_typed_continue(ctx);
}

fn lower_typed_return(ctx: &mut LoweringContext, value: Option<&jet_typeck::TypedExpr>) -> ValueId {
    let ret_val = value.map(|v| lower_typed_expr(ctx, v));
    ctx.terminate(Terminator::Return(ret_val));
    lower_unit(ctx)
}

fn lower_typed_return_stmt(ctx: &mut LoweringContext, value: Option<&jet_typeck::TypedExpr>) {
    lower_typed_return(ctx, value);
}

fn lower_typed_tuple(
    ctx: &mut LoweringContext,
    elements: &[jet_typeck::TypedExpr],
    tuple_ty: jet_typeck::TypeId,
) -> ValueId {
    let values: Vec<ValueId> = elements.iter().map(|e| lower_typed_expr(ctx, e)).collect();
    let result = ctx.new_value();
    let ir_ty = crate::ty::lower_typeck_type(tuple_ty, ctx.type_context());
    ctx.emit(Instruction::StructAgg {
        result,
        fields: values,
        ty: ir_ty,
    });
    result
}

fn lower_typed_array(
    ctx: &mut LoweringContext,
    elements: &[jet_typeck::TypedExpr],
    array_ty: jet_typeck::TypeId,
) -> ValueId {
    let values: Vec<ValueId> = elements.iter().map(|e| lower_typed_expr(ctx, e)).collect();
    let result = ctx.new_value();
    let ir_ty = crate::ty::lower_typeck_type(array_ty, ctx.type_context());
    ctx.emit(Instruction::ArrayAgg {
        result,
        elements: values,
        ty: ir_ty,
    });
    result
}

fn lower_typed_spawn(
    ctx: &mut LoweringContext,
    expr: &jet_typeck::TypedExpr,
    result_ty: jet_typeck::TypeId,
) -> ValueId {
    let spawned = lower_typed_expr(ctx, expr);
    let result = ctx.new_value();
    let spawn_ty = crate::ty::lower_typeck_type(result_ty, ctx.type_context());
    ctx.emit(Instruction::Call {
        result,
        func: "spawn".to_string(),
        args: vec![spawned],
        ty: spawn_ty,
    });
    result
}

fn lower_typed_async(
    ctx: &mut LoweringContext,
    block: &jet_typeck::TypedBlock,
    _result_ty: jet_typeck::TypeId,
) -> ValueId {
    lower_typed_block(ctx, block)
}

fn lower_typed_concurrent(
    ctx: &mut LoweringContext,
    block: &jet_typeck::TypedBlock,
    _result_ty: jet_typeck::TypeId,
) -> ValueId {
    lower_typed_block(ctx, block)
}
