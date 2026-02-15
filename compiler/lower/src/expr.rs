//! Expression lowering from AST to IR.
//!
//! This module provides functions for lowering AST expressions to Jet IR instructions.

use crate::closure::convert_lambda;
use crate::context::{LoopTarget, LoweringContext};
use crate::pattern::compile_match;
use crate::stmt::lower_block;
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

/// Lowers an AST expression to IR, returning the value ID of the result.
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
            compile_match(ctx, scrutinee, Ty::I64, arms) // Type would be determined from type checking
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
/// Returns the pointer to the variable (alloca). The caller must load if needed.
fn lower_variable(ctx: &mut LoweringContext, ident: &ast::Ident) -> ValueId {
    if let Some(ptr) = ctx.lookup_variable(&ident.name) {
        ptr
    } else {
        // For external variables, create a placeholder pointer
        let result = ctx.new_value();
        let value = ConstantValue::Zero(Ty::Ptr(Box::new(Ty::Void)));
        ctx.emit(Instruction::Const { result, value });
        result
    }
}

/// Loads a value from a variable pointer if it's not a struct.
fn maybe_load_variable(ctx: &mut LoweringContext, ident: &ast::Ident, ptr: ValueId) -> ValueId {
    if let Some(ty) = ctx.lookup_variable_type(&ident.name) {
        if matches!(ty, Ty::Struct(_)) {
            // Return pointer for structs
            return ptr;
        }
    }
    // Load value for primitive types
    let loaded = ctx.new_value();
    ctx.emit(Instruction::Load {
        result: loaded,
        ptr,
    });
    loaded
}
    } else {
        // For external variables, create a placeholder
        let result = ctx.new_value();
        let value = ConstantValue::Zero(Ty::I64);
        ctx.emit(Instruction::Const { result, value });
        result
    }
}

/// Lowers a path expression.
fn lower_path(ctx: &mut LoweringContext, path: &ast::Path) -> ValueId {
    if path.segments.is_empty() {
        return lower_unit(ctx);
    }

    let name = &path.segments[0].name;
    if let Some(ptr) = ctx.lookup_variable(name) {
        let loaded = ctx.new_value();
        ctx.emit(Instruction::Load {
            result: loaded,
            ptr,
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
            ctx.emit(Instruction::Load {
                result,
                ptr: operand,
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

    if func_name == "indirect" {
        // Indirect call through function pointer
        let func_ptr = lower_expr(ctx, func);
        ctx.emit(Instruction::CallIndirect {
            result,
            ptr: func_ptr,
            args: arg_values,
        });
    } else {
        // Direct function call
        ctx.emit(Instruction::Call {
            result,
            func: func_name,
            args: arg_values,
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

    ctx.emit(Instruction::Call {
        result,
        func: func_name,
        args: arg_values,
    });

    result
}

/// Lowers a field access.
fn lower_field_access(
    ctx: &mut LoweringContext,
    object: &ast::Expr,
    field: &ast::Ident,
) -> ValueId {
    let obj = lower_expr(ctx, object);

    // Get field index from struct type info if available
    let field_index = if let ast::Expr::Variable(ident) = object {
        ctx.get_field_index(&ident.name, &field.name)
    } else {
        None
    }
    .unwrap_or(0);

    // Get pointer to the field
    let field_ptr = ctx.new_value();
    ctx.emit(Instruction::GetFieldPtr {
        result: field_ptr,
        ptr: obj,
        field_index,
    });

    // Load from the field pointer
    let result = ctx.new_value();
    ctx.emit(Instruction::Load {
        result,
        ptr: field_ptr,
    });

    result
}

/// Lowers an index operation.
fn lower_index(ctx: &mut LoweringContext, object: &ast::Expr, index: &ast::Expr) -> ValueId {
    let obj = lower_expr(ctx, object);
    let idx = lower_expr(ctx, index);

    let result = ctx.new_value();

    // Get element pointer
    let elem_ptr = ctx.new_value();
    ctx.emit(Instruction::GetElementPtr {
        result: elem_ptr,
        ptr: obj,
        index: idx,
    });

    // Load from the element
    ctx.emit(Instruction::Load {
        result,
        ptr: elem_ptr,
    });

    result
}

/// Lowers a block expression.
fn lower_block_expr(ctx: &mut LoweringContext, block: &ast::Block) -> ValueId {
    lower_block(ctx, block)
}

/// Lowers an if expression.
fn lower_if(
    ctx: &mut LoweringContext,
    cond: &ast::Expr,
    then_branch: &ast::Expr,
    else_branch: Option<&ast::Expr>,
) -> ValueId {
    let cond_val = lower_expr(ctx, cond);

    let then_block = ctx.create_block("then");
    let else_block = ctx.create_block("else");
    let merge_block = ctx.create_block("merge");

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
    if !ctx.get_current_block().unwrap().is_terminated() {
        ctx.terminate(Terminator::Branch(merge_block));
    }

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
    if !ctx.get_current_block().unwrap().is_terminated() {
        ctx.terminate(Terminator::Branch(merge_block));
    }

    // Merge block with phi
    ctx.set_current_block(merge_block);
    let result = ctx.new_value();

    ctx.emit(Instruction::Phi {
        result,
        incoming: vec![(then_block, then_val), (else_block, else_val)],
    });

    result
}

/// Lowers a match expression.
#[allow(dead_code)]
fn lower_match(ctx: &mut LoweringContext, expr: &ast::Expr, arms: &[ast::MatchArm]) -> ValueId {
    let _match_val = lower_expr(ctx, expr);
    let merge_block = ctx.create_block("match_merge");

    let mut arm_blocks = Vec::new();
    let mut arm_results = Vec::new();

    // Create blocks for each arm
    for (i, _) in arms.iter().enumerate() {
        arm_blocks.push(ctx.create_block(format!("arm_{}", i)));
    }

    // For simplicity, lower to a series of if-else chains
    // In a real implementation, this would use switch/jump tables
    for (i, arm) in arms.iter().enumerate() {
        let arm_block = arm_blocks[i];
        ctx.set_current_block(arm_block);
        ctx.enter_scope();

        // Bind pattern variables (simplified)
        let arm_val = lower_expr(ctx, &arm.body);
        arm_results.push((arm_block, arm_val));

        ctx.exit_scope();
        ctx.terminate(Terminator::Branch(merge_block));
    }

    // Jump to first arm (simplified - should check patterns)
    ctx.set_current_block(ctx.current_block().unwrap_or(merge_block));
    ctx.terminate(Terminator::Branch(arm_blocks[0]));

    // Merge block
    ctx.set_current_block(merge_block);
    let result = ctx.new_value();

    ctx.emit(Instruction::Phi {
        result,
        incoming: arm_results,
    });

    result
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

    // Bind pattern variable (simplified)
    if let ast::Pattern::Ident { name, .. } = pattern {
        let loop_var = ctx.new_value();
        ctx.bind_variable(name.name.clone(), loop_var, Ty::I64, false);
    }

    lower_expr(ctx, body);
    ctx.exit_scope();
    ctx.terminate(Terminator::Branch(cond_block));

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

    // Get the pointer to store to
    let ptr = match target {
        ast::Expr::Variable(ident) => {
            // Look up the variable's allocation
            ctx.lookup_variable(&ident.name)
                .expect("Variable not found")
        }
        ast::Expr::Path(path) if path.segments.len() == 1 => ctx
            .lookup_variable(&path.segments[0].name)
            .expect("Variable not found"),
        ast::Expr::FieldAccess { object, .. } => lower_expr(ctx, object),
        ast::Expr::Index { object, index } => {
            let obj = lower_expr(ctx, object);
            let idx = lower_expr(ctx, index);
            let ptr = ctx.new_value();
            ctx.emit(Instruction::GetElementPtr {
                result: ptr,
                ptr: obj,
                index: idx,
            });
            ptr
        }
        _ => {
            // Keep lowering resilient: produce a no-op assignment target.
            // Semantic validation should reject invalid targets earlier.
            let tmp = ctx.new_value();
            ctx.emit(Instruction::Alloc {
                result: tmp,
                ty: Ty::I64,
            });
            tmp
        }
    };

    // Handle compound assignment operators
    let final_val = if op != ast::AssignOp::Assign {
        let loaded = ctx.new_value();
        ctx.emit(Instruction::Load {
            result: loaded,
            ptr,
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
        ty: struct_ty.clone(),
    });

    // Allocate space on stack for the struct
    let result = ctx.new_value();
    ctx.emit(Instruction::Alloc {
        result,
        ty: struct_ty,
    });

    // Store the struct value to the allocated space
    ctx.emit(Instruction::Store {
        ptr: result,
        value: struct_val,
    });

    result
}

/// Lowers a self expression.
fn lower_self(ctx: &mut LoweringContext) -> ValueId {
    // Look up 'self' in scope
    if let Some(value) = ctx.lookup_variable("self") {
        value
    } else {
        // Create placeholder
        let result = ctx.new_value();
        let value = ConstantValue::Zero(Ty::Ptr(Box::new(Ty::Void)));
        ctx.emit(Instruction::Const { result, value });
        result
    }
}

/// Lowers a spawn expression.
fn lower_spawn(ctx: &mut LoweringContext, expr: &ast::Expr) -> ValueId {
    let spawned = lower_expr(ctx, expr);
    let result = ctx.new_value();

    // Create spawn instruction
    // In a real implementation, this would create a new task
    ctx.emit(Instruction::Call {
        result,
        func: "spawn".to_string(),
        args: vec![spawned],
    });

    result
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
