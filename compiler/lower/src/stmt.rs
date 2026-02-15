//! Statement lowering from AST to IR.
//!
//! This module provides functions for lowering AST statements to Jet IR instructions.

use crate::context::LoweringContext;
use crate::expr::lower_expr;
use crate::ty::lower_type;
use jet_ir::{Instruction, Terminator, Ty, ValueId};
use jet_parser::ast;

/// Lowers a statement to IR.
pub fn lower_stmt(ctx: &mut LoweringContext, stmt: &ast::Stmt) -> Option<ValueId> {
    match stmt {
        ast::Stmt::Let { pattern, ty, value } => {
            lower_let(ctx, pattern, ty.as_ref(), value);
            None
        }
        ast::Stmt::Expr(expr) => Some(lower_expr(ctx, expr)),
        ast::Stmt::Assign { target, op, value } => {
            lower_assign_stmt(ctx, target, *op, value);
            None
        }
        ast::Stmt::Return(value) => {
            lower_return_stmt(ctx, value.as_deref());
            None
        }
        ast::Stmt::Break { label, value } => {
            lower_break_stmt(ctx, label.as_ref(), value.as_deref());
            None
        }
        ast::Stmt::Continue { label } => {
            lower_continue_stmt(ctx, label.as_ref());
            None
        }
        ast::Stmt::Handle { body, handlers } => {
            // Handle statement - lower to effect handling
            Some(crate::effect::lower_handle(ctx, body, handlers))
        }
    }
}

/// Lowers a let statement.
fn lower_let(
    ctx: &mut LoweringContext,
    pattern: &ast::Pattern,
    ty: Option<&ast::Type>,
    value: &ast::Expr,
) {
    // Lower the initializer expression
    let init_val = lower_expr(ctx, value);

    // Get the type for the variable
    // Priority: explicit type annotation > inferred from expression > default
    let var_ty = if let Some(t) = ty {
        lower_type(t)
    } else {
        // Try to infer from the expression
        infer_expr_type(ctx, value)
    };

    // Allocate space for the variable
    let alloc = ctx.new_value();
    ctx.emit(Instruction::Alloc {
        result: alloc,
        ty: var_ty.clone(),
    });

    // Store the initial value
    ctx.emit(Instruction::Store {
        ptr: alloc,
        value: init_val,
    });

    // Bind the pattern to the allocation
    bind_pattern(ctx, pattern, alloc, var_ty);
}

/// Infers the type of an expression.
fn infer_expr_type(ctx: &mut LoweringContext, expr: &ast::Expr) -> Ty {
    use jet_parser::ast::Expr;

    match expr {
        Expr::Literal(lit) => match lit {
            ast::Literal::Integer(_) => Ty::I64,
            ast::Literal::Float(_) => Ty::F64,
            ast::Literal::Bool(_) => Ty::Bool,
            ast::Literal::String(_) => Ty::Ptr(Box::new(Ty::I8)),
            ast::Literal::Char(_) => Ty::I8,
            ast::Literal::Unit => Ty::Void,
        },
        Expr::StructLiteral { path, fields } => {
            // Get struct name from path
            let struct_name = if path.segments.is_empty() {
                "Unknown".to_string()
            } else {
                path.segments[0].name.clone()
            };

            // Look up struct info to get field types
            if let Some(info) = ctx.lookup_struct(&struct_name) {
                Ty::Struct(info.field_types.clone())
            } else {
                // Infer field types from the field values
                let field_types: Vec<Ty> = fields
                    .iter()
                    .map(|f| {
                        f.value
                            .as_ref()
                            .map(|v| infer_expr_type(ctx, v))
                            .unwrap_or(Ty::I64)
                    })
                    .collect();
                Ty::Struct(field_types)
            }
        }
        Expr::Variable(ident) => {
            // Look up variable type
            ctx.lookup_variable_type(&ident.name)
                .cloned()
                .unwrap_or(Ty::I64)
        }
        _ => Ty::I64, // Default for unhandled expressions
    }
}

/// Binds a pattern to a value in the current scope.
fn bind_pattern(ctx: &mut LoweringContext, pattern: &ast::Pattern, value: ValueId, ty: Ty) {
    match pattern {
        ast::Pattern::Wildcard(_) => {
            // Nothing to bind
        }
        ast::Pattern::Ident { name, mutable } => {
            ctx.bind_variable(name.name.clone(), value, ty, *mutable);
        }
        ast::Pattern::Literal(_) => {
            // Literal patterns don't bind variables
        }
        ast::Pattern::Tuple(patterns) => {
            // Bind each element of the tuple
            for (i, pat) in patterns.iter().enumerate() {
                let elem_val = ctx.new_value();
                ctx.emit(Instruction::ExtractField {
                    result: elem_val,
                    aggregate: value,
                    field_index: i,
                });
                // Type would be extracted from the tuple type
                bind_pattern(ctx, pat, elem_val, Ty::I64);
            }
        }
        ast::Pattern::Struct { fields, .. } => {
            // Bind each field
            for (i, field) in fields.iter().enumerate() {
                let field_val = ctx.new_value();
                ctx.emit(Instruction::ExtractField {
                    result: field_val,
                    aggregate: value,
                    field_index: i,
                });
                if let Some(ref pat) = field.pattern {
                    bind_pattern(ctx, pat, field_val, Ty::I64);
                } else {
                    // Shorthand pattern: bind the field name
                    ctx.bind_variable(field.name.name.clone(), field_val, Ty::I64, false);
                }
            }
        }
        ast::Pattern::Enum { .. } => {
            // Enum pattern matching - simplified
        }
        ast::Pattern::Array(patterns) => {
            // Bind each element of the array
            for (i, pat) in patterns.iter().enumerate() {
                let elem_ptr = ctx.new_value();
                let index_val = ctx.new_value();
                ctx.emit(Instruction::Const {
                    result: index_val,
                    value: jet_ir::ConstantValue::Int(i as i64, Ty::I64),
                });
                ctx.emit(Instruction::GetElementPtr {
                    result: elem_ptr,
                    ptr: value,
                    index: index_val,
                });
                let elem_val = ctx.new_value();
                ctx.emit(Instruction::Load {
                    result: elem_val,
                    ptr: elem_ptr,
                });
                bind_pattern(ctx, pat, elem_val, Ty::I64);
            }
        }
        ast::Pattern::Rest(_) => {
            // Rest pattern - nothing to bind for now
        }
        ast::Pattern::Or(left, right) => {
            // Or pattern - bind both sides
            bind_pattern(ctx, left, value, ty.clone());
            bind_pattern(ctx, right, value, ty);
        }
        ast::Pattern::Bind { name, pattern } => {
            // Bind the name to the value, then bind the inner pattern
            ctx.bind_variable(name.name.clone(), value, ty.clone(), false);
            bind_pattern(ctx, pattern, value, ty);
        }
        ast::Pattern::Mut(inner) => {
            // Mutable pattern wrapper - mark as mutable
            if let ast::Pattern::Ident { name, .. } = inner.as_ref() {
                ctx.bind_variable(name.name.clone(), value, ty, true);
            } else {
                bind_pattern(ctx, inner, value, ty);
            }
        }
        ast::Pattern::Ref { pattern, .. } => {
            // Reference pattern - the value is already a pointer
            bind_pattern(ctx, pattern, value, Ty::Ptr(Box::new(ty)));
        }
    }
}

/// Gets the name from a simple identifier pattern.
fn get_pattern_name(pattern: &ast::Pattern) -> &str {
    match pattern {
        ast::Pattern::Ident { name, .. } => &name.name,
        _ => "_",
    }
}

/// Lowers an assignment statement.
fn lower_assign_stmt(
    ctx: &mut LoweringContext,
    target: &ast::Expr,
    op: ast::AssignOp,
    value: &ast::Expr,
) {
    use jet_ir::BinaryOp;

    let val = lower_expr(ctx, value);

    // Get the pointer to store to
    let ptr = match target {
        ast::Expr::Variable(ident) => {
            // Look up the variable's allocation
            // In a real implementation, we'd track the allocation separately from the loaded value
            ctx.lookup_variable(&ident.name)
                .expect("Variable not found")
        }
        ast::Expr::Path(path) if path.segments.len() == 1 => ctx
            .lookup_variable(&path.segments[0].name)
            .expect("Variable not found"),
        ast::Expr::FieldAccess { object, field: _ } => {
            let obj = lower_expr(ctx, object);
            // Get field pointer
            let field_ptr = ctx.new_value();
            // Field index would be looked up from the struct definition
            let field_index = 0; // Simplified
            ctx.emit(Instruction::GetFieldPtr {
                result: field_ptr,
                ptr: obj,
                field_index,
            });
            field_ptr
        }
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
            // Keep lowering resilient: use a temporary sink slot for invalid targets.
            // Frontend validation is expected to catch these before codegen.
            let sink = ctx.new_value();
            ctx.emit(Instruction::Alloc {
                result: sink,
                ty: Ty::I64,
            });
            sink
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
            ast::AssignOp::AddAssign => BinaryOp::Add,
            ast::AssignOp::SubAssign => BinaryOp::Sub,
            ast::AssignOp::MulAssign => BinaryOp::Mul,
            ast::AssignOp::DivAssign => BinaryOp::Div,
            ast::AssignOp::ModAssign => BinaryOp::Rem,
            ast::AssignOp::BitAndAssign => BinaryOp::And,
            ast::AssignOp::BitOrAssign => BinaryOp::Or,
            ast::AssignOp::BitXorAssign => BinaryOp::Xor,
            ast::AssignOp::ShlAssign => BinaryOp::Shl,
            ast::AssignOp::ShrAssign => BinaryOp::Shr,
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
}

/// Lowers a return statement.
fn lower_return_stmt(ctx: &mut LoweringContext, value: Option<&ast::Expr>) {
    let ret_val = value.map(|v| lower_expr(ctx, v));
    ctx.terminate(Terminator::Return(ret_val));
}

/// Lowers a break statement.
fn lower_break_stmt(
    ctx: &mut LoweringContext,
    label: Option<&ast::Ident>,
    value: Option<&ast::Expr>,
) {
    let label_str = label.map(|l| l.name.as_str());
    let target = ctx
        .find_loop_target(label_str)
        .expect("No matching loop for break")
        .clone();

    let _break_val = value.map(|v| lower_expr(ctx, v));

    ctx.terminate(Terminator::Branch(target.break_block));
}

/// Lowers a continue statement.
fn lower_continue_stmt(ctx: &mut LoweringContext, label: Option<&ast::Ident>) {
    let label_str = label.map(|l| l.name.as_str());
    let target = ctx
        .find_loop_target(label_str)
        .expect("No matching loop for continue")
        .clone();

    ctx.terminate(Terminator::Branch(target.continue_block));
}

/// Lowers a block expression/statement.
pub fn lower_block(ctx: &mut LoweringContext, block: &ast::Block) -> ValueId {
    ctx.enter_scope();

    let mut last_value = None;

    for stmt in &block.stmts {
        last_value = lower_stmt(ctx, stmt);
        // If the block has been terminated, stop processing statements
        if let Some(current) = ctx.get_current_block() {
            if current.is_terminated() {
                break;
            }
        }
    }

    // If there's a trailing expression, use its value
    let result = if let Some(expr) = &block.expr {
        lower_expr(ctx, expr)
    } else {
        last_value.unwrap_or_else(|| {
            // Return unit if no value
            let unit = ctx.new_value();
            ctx.emit(Instruction::Const {
                result: unit,
                value: jet_ir::ConstantValue::Zero(Ty::Void),
            });
            unit
        })
    };

    ctx.exit_scope();
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_ir::Ty;
    use jet_lexer::Span;
    use jet_parser::ast::{Ident, Literal, Pattern};

    fn make_ident(name: &str) -> Ident {
        Ident::new(name, Span::new(0, 0))
    }

    #[test]
    fn test_lower_let_simple() {
        let mut ctx = LoweringContext::new("test");

        // Add a function first
        let func = jet_ir::Function::new("test_func", vec![], Ty::Void);
        ctx.module.add_function(func);
        ctx.start_function(0);

        let entry = ctx.create_block("entry");
        ctx.set_current_block(entry);

        // let x = 42
        let stmt = ast::Stmt::Let {
            pattern: Pattern::Ident {
                mutable: false,
                name: make_ident("x"),
            },
            ty: None,
            value: Box::new(ast::Expr::Literal(Literal::Integer(42))),
        };

        lower_stmt(&mut ctx, &stmt);

        let block = ctx.get_current_block().unwrap();
        // Should have: alloc, const 42, store
        assert_eq!(block.instructions.len(), 3);
    }

    #[test]
    fn test_lower_return() {
        let mut ctx = LoweringContext::new("test");

        let func = jet_ir::Function::new("test_func", vec![], Ty::I32);
        ctx.module.add_function(func);
        ctx.start_function(0);

        let entry = ctx.create_block("entry");
        ctx.set_current_block(entry);

        // return 42
        let stmt = ast::Stmt::Return(Some(Box::new(ast::Expr::Literal(Literal::Integer(42)))));

        lower_stmt(&mut ctx, &stmt);

        let block = ctx.get_current_block().unwrap();
        assert!(block.is_terminated());
        assert!(matches!(block.terminator, Terminator::Return { .. }));
    }
}
