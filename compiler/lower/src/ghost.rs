//! Ghost code erasure for the Jet compiler
//!
//! This module handles the erasure of ghost code during lowering.
//! Ghost types and ghost code blocks exist only for verification purposes
//! and are completely erased during code generation.
//!
//! # Ghost Types
//!
//! Ghost types are marked with the `ghost` keyword:
//! ```jet
//! fn example(x: int, ghost proof: Proof) -> int
//!     requires x > 0
//!     ensures result > x
//! {
//!     // proof is a ghost parameter, erased at runtime
//!     x + 1
//! }
//! ```
//!
//! # Ghost Code Blocks
//!
//! Ghost code blocks contain verification-only logic:
//! ```jet
//! fn verify() {
//!     ghost {
//!         // This entire block is erased at runtime
//!         assert some_property;
//!         let ghost_var: ghost int = compute_ghost_value();
//!     }
//! }
//! ```

use crate::context::LoweringContext;
use jet_ir::{Instruction, Ty, ValueId};
use jet_typeck::{TypedExpr, TypedExprKind, TypedStmt};

/// Check if a type is a ghost type
pub fn is_ghost_type(_ty: Ty, _tcx: &jet_typeck::TypeContext) -> bool {
    // Check if the underlying typeck type is ghost
    // This requires access to the original typeck type
    // For now, we return false - ghost types are handled at the typeck level
    false
}

/// Erase ghost expressions from the typed AST
///
/// This returns `None` if the entire expression should be erased,
/// or `Some(expr)` with ghost sub-expressions removed.
pub fn erase_ghost_expr(expr: &TypedExpr) -> Option<TypedExpr> {
    // Check if this is a ghost-typed expression
    // For now, we check based on expression kind
    match &expr.kind {
        // Ghost variable references are erased
        TypedExprKind::Variable(_) => {
            // In a full implementation, we'd check if the variable has ghost type
            Some(expr.clone())
        }

        // Ghost blocks are completely erased
        TypedExprKind::Block(block) => {
            // Check if this is a ghost block
            // For now, we filter out ghost statements
            let filtered_stmts: Vec<_> = block.stmts.iter().filter_map(erase_ghost_stmt).collect();

            let filtered_expr = block.expr.as_ref().and_then(|e| erase_ghost_expr(e));

            if filtered_stmts.is_empty() && filtered_expr.is_none() {
                // Entire block is ghost, erase it
                None
            } else {
                Some(TypedExpr {
                    kind: TypedExprKind::Block(jet_typeck::TypedBlock {
                        stmts: filtered_stmts,
                        expr: filtered_expr.map(Box::new),
                        ty: block.ty,
                        span: block.span,
                    }),
                    ty: expr.ty,
                    span: expr.span,
                })
            }
        }

        // For other expressions, recursively erase ghost sub-expressions
        TypedExprKind::Binary { op, left, right } => {
            let left_erased = erase_ghost_expr(left)?;
            let right_erased = erase_ghost_expr(right)?;

            Some(TypedExpr {
                kind: TypedExprKind::Binary {
                    op: *op,
                    left: Box::new(left_erased),
                    right: Box::new(right_erased),
                },
                ty: expr.ty,
                span: expr.span,
            })
        }

        TypedExprKind::Unary { op, expr: inner } => {
            let inner_erased = erase_ghost_expr(inner)?;

            Some(TypedExpr {
                kind: TypedExprKind::Unary {
                    op: *op,
                    expr: Box::new(inner_erased),
                },
                ty: expr.ty,
                span: expr.span,
            })
        }

        TypedExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_erased = erase_ghost_expr(cond)?;
            let then_erased = erase_ghost_expr(then_branch)?;
            let else_erased = else_branch.as_ref().and_then(|e| erase_ghost_expr(e));

            Some(TypedExpr {
                kind: TypedExprKind::If {
                    cond: Box::new(cond_erased),
                    then_branch: Box::new(then_erased),
                    else_branch: else_erased.map(Box::new),
                },
                ty: expr.ty,
                span: expr.span,
            })
        }

        TypedExprKind::Call { func, args } => {
            // Filter out ghost arguments
            let func_erased = erase_ghost_expr(func)?;
            let args_erased: Vec<_> = args.iter().filter_map(erase_ghost_expr).collect();

            Some(TypedExpr {
                kind: TypedExprKind::Call {
                    func: Box::new(func_erased),
                    args: args_erased,
                },
                ty: expr.ty,
                span: expr.span,
            })
        }

        TypedExprKind::Tuple(elements) => {
            let elements_erased: Vec<_> = elements.iter().filter_map(erase_ghost_expr).collect();

            Some(TypedExpr {
                kind: TypedExprKind::Tuple(elements_erased),
                ty: expr.ty,
                span: expr.span,
            })
        }

        TypedExprKind::Array(elements) => {
            let elements_erased: Vec<_> = elements.iter().filter_map(erase_ghost_expr).collect();

            Some(TypedExpr {
                kind: TypedExprKind::Array(elements_erased),
                ty: expr.ty,
                span: expr.span,
            })
        }

        // Literals and simple expressions are preserved
        TypedExprKind::Literal(_)
        | TypedExprKind::Variable(_)
        | TypedExprKind::Hole(_)
        | TypedExprKind::Continue => Some(expr.clone()),

        // Return statements with ghost values become unit returns
        TypedExprKind::Return(ret_expr) => {
            let ret_erased = ret_expr.as_ref().and_then(|e| erase_ghost_expr(e));

            Some(TypedExpr {
                kind: TypedExprKind::Return(ret_erased.map(Box::new)),
                ty: expr.ty,
                span: expr.span,
            })
        }

        // Other expressions - for now, pass through
        // In a full implementation, we'd handle all cases
        _ => Some(expr.clone()),
    }
}

/// Erase ghost statements
fn erase_ghost_stmt(stmt: &TypedStmt) -> Option<TypedStmt> {
    match stmt {
        TypedStmt::Let { pattern, ty, value } => {
            // Check if this is a ghost variable binding
            // For now, we erase based on the value being ghost
            let value_erased = erase_ghost_expr(value)?;

            Some(TypedStmt::Let {
                pattern: pattern.clone(),
                ty: *ty,
                value: value_erased,
            })
        }

        TypedStmt::Expr(expr) => erase_ghost_expr(expr).map(TypedStmt::Expr),

        TypedStmt::Return(ret_expr) => {
            let ret_erased = ret_expr.as_ref().and_then(|e| erase_ghost_expr(e));
            Some(TypedStmt::Return(ret_erased))
        }

        // Other statements pass through
        _ => Some(stmt.clone()),
    }
}

/// Filter ghost parameters from a function
pub fn filter_ghost_params(
    params: &[jet_typeck::TypedParam],
    tcx: &jet_typeck::TypeContext,
) -> Vec<jet_typeck::TypedParam> {
    params
        .iter()
        .filter(|p| !tcx.is_ghost(p.ty))
        .cloned()
        .collect()
}

/// Erase contract-related code from a function body
///
/// This removes:
/// - `requires` checks (already verified at call sites)
/// - `ensures` assertions (already verified at return points)
/// - `invariant` checks (already verified at loop boundaries)
/// - Ghost variable declarations and usages
pub fn erase_contracts_from_body(body: &TypedExpr) -> TypedExpr {
    // For now, just erase ghost expressions
    erase_ghost_expr(body).unwrap_or_else(|| TypedExpr {
        kind: TypedExprKind::Literal(jet_parser::ast::Literal::Unit),
        ty: jet_typeck::TypeId::UNIT,
        span: jet_diagnostics::Span::default(),
    })
}

/// Check if an instruction should be erased (is ghost code)
pub fn is_ghost_instruction(_instr: &Instruction) -> bool {
    // Ghost instructions would be marked in a full implementation
    // For now, we return false - ghost code is handled at the AST level
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_diagnostics::Span;
    use jet_lexer::Span as LexerSpan;
    use jet_parser::ast::{Ident, Literal, Pattern};

    fn make_simple_expr(kind: TypedExprKind, ty: jet_typeck::TypeId) -> TypedExpr {
        TypedExpr {
            kind,
            ty,
            span: Span::default(),
        }
    }

    #[test]
    fn test_erase_ghost_block() {
        // Create a simple block with ghost and non-ghost statements
        let block = jet_typeck::TypedBlock {
            stmts: vec![
                // This would be a ghost statement in a full implementation
                TypedStmt::Expr(make_simple_expr(
                    TypedExprKind::Literal(Literal::Integer(1)),
                    jet_typeck::TypeId::INT,
                )),
            ],
            expr: Some(Box::new(make_simple_expr(
                TypedExprKind::Literal(Literal::Integer(2)),
                jet_typeck::TypeId::INT,
            ))),
            ty: jet_typeck::TypeId::INT,
            span: Span::default(),
        };

        let expr = TypedExpr {
            kind: TypedExprKind::Block(block),
            ty: jet_typeck::TypeId::INT,
            span: Span::default(),
        };

        // The block should not be erased (contains non-ghost code)
        let result = erase_ghost_expr(&expr);
        assert!(result.is_some());
    }

    #[test]
    fn test_filter_ghost_params() {
        let tcx = jet_typeck::TypeContext::new();

        let params = vec![
            jet_typeck::TypedParam {
                pattern: Pattern::Ident {
                    mutable: false,
                    name: Ident::new("x", LexerSpan::default()),
                },
                ty: jet_typeck::TypeId::INT,
            },
            jet_typeck::TypedParam {
                pattern: Pattern::Ident {
                    mutable: false,
                    name: Ident::new("ghost_proof", LexerSpan::default()),
                },
                ty: jet_typeck::TypeId::INT, // Would be ghost type in full impl
            },
        ];

        let filtered = filter_ghost_params(&params, &tcx);
        assert_eq!(filtered.len(), 2); // Both pass through since we don't have ghost types yet
    }
}
