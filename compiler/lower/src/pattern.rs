//! Pattern matching compilation using decision trees.
//!
//! This module implements pattern matching compilation based on the
//! algorithm described in "The Implementation of Functional Programming Languages"
//! by Simon Peyton Jones.
//!
//! The key insight is to compile pattern matching into a decision tree
//! that minimizes redundant tests and produces efficient code.

use crate::context::LoweringContext;
use crate::expr::{infer_expr_type, lower_expr};
use jet_ir::{BlockId, ConstantValue, Instruction, Terminator, Ty, ValueId};
use jet_parser::ast;

/// A decision tree node for pattern matching.
#[derive(Debug, Clone)]
pub enum DecisionTree {
    /// Leaf node - execute the corresponding arm body.
    Leaf(usize), // arm index
    /// Switch on a constructor, with cases for each variant.
    Switch {
        /// The value being scrutinized.
        value: ValueId,
        /// The type of the value.
        ty: Ty,
        /// Cases for each constructor.
        cases: Vec<(Constructor, DecisionTree)>,
        /// Default case if no constructor matches.
        default: Option<Box<DecisionTree>>,
    },
    /// Bind a variable and continue.
    Bind {
        /// Variable name.
        name: String,
        /// Whether the binding is mutable.
        mutable: bool,
        /// The value to bind.
        value: ValueId,
        /// Continue with this subtree.
        continuation: Box<DecisionTree>,
    },
    /// Test a guard condition.
    Guard {
        /// The guard expression (as AST for now).
        guard: ast::Expr,
        /// If guard is true, take this path.
        then_branch: Box<DecisionTree>,
        /// If guard is false, take this path.
        else_branch: Box<DecisionTree>,
    },
}

/// A constructor for pattern matching.
#[derive(Debug, Clone, PartialEq)]
pub enum Constructor {
    /// Literal value.
    Literal(ast::Literal),
    /// Tuple constructor with arity.
    Tuple(usize),
    /// Struct constructor with field count.
    Struct(String, usize),
    /// Enum variant.
    Variant(String, String), // type name, variant name
    /// Array constructor with length.
    Array(usize),
    /// Wildcard - matches anything.
    Wildcard,
}

/// A pattern matrix for compilation.
///
/// Rows are patterns, columns are positions being matched.
#[derive(Debug, Clone)]
pub struct PatternMatrix {
    /// The patterns for each row.
    pub rows: Vec<PatternRow>,
}

/// A row in the pattern matrix.
#[derive(Debug, Clone)]
pub struct PatternRow {
    /// The patterns for this row (one per column).
    pub patterns: Vec<ast::Pattern>,
    /// The arm index this row corresponds to.
    pub arm_index: usize,
    /// Whether this row has a guard.
    pub has_guard: bool,
}

/// Compiles a match expression to IR using decision trees.
pub fn compile_match(
    ctx: &mut LoweringContext,
    scrutinee: ValueId,
    scrutinee_ty: Ty,
    arms: &[ast::MatchArm],
) -> ValueId {
    // Build pattern matrix
    let matrix = build_pattern_matrix(arms);

    // Compile decision tree
    let tree = compile_matrix(&matrix, 0);

    // Generate code from decision tree
    let merge_block = ctx.create_block("match_merge");
    let result = ctx.new_value();
    let mut arm_results: Vec<(BlockId, ValueId)> = Vec::new();

    // Generate code for the decision tree
    let start_block = ctx
        .current_block()
        .unwrap_or_else(|| ctx.create_block("match_start"));
    ctx.set_current_block(start_block);

    generate_decision_tree_code(
        ctx,
        &tree,
        scrutinee,
        &scrutinee_ty,
        merge_block,
        &mut arm_results,
        arms,
    );

    // Merge block with phi
    ctx.set_current_block(merge_block);

    if arm_results.is_empty() {
        // No arms - return unit (should be unreachable)
        lower_unit(ctx)
    } else {
        // Get type from first arm result
        let ty = infer_expr_type(ctx, &arms[0].body);

        ctx.emit(Instruction::Phi {
            result,
            incoming: arm_results,
            ty,
        });
        result
    }
}

/// Builds a pattern matrix from match arms.
fn build_pattern_matrix(arms: &[ast::MatchArm]) -> PatternMatrix {
    let rows: Vec<PatternRow> = arms
        .iter()
        .enumerate()
        .map(|(i, arm)| PatternRow {
            patterns: vec![arm.pattern.clone()],
            arm_index: i,
            has_guard: arm.guard.is_some(),
        })
        .collect();

    PatternMatrix { rows }
}

/// Compiles a pattern matrix to a decision tree.
fn compile_matrix(matrix: &PatternMatrix, column: usize) -> DecisionTree {
    if matrix.rows.is_empty() {
        // No rows - this is a match failure (shouldn't happen with exhaustive checking)
        panic!("Non-exhaustive pattern match");
    }

    // Check if first row has a wildcard pattern at this column
    if let Some(first_row) = matrix.rows.first() {
        if let Some(pat) = first_row.patterns.get(column) {
            if is_wildcard_pattern(pat) {
                // This row matches - if it has a guard, we need to check it
                if first_row.has_guard {
                    // For now, simplify and just return the leaf
                    // In full implementation, generate guard test
                    return DecisionTree::Leaf(first_row.arm_index);
                }
                return DecisionTree::Leaf(first_row.arm_index);
            }
        }
    }

    // Need to do a constructor test
    // Group rows by constructor at this column
    // Use Vec instead of HashMap since Constructor contains Literal which doesn't implement Hash
    let mut constructor_groups: Vec<(Constructor, Vec<PatternRow>)> = Vec::new();

    for row in &matrix.rows {
        if let Some(pat) = row.patterns.get(column) {
            let ctor = pattern_to_constructor(pat);
            // Check if we already have this constructor
            if let Some(pos) = constructor_groups
                .iter()
                .position(|(c, _)| constructors_equal(c, &ctor))
            {
                constructor_groups[pos].1.push(row.clone());
            } else {
                constructor_groups.push((ctor, vec![row.clone()]));
            }
        }
    }

    // Build switch cases
    let cases: Vec<(Constructor, DecisionTree)> = constructor_groups
        .into_iter()
        .map(|(ctor, rows)| {
            let sub_matrix = PatternMatrix { rows };
            let subtree = compile_matrix(&sub_matrix, column + 1);
            (ctor, subtree)
        })
        .collect();

    DecisionTree::Switch {
        value: ValueId::new(0), // Placeholder - will be set during code generation
        ty: Ty::I64,            // Placeholder
        cases,
        default: None,
    }
}

/// Checks if a pattern is a wildcard (matches anything).
fn is_wildcard_pattern(pat: &ast::Pattern) -> bool {
    matches!(pat, ast::Pattern::Wildcard(_))
}

/// Compares two constructors for equality.
///
/// This is used when grouping patterns by constructor during decision tree compilation.
fn constructors_equal(a: &Constructor, b: &Constructor) -> bool {
    match (a, b) {
        (Constructor::Literal(lit_a), Constructor::Literal(lit_b)) => lit_a == lit_b,
        (Constructor::Tuple(arity_a), Constructor::Tuple(arity_b)) => arity_a == arity_b,
        (Constructor::Struct(name_a, arity_a), Constructor::Struct(name_b, arity_b)) => {
            name_a == name_b && arity_a == arity_b
        }
        (Constructor::Variant(type_a, var_a), Constructor::Variant(type_b, var_b)) => {
            type_a == type_b && var_a == var_b
        }
        (Constructor::Array(len_a), Constructor::Array(len_b)) => len_a == len_b,
        (Constructor::Wildcard, Constructor::Wildcard) => true,
        _ => false,
    }
}

/// Converts a pattern to its constructor.
fn pattern_to_constructor(pat: &ast::Pattern) -> Constructor {
    match pat {
        ast::Pattern::Wildcard(_) => Constructor::Wildcard,
        ast::Pattern::Literal(lit) => Constructor::Literal(lit.clone()),
        ast::Pattern::Tuple(pats) => Constructor::Tuple(pats.len()),
        ast::Pattern::Struct { path, fields, .. } => {
            let name = path
                .segments
                .last()
                .map(|s| s.name.clone())
                .unwrap_or_default();
            Constructor::Struct(name, fields.len())
        }
        ast::Pattern::Enum { path, variant, .. } => {
            let type_name = path
                .segments
                .last()
                .map(|s| s.name.clone())
                .unwrap_or_default();
            Constructor::Variant(type_name, variant.name.clone())
        }
        ast::Pattern::Array(pats) => Constructor::Array(pats.len()),
        ast::Pattern::Ident { .. } => Constructor::Wildcard,
        ast::Pattern::Rest(_) => Constructor::Wildcard,
        ast::Pattern::Or(left, _) => pattern_to_constructor(left),
        ast::Pattern::Bind { pattern, .. } => pattern_to_constructor(pattern),
        ast::Pattern::Mut(inner) => pattern_to_constructor(inner),
        ast::Pattern::Ref { pattern, .. } => pattern_to_constructor(pattern),
    }
}

/// Generates IR code from a decision tree.
fn generate_decision_tree_code(
    ctx: &mut LoweringContext,
    tree: &DecisionTree,
    scrutinee: ValueId,
    scrutinee_ty: &Ty,
    merge_block: BlockId,
    arm_results: &mut Vec<(BlockId, ValueId)>,
    arms: &[ast::MatchArm],
) {
    match tree {
        DecisionTree::Leaf(arm_index) => {
            // Execute the arm body
            let arm = &arms[*arm_index];
            let arm_block = ctx.current_block().unwrap();

            ctx.enter_scope();

            // Bind pattern variables
            bind_match_pattern(ctx, &arm.pattern, scrutinee, scrutinee_ty);

            // Lower guard if present
            let should_execute_body = if let Some(ref guard) = arm.guard {
                let guard_val = lower_expr(ctx, guard);

                let guard_then = ctx.create_block("guard_then");
                let guard_else = ctx.create_block("guard_else");

                ctx.terminate(Terminator::CondBranch {
                    cond: guard_val,
                    then_block: guard_then,
                    else_block: guard_else,
                });

                // Guard failed - try next arm (simplified: just go to merge)
                ctx.set_current_block(guard_else);
                ctx.terminate(Terminator::Branch(merge_block));

                ctx.set_current_block(guard_then);
                true
            } else {
                true
            };

            if should_execute_body {
                let body_val = lower_expr(ctx, &arm.body);
                arm_results.push((arm_block, body_val));

                if !ctx.get_current_block().unwrap().is_terminated() {
                    ctx.terminate(Terminator::Branch(merge_block));
                }
            }

            ctx.exit_scope();
        }
        DecisionTree::Switch { cases, .. } => {
            // Generate switch on constructor
            // For now, generate a series of if-else tests

            let mut test_blocks = Vec::new();
            for (i, _) in cases.iter().enumerate() {
                test_blocks.push(ctx.create_block(format!("test_{}", i)));
            }

            // Branch to first test
            if let Some(first_test) = test_blocks.first() {
                if !ctx.get_current_block().unwrap().is_terminated() {
                    ctx.terminate(Terminator::Branch(*first_test));
                }
            }

            // Generate each test
            for (i, (ctor, subtree)) in cases.iter().enumerate() {
                ctx.set_current_block(test_blocks[i]);

                // Test if scrutinee matches this constructor
                let matches = test_constructor(ctx, scrutinee, scrutinee_ty, ctor);

                let then_block = ctx.create_block(format!("match_then_{}", i));
                let else_block = if i + 1 < test_blocks.len() {
                    test_blocks[i + 1]
                } else {
                    merge_block
                };

                ctx.terminate(Terminator::CondBranch {
                    cond: matches,
                    then_block,
                    else_block,
                });

                // Generate code for matching case
                ctx.set_current_block(then_block);
                generate_decision_tree_code(
                    ctx,
                    subtree,
                    scrutinee,
                    scrutinee_ty,
                    merge_block,
                    arm_results,
                    arms,
                );
            }
        }
        DecisionTree::Bind {
            name,
            mutable,
            value,
            continuation,
        } => {
            // Bind the variable
            ctx.bind_variable(name.clone(), *value, scrutinee_ty.clone(), *mutable);

            // Continue with subtree
            generate_decision_tree_code(
                ctx,
                continuation,
                scrutinee,
                scrutinee_ty,
                merge_block,
                arm_results,
                arms,
            );
        }
        DecisionTree::Guard { .. } => {
            // Guard handling is done inline in the Leaf arm
            // This arm should not be reached if the decision tree is properly constructed
            // We generate a simple branch to the merge block as a fallback
            ctx.terminate(Terminator::Branch(merge_block));
        }
    }
}

/// Tests if a value matches a constructor.
fn test_constructor(
    ctx: &mut LoweringContext,
    value: ValueId,
    ty: &Ty,
    ctor: &Constructor,
) -> ValueId {
    match ctor {
        Constructor::Literal(lit) => {
            // Compare value with literal
            let lit_val = lower_literal_const(ctx, lit);
            let result = ctx.new_value();
            ctx.emit(Instruction::Binary {
                result,
                op: jet_ir::BinaryOp::Eq,
                lhs: value,
                rhs: lit_val,
            });
            result
        }
        Constructor::Variant(_type_name, variant_name) => {
            // Load discriminant and compare
            let discr_ptr = ctx.new_value();
            ctx.emit(Instruction::GetFieldPtr {
                result: discr_ptr,
                ptr: value,
                field_index: 0,
                struct_ty: ty.clone(),
            });

            let discr = ctx.new_value();
            ctx.emit(Instruction::Load {
                result: discr,
                ptr: discr_ptr,
                ty: Ty::I64,
            });

            // For now, use a hash of the variant name as discriminant value
            let expected_discr = variant_name.len() as i64;
            let expected_val = ctx.new_value();
            ctx.emit(Instruction::Const {
                result: expected_val,
                value: ConstantValue::Int(expected_discr, Ty::I64),
            });

            let result = ctx.new_value();
            ctx.emit(Instruction::Binary {
                result,
                op: jet_ir::BinaryOp::Eq,
                lhs: discr,
                rhs: expected_val,
            });
            result
        }
        _ => {
            // For other constructors, assume match (more specific tests later)
            let result = ctx.new_value();
            ctx.emit(Instruction::Const {
                result,
                value: ConstantValue::Bool(true),
            });
            result
        }
    }
}

/// Lowers a literal to a constant value.
fn lower_literal_const(ctx: &mut LoweringContext, lit: &ast::Literal) -> ValueId {
    let (value, _ty) = match lit {
        ast::Literal::Integer(n) => (ConstantValue::Int(*n, Ty::I64), Ty::I64),
        ast::Literal::Float(f) => (ConstantValue::Float(*f, Ty::F64), Ty::F64),
        ast::Literal::Bool(b) => (ConstantValue::Bool(*b), Ty::Bool),
        ast::Literal::String(s) => (ConstantValue::String(s.clone()), Ty::Ptr(Box::new(Ty::I8))),
        ast::Literal::Char(c) => (ConstantValue::Int(*c as i64, Ty::I32), Ty::I32),
        ast::Literal::Unit => return lower_unit(ctx),
    };

    let result = ctx.new_value();
    ctx.emit(Instruction::Const { result, value });
    result
}

/// Binds a pattern to a value during match.
pub fn bind_match_pattern(ctx: &mut LoweringContext, pattern: &ast::Pattern, value: ValueId, ty: &Ty) {
    match pattern {
        ast::Pattern::Wildcard(_) => {
            // Nothing to bind
        }
        ast::Pattern::Ident { name, mutable } => {
            ctx.bind_variable(name.name.clone(), value, ty.clone(), *mutable);
        }
        ast::Pattern::Literal(_) => {
            // Literal patterns don't bind
        }
        ast::Pattern::Tuple(patterns) => {
            for (i, pat) in patterns.iter().enumerate() {
                let elem_val = ctx.new_value();
                ctx.emit(Instruction::ExtractField {
                    result: elem_val,
                    aggregate: value,
                    field_index: i,
                });
                bind_match_pattern(ctx, pat, elem_val, ty);
            }
        }
        ast::Pattern::Struct { fields, .. } => {
            for (i, field) in fields.iter().enumerate() {
                let field_val = ctx.new_value();
                ctx.emit(Instruction::ExtractField {
                    result: field_val,
                    aggregate: value,
                    field_index: i,
                });
                if let Some(ref pat) = field.pattern {
                    bind_match_pattern(ctx, pat, field_val, ty);
                } else {
                    ctx.bind_variable(field.name.name.clone(), field_val, ty.clone(), false);
                }
            }
        }
        ast::Pattern::Enum {
            variant: _, inner, ..
        } => {
            // Skip discriminant (field 0), extract payload
            if let Some(inner_pat) = inner {
                let payload = ctx.new_value();
                ctx.emit(Instruction::ExtractField {
                    result: payload,
                    aggregate: value,
                    field_index: 1,
                });
                bind_match_pattern(ctx, inner_pat, payload, ty);
            }
        }
        ast::Pattern::Array(patterns) => {
            // Extract element type
            let elem_ty = if let Ty::Array(elem, _) = ty {
                *elem.clone()
            } else {
                Ty::I64 // Fallback
            };

            for (i, pat) in patterns.iter().enumerate() {
                let elem_ptr = ctx.new_value();
                let index_val = ctx.new_value();
                ctx.emit(Instruction::Const {
                    result: index_val,
                    value: ConstantValue::Int(i as i64, Ty::I64),
                });
                ctx.emit(Instruction::GetElementPtr {
                    result: elem_ptr,
                    ptr: value,
                    index: index_val,
                    elem_ty: elem_ty.clone(),
                });
                let elem_val = ctx.new_value();
                ctx.emit(Instruction::Load {
                    result: elem_val,
                    ptr: elem_ptr,
                    ty: elem_ty.clone(),
                });
                bind_match_pattern(ctx, pat, elem_val, ty);
            }
        }
        ast::Pattern::Rest(_) => {
            // Nothing to bind
        }
        ast::Pattern::Or(left, _) => {
            // Bind the first pattern (they should bind the same variables)
            bind_match_pattern(ctx, left, value, ty);
        }
        ast::Pattern::Bind { name, pattern } => {
            ctx.bind_variable(name.name.clone(), value, ty.clone(), false);
            bind_match_pattern(ctx, pattern, value, ty);
        }
        ast::Pattern::Mut(inner) => {
            if let ast::Pattern::Ident { name, .. } = inner.as_ref() {
                ctx.bind_variable(name.name.clone(), value, ty.clone(), true);
            } else {
                bind_match_pattern(ctx, inner, value, ty);
            }
        }
        ast::Pattern::Ref { pattern, .. } => {
            bind_match_pattern(ctx, pattern, value, &Ty::Ptr(Box::new(ty.clone())));
        }
    }
}

/// Lowers a unit value.
fn lower_unit(ctx: &mut LoweringContext) -> ValueId {
    let result = ctx.new_value();
    let value = ConstantValue::Zero(Ty::Void);
    ctx.emit(Instruction::Const { result, value });
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_ir::{Function, Ty};
    use jet_lexer::Span;
    use jet_parser::ast::{Literal, Pattern};

    #[test]
    fn test_pattern_to_constructor() {
        let wildcard = Pattern::Wildcard(Span::new(0, 0));
        assert_eq!(pattern_to_constructor(&wildcard), Constructor::Wildcard);

        let lit = Pattern::Literal(Literal::Integer(42));
        assert_eq!(
            pattern_to_constructor(&lit),
            Constructor::Literal(Literal::Integer(42))
        );

        let tuple = Pattern::Tuple(vec![
            Pattern::Wildcard(Span::new(0, 0)),
            Pattern::Wildcard(Span::new(0, 0)),
        ]);
        assert_eq!(pattern_to_constructor(&tuple), Constructor::Tuple(2));
    }

    #[test]
    fn test_build_pattern_matrix() {
        let arms = vec![
            ast::MatchArm {
                pattern: Pattern::Literal(Literal::Integer(1)),
                guard: None,
                body: Box::new(ast::Expr::Literal(Literal::Integer(10))),
            },
            ast::MatchArm {
                pattern: Pattern::Wildcard(Span::new(0, 0)),
                guard: None,
                body: Box::new(ast::Expr::Literal(Literal::Integer(20))),
            },
        ];

        let matrix = build_pattern_matrix(&arms);
        assert_eq!(matrix.rows.len(), 2);
        assert_eq!(matrix.rows[0].arm_index, 0);
        assert_eq!(matrix.rows[1].arm_index, 1);
    }

    #[test]
    fn test_compile_simple_match() {
        let tcx = jet_typeck::TypeContext::new();
        let mut ctx = LoweringContext::new("test", &tcx);

        // Add a function
        let func = Function::new("test_func", vec![], Ty::I32);
        ctx.module.add_function(func);
        ctx.start_function(0);

        let entry = ctx.create_block("entry");
        ctx.set_current_block(entry);

        let arms = vec![
            ast::MatchArm {
                pattern: Pattern::Literal(Literal::Integer(1)),
                guard: None,
                body: Box::new(ast::Expr::Literal(Literal::Integer(10))),
            },
            ast::MatchArm {
                pattern: Pattern::Wildcard(Span::new(0, 0)),
                guard: None,
                body: Box::new(ast::Expr::Literal(Literal::Integer(20))),
            },
        ];

        let scrutinee = ctx.new_value();
        ctx.emit(Instruction::Const {
            result: scrutinee,
            value: ConstantValue::Int(1, Ty::I64),
        });

        let _result = compile_match(&mut ctx, scrutinee, Ty::I64, &arms);

        // Check that we created the expected blocks
        let func = &ctx.module.functions[0];
        assert!(func.blocks.len() >= 3); // entry, match arm blocks, merge
    }
}
