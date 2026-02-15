//! Pattern Matching Compilation
//!
//! This module provides LLVM code generation for Jet pattern matching.
//!
//! # Pattern Matching Strategy
//!
//! Pattern matching is compiled to a decision tree using:
//! - Switch instructions for variant/integer matching
//! - Chain of comparisons for guards
//! - ExtractValue for destructuring
//!
//! # Example
//!
//! ```jet
//! match result:
//!     Ok(value) -> handle(value)
//!     Err(e) -> log(e)
//! ```
//!
//! Compiles to:
//! ```llvm
//! %tag = extractvalue %Result %result, 0
//! switch i32 %tag, label %default [
//!     i32 0, label %case_ok
//!     i32 1, label %case_err
//! ]
//!
//! case_ok:
//! %value = extractvalue %Result %result, 1
//! call void @handle(i32 %value)
//! br label %merge
//!
//! case_err:
//! %e = extractvalue %Result %result, 1
//! call void @log(i32 %e)
//! br label %merge
//! ```

use crate::context::CodeGen;
use crate::error::{CodegenError, CodegenResult};
use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicValueEnum, IntValue};
use jet_ir::{BlockId, Ty, ValueId};

/// A pattern in a match expression.
#[derive(Debug, Clone)]
pub enum Pattern {
    /// Wildcard pattern (_).
    Wildcard,
    /// Variable binding (x).
    Variable(String, ValueId),
    /// Literal value (1, "hello", etc.).
    Literal(PatternLiteral),
    /// Variant pattern (Some(x), None, etc.).
    Variant {
        /// The variant index/tag.
        tag: u32,
        /// The fields to bind.
        fields: Vec<Pattern>,
    },
    /// Struct pattern (Point { x, y }).
    Struct {
        /// Field patterns.
        fields: Vec<(String, Pattern)>,
    },
    /// Tuple pattern ((a, b, c)).
    Tuple(Vec<Pattern>),
    /// Or pattern (p1 | p2).
    Or(Box<Pattern>, Box<Pattern>),
    /// Guarded pattern (p if condition).
    Guarded {
        /// The inner pattern.
        pattern: Box<Pattern>,
        /// The guard condition value ID.
        guard: ValueId,
    },
}

/// A literal pattern value.
#[derive(Debug, Clone)]
pub enum PatternLiteral {
    Int(i64),
    Bool(bool),
    String(String),
    Unit,
}

/// A case in a match expression.
#[derive(Debug, Clone)]
pub struct MatchCase {
    /// The pattern to match.
    pub pattern: Pattern,
    /// The block to execute if matched.
    pub body_block: BlockId,
    /// Whether this is the default case.
    pub is_default: bool,
}

/// Compiles a match expression.
///
/// # Arguments
///
/// * `codegen` - The code generation context
/// * `scrutinee` - The value being matched
/// * `scrutinee_ty` - The type of the scrutinee
/// * `cases` - The match cases
/// * `merge_block` - The block to branch to after the match
///
/// # Returns
///
/// Returns Ok(()) on success.
pub fn compile_match<'ctx>(
    _codegen: &mut CodeGen<'ctx>,
    scrutinee: BasicValueEnum<'ctx>,
    scrutinee_ty: &Ty,
    cases: &[MatchCase],
    merge_block: BasicBlock<'ctx>,
) -> CodegenResult<()> {
    // Sort cases: put default case at the end, sort others by specificity
    let sorted_cases = sort_cases(cases);

    // Generate decision tree
    let decision_tree = build_decision_tree(&sorted_cases, scrutinee_ty)?;

    // Compile decision tree to LLVM
    compile_decision_tree(
        _codegen,
        &decision_tree,
        scrutinee,
        scrutinee_ty,
        merge_block,
    )
}

/// Sorts match cases for optimal compilation.
fn sort_cases(cases: &[MatchCase]) -> Vec<MatchCase> {
    let mut sorted = cases.to_vec();

    // Sort by specificity: literals first, then variants, then wildcards
    sorted.sort_by(|a, b| {
        let a_score = pattern_specificity(&a.pattern);
        let b_score = pattern_specificity(&b.pattern);
        b_score.cmp(&a_score) // Higher specificity first
    });

    sorted
}

/// Calculates the specificity of a pattern (higher = more specific).
fn pattern_specificity(pattern: &Pattern) -> u32 {
    match pattern {
        Pattern::Wildcard => 0,
        Pattern::Variable(_, _) => 1,
        Pattern::Literal(_) => 10,
        Pattern::Variant { fields, .. } => 5 + fields.iter().map(pattern_specificity).sum::<u32>(),
        Pattern::Struct { fields } => {
            5 + fields
                .iter()
                .map(|(_, p)| pattern_specificity(p))
                .sum::<u32>()
        }
        Pattern::Tuple(fields) => 5 + fields.iter().map(pattern_specificity).sum::<u32>(),
        Pattern::Or(a, b) => pattern_specificity(a).max(pattern_specificity(b)),
        Pattern::Guarded { pattern, .. } => pattern_specificity(pattern) + 1,
    }
}

/// A node in the decision tree.
#[derive(Debug)]
enum DecisionTree {
    /// Leaf node - execute this block.
    Leaf(BlockId),
    /// Switch on a value.
    Switch {
        /// What to switch on (variant tag, integer value, etc.).
        discriminant: Discriminant,
        /// Cases for the switch.
        cases: Vec<(SwitchValue, Box<DecisionTree>)>,
        /// Default case.
        default: Option<Box<DecisionTree>>,
    },
    /// Test a guard condition.
    Guard {
        /// The condition value ID.
        condition: ValueId,
        /// If true.
        then_branch: Box<DecisionTree>,
        /// If false (try next pattern).
        else_branch: Box<DecisionTree>,
    },
    /// Bind a variable.
    Bind {
        /// The variable ID.
        var: ValueId,
        /// How to extract the value.
        extractor: ValueExtractor,
        /// Continue with this subtree.
        subtree: Box<DecisionTree>,
    },
}

/// What to switch on.
#[derive(Debug)]
enum Discriminant {
    /// Variant tag (for enums).
    VariantTag,
    /// Integer value.
    IntValue,
    /// Boolean value.
    BoolValue,
}

/// A value to match in a switch.
#[derive(Debug, Clone)]
enum SwitchValue {
    Int(i64),
    Bool(bool),
    Variant(u32),
}

/// How to extract a value from the scrutinee.
#[derive(Debug)]
#[allow(dead_code)]
enum ValueExtractor {
    /// Extract a struct field.
    StructField(usize),
    /// Extract a tuple element.
    TupleElement(usize),
    /// Extract a variant payload.
    VariantPayload {
        #[allow(dead_code)]
        tag: u32,
        field_index: usize,
    },
    /// The value itself.
    Identity,
}

/// Builds a decision tree from match cases.
fn build_decision_tree(cases: &[MatchCase], _scrutinee_ty: &Ty) -> CodegenResult<DecisionTree> {
    if cases.is_empty() {
        return Err(CodegenError::invalid_operand(
            "match expression",
            "no cases",
        ));
    }

    // Simple implementation: just chain the cases
    // A full implementation would use the algorithm from
    // "Compiling Pattern Matching to Good Decision Trees"
    build_tree_recursive(cases, 0)
}

fn build_tree_recursive(cases: &[MatchCase], index: usize) -> CodegenResult<DecisionTree> {
    if index >= cases.len() {
        // No more cases - this shouldn't happen if there's a default
        return Ok(DecisionTree::Leaf(BlockId::new(0)));
    }

    let case = &cases[index];

    // Build tree for this pattern
    let tree = pattern_to_tree(&case.pattern, case.body_block)?;

    // If there are more cases, add them as fallback
    if index + 1 < cases.len() {
        // For now, just use the next case as fallback
        // In a full implementation, we'd build a proper decision tree
        Ok(tree)
    } else {
        Ok(tree)
    }
}

fn pattern_to_tree(pattern: &Pattern, body_block: BlockId) -> CodegenResult<DecisionTree> {
    match pattern {
        Pattern::Wildcard | Pattern::Variable(_, _) => Ok(DecisionTree::Leaf(body_block)),
        Pattern::Literal(lit) => {
            let switch_val = literal_to_switch_value(lit);
            Ok(DecisionTree::Switch {
                discriminant: discriminant_for_literal(lit),
                cases: vec![(switch_val, Box::new(DecisionTree::Leaf(body_block)))],
                default: None,
            })
        }
        Pattern::Variant { tag, fields } => {
            // Build subtree for field bindings
            let mut subtree = DecisionTree::Leaf(body_block);

            // Add bindings for fields (in reverse order)
            for (i, field_pattern) in fields.iter().enumerate().rev() {
                if let Pattern::Variable(_name, var_id) = field_pattern {
                    subtree = DecisionTree::Bind {
                        var: *var_id,
                        extractor: ValueExtractor::VariantPayload {
                            tag: *tag,
                            field_index: i,
                        },
                        subtree: Box::new(subtree),
                    };
                }
            }

            Ok(DecisionTree::Switch {
                discriminant: Discriminant::VariantTag,
                cases: vec![(SwitchValue::Variant(*tag), Box::new(subtree))],
                default: None,
            })
        }
        Pattern::Struct { fields } => {
            let mut subtree = DecisionTree::Leaf(body_block);

            // Add bindings for fields
            for (i, (_name, field_pattern)) in fields.iter().enumerate().rev() {
                if let Pattern::Variable(_, var_id) = field_pattern {
                    subtree = DecisionTree::Bind {
                        var: *var_id,
                        extractor: ValueExtractor::StructField(i),
                        subtree: Box::new(subtree),
                    };
                }
            }

            Ok(subtree)
        }
        Pattern::Tuple(elements) => {
            let mut subtree = DecisionTree::Leaf(body_block);

            // Add bindings for elements
            for (i, elem_pattern) in elements.iter().enumerate().rev() {
                if let Pattern::Variable(_, var_id) = elem_pattern {
                    subtree = DecisionTree::Bind {
                        var: *var_id,
                        extractor: ValueExtractor::TupleElement(i),
                        subtree: Box::new(subtree),
                    };
                }
            }

            Ok(subtree)
        }
        Pattern::Or(a, b) => {
            // Compile both branches
            let tree_a = pattern_to_tree(a, body_block)?;
            let _tree_b = pattern_to_tree(b, body_block)?;

            // For now, just use the first one
            // A full implementation would merge the trees
            Ok(tree_a)
        }
        Pattern::Guarded { pattern, guard } => {
            let inner_tree = pattern_to_tree(pattern, body_block)?;
            Ok(DecisionTree::Guard {
                condition: *guard,
                then_branch: Box::new(inner_tree),
                else_branch: Box::new(DecisionTree::Leaf(BlockId::new(0))), // Fallback
            })
        }
    }
}

fn literal_to_switch_value(lit: &PatternLiteral) -> SwitchValue {
    match lit {
        PatternLiteral::Int(n) => SwitchValue::Int(*n),
        PatternLiteral::Bool(b) => SwitchValue::Bool(*b),
        PatternLiteral::String(_) => SwitchValue::Int(0), // Strings use different matching
        PatternLiteral::Unit => SwitchValue::Int(0),
    }
}

fn discriminant_for_literal(lit: &PatternLiteral) -> Discriminant {
    match lit {
        PatternLiteral::Int(_) => Discriminant::IntValue,
        PatternLiteral::Bool(_) => Discriminant::BoolValue,
        PatternLiteral::String(_) => Discriminant::IntValue,
        PatternLiteral::Unit => Discriminant::IntValue,
    }
}

/// Compiles a decision tree to LLVM IR.
fn compile_decision_tree<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    tree: &DecisionTree,
    scrutinee: BasicValueEnum<'ctx>,
    scrutinee_ty: &Ty,
    merge_block: BasicBlock<'ctx>,
) -> CodegenResult<()> {
    match tree {
        DecisionTree::Leaf(block_id) => {
            let target = codegen.get_block(*block_id)?;
            codegen
                .builder
                .build_unconditional_branch(target)
                .map_err(|e| CodegenError::instruction_error(e.to_string()))?;
            Ok(())
        }
        DecisionTree::Switch {
            discriminant,
            cases,
            default,
        } => compile_switch(
            codegen,
            discriminant,
            cases,
            default.as_deref(),
            scrutinee,
            scrutinee_ty,
            merge_block,
        ),
        DecisionTree::Guard {
            condition,
            then_branch,
            else_branch,
        } => compile_guard(
            codegen,
            *condition,
            then_branch,
            else_branch,
            scrutinee,
            scrutinee_ty,
            merge_block,
        ),
        DecisionTree::Bind {
            var,
            extractor,
            subtree,
        } => {
            // Extract value and bind to variable
            let extracted = extract_value(codegen, scrutinee, extractor, scrutinee_ty)?;
            codegen.set_value(*var, extracted);

            // Continue with subtree
            compile_decision_tree(codegen, subtree, scrutinee, scrutinee_ty, merge_block)
        }
    }
}

/// Compiles a switch in the decision tree.
fn compile_switch<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    discriminant: &Discriminant,
    cases: &[(SwitchValue, Box<DecisionTree>)],
    default: Option<&DecisionTree>,
    scrutinee: BasicValueEnum<'ctx>,
    scrutinee_ty: &Ty,
    merge_block: BasicBlock<'ctx>,
) -> CodegenResult<()> {
    // Get the value to switch on
    let switch_val = match discriminant {
        Discriminant::VariantTag => extract_variant_tag(codegen, scrutinee, scrutinee_ty)?,
        Discriminant::IntValue => scrutinee.into_int_value(),
        Discriminant::BoolValue => scrutinee.into_int_value(),
    };

    // Create blocks for each case
    let mut llvm_cases = Vec::new();
    let mut case_trees = Vec::new();

    for (val, tree) in cases {
        let case_block = codegen
            .context
            .append_basic_block(codegen.current_function().unwrap(), "match_case");

        let llvm_val = switch_value_to_llvm(codegen, val, switch_val.get_type())?;
        llvm_cases.push((llvm_val, case_block));
        case_trees.push((case_block, tree.as_ref()));
    }

    // Create default block
    let default_block = if let Some(_default_tree) = default {
        let block = codegen
            .context
            .append_basic_block(codegen.current_function().unwrap(), "match_default");
        block
    } else {
        // No default - use merge block
        merge_block
    };

    // Build switch instruction
    codegen
        .builder
        .build_switch(switch_val, default_block, &llvm_cases)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Compile each case body
    for (block, tree) in case_trees {
        codegen.builder.position_at_end(block);
        compile_decision_tree(codegen, tree, scrutinee, scrutinee_ty, merge_block)?;
    }

    // Compile default if present
    if let Some(default_tree) = default {
        codegen.builder.position_at_end(default_block);
        compile_decision_tree(codegen, default_tree, scrutinee, scrutinee_ty, merge_block)?;
    }

    Ok(())
}

/// Compiles a guard condition.
fn compile_guard<'ctx>(
    codegen: &mut CodeGen<'ctx>,
    condition: ValueId,
    then_branch: &DecisionTree,
    else_branch: &DecisionTree,
    scrutinee: BasicValueEnum<'ctx>,
    scrutinee_ty: &Ty,
    merge_block: BasicBlock<'ctx>,
) -> CodegenResult<()> {
    let then_block = codegen
        .context
        .append_basic_block(codegen.current_function().unwrap(), "guard_then");
    let else_block = codegen
        .context
        .append_basic_block(codegen.current_function().unwrap(), "guard_else");

    // Get condition value
    let cond_val = codegen.get_value(condition)?.into_int_value();

    codegen
        .builder
        .build_conditional_branch(cond_val, then_block, else_block)
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    // Compile then branch
    codegen.builder.position_at_end(then_block);
    compile_decision_tree(codegen, then_branch, scrutinee, scrutinee_ty, merge_block)?;

    // Compile else branch
    codegen.builder.position_at_end(else_block);
    compile_decision_tree(codegen, else_branch, scrutinee, scrutinee_ty, merge_block)?;

    Ok(())
}

/// Extracts a value using an extractor.
fn extract_value<'ctx>(
    codegen: &CodeGen<'ctx>,
    scrutinee: BasicValueEnum<'ctx>,
    extractor: &ValueExtractor,
    _scrutinee_ty: &Ty,
) -> CodegenResult<BasicValueEnum<'ctx>> {
    match extractor {
        ValueExtractor::Identity => Ok(scrutinee),
        ValueExtractor::StructField(index) => codegen
            .builder
            .build_extract_value(scrutinee.into_struct_value(), *index as u32, "struct_field")
            .map_err(|e| CodegenError::instruction_error(e.to_string())),
        ValueExtractor::TupleElement(index) => codegen
            .builder
            .build_extract_value(scrutinee.into_struct_value(), *index as u32, "tuple_elem")
            .map_err(|e| CodegenError::instruction_error(e.to_string())),
        ValueExtractor::VariantPayload {
            tag: _,
            field_index,
        } => {
            // Variant payload is stored after the tag
            // For now, assume it's at index 1
            codegen
                .builder
                .build_extract_value(
                    scrutinee.into_struct_value(),
                    (*field_index + 1) as u32,
                    "variant_payload",
                )
                .map_err(|e| CodegenError::instruction_error(e.to_string()))
        }
    }
}

/// Extracts the variant tag from a value.
fn extract_variant_tag<'ctx>(
    codegen: &CodeGen<'ctx>,
    scrutinee: BasicValueEnum<'ctx>,
    _scrutinee_ty: &Ty,
) -> CodegenResult<IntValue<'ctx>> {
    // Assume tag is the first field
    let tag_val = codegen
        .builder
        .build_extract_value(scrutinee.into_struct_value(), 0, "variant_tag")
        .map_err(|e| CodegenError::instruction_error(e.to_string()))?;

    Ok(tag_val.into_int_value())
}

/// Converts a switch value to LLVM.
fn switch_value_to_llvm<'ctx>(
    _codegen: &CodeGen<'ctx>,
    val: &SwitchValue,
    int_type: inkwell::types::IntType<'ctx>,
) -> CodegenResult<IntValue<'ctx>> {
    match val {
        SwitchValue::Int(n) => Ok(int_type.const_int(*n as u64, true)),
        SwitchValue::Bool(b) => Ok(int_type.const_int(*b as u64, false)),
        SwitchValue::Variant(tag) => Ok(int_type.const_int(*tag as u64, false)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_specificity() {
        assert_eq!(pattern_specificity(&Pattern::Wildcard), 0);
        assert_eq!(
            pattern_specificity(&Pattern::Variable("x".to_string(), ValueId::new(0))),
            1
        );
        assert_eq!(
            pattern_specificity(&Pattern::Literal(PatternLiteral::Int(42))),
            10
        );

        let variant = Pattern::Variant {
            tag: 0,
            fields: vec![Pattern::Wildcard],
        };
        // Variant with wildcard field has specificity 5 + 0 = 5
        assert_eq!(pattern_specificity(&variant), 5);
    }

    #[test]
    fn test_literal_to_switch_value() {
        assert!(matches!(
            literal_to_switch_value(&PatternLiteral::Int(42)),
            SwitchValue::Int(42)
        ));
        assert!(matches!(
            literal_to_switch_value(&PatternLiteral::Bool(true)),
            SwitchValue::Bool(true)
        ));
    }
}
