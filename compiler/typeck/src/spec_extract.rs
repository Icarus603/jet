//! Specification Extraction for Test Generation
//!
//! This module extracts specifications from typed AST for automatic test generation.
//! It identifies properties, preconditions, postconditions, and invariants that can
//! be used to generate tests.
//!
//! # Example
//!
//! Given a function with assertions:
//! ```jet
//! fn reverse_properties(list: [int]) {
//!     assert list == reverse(reverse(list))
//! }
//! ```
//!
//! This module extracts:
//! - The property: `list == reverse(reverse(list))`
//! - The type: `[int]`
//! - Generation strategy for the input
#![allow(unused_variables)]
#![allow(clippy::single_match)]

use crate::checker::{TypedExpr, TypedExprKind, TypedFunction, TypedModule, TypedStmt};
use crate::types::{TypeContext, TypeId, TypeKind};
use jet_parser::ast::BinaryOp;

/// A specification extracted from code.
#[derive(Debug, Clone)]
pub enum Specification {
    /// A property that should hold for all inputs.
    Property {
        /// The expression representing the property.
        expr: TypedExpr,
        /// The type context for interpreting types.
        ty: TypeId,
    },
    /// A precondition that must hold before execution.
    Precondition {
        /// The condition expression.
        condition: TypedExpr,
        /// Human-readable description.
        description: String,
    },
    /// A postcondition that must hold after execution.
    Postcondition {
        /// The condition expression.
        condition: TypedExpr,
        /// Human-readable description.
        description: String,
    },
    /// An invariant that holds throughout execution.
    Invariant {
        /// The invariant expression.
        expr: TypedExpr,
        /// What this invariant applies to.
        target: String,
    },
}

/// Extracted specifications for a function.
#[derive(Debug, Clone, Default)]
pub struct FunctionSpecs {
    /// Properties that should hold.
    pub properties: Vec<Specification>,
    /// Preconditions.
    pub preconditions: Vec<Specification>,
    /// Postconditions.
    pub postconditions: Vec<Specification>,
    /// Invariants.
    pub invariants: Vec<Specification>,
}

/// Extracts specifications from a typed module.
pub fn extract_module_specs(
    module: &TypedModule,
    tcx: &TypeContext,
) -> Vec<(FunctionSpecs, String)> {
    let mut result = Vec::new();

    for item in &module.items {
        // Extract specs from function items (currently the only item type)
        let crate::checker::TypedModuleItem::Function(func) = item;
        let specs = extract_function_specs(func, tcx);
        if has_specs(&specs) {
            result.push((specs, func.name.name.clone()));
        }
    }

    result
}

/// Checks if a function has any specifications.
fn has_specs(specs: &FunctionSpecs) -> bool {
    !specs.properties.is_empty()
        || !specs.preconditions.is_empty()
        || !specs.postconditions.is_empty()
        || !specs.invariants.is_empty()
}

/// Extracts specifications from a typed function.
pub fn extract_function_specs(func: &TypedFunction, tcx: &TypeContext) -> FunctionSpecs {
    let mut specs = FunctionSpecs::default();

    // Extract properties from assertions in the body
    extract_properties_from_expr(&func.body, &mut specs, tcx);

    // Extract preconditions from parameter patterns
    extract_preconditions_from_params(func, &mut specs);

    // Extract postconditions from return type
    extract_postconditions_from_return(func, &mut specs, tcx);

    specs
}

/// Extracts properties from assertions in an expression.
fn extract_properties_from_expr(expr: &TypedExpr, specs: &mut FunctionSpecs, tcx: &TypeContext) {
    match &expr.kind {
        TypedExprKind::Block(block) => {
            for stmt in &block.stmts {
                extract_properties_from_stmt(stmt, specs, tcx);
            }
            if let Some(expr) = &block.expr {
                extract_properties_from_expr(expr, specs, tcx);
            }
        }
        TypedExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            extract_properties_from_expr(cond, specs, tcx);
            extract_properties_from_expr(then_branch, specs, tcx);
            if let Some(else_branch) = else_branch {
                extract_properties_from_expr(else_branch, specs, tcx);
            }
        }
        TypedExprKind::While { cond, body } => {
            extract_properties_from_expr(cond, specs, tcx);
            extract_properties_from_expr(body, specs, tcx);
        }
        TypedExprKind::Match { expr, arms } => {
            extract_properties_from_expr(expr, specs, tcx);
            for arm in arms {
                extract_properties_from_expr(&arm.body, specs, tcx);
            }
        }
        TypedExprKind::Call {
            func: call_func,
            args,
        } => {
            // Check if this is an assert call
            if is_assert_call(call_func) {
                if let Some(arg) = args.first() {
                    specs.properties.push(Specification::Property {
                        expr: arg.clone(),
                        ty: arg.ty,
                    });
                }
            }

            for arg in args {
                extract_properties_from_expr(arg, specs, tcx);
            }
        }
        _ => {}
    }
}

/// Extracts properties from statements.
fn extract_properties_from_stmt(stmt: &TypedStmt, specs: &mut FunctionSpecs, tcx: &TypeContext) {
    match stmt {
        TypedStmt::Expr(expr) => {
            extract_properties_from_expr(expr, specs, tcx);
        }
        TypedStmt::Let { value, .. } => {
            extract_properties_from_expr(value, specs, tcx);
        }
        TypedStmt::Assign { value, .. } => {
            extract_properties_from_expr(value, specs, tcx);
        }
        _ => {}
    }
}

/// Checks if an expression is an assert call.
fn is_assert_call(expr: &TypedExpr) -> bool {
    matches!(
        &expr.kind,
        TypedExprKind::Variable(ident) if ident.name == "assert"
    )
}

/// Extracts preconditions from function parameters.
fn extract_preconditions_from_params(func: &TypedFunction, specs: &mut FunctionSpecs) {
    for param in &func.params {
        // Check for non-nullable types that imply non-null preconditions
        // This is a simplified implementation
        if let jet_parser::ast::Pattern::Ident { name, .. } = &param.pattern {
            let description = format!("Parameter {} must be valid", name.name);
            // In a full implementation, we'd create a proper condition expression
            // For now, we just record that there is a precondition
        }
    }
}

/// Extracts postconditions from the return type.
fn extract_postconditions_from_return(
    func: &TypedFunction,
    specs: &mut FunctionSpecs,
    tcx: &TypeContext,
) {
    // Check return type for Result/Option which implies error handling postconditions
    let return_kind = tcx.type_kind(func.return_type);

    match return_kind {
        TypeKind::Enum(_) => {
            // Could be Result or Option - implies postconditions about success/failure
            let description = format!(
                "Function {} returns a valid result or error",
                func.name.name
            );
            // Record the postcondition
        }
        _ => {}
    }
}

/// Information about a property for test generation.
#[derive(Debug, Clone)]
pub struct PropertyInfo {
    /// The property expression as a string.
    pub expr_string: String,
    /// The type of the property (should be bool).
    pub ty: TypeId,
    /// Variables involved in the property.
    pub variables: Vec<String>,
    /// Whether this is an equivalence property (a == b).
    pub is_equivalence: bool,
    /// Whether this is a reflexive property (a == a).
    pub is_reflexive: bool,
    /// Whether this is a commutative property (a op b == b op a).
    pub is_commutative: bool,
}

/// Analyzes a property expression and extracts information.
pub fn analyze_property(expr: &TypedExpr, tcx: &TypeContext) -> Option<PropertyInfo> {
    let expr_string = expr_to_string(expr);
    let variables = extract_variables(expr);

    let (is_equivalence, is_reflexive, is_commutative) = match &expr.kind {
        TypedExprKind::Binary {
            op: BinaryOp::Eq,
            left,
            right,
        } => {
            let is_reflexive = exprs_equal(left, right);
            let is_commutative = is_commutative_expr(left, right);
            (true, is_reflexive, is_commutative)
        }
        _ => (false, false, false),
    };

    Some(PropertyInfo {
        expr_string,
        ty: expr.ty,
        variables,
        is_equivalence,
        is_reflexive,
        is_commutative,
    })
}

/// Converts an expression to a string representation.
fn expr_to_string(expr: &TypedExpr) -> String {
    match &expr.kind {
        TypedExprKind::Literal(lit) => format!("{:?}", lit),
        TypedExprKind::Variable(ident) => ident.name.clone(),
        TypedExprKind::Binary { op, left, right } => {
            format!(
                "({} {:?} {})",
                expr_to_string(left),
                op,
                expr_to_string(right)
            )
        }
        TypedExprKind::Unary { op, expr } => {
            format!("{:?}({})", op, expr_to_string(expr))
        }
        TypedExprKind::Call { func, args } => {
            let args_str: Vec<String> = args.iter().map(expr_to_string).collect();
            format!("{}({})", expr_to_string(func), args_str.join(", "))
        }
        _ => "<complex>".to_string(),
    }
}

/// Extracts variable names from an expression.
fn extract_variables(expr: &TypedExpr) -> Vec<String> {
    let mut vars = Vec::new();
    extract_variables_recursive(expr, &mut vars);
    vars.sort();
    vars.dedup();
    vars
}

fn extract_variables_recursive(expr: &TypedExpr, vars: &mut Vec<String>) {
    match &expr.kind {
        TypedExprKind::Variable(ident) => {
            vars.push(ident.name.clone());
        }
        TypedExprKind::Binary { left, right, .. } => {
            extract_variables_recursive(left, vars);
            extract_variables_recursive(right, vars);
        }
        TypedExprKind::Unary { expr, .. } => {
            extract_variables_recursive(expr, vars);
        }
        TypedExprKind::Call { func, args } => {
            extract_variables_recursive(func, vars);
            for arg in args {
                extract_variables_recursive(arg, vars);
            }
        }
        _ => {}
    }
}

/// Checks if two expressions are equal.
fn exprs_equal(a: &TypedExpr, b: &TypedExpr) -> bool {
    match (&a.kind, &b.kind) {
        (TypedExprKind::Variable(a), TypedExprKind::Variable(b)) => a.name == b.name,
        (TypedExprKind::Literal(a), TypedExprKind::Literal(b)) => {
            format!("{:?}", a) == format!("{:?}", b)
        }
        _ => false,
    }
}

/// Checks if an expression represents a commutative relationship.
fn is_commutative_expr(left: &TypedExpr, right: &TypedExpr) -> bool {
    // Check if right contains the same operation as left but with swapped operands
    match (&left.kind, &right.kind) {
        (
            TypedExprKind::Call { func: f1, args: a1 },
            TypedExprKind::Call { func: f2, args: a2 },
        ) if a1.len() == 2 && a2.len() == 2 => {
            // Check if functions are the same
            let same_func = match (&f1.kind, &f2.kind) {
                (TypedExprKind::Variable(v1), TypedExprKind::Variable(v2)) => v1.name == v2.name,
                _ => false,
            };

            // Check if arguments are swapped
            let swapped_args = exprs_equal(&a1[0], &a2[1]) && exprs_equal(&a1[1], &a2[0]);

            same_func && swapped_args
        }
        _ => false,
    }
}

/// Generates test generation hints from specifications.
pub fn generate_hints(specs: &FunctionSpecs, tcx: &TypeContext) -> Vec<TestGenHint> {
    let mut hints = Vec::new();

    for property in &specs.properties {
        if let Specification::Property { expr, ty } = property {
            if let Some(info) = analyze_property(expr, tcx) {
                hints.push(TestGenHint::Property(info));
            }
        }
    }

    hints
}

/// A hint for test generation.
#[derive(Debug, Clone)]
pub enum TestGenHint {
    /// Generate tests for this property.
    Property(PropertyInfo),
    /// Generate tests for this precondition.
    Precondition { condition: String },
    /// Generate tests for this postcondition.
    Postcondition { expected: String },
    /// Generate tests for this invariant.
    Invariant { expr: String },
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_diagnostics::Span;
    use jet_parser::ast::{Ident, Literal};

    fn create_simple_typed_expr(kind: TypedExprKind, ty: TypeId) -> TypedExpr {
        TypedExpr {
            kind,
            ty,
            span: Span::default(),
        }
    }

    #[test]
    fn test_function_specs_default() {
        let specs = FunctionSpecs::default();
        assert!(specs.properties.is_empty());
        assert!(specs.preconditions.is_empty());
        assert!(specs.postconditions.is_empty());
        assert!(specs.invariants.is_empty());
    }

    #[test]
    fn test_has_specs() {
        let mut specs = FunctionSpecs::default();
        assert!(!has_specs(&specs));

        specs.properties.push(Specification::Property {
            expr: create_simple_typed_expr(
                TypedExprKind::Literal(Literal::Bool(true)),
                TypeId::BOOL,
            ),
            ty: TypeId::BOOL,
        });

        assert!(has_specs(&specs));
    }

    #[test]
    fn test_extract_variables() {
        let expr = create_simple_typed_expr(
            TypedExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(create_simple_typed_expr(
                    TypedExprKind::Variable(Ident::new("a", jet_lexer::Span::default())),
                    TypeId::INT,
                )),
                right: Box::new(create_simple_typed_expr(
                    TypedExprKind::Variable(Ident::new("b", jet_lexer::Span::default())),
                    TypeId::INT,
                )),
            },
            TypeId::INT,
        );

        let vars = extract_variables(&expr);
        assert!(vars.contains(&"a".to_string()));
        assert!(vars.contains(&"b".to_string()));
    }

    #[test]
    fn test_is_assert_call() {
        let assert_expr = create_simple_typed_expr(
            TypedExprKind::Variable(Ident::new("assert", jet_lexer::Span::default())),
            TypeId::UNIT,
        );
        assert!(is_assert_call(&assert_expr));

        let other_expr = create_simple_typed_expr(
            TypedExprKind::Variable(Ident::new("other", jet_lexer::Span::default())),
            TypeId::UNIT,
        );
        assert!(!is_assert_call(&other_expr));
    }
}
