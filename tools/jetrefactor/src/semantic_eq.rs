//! Semantic equality checking for refactoring verification
//!
//! This module provides utilities to check if two code fragments are
//! semantically equivalent, which is essential for verifying that
//! refactorings preserve behavior.

use jet_diagnostics::Span;
use jet_parser::ast::{BinaryOp, Expr, Literal, Module, Stmt, UnaryOp};
use std::collections::HashMap;

/// Trait for types that can be checked for semantic equality
pub trait SemanticEq {
    /// Check if two values are semantically equal
    fn semantic_eq(&self, other: &Self) -> bool;

    /// Check if two values are semantically equal with variable renaming
    fn semantic_eq_with_renaming(&self, other: &Self, renaming: &HashMap<String, String>) -> bool;
}

impl SemanticEq for Expr {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.semantic_eq_with_renaming(other, &HashMap::new())
    }

    fn semantic_eq_with_renaming(&self, other: &Self, renaming: &HashMap<String, String>) -> bool {
        use Expr::*;

        match (self, other) {
            // Literals are semantically equal if they're the same value
            (Literal(l1), Literal(l2)) => l1 == l2,

            // Variables are semantically equal if they're the same or renamed
            (Variable(v1), Variable(v2)) => {
                let name1 = &v1.name;
                let name2 = &v2.name;
                name1 == name2
                    || renaming.get(name1).map(|s| s == name2).unwrap_or(false)
                    || renaming.get(name2).map(|s| s == name1).unwrap_or(false)
            }

            // Binary operations
            (Binary { op: op1, left: l1, right: r1 }, Binary { op: op2, left: l2, right: r2 }) => {
                op1 == op2
                    && l1.semantic_eq_with_renaming(l2, renaming)
                    && r1.semantic_eq_with_renaming(r2, renaming)
            }

            // Unary operations
            (Unary { op: op1, expr: e1 }, Unary { op: op2, expr: e2 }) => {
                op1 == op2 && e1.semantic_eq_with_renaming(e2, renaming)
            }

            // Function calls
            (Call { func: f1, args: a1 }, Call { func: f2, args: a2 }) => {
                f1.semantic_eq_with_renaming(f2, renaming)
                    && a1.len() == a2.len()
                    && a1.iter()
                        .zip(a2.iter())
                        .all(|(x, y)| x.semantic_eq_with_renaming(y, renaming))
            }

            // Method calls
            (
                MethodCall {
                    receiver: r1,
                    method: m1,
                    args: a1,
                },
                MethodCall {
                    receiver: r2,
                    method: m2,
                    args: a2,
                },
            ) => {
                m1.name == m2.name
                    && r1.semantic_eq_with_renaming(r2, renaming)
                    && a1.len() == a2.len()
                    && a1.iter()
                        .zip(a2.iter())
                        .all(|(x, y)| x.semantic_eq_with_renaming(y, renaming))
            }

            // Field access
            (FieldAccess { object: o1, field: f1 }, FieldAccess { object: o2, field: f2 }) => {
                f1.name == f2.name && o1.semantic_eq_with_renaming(o2, renaming)
            }

            // Index access
            (Index { object: o1, index: i1 }, Index { object: o2, index: i2 }) => {
                o1.semantic_eq_with_renaming(o2, renaming) && i1.semantic_eq_with_renaming(i2, renaming)
            }

            // Blocks
            (Block(b1), Block(b2)) => {
                b1.stmts.len() == b2.stmts.len()
                    && b1.stmts.iter().zip(b2.stmts.iter()).all(|(s1, s2)| {
                        s1.semantic_eq_with_renaming(s2, renaming)
                    })
                    && match (&b1.expr, &b2.expr) {
                        (Some(e1), Some(e2)) => e1.semantic_eq_with_renaming(e2, renaming),
                        (None, None) => true,
                        _ => false,
                    }
            }

            // If expressions
            (
                If {
                    cond: c1,
                    then_branch: t1,
                    else_branch: e1,
                },
                If {
                    cond: c2,
                    then_branch: t2,
                    else_branch: e2,
                },
            ) => {
                c1.semantic_eq_with_renaming(c2, renaming)
                    && t1.semantic_eq_with_renaming(t2, renaming)
                    && match (e1, e2) {
                        (Some(a), Some(b)) => a.semantic_eq_with_renaming(b, renaming),
                        (None, None) => true,
                        _ => false,
                    }
            }

            // Match expressions
            (Match { expr: e1, arms: a1 }, Match { expr: e2, arms: a2 }) => {
                e1.semantic_eq_with_renaming(e2, renaming)
                    && a1.len() == a2.len()
                    && a1.iter().zip(a2.iter()).all(|(arm1, arm2)| {
                        // Patterns should match (with renaming)
                        // Guards should match
                        // Bodies should match
                        arm1.body.semantic_eq_with_renaming(&arm2.body, renaming)
                    })
            }

            // Tuples
            (Tuple(t1), Tuple(t2)) => {
                t1.len() == t2.len()
                    && t1.iter()
                        .zip(t2.iter())
                        .all(|(x, y)| x.semantic_eq_with_renaming(y, renaming))
            }

            // Arrays
            (Array(a1), Array(a2)) => {
                a1.len() == a2.len()
                    && a1.iter()
                        .zip(a2.iter())
                        .all(|(x, y)| x.semantic_eq_with_renaming(y, renaming))
            }

            // Different expression types are not semantically equal
            _ => false,
        }
    }
}

impl SemanticEq for Stmt {
    fn semantic_eq(&self, other: &Self) -> bool {
        self.semantic_eq_with_renaming(other, &HashMap::new())
    }

    fn semantic_eq_with_renaming(&self, other: &Self, renaming: &HashMap<String, String>) -> bool {
        use Stmt::*;

        match (self, other) {
            // Let bindings
            (
                Let {
                    pattern: p1,
                    ty: t1,
                    value: v1,
                },
                Let {
                    pattern: p2,
                    ty: t2,
                    value: v2,
                },
            ) => {
                // Types should match
                let types_match = match (t1, t2) {
                    (Some(ty1), Some(ty2)) => ty1 == ty2, // Simplified type comparison
                    (None, None) => true,
                    _ => false,
                };

                // Values should be semantically equal
                types_match && v1.semantic_eq_with_renaming(v2, renaming)
                // Note: pattern comparison would need renaming tracking
            }

            // Expression statements
            (Expr(e1), Expr(e2)) => e1.semantic_eq_with_renaming(e2, renaming),

            // Assignment statements
            (
                Assign {
                    target: t1,
                    op: op1,
                    value: v1,
                },
                Assign {
                    target: t2,
                    op: op2,
                    value: v2,
                },
            ) => {
                op1 == op2
                    && t1.semantic_eq_with_renaming(t2, renaming)
                    && v1.semantic_eq_with_renaming(v2, renaming)
            }

            // Return statements
            (Return(r1), Return(r2)) => match (r1, r2) {
                (Some(e1), Some(e2)) => e1.semantic_eq_with_renaming(e2, renaming),
                (None, None) => true,
                _ => false,
            },

            // Different statement types are not semantically equal
            _ => false,
        }
    }
}

/// Check if two modules are semantically equal
pub fn modules_semantic_eq(m1: &Module, m2: &Module) -> bool {
    if m1.items.len() != m2.items.len() {
        return false;
    }

    // Compare each module item
    for (item1, item2) in m1.items.iter().zip(m2.items.iter()) {
        if !module_item_semantic_eq(item1, item2) {
            return false;
        }
    }

    true
}

/// Check if two module items are semantically equal
fn module_item_semantic_eq(
    item1: &jet_parser::ast::ModuleItem,
    item2: &jet_parser::ast::ModuleItem,
) -> bool {
    use jet_parser::ast::ModuleItem::*;

    match (item1, item2) {
        (Function(f1), Function(f2)) => functions_semantic_eq(f1, f2),
        (Struct(s1), Struct(s2)) => s1.name.name == s2.name.name,
        (Enum(e1), Enum(e2)) => e1.name.name == e2.name.name,
        // For now, other items are compared by name
        _ => false,
    }
}

/// Check if two functions are semantically equal
fn functions_semantic_eq(f1: &jet_parser::ast::Function, f2: &jet_parser::ast::Function) -> bool {
    // Functions are semantically equal if:
    // 1. They have the same name
    // 2. They have the same number of parameters
    // 3. Their bodies are semantically equal

    if f1.name.name != f2.name.name {
        return false;
    }

    if f1.params.len() != f2.params.len() {
        return false;
    }

    // Build parameter renaming map
    let mut renaming = HashMap::new();
    for (p1, p2) in f1.params.iter().zip(f2.params.iter()) {
        if let (Some(n1), Some(n2)) = (pattern_name(&p1.pattern), pattern_name(&p2.pattern)) {
            if n1 != n2 {
                renaming.insert(n1, n2);
            }
        }
    }

    f1.body.semantic_eq_with_renaming(&f2.body, &renaming)
}

/// Extract a name from a pattern
fn pattern_name(pattern: &jet_parser::ast::Pattern) -> Option<String> {
    match pattern {
        jet_parser::ast::Pattern::Ident { name, .. } => Some(name.name.clone()),
        _ => None,
    }
}

/// A semantic diff between two code fragments
#[derive(Debug, Clone)]
pub struct SemanticDiff {
    /// Whether the fragments are semantically equal
    pub is_equal: bool,
    /// Differences found (if any)
    pub differences: Vec<SemanticDifference>,
}

/// A single semantic difference
#[derive(Debug, Clone)]
pub struct SemanticDifference {
    /// Location in the first fragment
    pub span1: Span,
    /// Location in the second fragment
    pub span2: Span,
    /// Description of the difference
    pub description: String,
}

/// Compute the semantic diff between two expressions
pub fn expr_semantic_diff(e1: &Expr, e2: &Expr) -> SemanticDiff {
    let is_equal = e1.semantic_eq(e2);

    let differences = if is_equal {
        vec![]
    } else {
        vec![SemanticDifference {
            span1: e1.span(),
            span2: e2.span(),
            description: "Expressions are not semantically equal".to_string(),
        }]
    };

    SemanticDiff { is_equal, differences }
}

/// Compute the semantic diff between two modules
pub fn module_semantic_diff(m1: &Module, m2: &Module) -> SemanticDiff {
    let mut differences = vec![];

    // Compare items
    let max_items = m1.items.len().max(m2.items.len());
    for i in 0..max_items {
        match (m1.items.get(i), m2.items.get(i)) {
            (Some(item1), Some(item2)) => {
                if !module_item_semantic_eq(item1, item2) {
                    differences.push(SemanticDifference {
                        span1: item1.span(),
                        span2: item2.span(),
                        description: format!(
                            "Module items differ: {} vs {}",
                            item1.name(),
                            item2.name()
                        ),
                    });
                }
            }
            (Some(item1), None) => {
                differences.push(SemanticDifference {
                    span1: item1.span(),
                    span2: Span::default(),
                    description: format!("Item {} only in first module", item1.name()),
                });
            }
            (None, Some(item2)) => {
                differences.push(SemanticDifference {
                    span1: Span::default(),
                    span2: item2.span(),
                    description: format!("Item {} only in second module", item2.name()),
                });
            }
            (None, None) => {}
        }
    }

    SemanticDiff {
        is_equal: differences.is_empty(),
        differences,
    }
}

/// Extension trait for getting spans from AST nodes
trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        // This would return the actual span from the AST
        // For now, return a default span
        Span::default()
    }
}

impl Spanned for jet_parser::ast::ModuleItem {
    fn span(&self) -> Span {
        use jet_parser::ast::ModuleItem::*;
        match self {
            Function(f) => f.span,
            Struct(s) => s.span,
            Enum(e) => e.span,
            _ => Span::default(),
        }
    }

    fn name(&self) -> String {
        use jet_parser::ast::ModuleItem::*;
        match self {
            Function(f) => f.name.name.clone(),
            Struct(s) => s.name.name.clone(),
            Enum(e) => e.name.name.clone(),
            _ => "unknown".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_semantic_eq() {
        let l1 = Expr::Literal(Literal::Integer(42));
        let l2 = Expr::Literal(Literal::Integer(42));
        let l3 = Expr::Literal(Literal::Integer(43));

        assert!(l1.semantic_eq(&l2));
        assert!(!l1.semantic_eq(&l3));
    }

    #[test]
    fn test_variable_semantic_eq_with_renaming() {
        let v1 = Expr::Variable(jet_parser::ast::Ident::new("x", Span::default()));
        let v2 = Expr::Variable(jet_parser::ast::Ident::new("y", Span::default()));

        // Without renaming, x != y
        assert!(!v1.semantic_eq(&v2));

        // With renaming x -> y, they are equal
        let mut renaming = HashMap::new();
        renaming.insert("x".to_string(), "y".to_string());
        assert!(v1.semantic_eq_with_renaming(&v2, &renaming));
    }

    #[test]
    fn test_binary_semantic_eq() {
        let b1 = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Literal(Literal::Integer(1))),
            right: Box::new(Expr::Literal(Literal::Integer(2))),
        };

        let b2 = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Literal(Literal::Integer(1))),
            right: Box::new(Expr::Literal(Literal::Integer(2))),
        };

        let b3 = Expr::Binary {
            op: BinaryOp::Sub,
            left: Box::new(Expr::Literal(Literal::Integer(1))),
            right: Box::new(Expr::Literal(Literal::Integer(2))),
        };

        assert!(b1.semantic_eq(&b2));
        assert!(!b1.semantic_eq(&b3));
    }
}
