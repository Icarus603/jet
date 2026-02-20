//! Refactoring suggestions based on code analysis
//!
//! This module provides AI-assisted refactoring hints by detecting code smells
//! and suggesting idiomatic alternatives.

use crate::{RefactoringConfig, RefactoringKind};
use jet_diagnostics::{Diagnostic, Span};
use jet_parser::ast::{BinaryOp, Expr, Module, Pattern, Stmt};

/// A refactoring suggestion
#[derive(Debug, Clone)]
pub struct RefactoringSuggestion {
    /// The kind of refactoring suggested
    pub kind: RefactoringKind,
    /// Description of the suggestion
    pub description: String,
    /// The span where the suggestion applies
    pub span: Span,
    /// Confidence level (0.0 to 1.0)
    pub confidence: f64,
    /// Example of the suggested change
    pub example: Option<String>,
}

/// Analyzer for detecting refactoring opportunities
pub struct SuggestionAnalyzer {
    config: RefactoringConfig,
}

impl SuggestionAnalyzer {
    /// Create a new suggestion analyzer
    pub fn new(config: RefactoringConfig) -> Self {
        Self { config }
    }

    /// Analyze a module and suggest refactorings
    pub fn analyze(&self, ast: &Module) -> Vec<RefactoringSuggestion> {
        let mut suggestions = Vec::new();

        for item in &ast.items {
            if let jet_parser::ast::ModuleItem::Function(func) = item {
                suggestions.extend(self.analyze_function(&func.body));
            }
        }

        // Sort by confidence (highest first)
        suggestions.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());

        suggestions
    }

    /// Analyze a function body for refactoring opportunities
    fn analyze_function(&self, body: &Expr) -> Vec<RefactoringSuggestion> {
        let mut suggestions = Vec::new();

        // Check for various patterns
        suggestions.extend(self.detect_clone_patterns(body));
        suggestions.extend(self.detect_unwrap_patterns(body));
        suggestions.extend(self.detect_manual_loop_patterns(body));
        suggestions.extend(self.detect_nested_if_patterns(body));
        suggestions.extend(self.detect_redundant_patterns(body));
        suggestions.extend(self.detect_complex_expression_patterns(body));

        suggestions
    }

    /// Detect patterns where clone() could be avoided
    fn detect_clone_patterns(&self, expr: &Expr) -> Vec<RefactoringSuggestion> {
        let mut suggestions = Vec::new();

        // Look for .clone() calls that might be unnecessary
        traverse_expr(expr, &mut |e| {
            if let Expr::MethodCall { receiver, method, .. } = e {
                if method.name == "clone" {
                    // Check if the clone is actually needed
                    suggestions.push(RefactoringSuggestion {
                        kind: RefactoringKind::InlineVariable,
                        description: "Consider if this clone is necessary".to_string(),
                        span: e.span(),
                        confidence: 0.5,
                        example: Some("Use references instead of cloning".to_string()),
                    });
                }
            }
        });

        suggestions
    }

    /// Detect patterns with unwrap() that could be handled better
    fn detect_unwrap_patterns(&self, expr: &Expr) -> Vec<RefactoringSuggestion> {
        let mut suggestions = Vec::new();

        traverse_expr(expr, &mut |e| {
            if let Expr::MethodCall { receiver, method, .. } = e {
                if method.name == "unwrap" || method.name == "expect" {
                    suggestions.push(RefactoringSuggestion {
                        kind: RefactoringKind::ConvertIfLetToMatch,
                        description: format!(
                            "Consider using pattern matching instead of {} for better error handling",
                            method.name
                        ),
                        span: e.span(),
                        confidence: 0.7,
                        example: Some("match result { Ok(v) => v, Err(e) => handle(e) }".to_string()),
                    });
                }
            }
        });

        suggestions
    }

    /// Detect manual loops that could be iterator methods
    fn detect_manual_loop_patterns(&self, expr: &Expr) -> Vec<RefactoringSuggestion> {
        let mut suggestions = Vec::new();

        traverse_expr(expr, &mut |e| {
            if let Expr::For { pattern, iterable, body, .. } = e {
                // Check for common patterns
                if is_simple_collect_pattern(pattern, iterable, body) {
                    suggestions.push(RefactoringSuggestion {
                        kind: RefactoringKind::ConvertLoopToIterator,
                        description: "This loop could be written as an iterator method chain".to_string(),
                        span: e.span(),
                        confidence: 0.8,
                        example: Some("items.iter().map(|x| ...).collect()".to_string()),
                    });
                }
            }
        });

        suggestions
    }

    /// Detect deeply nested if statements
    fn detect_nested_if_patterns(&self, expr: &Expr) -> Vec<RefactoringSuggestion> {
        let mut suggestions = Vec::new();

        traverse_expr(expr, &mut |e| {
            if let Expr::If { cond: _, then_branch, else_branch } = e {
                // Check for nested if-else chains
                let depth = count_if_depth(e);
                if depth > 3 {
                    suggestions.push(RefactoringSuggestion {
                        kind: RefactoringKind::ConvertIfLetToMatch,
                        description: format!(
                            "Deeply nested if-else chain (depth {}). Consider using match.",
                            depth
                        ),
                        span: e.span(),
                        confidence: 0.75,
                        example: Some("match value { pattern1 => ..., pattern2 => ... }".to_string()),
                    });
                }

                // Check for if-let pattern
                if is_if_let_pattern(e) {
                    suggestions.push(RefactoringSuggestion {
                        kind: RefactoringKind::ConvertIfLetToMatch,
                        description: "if-let can be converted to a match expression".to_string(),
                        span: e.span(),
                        confidence: 0.9,
                        example: Some("match opt { Some(v) => ..., None => ... }".to_string()),
                    });
                }
            }
        });

        suggestions
    }

    /// Detect redundant patterns
    fn detect_redundant_patterns(&self, expr: &Expr) -> Vec<RefactoringSuggestion> {
        let mut suggestions = Vec::new();

        traverse_expr(expr, &mut |e| {
            // Check for redundant clones
            if let Expr::MethodCall { receiver, method, .. } = e {
                if method.name == "clone" {
                    if let Expr::MethodCall { method: inner_method, .. } = receiver.as_ref() {
                        if inner_method.name == "clone" {
                            suggestions.push(RefactoringSuggestion {
                                kind: RefactoringKind::RemoveUnused,
                                description: "Redundant clone() call".to_string(),
                                span: e.span(),
                                confidence: 0.95,
                                example: Some("Remove the outer clone()".to_string()),
                            });
                        }
                    }
                }
            }

            // Check for unnecessary into() / from() combinations
            // Check for redundant collect() -> into_iter()
        });

        suggestions
    }

    /// Detect complex expressions that could be extracted
    fn detect_complex_expression_patterns(&self, expr: &Expr) -> Vec<RefactoringSuggestion> {
        let mut suggestions = Vec::new();

        traverse_expr(expr, &mut |e| {
            let complexity = calculate_complexity(e);
            if complexity > 10 {
                suggestions.push(RefactoringSuggestion {
                    kind: RefactoringKind::ExtractVariable,
                    description: format!(
                        "Complex expression (complexity: {}). Consider extracting into a variable.",
                        complexity
                    ),
                    span: e.span(),
                    confidence: 0.6,
                    example: Some("let result = complex_expression;".to_string()),
                });
            }
        });

        suggestions
    }
}

/// Check if a for loop is a simple collect pattern
fn is_simple_collect_pattern(pattern: &Pattern, _iterable: &Expr, body: &Expr) -> bool {
    // Check if it's a simple push pattern
    if let Expr::Block(block) = body {
        if block.stmts.len() == 1 {
            if let Stmt::Expr(expr) | Stmt::Return(Some(expr)) = &block.stmts[0] {
                if let Expr::MethodCall { method, .. } = expr {
                    return method.name == "push" || method.name == "append";
                }
            }
        }
    }
    false
}

/// Count the nesting depth of if expressions
fn count_if_depth(expr: &Expr) -> usize {
    match expr {
        Expr::If { else_branch: Some(else_expr), .. } => {
            1 + count_if_depth(else_expr)
        }
        Expr::If { else_branch: None, .. } => 1,
        _ => 0,
    }
}

/// Check if an expression is an if-let pattern
fn is_if_let_pattern(expr: &Expr) -> bool {
    // Check if this is an if-let pattern
    // This is a simplified check - would need proper pattern matching
    false
}

/// Calculate the complexity of an expression
fn calculate_complexity(expr: &Expr) -> usize {
    match expr {
        Expr::Literal(_) => 1,
        Expr::Variable(_) => 1,
        Expr::Binary { left, right, .. } => {
            1 + calculate_complexity(left) + calculate_complexity(right)
        }
        Expr::Unary { expr, .. } => 1 + calculate_complexity(expr),
        Expr::Call { func, args } => {
            1 + calculate_complexity(func)
                + args.iter().map(|a| calculate_complexity(a)).sum::<usize>()
        }
        Expr::MethodCall { receiver, args, .. } => {
            1 + calculate_complexity(receiver)
                + args.iter().map(|a| calculate_complexity(a)).sum::<usize>()
        }
        Expr::Block(block) => {
            block.stmts.iter().map(|s| stmt_complexity(s)).sum::<usize>()
                + block.expr.as_ref().map(|e| calculate_complexity(e)).unwrap_or(0)
        }
        _ => 5,
    }
}

/// Calculate the complexity of a statement
fn stmt_complexity(stmt: &Stmt) -> usize {
    match stmt {
        Stmt::Let { value, .. } => 1 + calculate_complexity(value),
        Stmt::Expr(expr) => calculate_complexity(expr),
        Stmt::Assign { value, .. } => 1 + calculate_complexity(value),
        Stmt::Return(Some(expr)) => calculate_complexity(expr),
        _ => 1,
    }
}

/// Traverse an expression tree and apply a function to each node
fn traverse_expr<F>(expr: &Expr, f: &mut F)
where
    F: FnMut(&Expr),
{
    f(expr);

    match expr {
        Expr::Binary { left, right, .. } => {
            traverse_expr(left, f);
            traverse_expr(right, f);
        }
        Expr::Unary { expr: e, .. } => {
            traverse_expr(e, f);
        }
        Expr::Call { func, args } => {
            traverse_expr(func, f);
            for arg in args {
                traverse_expr(arg, f);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            traverse_expr(receiver, f);
            for arg in args {
                traverse_expr(arg, f);
            }
        }
        Expr::FieldAccess { object, .. } => {
            traverse_expr(object, f);
        }
        Expr::Index { object, index } => {
            traverse_expr(object, f);
            traverse_expr(index, f);
        }
        Expr::Block(block) => {
            for stmt in &block.stmts {
                traverse_stmt(stmt, f);
            }
            if let Some(e) = &block.expr {
                traverse_expr(e, f);
            }
        }
        Expr::If { cond, then_branch, else_branch } => {
            traverse_expr(cond, f);
            traverse_expr(then_branch, f);
            if let Some(e) = else_branch {
                traverse_expr(e, f);
            }
        }
        Expr::Match { expr: e, arms } => {
            traverse_expr(e, f);
            for arm in arms {
                traverse_expr(&arm.body, f);
            }
        }
        Expr::While { cond, body } => {
            traverse_expr(cond, f);
            traverse_expr(body, f);
        }
        Expr::For { iterable, body, .. } => {
            traverse_expr(iterable, f);
            traverse_expr(body, f);
        }
        Expr::Tuple(exprs) => {
            for e in exprs {
                traverse_expr(e, f);
            }
        }
        Expr::Array(exprs) => {
            for e in exprs {
                traverse_expr(e, f);
            }
        }
        _ => {}
    }
}

/// Traverse a statement and apply a function to each expression
fn traverse_stmt<F>(stmt: &Stmt, f: &mut F)
where
    F: FnMut(&Expr),
{
    match stmt {
        Stmt::Let { value, .. } => {
            traverse_expr(value, f);
        }
        Stmt::Expr(expr) => {
            traverse_expr(expr, f);
        }
        Stmt::Assign { target, value, .. } => {
            traverse_expr(target, f);
            traverse_expr(value, f);
        }
        Stmt::Return(Some(expr)) => {
            traverse_expr(expr, f);
        }
        _ => {}
    }
}

/// Extension trait for getting spans from expressions
trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        // Return a default span - in practice, this would come from the AST
        Span::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_complexity() {
        let lit = Expr::Literal(jet_parser::ast::Literal::Integer(42));
        assert_eq!(calculate_complexity(&lit), 1);

        let bin = Expr::Binary {
            op: BinaryOp::Add,
            left: Box::new(Expr::Literal(jet_parser::ast::Literal::Integer(1))),
            right: Box::new(Expr::Literal(jet_parser::ast::Literal::Integer(2))),
        };
        assert_eq!(calculate_complexity(&bin), 3);
    }

    #[test]
    fn test_count_if_depth() {
        let simple_if = Expr::If {
            cond: Box::new(Expr::Literal(jet_parser::ast::Literal::Bool(true))),
            then_branch: Box::new(Expr::Block(jet_parser::ast::Block {
                stmts: vec![],
                expr: None,
                span: Span::default(),
            })),
            else_branch: None,
        };
        assert_eq!(count_if_depth(&simple_if), 1);
    }
}
