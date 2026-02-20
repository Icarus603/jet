//! Lint implementations
//!
//! This module contains all the built-in lint passes organized by category.

pub mod correctness;
pub mod performance;
pub mod style;
pub mod suspicious;

use jet_lexer::Span;
use jet_parser::ast::{Expr, Ident, Pattern};

/// Helper trait for checking if an expression has a specific type
pub trait TypeHint {
    /// Check if this expression returns a Result type
    fn is_result_type(&self) -> bool;
    /// Check if this expression returns an Option type
    fn is_option_type(&self) -> bool;
    /// Check if this expression must be used (has #[must_use] attribute)
    fn is_must_use(&self) -> bool;
}

impl TypeHint for Expr {
    fn is_result_type(&self) -> bool {
        // This would need type information to be accurate
        // For now, we use heuristics based on method calls
        false
    }

    fn is_option_type(&self) -> bool {
        // This would need type information to be accurate
        false
    }

    fn is_must_use(&self) -> bool {
        // This would check for #[must_use] annotation
        false
    }
}

/// Helper to check if a pattern is a wildcard or starts with underscore
pub fn is_ignored_pattern(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Wildcard(_) => true,
        Pattern::Ident { name, .. } => name.name.starts_with('_'),
        _ => false,
    }
}

/// Helper to check if a name follows snake_case
pub fn is_snake_case(name: &str) -> bool {
    if name.is_empty() {
        return true;
    }
    // Check for uppercase letters
    !name.chars().any(|c| c.is_uppercase())
}

/// Helper to check if a name follows CamelCase (PascalCase)
pub fn is_camel_case(name: &str) -> bool {
    if name.is_empty() {
        return true;
    }
    // First character should be uppercase
    name.chars()
        .next()
        .map(|c| c.is_uppercase())
        .unwrap_or(false)
}

/// Helper to check if a name follows UPPER_CASE
pub fn is_upper_case(name: &str) -> bool {
    if name.is_empty() {
        return true;
    }
    // All characters should be uppercase or digits/underscores
    name.chars()
        .all(|c| c.is_uppercase() || c.is_ascii_digit() || c == '_')
}

/// Helper to get the identifier from a pattern if it's a simple binding
pub fn pattern_ident(pattern: &Pattern) -> Option<&Ident> {
    match pattern {
        Pattern::Ident { name, .. } => Some(name),
        _ => None,
    }
}

/// Helper to collect all identifiers from a pattern
pub fn collect_pattern_idents(pattern: &Pattern, idents: &mut Vec<(String, Span)>) {
    match pattern {
        Pattern::Ident { name, .. } => {
            idents.push((name.name.clone(), name.span));
        }
        Pattern::Tuple(patterns) => {
            for p in patterns {
                collect_pattern_idents(p, idents);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                if let Some(pat) = &field.pattern {
                    collect_pattern_idents(pat, idents);
                }
            }
        }
        Pattern::Enum {
            inner: Some(inner), ..
        } => {
            collect_pattern_idents(inner, idents);
        }
        Pattern::Enum { .. } => {}
        Pattern::Array(patterns) => {
            for p in patterns {
                collect_pattern_idents(p, idents);
            }
        }
        Pattern::Bind { name, pattern } => {
            idents.push((name.name.clone(), name.span));
            collect_pattern_idents(pattern, idents);
        }
        Pattern::Mut(inner) => {
            collect_pattern_idents(inner, idents);
        }
        Pattern::Ref { pattern, .. } => {
            collect_pattern_idents(pattern, idents);
        }
        Pattern::Or(left, right) => {
            collect_pattern_idents(left, idents);
            collect_pattern_idents(right, idents);
        }
        _ => {}
    }
}

/// Check if an expression is a simple literal
pub fn is_literal(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(_))
}

/// Check if an expression is a path/variable
pub fn is_path(expr: &Expr) -> bool {
    matches!(expr, Expr::Variable(_) | Expr::Path(_))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_snake_case() {
        assert!(is_snake_case("snake_case"));
        assert!(is_snake_case("simple"));
        assert!(is_snake_case("with_123"));
        assert!(!is_snake_case("CamelCase"));
        assert!(!is_snake_case("mixed_Case"));
    }

    #[test]
    fn test_camel_case() {
        assert!(is_camel_case("CamelCase"));
        assert!(is_camel_case("Simple"));
        assert!(!is_camel_case("snake_case"));
        assert!(!is_camel_case("lowercase"));
    }

    #[test]
    fn test_upper_case() {
        assert!(is_upper_case("UPPER_CASE"));
        assert!(is_upper_case("CONST"));
        assert!(is_upper_case("MAX_VALUE_123"));
        assert!(!is_upper_case("lower"));
        assert!(!is_upper_case("MixedCase"));
    }
}
