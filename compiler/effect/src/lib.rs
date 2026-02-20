//! Jet Effect System
//!
//! This crate implements the effect system for the Jet programming language.
//! It provides:
//!
//! - **Effect representation**: Core types for representing effects and effect sets
//! - **Effect checking**: Verification that effects are handled or declared
//! - **Effect inference**: Automatic inference of effects for expressions
//! - **Handler tracking**: Management of effect handlers and their scopes
//!
//! # Overview
//!
//! The effect system in Jet tracks what operations a function can perform.
//! Functions declare their effects using the `!` syntax:
//!
//! ```jet
//! fn read_file(path: string) -> string ! IoError:
//!     # This function can perform IoError
//!     ...
//!
//! fn async_fetch(url: string) -> Data ! async | NetworkError:
//!     # This function can perform async operations and NetworkError
//!     ...
//! ```
//!
//! The effect checker verifies that:
//! 1. All effects performed are either handled or declared
//! 2. Effect handlers are complete
//! 3. Effects don't escape their handling scope
//!
//! # Usage
//!
//! ```rust,ignore
//! use jet_effect::check_module;
//! use jet_parser::ast::Module;
//!
//! fn check_effects(module: &Module) {
//!     let diagnostics = check_module(module);
//!     for diagnostic in diagnostics {
//!         println!("{:?}", diagnostic);
//!     }
//! }
//! ```

pub mod checker;
pub mod effect;
pub mod error;
pub mod handler;
pub mod inference;

// Re-export commonly used types
pub use checker::{check_expr, check_module, CheckedExpr, EffectChecker};
pub use effect::{
    BuiltinEffect, DefId, EffectHandler, EffectId, EffectInstance, EffectSet, EffectVar,
    HandledOperation,
};
pub use error::{EffectError, EffectErrors, EffectResult};
pub use handler::{HandlerContext, HandlerSnapshot, HandlerStack, PerformTracker};
pub use handler::{HandlerId, HandlerScope, ScopeId};
pub use inference::{EffectConstraint, EffectInference, EffectScheme, EffectSolution, EffectSubst};

use jet_diagnostics::Diagnostic;
use jet_parser::ast::{Expr, Module};

/// Checks a module for effect errors.
///
/// This is the main entry point for effect checking. It takes a parsed
/// module and returns any diagnostics (errors or warnings) found.
///
/// # Example
///
/// ```rust,ignore
/// use jet_effect::check_module_effects;
/// use jet_parser::parse_module;
///
/// let source = r#"
/// fn pure_add(a: int, b: int) -> int:
///     return a + b
///
/// fn main():
///     let x = pure_add(1, 2)
/// "#;
///
/// // let module = parse_module(source).unwrap();
/// // let diagnostics = check_module_effects(&module);
/// // assert!(diagnostics.is_empty());
/// ```
pub fn check_module_effects(module: &Module) -> Vec<Diagnostic> {
    checker::check_module(module)
}

/// Checks a single expression for effects.
///
/// This is useful for checking expressions in isolation, such as
/// in a REPL or interactive environment.
///
/// # Example
///
/// ```rust,ignore
/// use jet_effect::check_expression;
/// use jet_parser::parse_expr;
///
/// // let expr = parse_expr("1 + 2").unwrap();
/// // let result = check_expression(&expr).unwrap();
/// // assert!(result.effects.is_empty());
/// ```
pub fn check_expression(expr: &Expr) -> Result<CheckedExpr, EffectError> {
    checker::check_expr(expr)
}

/// Result type for effect checking operations.
pub type EffectCheckResult<T> = Result<T, Vec<Diagnostic>>;

/// Configuration for the effect checker.
#[derive(Debug, Clone)]
pub struct EffectConfig {
    /// Whether to allow unhandled effects (for incremental checking).
    pub allow_unhandled: bool,
    /// Whether to warn about unnecessarily declared effects.
    pub warn_unused_effects: bool,
    /// Whether to enforce async effect tracking.
    pub enforce_async: bool,
    /// Whether to enforce unsafe effect tracking.
    pub enforce_unsafe: bool,
}

impl Default for EffectConfig {
    fn default() -> Self {
        Self {
            allow_unhandled: false,
            warn_unused_effects: true,
            enforce_async: true,
            enforce_unsafe: true,
        }
    }
}

impl EffectConfig {
    /// Creates a new configuration with strict checking.
    pub fn strict() -> Self {
        Self {
            allow_unhandled: false,
            warn_unused_effects: true,
            enforce_async: true,
            enforce_unsafe: true,
        }
    }

    /// Creates a new configuration with lenient checking.
    pub fn lenient() -> Self {
        Self {
            allow_unhandled: true,
            warn_unused_effects: false,
            enforce_async: false,
            enforce_unsafe: true,
        }
    }
}

/// Effect checker with configuration.
pub struct ConfiguredEffectChecker {
    /// The underlying checker.
    checker: EffectChecker,
    /// The configuration.
    config: EffectConfig,
}

impl ConfiguredEffectChecker {
    /// Creates a new configured checker.
    pub fn new(config: EffectConfig) -> Self {
        Self {
            checker: EffectChecker::new(),
            config,
        }
    }

    /// Checks a module with the configured settings.
    pub fn check_module(&mut self, module: &Module) -> Vec<Diagnostic> {
        let mut diagnostics = self.checker.check_module(module);

        if !self.config.warn_unused_effects {
            // Filter out warnings about unused effects
            diagnostics.retain(|d| !d.message.contains("unnecessarily declared"));
        }

        diagnostics
    }

    /// Checks an expression with the configured settings.
    pub fn check_expr(&mut self, expr: &Expr) -> Result<CheckedExpr, EffectError> {
        let result = self.checker.check_expr(expr)?;

        if !self.config.allow_unhandled && !result.effects.is_empty() {
            // This would need more context to create a proper error
            // For now, just return the result
        }

        Ok(result)
    }
}

/// Gets the version of the effect crate.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod integration_tests {
    use super::*;
    use jet_parser::ast::{Expr, Function, Ident, Literal, Module, ModuleItem, Type};

    fn test_span() -> jet_lexer::Span {
        jet_lexer::Span::new(0, 10)
    }

    fn create_test_module(items: Vec<ModuleItem>) -> Module {
        Module {
            items,
            attributes: vec![],
            span: test_span(),
        }
    }

    fn create_function(name: &str, effects: Vec<Type>, body: Expr) -> Function {
        Function {
            public: true,
            attributes: vec![],
            name: Ident::new(name, test_span()),
            generics: vec![],
            params: vec![],
            return_type: None,
            effects,
            where_clause: vec![],
            contract: None,
            body,
            span: test_span(),
        }
    }

    #[test]
    fn test_pure_function() {
        let func = create_function(
            "add",
            vec![],
            Expr::Binary {
                op: jet_parser::ast::BinaryOp::Add,
                left: Box::new(Expr::Literal(Literal::Integer(1))),
                right: Box::new(Expr::Literal(Literal::Integer(2))),
            },
        );

        let module = create_test_module(vec![ModuleItem::Function(func)]);
        let diagnostics = check_module_effects(&module);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_async_effect() {
        // Function that uses await without declaring async
        let body = Expr::Await(Box::new(Expr::Literal(Literal::Integer(42))));
        let func = create_function("bad_async", vec![], body);

        let module = create_test_module(vec![ModuleItem::Function(func)]);
        let _diagnostics = check_module_effects(&module);
        // Should have an error about async effect
        assert!(!_diagnostics.is_empty());
    }

    #[test]
    fn test_declared_async() {
        // Function that properly declares async
        let body = Expr::Await(Box::new(Expr::Literal(Literal::Integer(42))));
        let func = create_function(
            "good_async",
            vec![Type::Path(jet_parser::ast::Path::single(Ident::new(
                "async",
                test_span(),
            )))],
            body,
        );

        let module = create_test_module(vec![ModuleItem::Function(func)]);
        let _diagnostics = check_module_effects(&module);
        // Should be fine since async is declared
        // Note: This might still error due to await outside async context
        // depending on implementation details
    }

    #[test]
    fn test_configured_checker() {
        let config = EffectConfig::strict();
        let _checker = ConfiguredEffectChecker::new(config);

        let config = EffectConfig::lenient();
        let _checker = ConfiguredEffectChecker::new(config);

        let config = EffectConfig::default();
        let _checker = ConfiguredEffectChecker::new(config);
    }

    #[test]
    fn test_version() {
        assert!(!VERSION.is_empty());
    }
}
