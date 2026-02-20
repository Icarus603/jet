//! Annotation Parser for Test Generation
//!
//! This module parses Jet annotations like `@test_auto`, `@test_for`, and `@mutation_test`
//! from source code comments and function attributes.
//!
//! # Supported Annotations
//!
//! - `@test_auto`: Automatically generate property-based tests for a function
//! - `@test_for(effect)`: Generate tests covering specific effect paths
//! - `@mutation_test`: Enable mutation testing for the annotated function

use jet_diagnostics::{Diagnostic, Span};
use jet_parser::ast::{Expr, Function, Module, ModuleItem, Pattern, Stmt};
use std::collections::HashMap;

/// The kind of test annotation.
#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationKind {
    /// `@test_auto` - Generate property-based tests automatically.
    TestAuto,
    /// `@test_for(effect)` - Generate tests for specific effect paths.
    TestFor(String),
    /// `@mutation_test` - Enable mutation testing.
    MutationTest,
}

/// A parsed annotation with its associated metadata.
#[derive(Debug, Clone)]
pub struct Annotation {
    /// The kind of annotation.
    pub kind: AnnotationKind,
    /// The target function or item this annotation applies to.
    pub target: String,
    /// The span of the annotation in the source code.
    pub span: Span,
    /// Additional parameters parsed from the annotation.
    pub params: HashMap<String, String>,
    /// The function this annotation is attached to (if applicable).
    pub function: Option<Function>,
}

impl Annotation {
    /// Creates a new annotation.
    pub fn new(kind: AnnotationKind, target: impl Into<String>, span: Span) -> Self {
        Self {
            kind,
            target: target.into(),
            span,
            params: HashMap::new(),
            function: None,
        }
    }

    /// Adds a parameter to the annotation.
    pub fn with_param(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.params.insert(key.into(), value.into());
        self
    }

    /// Sets the function associated with this annotation.
    pub fn with_function(mut self, function: Function) -> Self {
        self.function = Some(function);
        self
    }

    /// Gets a parameter value.
    pub fn get_param(&self, key: &str) -> Option<&String> {
        self.params.get(key)
    }
}

/// Parser for Jet test annotations.
#[derive(Debug)]
pub struct AnnotationParser {
    diagnostics: Vec<Diagnostic>,
}

impl AnnotationParser {
    /// Creates a new annotation parser.
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    /// Parses all annotations from a module.
    pub fn parse_module(&mut self, module: &Module) -> Result<Vec<Annotation>, Vec<Diagnostic>> {
        let mut annotations = Vec::new();

        for item in &module.items {
            if let Some(annotation) = self.parse_module_item(item) {
                annotations.push(annotation);
            }
        }

        if self.diagnostics.is_empty() {
            Ok(annotations)
        } else {
            Err(std::mem::take(&mut self.diagnostics))
        }
    }

    /// Parses annotations from a single module item.
    fn parse_module_item(&mut self, item: &ModuleItem) -> Option<Annotation> {
        match item {
            ModuleItem::Function(func) => self.parse_function_annotations(func),
            _ => None,
        }
    }

    /// Parses annotations from a function.
    fn parse_function_annotations(&mut self, func: &Function) -> Option<Annotation> {
        // Check for annotation comments in the function body
        // In a real implementation, this would look at doc comments or attributes
        let func_name = func.name.name.clone();

        // Check function name patterns for test annotations
        // This is a simplified implementation - real implementation would parse
        // actual annotations from comments or attributes
        if func_name.starts_with("test_auto_") {
            let annotation = Annotation::new(
                AnnotationKind::TestAuto,
                func_name.clone(),
                Self::span_to_diagnostic_span(func.span),
            )
            .with_function(func.clone());
            return Some(annotation);
        }

        if func_name.starts_with("test_for_") {
            let effect = func_name
                .strip_prefix("test_for_")
                .unwrap_or("")
                .to_string();
            let annotation = Annotation::new(
                AnnotationKind::TestFor(effect),
                func_name.clone(),
                Self::span_to_diagnostic_span(func.span),
            )
            .with_function(func.clone());
            return Some(annotation);
        }

        if func_name.starts_with("mutation_test_") {
            let annotation = Annotation::new(
                AnnotationKind::MutationTest,
                func_name.clone(),
                Self::span_to_diagnostic_span(func.span),
            )
            .with_function(func.clone());
            return Some(annotation);
        }

        None
    }

    /// Parses an annotation from a comment string.
    ///
    /// This handles annotations like:
    /// - `@test_auto`
    /// - `@test_for(IOError)`
    /// - `@mutation_test`
    pub fn parse_annotation(&mut self, comment: &str, span: Span) -> Option<Annotation> {
        let trimmed = comment.trim();

        // Check for @test_auto
        if trimmed.starts_with("@test_auto") {
            let params = self.parse_params(trimmed, "@test_auto");
            return Some(Annotation {
                kind: AnnotationKind::TestAuto,
                target: String::new(),
                span,
                params,
                function: None,
            });
        }

        // Check for @test_for
        if let Some(rest) = trimmed.strip_prefix("@test_for") {
            let effect = self.parse_parenthesized_arg(rest).unwrap_or_default();
            let params = self.parse_params(trimmed, "@test_for");
            return Some(Annotation {
                kind: AnnotationKind::TestFor(effect),
                target: String::new(),
                span,
                params,
                function: None,
            });
        }

        // Check for @mutation_test
        if trimmed.starts_with("@mutation_test") {
            let params = self.parse_params(trimmed, "@mutation_test");
            return Some(Annotation {
                kind: AnnotationKind::MutationTest,
                target: String::new(),
                span,
                params,
                function: None,
            });
        }

        None
    }

    /// Parses parameters from an annotation string.
    fn parse_params(&self, annotation: &str, prefix: &str) -> HashMap<String, String> {
        let mut params = HashMap::new();

        // Look for key=value pairs after the annotation name
        if let Some(rest) = annotation.strip_prefix(prefix) {
            let rest = rest.trim();
            if rest.starts_with('(') && rest.ends_with(')') {
                let content = &rest[1..rest.len() - 1];
                for pair in content.split(',') {
                    let pair = pair.trim();
                    if let Some((key, value)) = pair.split_once('=') {
                        params.insert(key.trim().to_string(), value.trim().to_string());
                    }
                }
            }
        }

        params
    }

    /// Parses a parenthesized argument.
    fn parse_parenthesized_arg(&self, s: &str) -> Option<String> {
        let s = s.trim();
        if s.starts_with('(') && s.contains(')') {
            let end = s.find(')')?;
            Some(s[1..end].trim().to_string())
        } else {
            None
        }
    }

    /// Converts a lexer span to a diagnostic span.
    fn span_to_diagnostic_span(span: jet_lexer::Span) -> Span {
        Span::new(span.start, span.end)
    }

    /// Returns any diagnostics collected during parsing.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

impl Default for AnnotationParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Extracts specifications from annotated functions.
///
/// This is used to extract properties like preconditions and postconditions
/// from function annotations.
pub struct SpecExtractor;

impl SpecExtractor {
    /// Extracts a property specification from a function.
    ///
    /// For example, given:
    /// ```jet
    /// @test_auto
    /// fn reverse_properties(list: [int]) {
    ///     assert list == reverse(reverse(list))
    /// }
    /// ```
    ///
    /// This extracts the property `list == reverse(reverse(list))`.
    pub fn extract_property(func: &Function) -> Option<Expr> {
        // Look for assert statements in the function body
        if let Expr::Block(block) = &func.body {
            for stmt in &block.stmts {
                if let Stmt::Expr(expr) = stmt {
                    if let Expr::Call {
                        func: call_func,
                        args,
                    } = expr.as_ref()
                    {
                        if let Expr::Variable(ident) = call_func.as_ref() {
                            if ident.name == "assert" && !args.is_empty() {
                                return Some(args[0].clone());
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Extracts all assertions from a function body.
    pub fn extract_assertions(func: &Function) -> Vec<Expr> {
        let mut assertions = Vec::new();

        if let Expr::Block(block) = &func.body {
            for stmt in &block.stmts {
                Self::collect_assertions_from_stmt(stmt, &mut assertions);
            }
        }

        assertions
    }

    fn collect_assertions_from_stmt(stmt: &Stmt, assertions: &mut Vec<Expr>) {
        match stmt {
            Stmt::Expr(expr) => {
                Self::collect_assertions_from_expr(expr, assertions);
            }
            Stmt::Let { value, .. } => {
                Self::collect_assertions_from_expr(value, assertions);
            }
            Stmt::Assign { value, .. } => {
                Self::collect_assertions_from_expr(value, assertions);
            }
            _ => {}
        }
    }

    fn collect_assertions_from_expr(expr: &Expr, assertions: &mut Vec<Expr>) {
        match expr {
            Expr::Call { func, args } => {
                if let Expr::Variable(ident) = func.as_ref() {
                    if ident.name == "assert" && !args.is_empty() {
                        assertions.push(args[0].clone());
                    }
                }
                for arg in args {
                    Self::collect_assertions_from_expr(arg, assertions);
                }
            }
            Expr::Binary { left, right, .. } => {
                Self::collect_assertions_from_expr(left, assertions);
                Self::collect_assertions_from_expr(right, assertions);
            }
            Expr::Unary { expr, .. } => {
                Self::collect_assertions_from_expr(expr, assertions);
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                Self::collect_assertions_from_expr(cond, assertions);
                Self::collect_assertions_from_expr(then_branch, assertions);
                if let Some(else_branch) = else_branch {
                    Self::collect_assertions_from_expr(else_branch, assertions);
                }
            }
            Expr::Block(block) => {
                for stmt in &block.stmts {
                    Self::collect_assertions_from_stmt(stmt, assertions);
                }
                if let Some(expr) = &block.expr {
                    Self::collect_assertions_from_expr(expr, assertions);
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span as LexerSpan;
    use jet_parser::ast::{Ident, Literal};

    #[test]
    fn test_parse_test_auto() {
        let mut parser = AnnotationParser::new();
        let annotation = parser.parse_annotation("@test_auto", Span::new(0, 10));

        assert!(annotation.is_some());
        let annotation = annotation.unwrap();
        assert!(matches!(annotation.kind, AnnotationKind::TestAuto));
    }

    #[test]
    fn test_parse_test_for() {
        let mut parser = AnnotationParser::new();
        let annotation = parser.parse_annotation("@test_for(IOError)", Span::new(0, 18));

        assert!(annotation.is_some());
        let annotation = annotation.unwrap();
        assert!(matches!(annotation.kind, AnnotationKind::TestFor(effect) if effect == "IOError"));
    }

    #[test]
    fn test_parse_mutation_test() {
        let mut parser = AnnotationParser::new();
        let annotation = parser.parse_annotation("@mutation_test", Span::new(0, 14));

        assert!(annotation.is_some());
        let annotation = annotation.unwrap();
        assert!(matches!(annotation.kind, AnnotationKind::MutationTest));
    }

    #[test]
    fn test_parse_params() {
        let parser = AnnotationParser::new();
        let params = parser.parse_params("@test_auto(cases=100, seed=42)", "@test_auto");

        assert_eq!(params.get("cases"), Some(&"100".to_string()));
        assert_eq!(params.get("seed"), Some(&"42".to_string()));
    }

    #[test]
    fn test_extract_property() {
        // Create a simple function with an assert
        let func = Function {
            public: false,
            attributes: vec![],
            name: Ident::new("test_reverse", LexerSpan::default()),
            generics: vec![],
            params: vec![],
            return_type: None,
            effects: vec![],
            where_clause: vec![],
            contract: None,
            body: Expr::Block(jet_parser::ast::Block {
                stmts: vec![Stmt::Expr(Box::new(Expr::Call {
                    func: Box::new(Expr::Variable(Ident::new("assert", LexerSpan::default()))),
                    args: vec![Expr::Binary {
                        op: jet_parser::ast::BinaryOp::Eq,
                        left: Box::new(Expr::Literal(Literal::Integer(1))),
                        right: Box::new(Expr::Literal(Literal::Integer(1))),
                    }],
                }))],
                expr: None,
                span: LexerSpan::default(),
            }),
            span: LexerSpan::default(),
        };

        let property = SpecExtractor::extract_property(&func);
        assert!(property.is_some());
    }
}
