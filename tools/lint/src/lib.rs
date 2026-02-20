//! Jet Linter - Comprehensive static analysis for code quality
//!
//! This crate provides the `jet-lint` tool for analyzing Jet source code
//! and detecting potential issues, style violations, and performance problems.
//!
//! # Architecture
//!
//! The linter is built around a plugin-based architecture with:
//! - `Lint` trait: Defines a lint check with metadata
//! - `LintPass` trait: Implements the actual checking logic
//! - `LintContext`: Provides access to AST and type information
//! - `LintLevel`: Controls severity (error, warn, info, allow, forbid)
//!
//! # Example
//!
//! ```rust,no_run
//! use std::path::Path;
//! use jet_lint::{Linter, LintConfig};
//!
//! let config = LintConfig::default();
//! let mut linter = Linter::new(config);
//! let results = linter.lint_file(Path::new("src/main.jet")).unwrap();
//! ```

#[allow(unused_imports)]
use std::collections::HashMap;
use std::path::Path;

use jet_diagnostics::Diagnostic;
use jet_lexer::Span;
use jet_parser::ast::Module;

pub mod config;
pub mod context;
pub mod lints;

pub use config::{LintConfig, LintLevel, LintOverride, OutputFormat};
pub use context::LintContext;

/// A lint check definition
pub trait Lint: Send + Sync {
    /// Unique identifier for this lint (e.g., "unused_variables")
    fn name(&self) -> &'static str;

    /// Human-readable description
    fn description(&self) -> &'static str;

    /// Default lint level
    fn default_level(&self) -> LintLevel;

    /// Category this lint belongs to
    fn category(&self) -> LintCategory;

    /// Whether this lint can be automatically fixed
    fn is_fixable(&self) -> bool {
        false
    }
}

/// A pass that performs linting on AST nodes
pub trait LintPass {
    /// Run this lint pass on the given module
    fn check_module(&mut self, cx: &mut LintContext, module: &Module);
}

/// Lint categories
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LintCategory {
    /// Code that will fail to compile or is clearly wrong
    Correctness,
    /// Code that is likely incorrect or problematic
    Suspicious,
    /// Style suggestions and improvements
    Style,
    /// Performance-related suggestions
    Performance,
    /// Complexity warnings
    Complexity,
    /// Pedantic suggestions
    Pedantic,
}

impl LintCategory {
    /// Get the default lint level for this category
    pub fn default_level(&self) -> LintLevel {
        match self {
            LintCategory::Correctness => LintLevel::Error,
            LintCategory::Suspicious => LintLevel::Warn,
            LintCategory::Style => LintLevel::Warn,
            LintCategory::Performance => LintLevel::Warn,
            LintCategory::Complexity => LintLevel::Allow,
            LintCategory::Pedantic => LintLevel::Allow,
        }
    }

    /// Get the category name
    pub fn as_str(&self) -> &'static str {
        match self {
            LintCategory::Correctness => "correctness",
            LintCategory::Suspicious => "suspicious",
            LintCategory::Style => "style",
            LintCategory::Performance => "performance",
            LintCategory::Complexity => "complexity",
            LintCategory::Pedantic => "pedantic",
        }
    }
}

/// A single lint violation
#[derive(Debug, Clone)]
pub struct LintViolation {
    /// The lint that was triggered
    pub lint_name: String,
    /// Severity level
    pub level: LintLevel,
    /// Message describing the issue
    pub message: String,
    /// Source location
    pub span: Span,
    /// Suggested fix (if available)
    pub suggestion: Option<String>,
    /// Whether this can be auto-fixed
    pub fixable: bool,
}

impl LintViolation {
    /// Create a new lint violation
    pub fn new(
        lint_name: impl Into<String>,
        level: LintLevel,
        message: impl Into<String>,
        span: Span,
    ) -> Self {
        Self {
            lint_name: lint_name.into(),
            level,
            message: message.into(),
            span,
            suggestion: None,
            fixable: false,
        }
    }

    /// Add a suggestion to this violation
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// Mark this violation as fixable
    pub fn with_fix(mut self) -> Self {
        self.fixable = true;
        self
    }

    /// Convert to a diagnostic
    pub fn to_diagnostic(&self) -> Diagnostic {
        // Convert jet_lexer::Span to jet_diagnostics::Span
        let diag_span = jet_diagnostics::Span::new(self.span.start, self.span.end);

        let mut diag = match self.level {
            LintLevel::Error | LintLevel::Deny | LintLevel::Forbid => {
                Diagnostic::error(&self.message, diag_span)
            }
            LintLevel::Warn => Diagnostic::warning(&self.message, diag_span),
            LintLevel::Info => Diagnostic::note(&self.message, diag_span),
            LintLevel::Allow => Diagnostic::help(&self.message, diag_span),
        };

        if let Some(ref suggestion) = self.suggestion {
            diag = diag.with_note(suggestion.as_str());
        }

        diag
    }
}

/// The main linter struct
pub struct Linter {
    config: LintConfig,
    passes: Vec<Box<dyn LintPass>>,
}

impl Linter {
    /// Create a new linter with the given configuration
    pub fn new(config: LintConfig) -> Self {
        let mut linter = Self {
            config,
            passes: Vec::new(),
        };
        linter.register_default_passes();
        linter
    }

    /// Register all default lint passes
    fn register_default_passes(&mut self) {
        // Correctness lints
        self.passes
            .push(Box::new(lints::correctness::UnreachableCodeLint));
        self.passes
            .push(Box::new(lints::correctness::UnusedMustUseLint));
        self.passes
            .push(Box::new(lints::correctness::InvalidDocAttributesLint));
        self.passes
            .push(Box::new(lints::correctness::DuplicateMatchArmsLint));

        // Suspicious lints
        self.passes
            .push(Box::new(lints::suspicious::UnusedVariablesLint));
        self.passes
            .push(Box::new(lints::suspicious::UnusedImportsLint));
        self.passes.push(Box::new(lints::suspicious::DeadCodeLint));
        self.passes.push(Box::new(lints::suspicious::ShadowingLint));
        self.passes.push(Box::new(lints::suspicious::UnusedMutLint));
        self.passes
            .push(Box::new(lints::suspicious::SingleMatchLint));
        self.passes
            .push(Box::new(lints::suspicious::NeedlessBorrowLint));

        // Style lints
        self.passes
            .push(Box::new(lints::style::NamingConventionsLint));
        self.passes.push(Box::new(lints::style::MissingDocsLint));
        self.passes.push(Box::new(lints::style::TrivialCastsLint));

        // Performance lints
        self.passes
            .push(Box::new(lints::performance::InefficientToStringLint));
        self.passes
            .push(Box::new(lints::performance::LargeEnumVariantLint));
        self.passes
            .push(Box::new(lints::performance::NeedlessAllocationLint));
        self.passes
            .push(Box::new(lints::performance::RedundantCloneLint));
    }

    /// Lint a single file
    pub fn lint_file(&mut self, path: &Path) -> anyhow::Result<LintResult> {
        let source = std::fs::read_to_string(path)?;
        self.lint_source(&source, Some(path.to_string_lossy().to_string()))
    }

    /// Lint source code directly
    pub fn lint_source(
        &mut self,
        source: &str,
        filename: Option<String>,
    ) -> anyhow::Result<LintResult> {
        use jet_lexer::Lexer;
        use jet_parser::Parser;

        // Parse the source
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();

        let mut parser = Parser::new(tokens);
        let module = parser.parse_module()?;

        // Create context and run lints
        let mut cx = LintContext::new(source, filename);

        for pass in &mut self.passes {
            pass.check_module(&mut cx, &module);
        }

        // Filter violations based on configuration
        let violations: Vec<LintViolation> = cx
            .violations
            .into_iter()
            .filter(|v| self.config.is_enabled(&v.lint_name))
            .map(|mut v| {
                v.level = self.config.effective_level(&v.lint_name, v.level);
                v
            })
            .collect();

        let error_count = violations.iter().filter(|v| v.level.is_error()).count();
        let warning_count = violations
            .iter()
            .filter(|v| v.level == LintLevel::Warn)
            .count();
        let fixable_count = violations.iter().filter(|v| v.fixable).count();

        Ok(LintResult {
            violations,
            error_count,
            warning_count,
            fixable_count,
        })
    }

    /// Get the linter configuration
    pub fn config(&self) -> &LintConfig {
        &self.config
    }
}

impl Default for Linter {
    fn default() -> Self {
        Self::new(LintConfig::default())
    }
}

/// Result of linting
#[derive(Debug, Clone)]
pub struct LintResult {
    /// All violations found
    pub violations: Vec<LintViolation>,
    /// Number of errors
    pub error_count: usize,
    /// Number of warnings
    pub warning_count: usize,
    /// Number of auto-fixable issues
    pub fixable_count: usize,
}

impl LintResult {
    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    /// Check if there are any warnings
    pub fn has_warnings(&self) -> bool {
        self.warning_count > 0
    }

    /// Get violations by level
    pub fn violations_by_level(&self, level: LintLevel) -> Vec<&LintViolation> {
        self.violations
            .iter()
            .filter(|v| v.level == level)
            .collect()
    }

    /// Convert all violations to diagnostics
    pub fn to_diagnostics(&self) -> Vec<Diagnostic> {
        self.violations.iter().map(|v| v.to_diagnostic()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lint_violation_creation() {
        let violation = LintViolation::new(
            "test_lint",
            LintLevel::Warn,
            "This is a test",
            Span::new(0, 10),
        );

        assert_eq!(violation.lint_name, "test_lint");
        assert_eq!(violation.level, LintLevel::Warn);
        assert_eq!(violation.message, "This is a test");
        assert!(!violation.fixable);
    }

    #[test]
    fn test_lint_violation_with_suggestion() {
        let violation = LintViolation::new(
            "test_lint",
            LintLevel::Warn,
            "This is a test",
            Span::new(0, 10),
        )
        .with_suggestion("Try this instead");

        assert_eq!(violation.suggestion, Some("Try this instead".to_string()));
    }

    #[test]
    fn test_lint_result_stats() {
        let result = LintResult {
            violations: vec![
                LintViolation::new("lint1", LintLevel::Error, "error", Span::new(0, 1)),
                LintViolation::new("lint2", LintLevel::Warn, "warning", Span::new(0, 1)),
                LintViolation::new("lint3", LintLevel::Warn, "warning2", Span::new(0, 1)),
            ],
            error_count: 1,
            warning_count: 2,
            fixable_count: 0,
        };

        assert!(result.has_errors());
        assert!(result.has_warnings());
        assert_eq!(result.violations_by_level(LintLevel::Error).len(), 1);
        assert_eq!(result.violations_by_level(LintLevel::Warn).len(), 2);
    }
}
