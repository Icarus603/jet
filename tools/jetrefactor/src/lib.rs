//! AI-safe refactoring tools for the Jet language
//!
//! This crate provides semantic-preserving refactoring operations that can be
//! safely applied by AI assistants. Each refactoring is verified to preserve:
//! - Same effects
//! - Same return type
//! - Same behavior for all inputs
//!
//! # Example Usage
//!
//! ```rust
//! use jet_refactor::{RefactoringEngine, RefactoringKind};
//!
//! let engine = RefactoringEngine::new();
//! let result = engine.apply_refactoring(
//!     RefactoringKind::ExtractVariable,
//!     &source_code,
//!     selection_range,
//! );
//! ```

use jet_diagnostics::{Diagnostic, Span};
use jet_parser::ast::Module;
use std::collections::HashMap;

pub mod extract;
pub mod inline;
pub mod rename;
pub mod semantic_eq;
pub mod suggestions;
pub mod verify;

pub use extract::{extract_function, extract_variable};
pub use inline::{inline_function, inline_variable};
pub use rename::rename_symbol;
pub use semantic_eq::SemanticEq;
pub use verify::RefactoringVerifier;

/// The result of a refactoring operation
#[derive(Debug, Clone)]
pub struct RefactoringResult {
    /// The transformed source code
    pub source: String,
    /// Any diagnostics generated during refactoring
    pub diagnostics: Vec<Diagnostic>,
    /// The kind of refactoring applied
    pub kind: RefactoringKind,
    /// Metadata about the refactoring
    pub metadata: RefactoringMetadata,
}

/// Metadata about a refactoring operation
#[derive(Debug, Clone, Default)]
pub struct RefactoringMetadata {
    /// Whether the refactoring preserves semantics
    pub semantic_preserving: bool,
    /// Whether the refactoring requires verification
    pub requires_verification: bool,
    /// The name of the new symbol introduced (if any)
    pub new_symbol: Option<String>,
    /// The locations of all changes made
    pub change_locations: Vec<Span>,
    /// AI tracking annotation (if applicable)
    pub tracking_annotation: Option<String>,
}

/// Kinds of refactoring operations supported
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefactoringKind {
    /// Extract selected expression into a variable
    ExtractVariable,
    /// Extract selected statements into a function
    ExtractFunction,
    /// Inline a variable (replace uses with definition)
    InlineVariable,
    /// Inline a function call (replace with body)
    InlineFunction,
    /// Rename a symbol
    RenameSymbol,
    /// Convert if-let to match expression
    ConvertIfLetToMatch,
    /// Convert loop to iterator method
    ConvertLoopToIterator,
    /// Add type annotation
    AddTypeAnnotation,
    /// Remove unused code
    RemoveUnused,
}

impl std::fmt::Display for RefactoringKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RefactoringKind::ExtractVariable => write!(f, "extract variable"),
            RefactoringKind::ExtractFunction => write!(f, "extract function"),
            RefactoringKind::InlineVariable => write!(f, "inline variable"),
            RefactoringKind::InlineFunction => write!(f, "inline function"),
            RefactoringKind::RenameSymbol => write!(f, "rename symbol"),
            RefactoringKind::ConvertIfLetToMatch => write!(f, "convert if-let to match"),
            RefactoringKind::ConvertLoopToIterator => write!(f, "convert loop to iterator"),
            RefactoringKind::AddTypeAnnotation => write!(f, "add type annotation"),
            RefactoringKind::RemoveUnused => write!(f, "remove unused code"),
        }
    }
}

/// Configuration for refactoring operations
#[derive(Debug, Clone)]
pub struct RefactoringConfig {
    /// Whether to require semantic verification before applying
    pub require_semantic_verification: bool,
    /// Whether to generate AI tracking annotations
    pub generate_tracking_annotations: bool,
    /// The name of the AI/system applying the refactoring
    pub applied_by: String,
    /// Maximum complexity for inline operations (to prevent code bloat)
    pub max_inline_complexity: usize,
    /// Whether to prefer immutable bindings in extract operations
    pub prefer_immutable: bool,
}

impl Default for RefactoringConfig {
    fn default() -> Self {
        Self {
            require_semantic_verification: true,
            generate_tracking_annotations: true,
            applied_by: "ai".to_string(),
            max_inline_complexity: 50,
            prefer_immutable: true,
        }
    }
}

/// The main refactoring engine
pub struct RefactoringEngine {
    config: RefactoringConfig,
    verifier: RefactoringVerifier,
}

impl RefactoringEngine {
    /// Create a new refactoring engine with default configuration
    pub fn new() -> Self {
        Self::with_config(RefactoringConfig::default())
    }

    /// Create a new refactoring engine with custom configuration
    pub fn with_config(config: RefactoringConfig) -> Self {
        let verifier = RefactoringVerifier::new(&config);
        Self { config, verifier }
    }

    /// Check if a refactoring can be applied at the given location
    pub fn can_apply(
        &self,
        kind: RefactoringKind,
        ast: &Module,
        span: Span,
    ) -> Result<bool, Vec<Diagnostic>> {
        match kind {
            RefactoringKind::ExtractVariable => {
                extract::can_extract_variable(ast, span)
            }
            RefactoringKind::ExtractFunction => {
                extract::can_extract_function(ast, span)
            }
            RefactoringKind::InlineVariable => {
                inline::can_inline_variable(ast, span)
            }
            RefactoringKind::InlineFunction => {
                inline::can_inline_function(ast, span)
            }
            RefactoringKind::RenameSymbol => {
                rename::can_rename_symbol(ast, span)
            }
            _ => Ok(true), // Other refactorings are context-dependent
        }
    }

    /// Apply a refactoring to the source code
    pub fn apply_refactoring(
        &self,
        kind: RefactoringKind,
        source: &str,
        span: Span,
    ) -> Result<RefactoringResult, Vec<Diagnostic>> {
        // Parse the source
        let tokens = jet_lexer::lex(source)
            .map_err(|e| vec![e])?;
        let mut parser = jet_parser::Parser::new(tokens);
        let ast = parser.parse_module()
            .map_err(|e| vec![e.into()])?;

        // Check if refactoring can be applied
        if !self.can_apply(kind, &ast, span)? {
            return Err(vec![Diagnostic::error(
                format!("Cannot apply {} at this location", kind),
                span,
            )]);
        }

        // Apply the refactoring
        let result = match kind {
            RefactoringKind::ExtractVariable => {
                extract::extract_variable(source, &ast, span, &self.config)
            }
            RefactoringKind::ExtractFunction => {
                extract::extract_function(source, &ast, span, &self.config)
            }
            RefactoringKind::InlineVariable => {
                inline::inline_variable(source, &ast, span, &self.config)
            }
            RefactoringKind::InlineFunction => {
                inline::inline_function(source, &ast, span, &self.config)
            }
            RefactoringKind::RenameSymbol => {
                rename::rename_symbol(source, &ast, span, &self.config)
            }
            _ => Err(vec![Diagnostic::error(
                format!("Refactoring {} not yet implemented", kind),
                span,
            )]),
        }?;

        // Verify semantic preservation if required
        if self.config.require_semantic_verification {
            match self.verifier.verify_semantic_preservation(source, &result.source, &ast) {
                Ok(_) => {}
                Err(e) => {
                    // Return error with verification failure
                    return Err(vec![Diagnostic::error(
                        format!("Refactoring does not preserve semantics: {}", e),
                        span,
                    )]);
                }
            }
        }

        Ok(result)
    }

    /// Get available refactorings at a given location
    pub fn available_refactorings(
        &self,
        ast: &Module,
        span: Span,
    ) -> Vec<(RefactoringKind, &'static str)> {
        let mut result = Vec::new();

        let refactorings = [
            (RefactoringKind::ExtractVariable, "Extract into variable"),
            (RefactoringKind::ExtractFunction, "Extract into function"),
            (RefactoringKind::InlineVariable, "Inline variable"),
            (RefactoringKind::InlineFunction, "Inline function"),
            (RefactoringKind::RenameSymbol, "Rename symbol"),
            (RefactoringKind::ConvertIfLetToMatch, "Convert if-let to match"),
            (RefactoringKind::ConvertLoopToIterator, "Convert loop to iterator"),
        ];

        for (kind, description) in refactorings {
            if let Ok(true) = self.can_apply(kind, ast, span) {
                result.push((kind, description));
            }
        }

        result
    }
}

impl Default for RefactoringEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Error types for refactoring operations
#[derive(Debug, thiserror::Error)]
pub enum RefactoringError {
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Semantic verification failed: {0}")]
    VerificationFailed(String),
    #[error("Invalid selection: {0}")]
    InvalidSelection(String),
    #[error("Refactoring would change semantics: {0}")]
    WouldChangeSemantics(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_refactoring_kind_display() {
        assert_eq!(
            RefactoringKind::ExtractVariable.to_string(),
            "extract variable"
        );
        assert_eq!(
            RefactoringKind::ExtractFunction.to_string(),
            "extract function"
        );
    }

    #[test]
    fn test_default_config() {
        let config = RefactoringConfig::default();
        assert!(config.require_semantic_verification);
        assert!(config.generate_tracking_annotations);
        assert_eq!(config.applied_by, "ai");
        assert_eq!(config.max_inline_complexity, 50);
        assert!(config.prefer_immutable);
    }
}
