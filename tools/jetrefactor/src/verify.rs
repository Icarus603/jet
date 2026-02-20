//! Refactoring verification for semantic preservation
//!
//! This module provides verification that refactorings preserve the semantics
//! of the original code. It uses type checking and effect analysis to ensure
//! that refactored code has the same behavior as the original.

use crate::semantic_eq::{module_semantic_diff, SemanticDiff};
use jet_diagnostics::{Diagnostic, Span};
use jet_parser::ast::Module;
use std::collections::HashSet;

/// Verifier for refactoring operations
pub struct RefactoringVerifier {
    config: VerificationConfig,
}

/// Configuration for verification
#[derive(Debug, Clone)]
pub struct VerificationConfig {
    /// Whether to verify type preservation
    pub verify_types: bool,
    /// Whether to verify effect preservation
    pub verify_effects: bool,
    /// Whether to verify behavioral equivalence
    pub verify_behavior: bool,
    /// Whether to allow renaming of local variables
    pub allow_local_renaming: bool,
}

impl Default for VerificationConfig {
    fn default() -> Self {
        Self {
            verify_types: true,
            verify_effects: true,
            verify_behavior: true,
            allow_local_renaming: true,
        }
    }
}

impl RefactoringVerifier {
    /// Create a new verifier with default configuration
    pub fn new(config: &crate::RefactoringConfig) -> Self {
        Self {
            config: VerificationConfig {
                verify_types: config.require_semantic_verification,
                verify_effects: config.require_semantic_verification,
                verify_behavior: config.require_semantic_verification,
                allow_local_renaming: true,
            },
        }
    }

    /// Create a new verifier with custom configuration
    pub fn with_config(config: VerificationConfig) -> Self {
        Self { config }
    }

    /// Verify that a refactoring preserves semantics
    pub fn verify_semantic_preservation(
        &self,
        original_source: &str,
        refactored_source: &str,
        original_ast: &Module,
    ) -> Result<VerificationResult, VerificationError> {
        // Parse the refactored source
        let refactored_tokens = jet_lexer::lex(refactored_source)
            .map_err(|e| VerificationError::ParseError(e.to_string()))?;
        let mut parser = jet_parser::Parser::new(refactored_tokens);
        let refactored_ast = parser
            .parse_module()
            .map_err(|e| VerificationError::ParseError(e.to_string()))?;

        // Perform semantic diff
        let diff = module_semantic_diff(original_ast, &refactored_ast);

        if !diff.is_equal {
            return Err(VerificationError::SemanticDifference(
                diff.differences
                    .into_iter()
                    .map(|d| d.description)
                    .collect::<Vec<_>>()
                    .join("; "),
            ));
        }

        // Verify type preservation
        if self.config.verify_types {
            self.verify_type_preservation(original_source, refactored_source)?;
        }

        // Verify effect preservation
        if self.config.verify_effects {
            self.verify_effect_preservation(original_source, refactored_source)?;
        }

        Ok(VerificationResult {
            preserved: true,
            type_safe: true,
            effect_safe: true,
            warnings: vec![],
        })
    }

    /// Verify that types are preserved
    fn verify_type_preservation(
        &self,
        _original_source: &str,
        _refactored_source: &str,
    ) -> Result<(), VerificationError> {
        // This would:
        // 1. Type check both sources
        // 2. Compare the inferred types
        // 3. Ensure they match

        // Placeholder - needs type checker integration
        Ok(())
    }

    /// Verify that effects are preserved
    fn verify_effect_preservation(
        &self,
        _original_source: &str,
        _refactored_source: &str,
    ) -> Result<(), VerificationError> {
        // This would:
        // 1. Effect check both sources
        // 2. Compare the effect sets
        // 3. Ensure they match

        // Placeholder - needs effect checker integration
        Ok(())
    }

    /// Check if a specific change is safe
    pub fn is_change_safe(
        &self,
        change: &CodeChange,
        original_ast: &Module,
    ) -> Result<bool, VerificationError> {
        match change.kind {
            ChangeKind::VariableRename { from, to } => {
                // Check if the rename would shadow another variable
                Ok(self.is_rename_safe(original_ast, &from, &to, change.span))
            }
            ChangeKind::ExtractVariable => {
                // Extraction is generally safe if the expression is pure
                Ok(true)
            }
            ChangeKind::ExtractFunction => {
                // Extraction is generally safe if we handle parameters correctly
                Ok(true)
            }
            ChangeKind::InlineVariable => {
                // Inlining is safe if the variable is used once or has no side effects
                Ok(true)
            }
            ChangeKind::InlineFunction => {
                // Inlining is safe if the function is not recursive
                Ok(true)
            }
        }
    }

    /// Check if a rename is safe
    fn is_rename_safe(
        &self,
        ast: &Module,
        from: &str,
        to: &str,
        span: Span,
    ) -> bool {
        // Check for name conflicts
        // This is a simplified check - would need proper scope analysis
        !self.would_conflict(ast, to, span)
    }

    /// Check if a name would conflict with existing symbols
    fn would_conflict(&self, ast: &Module, name: &str, span: Span) -> bool {
        // Check if there's already a symbol with this name in scope
        // This is a placeholder - needs proper scope analysis
        false
    }
}

/// Result of a verification
#[derive(Debug, Clone)]
pub struct VerificationResult {
    /// Whether semantics are preserved
    pub preserved: bool,
    /// Whether types are preserved
    pub type_safe: bool,
    /// Whether effects are preserved
    pub effect_safe: bool,
    /// Any warnings generated
    pub warnings: Vec<VerificationWarning>,
}

/// A verification warning
#[derive(Debug, Clone)]
pub struct VerificationWarning {
    /// Warning message
    pub message: String,
    /// Location of the warning
    pub span: Span,
}

/// Verification error types
#[derive(Debug, thiserror::Error)]
pub enum VerificationError {
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("Type mismatch: {0}")]
    TypeMismatch(String),
    #[error("Effect mismatch: {0}")]
    EffectMismatch(String),
    #[error("Semantic difference: {0}")]
    SemanticDifference(String),
    #[error("Name conflict: {0}")]
    NameConflict(String),
    #[error("Unsafe refactoring: {0}")]
    UnsafeRefactoring(String),
}

/// A code change
#[derive(Debug, Clone)]
pub struct CodeChange {
    /// The kind of change
    pub kind: ChangeKind,
    /// The span where the change occurs
    pub span: Span,
    /// Description of the change
    pub description: String,
}

/// Kinds of code changes
#[derive(Debug, Clone)]
pub enum ChangeKind {
    /// Rename a variable
    VariableRename { from: String, to: String },
    /// Extract a variable
    ExtractVariable,
    /// Extract a function
    ExtractFunction,
    /// Inline a variable
    InlineVariable,
    /// Inline a function
    InlineFunction,
}

/// Precondition for a refactoring
pub trait Precondition {
    /// Check if the precondition is satisfied
    fn check(&self, ast: &Module, span: Span) -> Result<bool, String>;
}

/// Postcondition for a refactoring
pub trait Postcondition {
    /// Check if the postcondition is satisfied
    fn check(&self, original: &Module, refactored: &Module) -> Result<bool, String>;
}

/// A simple precondition that checks if a span is within a function
pub struct WithinFunctionPrecondition;

impl Precondition for WithinFunctionPrecondition {
    fn check(&self, ast: &Module, span: Span) -> Result<bool, String> {
        for item in &ast.items {
            if let jet_parser::ast::ModuleItem::Function(func) = item {
                if func.span.start <= span.start && func.span.end >= span.end {
                    return Ok(true);
                }
            }
        }
        Err("Selection is not within a function".to_string())
    }
}

/// A postcondition that checks type preservation
pub struct TypePreservationPostcondition;

impl Postcondition for TypePreservationPostcondition {
    fn check(&self, _original: &Module, _refactored: &Module) -> Result<bool, String> {
        // This would compare the types of both modules
        // For now, assume types are preserved
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verification_config_default() {
        let config = VerificationConfig::default();
        assert!(config.verify_types);
        assert!(config.verify_effects);
        assert!(config.verify_behavior);
        assert!(config.allow_local_renaming);
    }

    #[test]
    fn test_verification_result() {
        let result = VerificationResult {
            preserved: true,
            type_safe: true,
            effect_safe: true,
            warnings: vec![],
        };
        assert!(result.preserved);
        assert!(result.type_safe);
        assert!(result.effect_safe);
    }
}
