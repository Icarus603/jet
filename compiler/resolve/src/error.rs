//! Error types and reporting for name resolution
//!
//! This module defines the error types that can occur during name resolution
//! and provides helper functions for creating diagnostic messages.

use crate::def_id::DefId;
use crate::symbol::BindingKind;
use jet_diagnostics::{Diagnostic, ErrorCode, Label, Span};
use jet_lexer::Span as LexerSpan;

/// Convert a lexer Span to a diagnostics Span
pub fn to_diag_span(span: LexerSpan) -> jet_diagnostics::Span {
    jet_diagnostics::Span::new(span.start, span.end)
}

/// Errors that can occur during name resolution
#[derive(Debug, Clone, PartialEq)]
pub enum ResolutionError {
    /// A name could not be resolved
    UnresolvedName {
        name: String,
        span: LexerSpan,
        suggestion: Option<String>,
    },

    /// A duplicate definition was found
    DuplicateDefinition {
        name: String,
        span: LexerSpan,
        previous_span: LexerSpan,
        kind: BindingKind,
    },

    /// Attempted to access a private item from outside its module
    PrivateItemAccess {
        name: String,
        span: LexerSpan,
        definition_span: LexerSpan,
        definition_module: DefId,
    },

    /// A module was not found
    ModuleNotFound { path: Vec<String>, span: LexerSpan },

    /// A circular dependency was detected
    CircularDependency { path: Vec<String>, span: LexerSpan },

    /// An invalid path was used
    InvalidPath { message: String, span: LexerSpan },

    /// A name is ambiguous (multiple possible resolutions)
    AmbiguousName {
        name: String,
        span: LexerSpan,
        candidates: Vec<(String, LexerSpan)>,
    },

    /// A generic parameter was used incorrectly
    InvalidGenericUsage {
        name: String,
        span: LexerSpan,
        message: String,
    },
}

impl ResolutionError {
    /// Converts this error to a diagnostic message
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            ResolutionError::UnresolvedName {
                name,
                span,
                suggestion,
            } => {
                let mut diag = Diagnostic::error(
                    format!("cannot find `{}` in this scope", name),
                    to_diag_span(*span),
                )
                .with_error_code(ErrorCode::UnresolvedName);

                if let Some(suggestion) = suggestion {
                    diag = diag.with_suggestion(jet_diagnostics::Suggestion::new(
                        format!("did you mean `{}`?", suggestion),
                        suggestion.clone(),
                        to_diag_span(*span),
                    ));
                }

                diag
            }

            ResolutionError::DuplicateDefinition {
                name,
                span,
                previous_span,
                kind,
            } => Diagnostic::error(
                format!("{} `{}` defined multiple times", kind, name),
                to_diag_span(*span),
            )
            .with_error_code(ErrorCode::DuplicateDefinition)
            .with_label(Label::secondary(
                to_diag_span(*previous_span),
                "previous definition here",
            )),

            ResolutionError::PrivateItemAccess {
                name,
                span,
                definition_span,
                ..
            } => Diagnostic::error(format!("`{}` is private", name), to_diag_span(*span))
                .with_error_code(ErrorCode::PrivateItem)
                .with_label(Label::secondary(
                    to_diag_span(*definition_span),
                    "defined here",
                ))
                .with_note("items are private by default; use `pub` to make them public"),

            ResolutionError::ModuleNotFound { path, span } => Diagnostic::error(
                format!("module not found: `{}`", path.join("::")),
                to_diag_span(*span),
            )
            .with_error_code(ErrorCode::ModuleNotFound),

            ResolutionError::CircularDependency { path, span } => Diagnostic::error(
                format!("circular dependency detected: `{}`", path.join(" -> ")),
                to_diag_span(*span),
            )
            .with_error_code(ErrorCode::CircularDependency),

            ResolutionError::InvalidPath { message, span } => {
                Diagnostic::error(format!("invalid path: {}", message), to_diag_span(*span))
                    .with_error_code(ErrorCode::UnresolvedName)
            }

            ResolutionError::AmbiguousName {
                name,
                span,
                candidates,
            } => {
                let mut diag =
                    Diagnostic::error(format!("`{}` is ambiguous", name), to_diag_span(*span))
                        .with_error_code(ErrorCode::UnresolvedName);

                for (desc, candidate_span) in candidates {
                    diag = diag.with_label(Label::secondary(
                        to_diag_span(*candidate_span),
                        format!("could refer to {}", desc),
                    ));
                }

                diag
            }

            ResolutionError::InvalidGenericUsage {
                name,
                span,
                message,
            } => Diagnostic::error(
                format!("invalid use of generic parameter `{}`: {}", name, message),
                to_diag_span(*span),
            )
            .with_error_code(ErrorCode::UnresolvedName),
        }
    }

    /// Returns the span of this error
    pub fn span(&self) -> Span {
        match self {
            ResolutionError::UnresolvedName { span, .. } => to_diag_span(*span),
            ResolutionError::DuplicateDefinition { span, .. } => to_diag_span(*span),
            ResolutionError::PrivateItemAccess { span, .. } => to_diag_span(*span),
            ResolutionError::ModuleNotFound { span, .. } => to_diag_span(*span),
            ResolutionError::CircularDependency { span, .. } => to_diag_span(*span),
            ResolutionError::InvalidPath { span, .. } => to_diag_span(*span),
            ResolutionError::AmbiguousName { span, .. } => to_diag_span(*span),
            ResolutionError::InvalidGenericUsage { span, .. } => to_diag_span(*span),
        }
    }
}

/// A collection of resolution errors
#[derive(Debug, Clone, Default)]
pub struct ResolutionErrors {
    errors: Vec<ResolutionError>,
}

impl ResolutionErrors {
    /// Creates a new empty error collection
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Adds an error to the collection
    pub fn push(&mut self, error: ResolutionError) {
        self.errors.push(error);
    }

    /// Returns true if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Returns the number of errors
    pub fn len(&self) -> usize {
        self.errors.len()
    }

    /// Returns true if there are no errors
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    /// Converts all errors to diagnostics
    pub fn to_diagnostics(&self) -> Vec<Diagnostic> {
        self.errors.iter().map(|e| e.to_diagnostic()).collect()
    }

    /// Returns all errors
    pub fn errors(&self) -> &[ResolutionError] {
        &self.errors
    }

    /// Takes all errors (clears the collection)
    pub fn take_errors(&mut self) -> Vec<ResolutionError> {
        std::mem::take(&mut self.errors)
    }
}

/// Helper functions for creating common errors
pub struct ErrorHelpers;

impl ErrorHelpers {
    /// Creates an unresolved name error
    pub fn unresolved_name(name: &str, span: LexerSpan) -> ResolutionError {
        ResolutionError::UnresolvedName {
            name: name.to_string(),
            span,
            suggestion: None,
        }
    }

    /// Creates an unresolved name error with a suggestion
    pub fn unresolved_name_with_suggestion(
        name: &str,
        span: LexerSpan,
        suggestion: &str,
    ) -> ResolutionError {
        ResolutionError::UnresolvedName {
            name: name.to_string(),
            span,
            suggestion: Some(suggestion.to_string()),
        }
    }

    /// Creates a duplicate definition error
    pub fn duplicate_definition(
        name: &str,
        span: LexerSpan,
        previous_span: LexerSpan,
        kind: BindingKind,
    ) -> ResolutionError {
        ResolutionError::DuplicateDefinition {
            name: name.to_string(),
            span,
            previous_span,
            kind,
        }
    }

    /// Creates a private item access error
    pub fn private_item_access(
        name: &str,
        span: LexerSpan,
        definition_span: LexerSpan,
        definition_module: DefId,
    ) -> ResolutionError {
        ResolutionError::PrivateItemAccess {
            name: name.to_string(),
            span,
            definition_span,
            definition_module,
        }
    }

    /// Creates a module not found error
    pub fn module_not_found(path: &[String], span: LexerSpan) -> ResolutionError {
        ResolutionError::ModuleNotFound {
            path: path.to_vec(),
            span,
        }
    }

    /// Creates a circular dependency error
    pub fn circular_dependency(path: &[String], span: LexerSpan) -> ResolutionError {
        ResolutionError::CircularDependency {
            path: path.to_vec(),
            span,
        }
    }

    /// Creates an invalid path error
    pub fn invalid_path(message: impl Into<String>, span: LexerSpan) -> ResolutionError {
        ResolutionError::InvalidPath {
            message: message.into(),
            span,
        }
    }

    /// Creates an ambiguous name error
    pub fn ambiguous_name(
        name: &str,
        span: LexerSpan,
        candidates: Vec<(String, LexerSpan)>,
    ) -> ResolutionError {
        ResolutionError::AmbiguousName {
            name: name.to_string(),
            span,
            candidates,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_diagnostics::Level;

    fn make_span() -> LexerSpan {
        LexerSpan::new(0, 10)
    }

    #[test]
    fn test_unresolved_name_error() {
        let error = ErrorHelpers::unresolved_name("foo", make_span());
        let diag = error.to_diagnostic();

        assert_eq!(diag.level, Level::Error);
        assert!(diag.message.contains("cannot find `foo`"));
        assert_eq!(diag.code(), Some("E0200".to_string()));
    }

    #[test]
    fn test_duplicate_definition_error() {
        let error = ErrorHelpers::duplicate_definition(
            "bar",
            LexerSpan::new(20, 30),
            LexerSpan::new(0, 10),
            BindingKind::Function,
        );
        let diag = error.to_diagnostic();

        assert!(diag
            .message
            .contains("function `bar` defined multiple times"));
        assert_eq!(diag.code(), Some("E0201".to_string()));
        assert_eq!(diag.labels.len(), 1);
    }

    #[test]
    fn test_private_item_error() {
        let error = ErrorHelpers::private_item_access(
            "secret",
            LexerSpan::new(20, 30),
            LexerSpan::new(0, 10),
            DefId::from_raw(1),
        );
        let diag = error.to_diagnostic();

        assert!(diag.message.contains("`secret` is private"));
        assert_eq!(diag.code(), Some("E0202".to_string()));
        assert!(!diag.notes.is_empty());
    }

    #[test]
    fn test_module_not_found_error() {
        let error = ErrorHelpers::module_not_found(
            &["std".to_string(), "unknown".to_string()],
            make_span(),
        );
        let diag = error.to_diagnostic();

        assert!(diag.message.contains("module not found: `std::unknown`"));
        assert_eq!(diag.code(), Some("E0204".to_string()));
    }

    #[test]
    fn test_resolution_errors_collection() {
        let mut errors = ResolutionErrors::new();
        assert!(!errors.has_errors());

        errors.push(ErrorHelpers::unresolved_name("x", make_span()));
        assert!(errors.has_errors());
        assert_eq!(errors.len(), 1);

        let diags = errors.to_diagnostics();
        assert_eq!(diags.len(), 1);
    }
}
