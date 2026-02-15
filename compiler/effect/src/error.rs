//! Error types for effect checking.
//!
//! This module defines all the error types that can be reported
//! during effect checking, including unhandled effects, mismatches,
//! and inference failures.

use jet_diagnostics::{Diagnostic, ErrorCode, Label, Span};
use thiserror::Error;

/// Convert a lexer Span to a diagnostics Span
pub fn to_diag_span(span: jet_lexer::Span) -> Span {
    Span::new(span.start, span.end)
}

use crate::effect::{EffectInstance, EffectSet};

/// The main error type for effect checking failures.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum EffectError {
    /// An effect is performed but not handled or declared.
    #[error("unhandled effect: {effect}")]
    UnhandledEffect {
        /// The effect that was not handled.
        effect: Box<EffectInstance>,
        /// The location where the effect was performed.
        span: Span,
        /// The function that performed the effect.
        in_function: Option<String>,
    },

    /// Effects declared in signature don't match body.
    #[error("effect mismatch: declared {declared:?}, but body has {actual:?}")]
    EffectMismatch {
        /// The effects declared in the function signature.
        declared: Box<EffectSet>,
        /// The effects inferred from the body.
        actual: Box<EffectSet>,
        /// The function location.
        span: Span,
        /// The function name.
        function_name: String,
    },

    /// Failed to infer effects for an expression.
    #[error("cannot infer effect: {message}")]
    InferenceFailure {
        /// Why inference failed.
        message: String,
        /// The location where inference failed.
        span: Span,
    },

    /// An effect is used that hasn't been defined.
    #[error("unknown effect: {name}")]
    UnknownEffect {
        /// The name of the unknown effect.
        name: String,
        /// Where it was referenced.
        span: Span,
        /// Suggested similar effect names.
        suggestions: Vec<String>,
    },

    /// A handler is missing operations for the effect.
    #[error("incomplete handler for effect {effect_name}: missing operations {missing:?}")]
    IncompleteHandler {
        /// The effect being handled.
        effect_name: String,
        /// The operations that are missing.
        missing: Vec<String>,
        /// The handler location.
        span: Span,
    },

    /// A handler operation doesn't match the effect's signature.
    #[error("handler operation mismatch: expected {expected}, found {found}")]
    HandlerMismatch {
        /// What was expected.
        expected: String,
        /// What was found.
        found: String,
        /// The location of the mismatch.
        span: Span,
    },

    /// Attempt to handle an effect that's not in scope.
    #[error("effect {effect_name} not in scope")]
    EffectNotInScope {
        /// The effect that was attempted to be handled.
        effect_name: String,
        /// Where the handler was defined.
        span: Span,
    },

    /// Polymorphic effect variable couldn't be resolved.
    #[error("unresolved effect variable")]
    UnresolvedEffectVar {
        /// The location where the variable appears.
        span: Span,
        /// Additional context.
        context: String,
    },

    /// Effect escape - an effect leaks out of a scope where it should be handled.
    #[error("effect escape: {effect} escapes handler scope")]
    EffectEscape {
        /// The effect that escaped.
        effect: Box<EffectInstance>,
        /// Where it escaped from.
        escape_span: Span,
        /// The handler that should have caught it.
        handler_span: Option<Span>,
    },

    /// Attempt to resume outside of a handler.
    #[error("resume outside of handler context")]
    InvalidResume {
        /// Where the resume was attempted.
        span: Span,
        /// Helpful note about why this is invalid.
        note: String,
    },

    /// Multiple handlers for the same effect in the same scope.
    #[error("duplicate handler for effect {effect_name}")]
    DuplicateHandler {
        /// The effect with duplicate handlers.
        effect_name: String,
        /// The first handler location.
        first_handler_span: Span,
        /// The duplicate handler location.
        duplicate_span: Span,
    },

    /// Effect is not allowed in the current context (e.g., async effect in sync context).
    #[error("effect {effect_name} not allowed in {context}")]
    EffectNotAllowed {
        /// The effect that is not allowed.
        effect_name: String,
        /// The context where it's not allowed.
        context: String,
        /// Where the effect was used.
        span: Span,
        /// Suggestion for how to fix.
        suggestion: Option<String>,
    },
}

impl EffectError {
    /// Converts this error to a diagnostic message.
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            EffectError::UnhandledEffect {
                effect,
                span,
                in_function,
            } => {
                let mut diag = Diagnostic::error(format!("unhandled effect: `{}`", effect), *span);
                diag = diag.with_error_code(ErrorCode::UnhandledEffect);

                if let Some(func) = in_function {
                    diag = diag.with_label(Label::secondary(
                        *span,
                        format!("this operation can perform `{}` but the function `{}` doesn't declare it", effect, func)
                    ));
                    diag = diag.with_note(format!(
                        "add `!{}` to the function signature of `{}`",
                        effect, func
                    ));
                }

                diag = diag.with_suggestion(jet_diagnostics::Suggestion::new(
                    format!("add `!{}` to function signature", effect),
                    format!(" !{}", effect),
                    Span::new(span.end, span.end),
                ));

                diag
            }

            EffectError::EffectMismatch {
                declared,
                actual,
                span,
                function_name,
            } => {
                let mut diag = Diagnostic::error(
                    format!(
                        "effect mismatch in function `{}`: declared `{}` but body has `{}`",
                        function_name, declared, actual
                    ),
                    *span,
                );
                diag = diag.with_error_code(ErrorCode::Custom(502));

                // Find effects that are in actual but not in declared
                let missing: Vec<_> = actual.iter().filter(|e| !declared.contains(e)).collect();

                if !missing.is_empty() {
                    diag = diag.with_note(format!(
                        "missing from declaration: {}",
                        missing
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                }

                // Find effects that are declared but not in actual
                let extra: Vec<_> = declared.iter().filter(|e| !actual.contains(e)).collect();

                if !extra.is_empty() {
                    diag = diag.with_note(format!(
                        "unnecessarily declared: {}",
                        extra
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                }

                diag
            }

            EffectError::InferenceFailure { message, span } => {
                let mut diag =
                    Diagnostic::error(format!("cannot infer effect: {}", message), *span);
                diag = diag.with_error_code(ErrorCode::Custom(503));
                diag
            }

            EffectError::UnknownEffect {
                name,
                span,
                suggestions,
            } => {
                let mut diag = Diagnostic::error(format!("unknown effect: `{}`", name), *span);
                diag = diag.with_error_code(ErrorCode::UndeclaredEffect);

                if !suggestions.is_empty() {
                    diag = diag.with_note(format!("did you mean: {}?", suggestions.join(", ")));
                }

                diag
            }

            EffectError::IncompleteHandler {
                effect_name,
                missing,
                span,
            } => {
                let mut diag = Diagnostic::error(
                    format!(
                        "incomplete handler for effect `{}`: missing operations: {}",
                        effect_name,
                        missing.join(", ")
                    ),
                    *span,
                );
                diag = diag.with_error_code(ErrorCode::Custom(505));
                diag = diag.with_note(
                    "handlers must handle all operations of an effect or be marked as partial",
                );
                diag
            }

            EffectError::HandlerMismatch {
                expected,
                found,
                span,
            } => {
                let mut diag = Diagnostic::error(
                    format!(
                        "handler operation type mismatch: expected `{}`, found `{}`",
                        expected, found
                    ),
                    *span,
                );
                diag = diag.with_error_code(ErrorCode::HandlerMismatch);
                diag
            }

            EffectError::EffectNotInScope { effect_name, span } => {
                let mut diag =
                    Diagnostic::error(format!("effect `{}` not in scope", effect_name), *span);
                diag = diag.with_error_code(ErrorCode::Custom(507));
                diag = diag.with_note(
                    "handlers can only handle effects that are performed in their scope",
                );
                diag
            }

            EffectError::UnresolvedEffectVar { span, context } => {
                let mut diag = Diagnostic::error("unresolved effect variable", *span);
                diag = diag.with_error_code(ErrorCode::Custom(508));
                diag = diag.with_note(format!("context: {}", context));
                diag
            }

            EffectError::EffectEscape {
                effect,
                escape_span,
                handler_span,
            } => {
                let mut diag = Diagnostic::error(
                    format!("effect `{}` escapes its handler scope", effect),
                    *escape_span,
                );
                diag = diag.with_error_code(ErrorCode::Custom(509));

                if let Some(handler) = handler_span {
                    diag = diag.with_label(Label::secondary(*handler, "handler defined here"));
                }

                diag = diag.with_note("effects must be handled within the scope of their handler");

                diag
            }

            EffectError::InvalidResume { span, note } => {
                let mut diag = Diagnostic::error("`resume` used outside of handler context", *span);
                diag = diag.with_error_code(ErrorCode::InvalidResume);
                diag = diag.with_note(note.clone());
                diag
            }

            EffectError::DuplicateHandler {
                effect_name,
                first_handler_span,
                duplicate_span,
            } => {
                let mut diag = Diagnostic::error(
                    format!("duplicate handler for effect `{}`", effect_name),
                    *duplicate_span,
                );
                diag = diag.with_error_code(ErrorCode::Custom(511));
                diag = diag.with_label(Label::secondary(
                    *first_handler_span,
                    "first handler defined here",
                ));
                diag = diag.with_note("only one handler per effect is allowed in a given scope");
                diag
            }

            EffectError::EffectNotAllowed {
                effect_name,
                context,
                span,
                suggestion,
            } => {
                let mut diag = Diagnostic::error(
                    format!("effect `{}` not allowed in {}", effect_name, context),
                    *span,
                );
                diag = diag.with_error_code(ErrorCode::Custom(512));

                if let Some(sugg) = suggestion {
                    diag = diag.with_note(sugg.clone());
                }

                diag
            }
        }
    }

    /// Returns the error code for this error.
    pub fn error_code(&self) -> &'static str {
        match self {
            EffectError::UnhandledEffect { .. } => "E0501",
            EffectError::EffectMismatch { .. } => "E0502",
            EffectError::InferenceFailure { .. } => "E0503",
            EffectError::UnknownEffect { .. } => "E0504",
            EffectError::IncompleteHandler { .. } => "E0505",
            EffectError::HandlerMismatch { .. } => "E0506",
            EffectError::EffectNotInScope { .. } => "E0507",
            EffectError::UnresolvedEffectVar { .. } => "E0508",
            EffectError::EffectEscape { .. } => "E0509",
            EffectError::InvalidResume { .. } => "E0510",
            EffectError::DuplicateHandler { .. } => "E0511",
            EffectError::EffectNotAllowed { .. } => "E0512",
        }
    }

    /// Returns the span where this error occurred.
    pub fn span(&self) -> Span {
        match self {
            EffectError::UnhandledEffect { span, .. } => *span,
            EffectError::EffectMismatch { span, .. } => *span,
            EffectError::InferenceFailure { span, .. } => *span,
            EffectError::UnknownEffect { span, .. } => *span,
            EffectError::IncompleteHandler { span, .. } => *span,
            EffectError::HandlerMismatch { span, .. } => *span,
            EffectError::EffectNotInScope { span, .. } => *span,
            EffectError::UnresolvedEffectVar { span, .. } => *span,
            EffectError::EffectEscape { escape_span, .. } => *escape_span,
            EffectError::InvalidResume { span, .. } => *span,
            EffectError::DuplicateHandler { duplicate_span, .. } => *duplicate_span,
            EffectError::EffectNotAllowed { span, .. } => *span,
        }
    }
}

/// A collection of effect errors.
#[derive(Debug, Clone, Default)]
pub struct EffectErrors {
    errors: Vec<EffectError>,
}

impl EffectErrors {
    /// Creates a new empty error collection.
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Adds an error to the collection.
    pub fn push(&mut self, error: EffectError) {
        self.errors.push(error);
    }

    /// Returns true if there are any errors.
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    /// Returns the number of errors.
    pub fn len(&self) -> usize {
        self.errors.len()
    }

    /// Converts all errors to diagnostics.
    pub fn to_diagnostics(&self) -> Vec<Diagnostic> {
        self.errors.iter().map(|e| e.to_diagnostic()).collect()
    }

    /// Takes all errors (clears the collection).
    pub fn take(&mut self) -> Vec<EffectError> {
        std::mem::take(&mut self.errors)
    }

    /// Returns a reference to the errors.
    pub fn errors(&self) -> &[EffectError] {
        &self.errors
    }
}

impl IntoIterator for EffectErrors {
    type Item = EffectError;
    type IntoIter = std::vec::IntoIter<EffectError>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}

/// Result type for effect checking operations.
pub type EffectResult<T> = Result<T, EffectError>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::effect::{EffectId, EffectInstance};

    fn test_span() -> Span {
        Span::new(0, 10)
    }

    #[test]
    fn test_unhandled_effect_error() {
        let effect = EffectInstance::new(EffectId(1), "IoError");
        let error = EffectError::UnhandledEffect {
            effect: Box::new(effect.clone()),
            span: test_span(),
            in_function: Some("read_file".to_string()),
        };

        assert_eq!(error.error_code(), "E0501");
        let diag = error.to_diagnostic();
        assert!(diag.message.contains("unhandled effect"));
        assert!(diag.message.contains("IoError"));
    }

    #[test]
    fn test_effect_mismatch_error() {
        let declared = EffectSet::singleton(EffectInstance::new(EffectId(1), "IoError"));
        let mut actual = EffectSet::empty();
        actual.insert(EffectInstance::new(EffectId(2), "NetworkError"));

        let error = EffectError::EffectMismatch {
            declared: Box::new(declared),
            actual: Box::new(actual),
            span: test_span(),
            function_name: "fetch".to_string(),
        };

        assert_eq!(error.error_code(), "E0502");
        let diag = error.to_diagnostic();
        assert!(diag.message.contains("effect mismatch"));
        assert!(diag.message.contains("fetch"));
    }

    #[test]
    fn test_unknown_effect_error() {
        let error = EffectError::UnknownEffect {
            name: "UnkownErrr".to_string(),
            span: test_span(),
            suggestions: vec!["UnknownError".to_string()],
        };

        assert_eq!(error.error_code(), "E0504");
        let diag = error.to_diagnostic();
        assert!(diag.message.contains("unknown effect"));
        assert!(!diag.notes.is_empty());
    }

    #[test]
    fn test_duplicate_handler_error() {
        let error = EffectError::DuplicateHandler {
            effect_name: "State".to_string(),
            first_handler_span: Span::new(0, 5),
            duplicate_span: Span::new(10, 15),
        };

        assert_eq!(error.error_code(), "E0511");
        let diag = error.to_diagnostic();
        assert!(diag.message.contains("duplicate handler"));
        assert_eq!(diag.labels.len(), 1);
    }

    #[test]
    fn test_effect_errors_collection() {
        let mut errors = EffectErrors::new();
        assert!(errors.is_empty());

        errors.push(EffectError::InferenceFailure {
            message: "test".to_string(),
            span: test_span(),
        });

        assert!(!errors.is_empty());
        assert_eq!(errors.len(), 1);

        let diags = errors.to_diagnostics();
        assert_eq!(diags.len(), 1);
    }
}
