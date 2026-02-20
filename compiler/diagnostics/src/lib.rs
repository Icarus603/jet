//! Jet Diagnostic Engine
//!
//! A unified error reporting system for the Jet compiler.
//! Provides rich error messages with source highlighting, suggestions,
//! and support for both human-readable and machine-readable output.

pub mod emitter;
pub mod sourcemap;

pub use emitter::{EmitConfig, Emitter};
pub use sourcemap::{FileId, SourceFile, SourceMap};

use std::fmt;

/// Standard error codes for the Jet compiler
///
/// Error codes follow the pattern E#### where:
/// - E0001-E0099: Lexer errors
/// - E0100-E0199: Parser errors
/// - E0200-E0299: Name resolution errors
/// - E0300-E0399: Type checking errors
/// - E0400-E0499: Effect system errors
/// - E0500-E0599: IR and lowering errors
/// - E0600-E0699: Code generation errors
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCode {
    // Lexer errors (E0001-E0099)
    /// Invalid indentation level
    InvalidIndentation,
    /// Unrecognized token
    UnrecognizedToken,
    /// Unterminated string literal
    UnterminatedString,
    /// Unterminated character literal
    UnterminatedChar,
    /// Invalid escape sequence
    InvalidEscape,
    /// Invalid numeric literal
    InvalidNumber,

    // Parser errors (E0100-E0199)
    /// Unexpected token
    UnexpectedToken,
    /// Missing closing delimiter
    MissingDelimiter,
    /// Invalid syntax construct
    InvalidSyntax,
    /// Expected expression
    ExpectedExpression,
    /// Expected type
    ExpectedType,
    /// Expected identifier
    ExpectedIdentifier,

    // Name resolution errors (E0200-E0299)
    /// Unresolved name/variable
    UnresolvedName,
    /// Duplicate definition
    DuplicateDefinition,
    /// Private item accessed from outside module
    PrivateItem,
    /// Circular dependency
    CircularDependency,
    /// Module not found
    ModuleNotFound,

    // Type checking errors (E0300-E0399)
    /// Type mismatch
    TypeMismatch,
    /// Infinite type (occurs check failed)
    InfiniteType,
    /// Arity mismatch in function call
    ArityMismatch,
    /// Missing return type annotation
    MissingReturnType,
    /// Unsatisfied trait constraint
    UnsatisfiedConstraint,
    /// Ambiguous type inference
    AmbiguousType,
    /// Mutability mismatch in reference types
    MutabilityMismatch,
    /// Cannot unify generic parameters
    CannotUnifyGeneric,
    /// Conflicting trait implementations
    ConflictingImpls,
    /// Trait bound not satisfied
    TraitBoundNotSatisfied,
    /// Cannot normalize associated type projection
    CannotNormalizeProjection,
    /// Non-exhaustive patterns
    NonExhaustivePatterns,
    /// Contract precondition not satisfied
    ContractPreconditionFailed,
    /// Contract postcondition not satisfied
    ContractPostconditionFailed,
    /// Loop invariant not preserved
    InvariantNotPreserved,
    /// Ghost variable used in runtime context
    GhostInRuntimeContext,

    // Effect system errors (E0400-E0499)
    /// Effect not handled
    UnhandledEffect,
    /// Effect handler mismatch
    HandlerMismatch,
    /// Invalid resume operation
    InvalidResume,
    /// Effect not declared in signature
    UndeclaredEffect,

    // Lowering errors (E0500-E0599)
    /// Invalid IR construct
    InvalidIr,
    /// Unsupported feature
    UnsupportedFeature,

    // Code generation errors (E0600-E0699)
    /// Code generation failed
    CodegenError,
    /// Linking failed
    LinkError,

    /// Custom error code
    Custom(u32),
}

impl ErrorCode {
    /// Get the numeric code as a string (e.g., "E0001")
    pub fn as_str(&self) -> String {
        match self {
            // Lexer errors
            ErrorCode::InvalidIndentation => "E0001".to_string(),
            ErrorCode::UnrecognizedToken => "E0002".to_string(),
            ErrorCode::UnterminatedString => "E0003".to_string(),
            ErrorCode::UnterminatedChar => "E0004".to_string(),
            ErrorCode::InvalidEscape => "E0005".to_string(),
            ErrorCode::InvalidNumber => "E0006".to_string(),

            // Parser errors
            ErrorCode::UnexpectedToken => "E0100".to_string(),
            ErrorCode::MissingDelimiter => "E0101".to_string(),
            ErrorCode::InvalidSyntax => "E0102".to_string(),
            ErrorCode::ExpectedExpression => "E0103".to_string(),
            ErrorCode::ExpectedType => "E0104".to_string(),
            ErrorCode::ExpectedIdentifier => "E0105".to_string(),

            // Name resolution errors
            ErrorCode::UnresolvedName => "E0200".to_string(),
            ErrorCode::DuplicateDefinition => "E0201".to_string(),
            ErrorCode::PrivateItem => "E0202".to_string(),
            ErrorCode::CircularDependency => "E0203".to_string(),
            ErrorCode::ModuleNotFound => "E0204".to_string(),

            // Type checking errors
            ErrorCode::TypeMismatch => "E0300".to_string(),
            ErrorCode::InfiniteType => "E0301".to_string(),
            ErrorCode::ArityMismatch => "E0302".to_string(),
            ErrorCode::MissingReturnType => "E0303".to_string(),
            ErrorCode::UnsatisfiedConstraint => "E0304".to_string(),
            ErrorCode::AmbiguousType => "E0305".to_string(),
            ErrorCode::MutabilityMismatch => "E0306".to_string(),
            ErrorCode::CannotUnifyGeneric => "E0307".to_string(),
            ErrorCode::ConflictingImpls => "E0308".to_string(),
            ErrorCode::TraitBoundNotSatisfied => "E0309".to_string(),
            ErrorCode::CannotNormalizeProjection => "E0310".to_string(),
            ErrorCode::NonExhaustivePatterns => "E0311".to_string(),
            ErrorCode::ContractPreconditionFailed => "E0350".to_string(),
            ErrorCode::ContractPostconditionFailed => "E0351".to_string(),
            ErrorCode::InvariantNotPreserved => "E0352".to_string(),
            ErrorCode::GhostInRuntimeContext => "E0353".to_string(),

            // Effect system errors
            ErrorCode::UnhandledEffect => "E0400".to_string(),
            ErrorCode::HandlerMismatch => "E0401".to_string(),
            ErrorCode::InvalidResume => "E0402".to_string(),
            ErrorCode::UndeclaredEffect => "E0403".to_string(),

            // Lowering errors
            ErrorCode::InvalidIr => "E0500".to_string(),
            ErrorCode::UnsupportedFeature => "E0501".to_string(),

            // Code generation errors
            ErrorCode::CodegenError => "E0600".to_string(),
            ErrorCode::LinkError => "E0601".to_string(),

            // Custom error code
            ErrorCode::Custom(n) => format!("E{:04}", n),
        }
    }

    /// Get a brief description of the error code
    pub fn description(&self) -> &'static str {
        match self {
            ErrorCode::InvalidIndentation => "invalid indentation",
            ErrorCode::UnrecognizedToken => "unrecognized token",
            ErrorCode::UnterminatedString => "unterminated string literal",
            ErrorCode::UnterminatedChar => "unterminated character literal",
            ErrorCode::InvalidEscape => "invalid escape sequence",
            ErrorCode::InvalidNumber => "invalid numeric literal",
            ErrorCode::UnexpectedToken => "unexpected token",
            ErrorCode::MissingDelimiter => "missing closing delimiter",
            ErrorCode::InvalidSyntax => "invalid syntax",
            ErrorCode::ExpectedExpression => "expected expression",
            ErrorCode::ExpectedType => "expected type",
            ErrorCode::ExpectedIdentifier => "expected identifier",
            ErrorCode::UnresolvedName => "unresolved name",
            ErrorCode::DuplicateDefinition => "duplicate definition",
            ErrorCode::PrivateItem => "private item",
            ErrorCode::CircularDependency => "circular dependency",
            ErrorCode::ModuleNotFound => "module not found",
            ErrorCode::TypeMismatch => "type mismatch",
            ErrorCode::InfiniteType => "infinite type",
            ErrorCode::ArityMismatch => "arity mismatch",
            ErrorCode::MissingReturnType => "missing return type",
            ErrorCode::UnsatisfiedConstraint => "unsatisfied constraint",
            ErrorCode::AmbiguousType => "ambiguous type",
            ErrorCode::MutabilityMismatch => "mutability mismatch",
            ErrorCode::CannotUnifyGeneric => "cannot unify generic parameters",
            ErrorCode::ConflictingImpls => "conflicting trait implementations",
            ErrorCode::TraitBoundNotSatisfied => "trait bound not satisfied",
            ErrorCode::CannotNormalizeProjection => "cannot normalize projection",
            ErrorCode::NonExhaustivePatterns => "non-exhaustive patterns",
            ErrorCode::ContractPreconditionFailed => "contract precondition failed",
            ErrorCode::ContractPostconditionFailed => "contract postcondition failed",
            ErrorCode::InvariantNotPreserved => "loop invariant not preserved",
            ErrorCode::GhostInRuntimeContext => "ghost variable in runtime context",
            ErrorCode::UnhandledEffect => "unhandled effect",
            ErrorCode::HandlerMismatch => "handler mismatch",
            ErrorCode::InvalidResume => "invalid resume",
            ErrorCode::UndeclaredEffect => "undeclared effect",
            ErrorCode::InvalidIr => "invalid IR",
            ErrorCode::UnsupportedFeature => "unsupported feature",
            ErrorCode::CodegenError => "code generation error",
            ErrorCode::LinkError => "link error",
            ErrorCode::Custom(_) => "custom error",
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// The severity level of a diagnostic
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Level {
    /// Fatal error that prevents compilation
    Error,
    /// Warning that doesn't prevent compilation
    Warning,
    /// Informational note
    Note,
    /// Helpful suggestion
    Help,
}

impl Level {
    /// Returns the string representation of the level
    pub fn as_str(&self) -> &'static str {
        match self {
            Level::Error => "error",
            Level::Warning => "warning",
            Level::Note => "note",
            Level::Help => "help",
        }
    }

    /// Returns true if this level is an error
    pub fn is_error(&self) -> bool {
        matches!(self, Level::Error)
    }
}

/// A diagnostic message (error, warning, or note)
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// The severity level
    pub level: Level,
    /// The main error message
    pub message: String,
    /// Optional error code (e.g., E0001)
    pub error_code: Option<ErrorCode>,
    /// The primary span where the error occurred
    pub span: Span,
    /// Additional labels for related spans
    pub labels: Vec<Label>,
    /// Suggested fixes
    pub suggestions: Vec<Suggestion>,
    /// Additional notes to display
    pub notes: Vec<String>,
}

/// Legacy alias for backward compatibility
pub type SourceSpan = Span;

impl Diagnostic {
    /// Create a new error diagnostic
    pub fn error(message: impl Into<String>, span: Span) -> Self {
        Self {
            level: Level::Error,
            message: message.into(),
            error_code: None,
            span,
            labels: Vec::new(),
            suggestions: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create a new warning diagnostic
    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self {
            level: Level::Warning,
            message: message.into(),
            error_code: None,
            span,
            labels: Vec::new(),
            suggestions: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create a new note diagnostic
    pub fn note(message: impl Into<String>, span: Span) -> Self {
        Self {
            level: Level::Note,
            message: message.into(),
            error_code: None,
            span,
            labels: Vec::new(),
            suggestions: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create a new help diagnostic
    pub fn help(message: impl Into<String>, span: Span) -> Self {
        Self {
            level: Level::Help,
            message: message.into(),
            error_code: None,
            span,
            labels: Vec::new(),
            suggestions: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Set the error code using ErrorCode enum
    pub fn with_error_code(mut self, code: ErrorCode) -> Self {
        self.error_code = Some(code);
        self
    }

    /// Set the error code (legacy method for backward compatibility)
    #[deprecated(since = "0.1.0", note = "Use with_error_code instead")]
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        // Try to parse as a known error code, otherwise ignore
        let code_str = code.into();
        self.error_code = Self::parse_error_code(&code_str);
        self
    }

    /// Parse an error code string into ErrorCode
    fn parse_error_code(code: &str) -> Option<ErrorCode> {
        match code {
            "E0001" => Some(ErrorCode::InvalidIndentation),
            "E0002" => Some(ErrorCode::UnrecognizedToken),
            "E0003" => Some(ErrorCode::UnterminatedString),
            "E0004" => Some(ErrorCode::UnterminatedChar),
            "E0005" => Some(ErrorCode::InvalidEscape),
            "E0006" => Some(ErrorCode::InvalidNumber),
            "E0100" => Some(ErrorCode::UnexpectedToken),
            "E0101" => Some(ErrorCode::MissingDelimiter),
            "E0102" => Some(ErrorCode::InvalidSyntax),
            "E0103" => Some(ErrorCode::ExpectedExpression),
            "E0104" => Some(ErrorCode::ExpectedType),
            "E0105" => Some(ErrorCode::ExpectedIdentifier),
            "E0200" => Some(ErrorCode::UnresolvedName),
            "E0201" => Some(ErrorCode::DuplicateDefinition),
            "E0202" => Some(ErrorCode::PrivateItem),
            "E0203" => Some(ErrorCode::CircularDependency),
            "E0204" => Some(ErrorCode::ModuleNotFound),
            "E0300" => Some(ErrorCode::TypeMismatch),
            "E0301" => Some(ErrorCode::InfiniteType),
            "E0302" => Some(ErrorCode::ArityMismatch),
            "E0303" => Some(ErrorCode::MissingReturnType),
            "E0304" => Some(ErrorCode::UnsatisfiedConstraint),
            "E0305" => Some(ErrorCode::AmbiguousType),
            "E0306" => Some(ErrorCode::MutabilityMismatch),
            "E0307" => Some(ErrorCode::CannotUnifyGeneric),
            "E0308" => Some(ErrorCode::ConflictingImpls),
            "E0309" => Some(ErrorCode::TraitBoundNotSatisfied),
            "E0310" => Some(ErrorCode::CannotNormalizeProjection),
            "E0311" => Some(ErrorCode::NonExhaustivePatterns),
            "E0350" => Some(ErrorCode::ContractPreconditionFailed),
            "E0351" => Some(ErrorCode::ContractPostconditionFailed),
            "E0352" => Some(ErrorCode::InvariantNotPreserved),
            "E0353" => Some(ErrorCode::GhostInRuntimeContext),
            "E0400" => Some(ErrorCode::UnhandledEffect),
            "E0401" => Some(ErrorCode::HandlerMismatch),
            "E0402" => Some(ErrorCode::InvalidResume),
            "E0403" => Some(ErrorCode::UndeclaredEffect),
            "E0500" => Some(ErrorCode::InvalidIr),
            "E0501" => Some(ErrorCode::UnsupportedFeature),
            "E0600" => Some(ErrorCode::CodegenError),
            "E0601" => Some(ErrorCode::LinkError),
            _ => {
                // Try to parse custom code
                if let Ok(n) = code.parse::<u32>() {
                    Some(ErrorCode::Custom(n))
                } else {
                    None
                }
            }
        }
    }

    /// Get the error code as a string (for backward compatibility)
    pub fn code(&self) -> Option<String> {
        self.error_code.map(|c| c.as_str())
    }

    /// Add a label to the diagnostic
    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    /// Add multiple labels to the diagnostic
    pub fn with_labels(mut self, labels: impl IntoIterator<Item = Label>) -> Self {
        self.labels.extend(labels);
        self
    }

    /// Add a suggestion to the diagnostic
    pub fn with_suggestion(mut self, suggestion: Suggestion) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    /// Add multiple suggestions to the diagnostic
    pub fn with_suggestions(mut self, suggestions: impl IntoIterator<Item = Suggestion>) -> Self {
        self.suggestions.extend(suggestions);
        self
    }

    /// Add a note to the diagnostic
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Add multiple notes to the diagnostic
    pub fn with_notes(mut self, notes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.notes.extend(notes.into_iter().map(|n| n.into()));
        self
    }

    /// Check if this diagnostic is an error
    pub fn is_error(&self) -> bool {
        self.level.is_error()
    }

    /// Check if this diagnostic is a warning
    pub fn is_warning(&self) -> bool {
        matches!(self.level, Level::Warning)
    }
}

/// A label pointing to a specific span with a message
#[derive(Debug, Clone)]
pub struct Label {
    /// The span this label points to
    pub span: Span,
    /// The message to display at this location
    pub message: String,
    /// The style of this label (primary or secondary)
    pub style: LabelStyle,
}

impl Label {
    /// Create a new primary label
    pub fn primary(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            style: LabelStyle::Primary,
        }
    }

    /// Create a new secondary label
    pub fn secondary(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
            style: LabelStyle::Secondary,
        }
    }
}

/// The style of a label, indicating its importance
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelStyle {
    /// The primary location of the error
    Primary,
    /// A secondary related location
    Secondary,
}

/// A suggested fix for an error
#[derive(Debug, Clone)]
pub struct Suggestion {
    /// Description of what this suggestion does
    pub message: String,
    /// The replacement text
    pub replacement: String,
    /// The span to replace
    pub span: Span,
}

impl Suggestion {
    /// Create a new suggestion
    pub fn new(message: impl Into<String>, replacement: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            replacement: replacement.into(),
            span,
        }
    }
}

/// A source span indicating a location in the source code
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Span {
    /// Byte offset start (inclusive)
    pub start: usize,
    /// Byte offset end (exclusive)
    pub end: usize,
    /// Line number (1-indexed, 0 means not set)
    pub line: usize,
    /// Column number (1-indexed, 0 means not set)
    pub column: usize,
    /// File identifier
    pub file: FileId,
}

impl Span {
    /// Create a new span with just byte offsets
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            line: 0,
            column: 0,
            file: FileId(0),
        }
    }

    /// Create a new span with file information
    pub fn new_in_file(file: FileId, start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            line: 0,
            column: 0,
            file,
        }
    }

    /// Set the line and column position
    pub fn with_position(mut self, line: usize, column: usize) -> Self {
        self.line = line;
        self.column = column;
        self
    }

    /// Set the file for this span
    pub fn with_file(mut self, file: FileId) -> Self {
        self.file = file;
        self
    }

    /// Returns the length of the span in bytes
    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    /// Returns true if the span is empty
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Returns a new span that covers both spans
    pub fn merge(&self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line,
            column: self.column,
            file: self.file,
        }
    }

    /// Check if this span contains a byte position
    pub fn contains(&self, pos: usize) -> bool {
        self.start <= pos && pos < self.end
    }
}

/// A collection of diagnostics
#[derive(Debug, Clone, Default)]
pub struct DiagnosticBag {
    diagnostics: Vec<Diagnostic>,
    has_errors: bool,
}

impl DiagnosticBag {
    /// Create a new empty diagnostic bag
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            has_errors: false,
        }
    }

    /// Add a diagnostic to the bag
    pub fn push(&mut self, diagnostic: Diagnostic) {
        if diagnostic.level.is_error() {
            self.has_errors = true;
        }
        self.diagnostics.push(diagnostic);
    }

    /// Returns true if any errors have been reported
    pub fn has_errors(&self) -> bool {
        self.has_errors
    }

    /// Returns the number of diagnostics
    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }

    /// Returns true if there are no diagnostics
    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    /// Get all diagnostics
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Take all diagnostics (clears the bag)
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        self.has_errors = false;
        std::mem::take(&mut self.diagnostics)
    }

    /// Clear all diagnostics
    pub fn clear(&mut self) {
        self.diagnostics.clear();
        self.has_errors = false;
    }

    /// Extend with diagnostics from another bag
    pub fn extend(&mut self, other: DiagnosticBag) {
        if other.has_errors {
            self.has_errors = true;
        }
        self.diagnostics.extend(other.diagnostics);
    }
}

impl IntoIterator for DiagnosticBag {
    type Item = Diagnostic;
    type IntoIter = std::vec::IntoIter<Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.into_iter()
    }
}

impl<'a> IntoIterator for &'a DiagnosticBag {
    type Item = &'a Diagnostic;
    type IntoIter = std::slice::Iter<'a, Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.iter()
    }
}

/// Trait for types that can report diagnostics
pub trait DiagnosticReporter {
    /// Report a diagnostic
    fn report(&mut self, diagnostic: Diagnostic);

    /// Report an error
    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.report(Diagnostic::error(message, span));
    }

    /// Report a warning
    fn warning(&mut self, message: impl Into<String>, span: Span) {
        self.report(Diagnostic::warning(message, span));
    }

    /// Returns true if any errors have been reported
    fn has_errors(&self) -> bool;
}

impl DiagnosticReporter for DiagnosticBag {
    fn report(&mut self, diagnostic: Diagnostic) {
        self.push(diagnostic);
    }

    fn has_errors(&self) -> bool {
        self.has_errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_creation() {
        let span = Span::new(10, 20);
        assert_eq!(span.start, 10);
        assert_eq!(span.end, 20);
        assert_eq!(span.len(), 10);
        assert!(!span.is_empty());
    }

    #[test]
    fn test_span_empty() {
        let span = Span::new(10, 10);
        assert!(span.is_empty());
        assert_eq!(span.len(), 0);
    }

    #[test]
    fn test_span_merge() {
        let span1 = Span::new(10, 20);
        let span2 = Span::new(15, 30);
        let merged = span1.merge(span2);
        assert_eq!(merged.start, 10);
        assert_eq!(merged.end, 30);
    }

    #[test]
    fn test_span_contains() {
        let span = Span::new(10, 20);
        assert!(span.contains(10));
        assert!(span.contains(15));
        assert!(!span.contains(20));
        assert!(!span.contains(5));
        assert!(!span.contains(25));
    }

    #[test]
    fn test_diagnostic_creation() {
        let span = Span::new(0, 5);
        let diag = Diagnostic::error("test error", span);
        assert_eq!(diag.level, Level::Error);
        assert_eq!(diag.message, "test error");
        assert!(diag.error_code.is_none());
        assert!(diag.code().is_none());
    }

    #[test]
    fn test_diagnostic_with_error_code() {
        let span = Span::new(0, 5);
        let diag =
            Diagnostic::error("test error", span).with_error_code(ErrorCode::InvalidIndentation);
        assert_eq!(diag.error_code, Some(ErrorCode::InvalidIndentation));
        assert_eq!(diag.code(), Some("E0001".to_string()));
    }

    #[test]
    fn test_diagnostic_with_legacy_code() {
        #![allow(deprecated)]
        let span = Span::new(0, 5);
        let diag = Diagnostic::error("test error", span).with_code("E0001");
        assert_eq!(diag.error_code, Some(ErrorCode::InvalidIndentation));
    }

    #[test]
    fn test_diagnostic_with_label() {
        let span = Span::new(0, 5);
        let label = Label::secondary(Span::new(10, 15), "related");
        let diag = Diagnostic::error("test error", span).with_label(label);
        assert_eq!(diag.labels.len(), 1);
    }

    #[test]
    fn test_diagnostic_with_note() {
        let span = Span::new(0, 5);
        let diag = Diagnostic::error("test error", span).with_note("additional info");
        assert_eq!(diag.notes.len(), 1);
        assert_eq!(diag.notes[0], "additional info");
    }

    #[test]
    fn test_diagnostic_bag() {
        let mut bag = DiagnosticBag::new();
        assert!(!bag.has_errors());
        assert!(bag.is_empty());

        bag.push(Diagnostic::error("error 1", Span::new(0, 5)));
        assert!(bag.has_errors());
        assert_eq!(bag.len(), 1);

        bag.push(Diagnostic::warning("warning 1", Span::new(6, 10)));
        assert_eq!(bag.len(), 2);

        let diags = bag.take_diagnostics();
        assert_eq!(diags.len(), 2);
        assert!(bag.is_empty());
        assert!(!bag.has_errors());
    }

    #[test]
    fn test_level_as_str() {
        assert_eq!(Level::Error.as_str(), "error");
        assert_eq!(Level::Warning.as_str(), "warning");
        assert_eq!(Level::Note.as_str(), "note");
        assert_eq!(Level::Help.as_str(), "help");
    }

    #[test]
    fn test_level_is_error() {
        assert!(Level::Error.is_error());
        assert!(!Level::Warning.is_error());
        assert!(!Level::Note.is_error());
        assert!(!Level::Help.is_error());
    }

    #[test]
    fn test_suggestion_creation() {
        let span = Span::new(10, 15);
        let suggestion = Suggestion::new("fix this", "replacement", span);
        assert_eq!(suggestion.message, "fix this");
        assert_eq!(suggestion.replacement, "replacement");
        assert_eq!(suggestion.span, span);
    }

    #[test]
    fn test_label_styles() {
        let span = Span::new(0, 5);
        let primary = Label::primary(span, "primary label");
        let secondary = Label::secondary(span, "secondary label");

        assert_eq!(primary.style, LabelStyle::Primary);
        assert_eq!(secondary.style, LabelStyle::Secondary);
    }

    #[test]
    fn test_error_code_as_str() {
        // Lexer errors
        assert_eq!(ErrorCode::InvalidIndentation.as_str(), "E0001");
        assert_eq!(ErrorCode::UnrecognizedToken.as_str(), "E0002");
        assert_eq!(ErrorCode::UnterminatedString.as_str(), "E0003");
        assert_eq!(ErrorCode::UnterminatedChar.as_str(), "E0004");
        assert_eq!(ErrorCode::InvalidEscape.as_str(), "E0005");
        assert_eq!(ErrorCode::InvalidNumber.as_str(), "E0006");

        // Parser errors
        assert_eq!(ErrorCode::UnexpectedToken.as_str(), "E0100");
        assert_eq!(ErrorCode::MissingDelimiter.as_str(), "E0101");

        // Name resolution errors
        assert_eq!(ErrorCode::UnresolvedName.as_str(), "E0200");
        assert_eq!(ErrorCode::DuplicateDefinition.as_str(), "E0201");

        // Type checking errors
        assert_eq!(ErrorCode::TypeMismatch.as_str(), "E0300");
        assert_eq!(ErrorCode::InfiniteType.as_str(), "E0301");

        // Effect system errors
        assert_eq!(ErrorCode::UnhandledEffect.as_str(), "E0400");
        assert_eq!(ErrorCode::HandlerMismatch.as_str(), "E0401");

        // Custom error code
        assert_eq!(ErrorCode::Custom(1234).as_str(), "E1234");
    }

    #[test]
    fn test_error_code_description() {
        assert_eq!(ErrorCode::TypeMismatch.description(), "type mismatch");
        assert_eq!(ErrorCode::UnresolvedName.description(), "unresolved name");
        assert_eq!(ErrorCode::UnhandledEffect.description(), "unhandled effect");
    }

    #[test]
    fn test_error_code_display() {
        let code = ErrorCode::TypeMismatch;
        assert_eq!(format!("{}", code), "E0300");
    }

    #[test]
    fn test_diagnostic_help() {
        let span = Span::new(0, 5);
        let diag = Diagnostic::help("try this", span);
        assert_eq!(diag.level, Level::Help);
    }

    #[test]
    fn test_diagnostic_with_multiple_labels() {
        let span = Span::new(0, 5);
        let labels = vec![
            Label::secondary(Span::new(10, 15), "label 1"),
            Label::secondary(Span::new(20, 25), "label 2"),
        ];
        let diag = Diagnostic::error("test", span).with_labels(labels);
        assert_eq!(diag.labels.len(), 2);
    }

    #[test]
    fn test_diagnostic_with_multiple_notes() {
        let span = Span::new(0, 5);
        let notes = vec!["note 1", "note 2"];
        let diag = Diagnostic::error("test", span).with_notes(notes);
        assert_eq!(diag.notes.len(), 2);
        assert_eq!(diag.notes[0], "note 1");
        assert_eq!(diag.notes[1], "note 2");
    }

    #[test]
    fn test_diagnostic_is_error() {
        let span = Span::new(0, 5);
        let error = Diagnostic::error("error", span);
        let warning = Diagnostic::warning("warning", span);

        assert!(error.is_error());
        assert!(!warning.is_error());
        assert!(warning.is_warning());
    }

    #[test]
    fn test_diagnostic_with_multiple_suggestions() {
        let span = Span::new(0, 5);
        let suggestions = vec![
            Suggestion::new("fix 1", "replacement 1", span),
            Suggestion::new("fix 2", "replacement 2", span),
        ];
        let diag = Diagnostic::error("test", span).with_suggestions(suggestions);
        assert_eq!(diag.suggestions.len(), 2);
    }

    #[test]
    fn test_source_span_alias() {
        // Test that SourceSpan is an alias for Span
        let span: SourceSpan = Span::new(0, 5);
        assert_eq!(span.start, 0);
        assert_eq!(span.end, 5);
    }
}
