//! Parse Error Types

use jet_lexer::{Span, Token};
use std::fmt;

/// A parse error
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
    pub message: String,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span, message: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            message: message.into(),
        }
    }

    pub fn unexpected_token(expected: &str, found: &Token, span: Span) -> Self {
        Self::new(
            ParseErrorKind::UnexpectedToken,
            span,
            format!("expected {}, found {}", expected, found),
        )
    }

    pub fn unexpected_eof(expected: &str, span: Span) -> Self {
        Self::new(
            ParseErrorKind::UnexpectedEof,
            span,
            format!("expected {}, found end of file", expected),
        )
    }

    pub fn invalid_syntax(message: impl Into<String>, span: Span) -> Self {
        Self::new(ParseErrorKind::InvalidSyntax, span, message)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error at {:?}: {}", self.span, self.message)
    }
}

impl std::error::Error for ParseError {}

/// Kinds of parse errors
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseErrorKind {
    UnexpectedToken,
    UnexpectedEof,
    InvalidSyntax,
    InvalidIndentation,
    UnmatchedDelimiter,
    ReservedKeyword,
}

/// Result type for parsing
pub type ParseResult<T> = Result<T, ParseError>;

/// A collection of parse errors with partial AST
#[derive(Debug)]
pub struct ParseOutput<T> {
    pub ast: Option<T>,
    pub errors: Vec<ParseError>,
}

impl<T> ParseOutput<T> {
    pub fn new(ast: Option<T>) -> Self {
        Self {
            ast,
            errors: Vec::new(),
        }
    }

    pub fn with_error(mut self, error: ParseError) -> Self {
        self.errors.push(error);
        self
    }

    pub fn is_ok(&self) -> bool {
        self.errors.is_empty() && self.ast.is_some()
    }

    pub fn into_result(self) -> Result<T, Vec<ParseError>> {
        if self.errors.is_empty() {
            self.ast.ok_or_else(Vec::new)
        } else {
            Err(self.errors)
        }
    }
}

/// Error recovery strategy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecoveryStrategy {
    /// Skip to next statement boundary
    SkipToStatement,
    /// Skip to next block boundary
    SkipToBlock,
    /// Skip to next line
    SkipToLine,
    /// Skip single token
    SkipToken,
}
