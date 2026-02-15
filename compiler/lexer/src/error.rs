use std::fmt;

/// Errors that can occur during lexing
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    /// Invalid character encountered
    InvalidChar {
        ch: char,
        line: usize,
        column: usize,
    },
    /// Unterminated string literal
    UnterminatedString { line: usize, column: usize },
    /// Unterminated character literal
    UnterminatedChar { line: usize, column: usize },
    /// Invalid escape sequence
    InvalidEscape {
        seq: String,
        line: usize,
        column: usize,
    },
    /// Mixed tabs and spaces in indentation
    MixedIndentation { line: usize },
    /// Inconsistent indentation
    InconsistentIndent {
        line: usize,
        expected: usize,
        found: usize,
    },
    /// Invalid numeric literal
    InvalidNumber {
        msg: String,
        line: usize,
        column: usize,
    },
    /// Invalid Unicode escape
    InvalidUnicodeEscape {
        seq: String,
        line: usize,
        column: usize,
    },
    /// Tab character in indentation (if not allowed)
    TabInIndentation { line: usize },
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::InvalidChar { ch, line, column } => {
                write!(
                    f,
                    "Invalid character '{}' at line {}, column {}",
                    ch, line, column
                )
            }
            LexError::UnterminatedString { line, column } => {
                write!(
                    f,
                    "Unterminated string literal at line {}, column {}",
                    line, column
                )
            }
            LexError::UnterminatedChar { line, column } => {
                write!(
                    f,
                    "Unterminated character literal at line {}, column {}",
                    line, column
                )
            }
            LexError::InvalidEscape { seq, line, column } => {
                write!(
                    f,
                    "Invalid escape sequence '{}' at line {}, column {}",
                    seq, line, column
                )
            }
            LexError::MixedIndentation { line } => {
                write!(f, "Mixed tabs and spaces in indentation at line {}", line)
            }
            LexError::InconsistentIndent {
                line,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Inconsistent indentation at line {}: expected {} spaces, found {}",
                    line, expected, found
                )
            }
            LexError::InvalidNumber { msg, line, column } => {
                write!(
                    f,
                    "Invalid number at line {}, column {}: {}",
                    line, column, msg
                )
            }
            LexError::InvalidUnicodeEscape { seq, line, column } => {
                write!(
                    f,
                    "Invalid Unicode escape '{}' at line {}, column {}",
                    seq, line, column
                )
            }
            LexError::TabInIndentation { line } => {
                write!(
                    f,
                    "Tab character in indentation at line {} (tabs not allowed)",
                    line
                )
            }
        }
    }
}

impl std::error::Error for LexError {}

/// Result type for lexing operations
pub type LexResult<T> = Result<T, LexError>;
