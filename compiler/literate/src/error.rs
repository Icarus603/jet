//! Error types for literate programming support

use std::fmt;

/// Result type for literate programming operations
pub type LiterateResult<T> = Result<T, LiterateError>;

/// Errors that can occur during literate programming operations
#[derive(Debug, Clone, PartialEq)]
pub enum LiterateError {
    /// Parse error from the Jet parser
    ParseError(String),
    /// Invalid markdown format
    InvalidMarkdown(String),
    /// Missing code block
    MissingCodeBlock(String),
    /// Weaving error
    WeaveError(String),
    /// Tangling error
    TangleError(String),
}

impl fmt::Display for LiterateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiterateError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            LiterateError::InvalidMarkdown(msg) => {
                write!(f, "Invalid markdown: {}", msg)
            }
            LiterateError::MissingCodeBlock(msg) => {
                write!(f, "Missing code block: {}", msg)
            }
            LiterateError::WeaveError(msg) => write!(f, "Weave error: {}", msg),
            LiterateError::TangleError(msg) => write!(f, "Tangle error: {}", msg),
        }
    }
}

impl std::error::Error for LiterateError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = LiterateError::ParseError("unexpected token".to_string());
        assert_eq!(err.to_string(), "Parse error: unexpected token");

        let err = LiterateError::InvalidMarkdown("unclosed block".to_string());
        assert_eq!(err.to_string(), "Invalid markdown: unclosed block");
    }
}
