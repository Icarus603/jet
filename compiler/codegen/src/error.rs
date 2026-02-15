//! Error types for code generation.
//!
//! This module defines all errors that can occur during the LLVM code generation process.

use thiserror::Error;

/// An error that occurred during code generation.
#[derive(Error, Debug, Clone, PartialEq)]
pub enum CodegenError {
    /// An unsupported type was encountered.
    #[error("unsupported type: {0}")]
    UnsupportedType(String),

    /// An unsupported instruction was encountered.
    #[error("unsupported instruction: {0}")]
    UnsupportedInstruction(String),

    /// A value was not found in the value table.
    #[error("value not found: {0}")]
    ValueNotFound(u32),

    /// A function was not found.
    #[error("function not found: {0}")]
    FunctionNotFound(String),

    /// A basic block was not found.
    #[error("basic block not found: {0}")]
    BlockNotFound(u32),

    /// Type mismatch error.
    #[error("type mismatch: expected {expected}, got {actual}")]
    TypeMismatch {
        /// The expected type.
        expected: String,
        /// The actual type encountered.
        actual: String,
    },

    /// Invalid operand for an operation.
    #[error("invalid operand for {operation}: {reason}")]
    InvalidOperand {
        /// The operation being performed.
        operation: String,
        /// The reason the operand is invalid.
        reason: String,
    },

    /// An LLVM-related error.
    #[error("LLVM error: {0}")]
    LLVMError(String),

    /// Module verification failed.
    #[error("module verification failed: {0}")]
    VerificationFailed(String),

    /// A constant value could not be created.
    #[error("invalid constant: {0}")]
    InvalidConstant(String),

    /// An error occurred during instruction building.
    #[error("instruction building failed: {0}")]
    InstructionError(String),
}

impl CodegenError {
    /// Creates a new unsupported type error.
    pub fn unsupported_type(ty: impl Into<String>) -> Self {
        CodegenError::UnsupportedType(ty.into())
    }

    /// Creates a new unsupported instruction error.
    pub fn unsupported_instruction(inst: impl Into<String>) -> Self {
        CodegenError::UnsupportedInstruction(inst.into())
    }

    /// Creates a new value not found error.
    pub fn value_not_found(id: u32) -> Self {
        CodegenError::ValueNotFound(id)
    }

    /// Creates a new function not found error.
    pub fn function_not_found(name: impl Into<String>) -> Self {
        CodegenError::FunctionNotFound(name.into())
    }

    /// Creates a new block not found error.
    pub fn block_not_found(id: u32) -> Self {
        CodegenError::BlockNotFound(id)
    }

    /// Creates a new type mismatch error.
    pub fn type_mismatch(expected: impl Into<String>, actual: impl Into<String>) -> Self {
        CodegenError::TypeMismatch {
            expected: expected.into(),
            actual: actual.into(),
        }
    }

    /// Creates a new invalid operand error.
    pub fn invalid_operand(operation: impl Into<String>, reason: impl Into<String>) -> Self {
        CodegenError::InvalidOperand {
            operation: operation.into(),
            reason: reason.into(),
        }
    }

    /// Creates a new LLVM error.
    pub fn llvm_error(msg: impl Into<String>) -> Self {
        CodegenError::LLVMError(msg.into())
    }

    /// Creates a new verification failed error.
    pub fn verification_failed(msg: impl Into<String>) -> Self {
        CodegenError::VerificationFailed(msg.into())
    }

    /// Creates a new invalid constant error.
    pub fn invalid_constant(msg: impl Into<String>) -> Self {
        CodegenError::InvalidConstant(msg.into())
    }

    /// Creates a new instruction error.
    pub fn instruction_error(msg: impl Into<String>) -> Self {
        CodegenError::InstructionError(msg.into())
    }
}

/// A result type for code generation operations.
pub type CodegenResult<T> = Result<T, CodegenError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let err = CodegenError::unsupported_type("unknown_type");
        assert!(matches!(err, CodegenError::UnsupportedType(_)));
        assert!(err.to_string().contains("unknown_type"));

        let err = CodegenError::value_not_found(42);
        assert!(matches!(err, CodegenError::ValueNotFound(42)));

        let err = CodegenError::type_mismatch("i32", "f64");
        assert!(matches!(err, CodegenError::TypeMismatch { .. }));
        assert!(err.to_string().contains("i32"));
        assert!(err.to_string().contains("f64"));
    }

    #[test]
    fn test_error_messages() {
        assert_eq!(
            CodegenError::unsupported_type("custom").to_string(),
            "unsupported type: custom"
        );

        assert_eq!(
            CodegenError::value_not_found(5).to_string(),
            "value not found: 5"
        );

        assert_eq!(
            CodegenError::function_not_found("main").to_string(),
            "function not found: main"
        );
    }
}
