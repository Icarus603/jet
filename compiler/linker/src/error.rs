//! Error types for the linker crate

use std::path::PathBuf;
use thiserror::Error;

/// Result type alias for linker operations
pub type LinkerResult<T> = Result<T, LinkerError>;

/// Errors that can occur during linking
#[derive(Error, Debug, Clone)]
pub enum LinkerError {
    /// Failed to invoke the system linker
    #[error("failed to invoke linker: {0}")]
    LinkerInvocation(String),

    /// Linker returned a non-zero exit code
    #[error("linker failed with exit code {exit_code}: {stderr}")]
    LinkerFailed { exit_code: i32, stderr: String },

    /// Object file not found
    #[error("object file not found: {0}")]
    ObjectFileNotFound(PathBuf),

    /// Runtime library not found
    #[error("runtime library not found: {library}")]
    RuntimeLibraryNotFound {
        library: String,
        search_paths: Vec<PathBuf>,
    },

    /// Invalid object file format
    #[error("invalid object file format: {0}")]
    InvalidObjectFormat(String),

    /// Platform not supported
    #[error("platform not supported: {platform}")]
    PlatformNotSupported { platform: String },

    /// Target triple not supported
    #[error("target triple not supported: {triple}")]
    TargetNotSupported { triple: String },

    /// Cache operation failed
    #[error("cache operation failed: {0}")]
    CacheError(String),

    /// IO error
    #[error("IO error: {0}")]
    Io(String),

    /// Build configuration error
    #[error("build configuration error: {0}")]
    ConfigError(String),

    /// Dependency resolution failed
    #[error("dependency resolution failed: {0}")]
    DependencyError(String),

    /// Cross-compilation not supported for target
    #[error("cross-compilation not supported: from {host} to {target}")]
    CrossCompilationNotSupported { host: String, target: String },
}

impl From<std::io::Error> for LinkerError {
    fn from(e: std::io::Error) -> Self {
        LinkerError::Io(e.to_string())
    }
}

/// Result type alias for build operations
pub type BuildResult<T> = Result<T, BuildError>;

/// Errors that can occur during the build process
#[derive(Error, Debug, Clone)]
pub enum BuildError {
    /// Compilation unit failed
    #[error("compilation failed for unit: {name}")]
    CompilationFailed {
        name: String,
        diagnostics: Vec<String>,
    },

    /// Manifest parsing error
    #[error("failed to parse manifest: {0}")]
    ManifestError(String),

    /// Source file error
    #[error("source file error: {path} - {message}")]
    SourceError { path: PathBuf, message: String },

    /// Linking error
    #[error("linking failed: {0}")]
    LinkError(#[from] LinkerError),

    /// Configuration error
    #[error("configuration error: {0}")]
    ConfigError(String),

    /// IO error
    #[error("IO error: {0}")]
    Io(String),

    /// Parallel compilation error
    #[error("parallel compilation error: {0}")]
    ParallelError(String),
}

impl From<std::io::Error> for BuildError {
    fn from(e: std::io::Error) -> Self {
        BuildError::Io(e.to_string())
    }
}
