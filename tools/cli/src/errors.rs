//! Error handling and helpful error messages for the Jet CLI
//!
//! This module provides:
//! - Structured error types
//! - Helpful error messages with suggestions
//! - Error codes for common issues
//! - Context-aware diagnostics

#![allow(dead_code)]

use jet_diagnostics::{Diagnostic, Level};
use std::fmt;
use std::io::Write;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

/// CLI error codes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    /// General error
    General = 1,
    /// Compilation error
    Compilation = 2,
    /// Test failure
    TestFailure = 3,
    /// Lint violation
    LintViolation = 4,
    /// Format check failed
    FormatCheckFailed = 5,
    /// Manifest error
    ManifestError = 6,
    /// Dependency resolution error
    DependencyError = 7,
    /// Network error
    NetworkError = 8,
    /// Permission error
    PermissionError = 9,
    /// Not found
    NotFound = 10,
    /// Internal compiler error
    InternalError = 101,
}

impl ErrorCode {
    /// Get the exit code
    pub fn exit_code(self) -> i32 {
        self as i32
    }

    /// Get a human-readable description
    pub fn description(self) -> &'static str {
        match self {
            ErrorCode::General => "general error",
            ErrorCode::Compilation => "compilation failed",
            ErrorCode::TestFailure => "test failure",
            ErrorCode::LintViolation => "lint violation",
            ErrorCode::FormatCheckFailed => "format check failed",
            ErrorCode::ManifestError => "manifest error",
            ErrorCode::DependencyError => "dependency resolution failed",
            ErrorCode::NetworkError => "network error",
            ErrorCode::PermissionError => "permission denied",
            ErrorCode::NotFound => "not found",
            ErrorCode::InternalError => "internal compiler error",
        }
    }
}

/// A CLI error with context
#[derive(Debug)]
pub struct CliError {
    /// Error message
    pub message: String,
    /// Error code
    pub code: ErrorCode,
    /// Help text
    pub help: Option<String>,
    /// Suggested fix
    pub suggestion: Option<String>,
    /// Related diagnostics
    pub diagnostics: Vec<Diagnostic>,
    /// Source file (if applicable)
    pub source_file: Option<std::path::PathBuf>,
}

impl CliError {
    /// Create a new CLI error
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            code: ErrorCode::General,
            help: None,
            suggestion: None,
            diagnostics: vec![],
            source_file: None,
        }
    }

    /// Set the error code
    pub fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = code;
        self
    }

    /// Add help text
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    /// Add a suggestion
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// Add a diagnostic
    pub fn with_diagnostic(mut self, diagnostic: Diagnostic) -> Self {
        self.diagnostics.push(diagnostic);
        self
    }

    /// Set the source file
    pub fn with_source_file(mut self, path: impl Into<std::path::PathBuf>) -> Self {
        self.source_file = Some(path.into());
        self
    }

    /// Print the error to stderr
    pub fn print(&self) {
        let mut stderr = StandardStream::stderr(ColorChoice::Auto);

        // Print error header
        stderr
            .set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))
            .ok();
        write!(&mut stderr, "error").ok();
        stderr.reset().ok();

        // Print error code if present
        write!(&mut stderr, "[E{:04}]: ", self.code as i32).ok();

        // Print message
        writeln!(&mut stderr, "{}", self.message).ok();

        // Print source file if present
        if let Some(ref path) = self.source_file {
            stderr
                .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
                .ok();
            writeln!(&mut stderr, "  --> {}", path.display()).ok();
            stderr.reset().ok();
        }

        // Print diagnostics
        for diagnostic in &self.diagnostics {
            self.print_diagnostic(&mut stderr, diagnostic);
        }

        // Print help
        if let Some(ref help) = self.help {
            stderr
                .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)).set_bold(true))
                .ok();
            write!(&mut stderr, "help").ok();
            stderr.reset().ok();
            writeln!(&mut stderr, ": {}", help).ok();
        }

        // Print suggestion
        if let Some(ref suggestion) = self.suggestion {
            stderr
                .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)).set_bold(true))
                .ok();
            write!(&mut stderr, "suggestion").ok();
            stderr.reset().ok();
            writeln!(&mut stderr, ": {}", suggestion).ok();
        }

        writeln!(&mut stderr).ok();
    }

    fn print_diagnostic(&self, stderr: &mut StandardStream, diagnostic: &Diagnostic) {
        let color = match diagnostic.level {
            Level::Error => Color::Red,
            Level::Warning => Color::Yellow,
            Level::Note => Color::Cyan,
            Level::Help => Color::Green,
        };

        stderr
            .set_color(ColorSpec::new().set_fg(Some(color)).set_bold(true))
            .ok();
        write!(stderr, "{}", diagnostic.level.as_str()).ok();
        stderr.reset().ok();

        if let Some(ref code) = diagnostic.error_code {
            write!(stderr, "[{}]", code).ok();
        }

        writeln!(stderr, ": {}", diagnostic.message).ok();

        if diagnostic.span.line > 0 && diagnostic.span.column > 0 {
            writeln!(
                stderr,
                "    at line {}, column {}",
                diagnostic.span.line, diagnostic.span.column
            )
            .ok();
        }

        for note in &diagnostic.notes {
            writeln!(stderr, "    = note: {}", note).ok();
        }
    }
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for CliError {}

impl From<anyhow::Error> for CliError {
    fn from(err: anyhow::Error) -> Self {
        CliError::new(err.to_string())
    }
}

/// Common error constructors
pub mod common {
    use super::*;

    /// Create a "manifest not found" error
    pub fn manifest_not_found() -> CliError {
        CliError::new("Could not find jet.toml")
            .with_code(ErrorCode::ManifestError)
            .with_help("Are you in a Jet project directory?")
            .with_suggestion("Run `jet new <name>` to create a new project")
    }

    /// Create an "invalid manifest" error
    pub fn invalid_manifest(message: impl Into<String>) -> CliError {
        CliError::new(message)
            .with_code(ErrorCode::ManifestError)
            .with_suggestion("Check the TOML syntax in jet.toml")
    }

    /// Create a "compilation failed" error
    pub fn compilation_failed(_diagnostics: Vec<Diagnostic>) -> CliError {
        CliError::new("Compilation failed")
            .with_code(ErrorCode::Compilation)
            .with_help("Fix the reported errors and try again")
    }

    /// Create a "test failed" error
    pub fn test_failed(count: usize) -> CliError {
        CliError::new(format!("{} test(s) failed", count)).with_code(ErrorCode::TestFailure)
    }

    /// Create a "dependency not found" error
    pub fn dependency_not_found(name: impl Into<String>) -> CliError {
        CliError::new(format!("Dependency '{}' not found", name.into()))
            .with_code(ErrorCode::DependencyError)
            .with_suggestion("Check the dependency name and version in jet.toml")
    }

    /// Create a "network error" error
    pub fn network_error(message: impl Into<String>) -> CliError {
        CliError::new(format!("Network error: {}", message.into()))
            .with_code(ErrorCode::NetworkError)
            .with_help("Check your internet connection")
    }

    /// Create a "permission denied" error
    pub fn permission_denied(path: impl AsRef<std::path::Path>) -> CliError {
        CliError::new(format!("Permission denied: {}", path.as_ref().display()))
            .with_code(ErrorCode::PermissionError)
            .with_suggestion("Check file permissions or run with appropriate privileges")
    }

    /// Create a "file not found" error
    pub fn file_not_found(path: impl AsRef<std::path::Path>) -> CliError {
        CliError::new(format!("File not found: {}", path.as_ref().display()))
            .with_code(ErrorCode::NotFound)
    }

    /// Create an "internal compiler error" error
    pub fn internal_error(message: impl Into<String>) -> CliError {
        CliError::new(format!("Internal compiler error: {}", message.into()))
            .with_code(ErrorCode::InternalError)
            .with_help("This is a bug in the Jet compiler")
            .with_suggestion("Please report this issue at https://github.com/jet-lang/jet/issues")
    }
}

/// Error reporter for batching and displaying errors
pub struct ErrorReporter {
    errors: Vec<CliError>,
    warnings: Vec<String>,
}

impl ErrorReporter {
    /// Create a new error reporter
    pub fn new() -> Self {
        Self {
            errors: vec![],
            warnings: vec![],
        }
    }

    /// Add an error
    pub fn error(&mut self, error: CliError) {
        self.errors.push(error);
    }

    /// Add a warning
    pub fn warn(&mut self, message: impl Into<String>) {
        self.warnings.push(message.into());
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get the number of errors
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    /// Get the number of warnings
    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }

    /// Print all errors and warnings
    pub fn report(&self) {
        for warning in &self.warnings {
            self.print_warning(warning);
        }

        for error in &self.errors {
            error.print();
        }

        if !self.errors.is_empty() {
            let mut stderr = StandardStream::stderr(ColorChoice::Auto);
            stderr
                .set_color(ColorSpec::new().set_fg(Some(Color::Red)).set_bold(true))
                .ok();
            writeln!(
                &mut stderr,
                "aborting due to {} error(s)",
                self.errors.len()
            )
            .ok();
            stderr.reset().ok();
        }
    }

    /// Get the most severe error code
    pub fn exit_code(&self) -> i32 {
        self.errors
            .iter()
            .map(|e| e.code.exit_code())
            .max()
            .unwrap_or(0)
    }

    fn print_warning(&self, message: &str) {
        let mut stderr = StandardStream::stderr(ColorChoice::Auto);
        stderr
            .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)).set_bold(true))
            .ok();
        write!(&mut stderr, "warning").ok();
        stderr.reset().ok();
        writeln!(&mut stderr, ": {}", message).ok();
    }
}

impl Default for ErrorReporter {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper functions for formatting error messages
pub mod format {
    /// Format a list of items with commas and "and"
    pub fn list(items: &[impl AsRef<str>]) -> String {
        match items.len() {
            0 => String::new(),
            1 => items[0].as_ref().to_string(),
            2 => format!("{} and {}", items[0].as_ref(), items[1].as_ref()),
            _ => {
                let all_but_last = items[..items.len() - 1]
                    .iter()
                    .map(|s| s.as_ref())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}, and {}", all_but_last, items.last().unwrap().as_ref())
            }
        }
    }

    /// Format a duration in a human-readable way
    pub fn duration(d: std::time::Duration) -> String {
        if d.as_secs() >= 60 {
            format!("{}m {:.1}s", d.as_secs() / 60, d.as_secs_f64() % 60.0)
        } else if d.as_secs() >= 1 {
            format!("{:.1}s", d.as_secs_f64())
        } else {
            format!("{}ms", d.as_millis())
        }
    }

    /// Format a byte size in a human-readable way
    pub fn bytes(bytes: u64) -> String {
        const UNITS: &[&str] = &["B", "KB", "MB", "GB", "TB"];

        if bytes == 0 {
            return "0 B".to_string();
        }

        let exp = (bytes as f64).log(1024.0).min(UNITS.len() as f64 - 1.0) as usize;
        let value = bytes as f64 / 1024f64.powi(exp as i32);

        if exp == 0 {
            format!("{} {}", bytes, UNITS[0])
        } else {
            format!("{:.1} {}", value, UNITS[exp])
        }
    }
}

/// Print a success message
pub fn print_success(message: impl AsRef<str>) {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    stdout
        .set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true))
        .ok();
    write!(&mut stdout, "success").ok();
    stdout.reset().ok();
    writeln!(&mut stdout, ": {}", message.as_ref()).ok();
}

/// Print an info message
pub fn print_info(message: impl AsRef<str>) {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    stdout
        .set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
        .ok();
    write!(&mut stdout, "info").ok();
    stdout.reset().ok();
    writeln!(&mut stdout, ": {}", message.as_ref()).ok();
}

/// Print a warning message
pub fn print_warning(message: impl AsRef<str>) {
    let mut stderr = StandardStream::stderr(ColorChoice::Auto);
    stderr
        .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)).set_bold(true))
        .ok();
    write!(&mut stderr, "warning").ok();
    stderr.reset().ok();
    writeln!(&mut stderr, ": {}", message.as_ref()).ok();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_code_exit_codes() {
        assert_eq!(ErrorCode::General.exit_code(), 1);
        assert_eq!(ErrorCode::Compilation.exit_code(), 2);
        assert_eq!(ErrorCode::InternalError.exit_code(), 101);
    }

    #[test]
    fn test_cli_error_builder() {
        let error = CliError::new("test error")
            .with_code(ErrorCode::ManifestError)
            .with_help("this is help")
            .with_suggestion("try this");

        assert_eq!(error.message, "test error");
        assert_eq!(error.code, ErrorCode::ManifestError);
        assert_eq!(error.help, Some("this is help".to_string()));
        assert_eq!(error.suggestion, Some("try this".to_string()));
    }

    #[test]
    fn test_error_reporter() {
        let mut reporter = ErrorReporter::new();

        assert!(!reporter.has_errors());
        assert_eq!(reporter.error_count(), 0);

        reporter.error(CliError::new("error 1"));
        reporter.error(CliError::new("error 2"));
        reporter.warn("warning 1");

        assert!(reporter.has_errors());
        assert_eq!(reporter.error_count(), 2);
        assert_eq!(reporter.warning_count(), 1);
    }

    #[test]
    fn test_format_list() {
        assert_eq!(format::list(&[] as &[&str]), "");
        assert_eq!(format::list(&["a"]), "a");
        assert_eq!(format::list(&["a", "b"]), "a and b");
        assert_eq!(format::list(&["a", "b", "c"]), "a, b, and c");
    }

    #[test]
    fn test_format_duration() {
        assert!(format::duration(std::time::Duration::from_millis(500)).contains("ms"));
        assert!(format::duration(std::time::Duration::from_secs(5)).contains("s"));
        assert!(format::duration(std::time::Duration::from_secs(90)).contains("m"));
    }

    #[test]
    fn test_format_bytes() {
        assert_eq!(format::bytes(0), "0 B");
        assert_eq!(format::bytes(100), "100 B");
        assert!(format::bytes(1024).contains("KB"));
        assert!(format::bytes(1024 * 1024).contains("MB"));
    }
}
