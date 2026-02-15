//! Test Harness for Jet Programs
//!
//! Provides utilities for compiling and running Jet programs in tests.

use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use tempfile::{tempdir, TempDir};

/// Result of compiling a Jet program
#[derive(Debug, Clone)]
pub struct CompileResult {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
    pub exit_code: Option<i32>,
    pub output_path: Option<PathBuf>,
}

/// Result of running a compiled Jet program
#[derive(Debug, Clone)]
pub struct RunResult {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
    pub exit_code: Option<i32>,
}

/// Test harness for compiling and running Jet programs
pub struct TestHarness {
    #[allow(dead_code)]
    temp_dir: TempDir,
    project_dir: PathBuf,
}

impl TestHarness {
    /// Create a new test harness with a temporary project
    pub fn new() -> Result<Self> {
        let temp_dir = tempdir().context("Failed to create temp directory")?;
        let project_dir = temp_dir.path().to_path_buf();

        // Create project structure
        std::fs::create_dir_all(project_dir.join("src"))?;

        // Create jet.toml
        let toml = r#"[package]
name = "test_project"
version = "1.0.0"
edition = "2024"
"#;
        std::fs::write(project_dir.join("jet.toml"), toml)?;

        Ok(Self {
            temp_dir,
            project_dir,
        })
    }

    /// Get the project directory path
    pub fn project_dir(&self) -> &Path {
        &self.project_dir
    }

    /// Get the source directory path
    pub fn src_dir(&self) -> PathBuf {
        self.project_dir.join("src")
    }

    /// Write a Jet source file
    pub fn write_source(&self, filename: &str, content: &str) -> Result<PathBuf> {
        let path = self.src_dir().join(filename);
        std::fs::write(&path, content)
            .with_context(|| format!("Failed to write {}", path.display()))?;
        Ok(path)
    }

    /// Write the main.jet file
    pub fn write_main(&self, content: &str) -> Result<PathBuf> {
        self.write_source("main.jet", content)
    }

    /// Compile the project
    pub fn compile(&self) -> Result<CompileResult> {
        self.compile_with_args(&[])
    }

    /// Compile the project with additional arguments
    pub fn compile_with_args(&self, args: &[&str]) -> Result<CompileResult> {
        // Find the jet compiler binary
        let jet_binary = self.find_jet_binary()?;

        // Run the actual jet compiler
        let mut cmd = std::process::Command::new(&jet_binary);
        cmd.current_dir(&self.project_dir).arg("build").args(args);

        let output = cmd.output().with_context(|| {
            format!("Failed to execute jet compiler at {}", jet_binary.display())
        })?;

        let success = output.status.success();
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let exit_code = output.status.code();

        // Determine output path if compilation succeeded
        let output_path = if success {
            let toml_path = self.project_dir.join("jet.toml");
            let binary_name = toml_path
                .parent()
                .and_then(|p| p.file_name())
                .and_then(|n| n.to_str())
                .unwrap_or("test_project");

            Some(self.project_dir.join("debug").join(binary_name))
        } else {
            None
        };

        Ok(CompileResult {
            success,
            stdout,
            stderr,
            exit_code,
            output_path,
        })
    }

    /// Find the jet compiler binary
    fn find_jet_binary(&self) -> Result<PathBuf> {
        // Try to find jet binary in the workspace
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let workspace_root = manifest_dir.parent().unwrap();

        // Check debug build first, then release
        let candidates = vec![
            workspace_root.join("target/debug/jet"),
            workspace_root.join("target/release/jet"),
        ];

        for candidate in candidates {
            if candidate.exists() {
                return Ok(candidate);
            }
        }

        anyhow::bail!(
            "Jet compiler binary not found. Please run 'cargo build -p jet-cli' first.\nSearched: {:?}",
            workspace_root.join("target/debug/jet")
        )
    }

    /// Run the compiled program
    pub fn run(&self) -> Result<RunResult> {
        self.run_with_args(&[])
    }

    /// Run the compiled program with arguments
    pub fn run_with_args(&self, args: &[&str]) -> Result<RunResult> {
        // Find the compiled binary
        let binary_name = "test_project"; // From jet.toml
        let binary_path = self.project_dir.join("debug").join(binary_name);

        if !binary_path.exists() {
            anyhow::bail!(
                "Compiled binary not found at {}. Did compilation succeed?",
                binary_path.display()
            );
        }

        // Execute the binary
        let mut cmd = std::process::Command::new(&binary_path);
        cmd.args(args);

        let output = cmd
            .output()
            .with_context(|| format!("Failed to execute binary at {}", binary_path.display()))?;

        Ok(RunResult {
            success: output.status.success(),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            exit_code: output.status.code(),
        })
    }

    /// Compile and run the program
    pub fn compile_and_run(&self) -> Result<RunResult> {
        let compile_result = self.compile()?;
        if !compile_result.success {
            return Ok(RunResult {
                success: false,
                stdout: String::new(),
                stderr: compile_result.stderr,
                exit_code: compile_result.exit_code,
            });
        }
        self.run()
    }

    /// Check that the program compiles successfully
    pub fn check_compiles(&self) -> Result<()> {
        let result = self.compile()?;
        if !result.success {
            anyhow::bail!("Compilation failed: {}", result.stderr);
        }
        Ok(())
    }

    /// Check that the program fails to compile with expected error
    pub fn check_fails_with_error(&self, expected_error: &str) -> Result<()> {
        let result = self.compile()?;
        if result.success {
            anyhow::bail!("Expected compilation to fail, but it succeeded");
        }
        if !result.stderr.contains(expected_error) {
            anyhow::bail!(
                "Expected error containing '{}', but got: {}",
                expected_error,
                result.stderr
            );
        }
        Ok(())
    }
}

/// Compile a Jet source string and return the result
pub fn compile_source(source: &str) -> Result<CompileResult> {
    let harness = TestHarness::new()?;
    harness.write_main(source)?;
    harness.compile()
}

/// Parse a Jet source string and return the AST
pub fn parse_source(source: &str) -> Result<jet_parser::ast::Module> {
    let tokens = jet_lexer::Lexer::new(source).tokenize();

    // Check for lexer errors
    for token in &tokens {
        if let jet_lexer::Token::Error(ref msg) = token.token {
            anyhow::bail!("Lex error: {}", msg);
        }
    }

    let mut parser = jet_parser::Parser::new(tokens);
    parser
        .parse_module()
        .map_err(|e| anyhow::anyhow!("{:?}", e))
}

/// Tokenize a Jet source string and return the tokens
pub fn tokenize_source(source: &str) -> Vec<jet_lexer::SpannedToken> {
    jet_lexer::Lexer::new(source).tokenize()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_harness_creation() {
        let harness = TestHarness::new().unwrap();
        assert!(harness.project_dir().exists());
        assert!(harness.src_dir().exists());
    }

    #[test]
    #[ignore = "Parser indentation handling needs review"]
    fn test_write_and_compile() {
        let harness = TestHarness::new().unwrap();
        harness.write_main("fn main():\n    42\n").unwrap();

        let result = harness.compile().unwrap();
        assert!(result.success, "Compilation failed: {}", result.stderr);
    }

    #[test]
    fn test_compile_error() {
        let harness = TestHarness::new().unwrap();
        harness.write_main("fn main()\n    pass\n").unwrap(); // Missing colon

        let result = harness.compile().unwrap();
        assert!(!result.success);
    }
}
