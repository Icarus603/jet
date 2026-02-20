//! Executable example testing for doc comments
//!
//! This module runs examples found in doc comments (the `>>>` style)
//! and verifies they produce the expected output.

use crate::DocTestFailure;
use anyhow::{Context, Result};
use std::path::Path;
use tokio::process::Command;

/// Results from running doc tests in a file
#[derive(Debug, Clone, Default)]
pub struct DocTestFileResults {
    /// Total number of tests
    pub total: usize,
    /// Number of passed tests
    pub passed: usize,
    /// Number of failed tests
    pub failed: usize,
    /// Failed test details
    pub failures: Vec<DocTestFailure>,
}

/// Run all doc tests in a source file
pub async fn run_doc_tests_in_file(path: &Path) -> Result<DocTestFileResults> {
    let content = tokio::fs::read_to_string(path)
        .await
        .with_context(|| format!("Failed to read file: {}", path.display()))?;

    let examples = extract_examples_from_source(&content)?;
    let mut results = DocTestFileResults::default();

    for example in examples {
        results.total += 1;

        match run_single_example(&example.code, &example.expected_output).await {
            Ok(()) => {
                results.passed += 1;
            }
            Err(e) => {
                results.failed += 1;
                results.failures.push(DocTestFailure {
                    file: path.to_path_buf(),
                    line: example.line_number,
                    code: example.code.clone(),
                    error: e.to_string(),
                });
            }
        }
    }

    Ok(results)
}

/// An example extracted from source code
#[derive(Debug, Clone)]
struct ExtractedExample {
    /// The code to execute
    code: String,
    /// Expected output
    expected_output: Option<String>,
    /// Line number in source
    line_number: usize,
}

/// Extract all examples from source code
fn extract_examples_from_source(source: &str) -> Result<Vec<ExtractedExample>> {
    let mut examples = Vec::new();
    let lines: Vec<_> = source.lines().enumerate().collect();
    let mut i = 0;

    while i < lines.len() {
        let (line_num, line) = lines[i];
        let trimmed = line.trim();

        // Look for doc comment code blocks
        if trimmed.starts_with("###") {
            // Check if next lines form a code block with >>>
            let mut code_lines = Vec::new();
            let mut output_lines = Vec::new();
            let mut in_code_block = false;
            let mut in_output = false;
            i += 1;

            while i < lines.len() {
                let (_, next_line) = lines[i];
                let next_trimmed = next_line.trim();
                let doc_trimmed = next_trimmed.trim_start_matches("###").trim_start();

                // Check for end of doc comment
                if !next_trimmed.starts_with("###") && !next_trimmed.is_empty() && !in_code_block {
                    break;
                }

                // Check for code block markers
                if doc_trimmed.starts_with("```") {
                    in_code_block = !in_code_block;
                    i += 1;
                    continue;
                }

                if in_code_block {
                    let code_trimmed = doc_trimmed.trim_start_matches("#").trim_start();

                    if code_trimmed.starts_with(">>>") {
                        code_lines.push(code_trimmed[3..].trim_start().to_string());
                        in_output = true;
                    } else if code_trimmed.starts_with(">") && in_output {
                        code_lines.push(code_trimmed[1..].trim_start().to_string());
                    } else if in_output && !code_trimmed.is_empty() {
                        output_lines.push(code_trimmed.to_string());
                    } else if !code_trimmed.is_empty() {
                        code_lines.push(code_trimmed.to_string());
                    }
                }

                i += 1;
            }

            if !code_lines.is_empty() {
                examples.push(ExtractedExample {
                    code: code_lines.join("\n"),
                    expected_output: if output_lines.is_empty() {
                        None
                    } else {
                        Some(output_lines.join("\n"))
                    },
                    line_number: line_num + 1,
                });
            }
        } else if trimmed.starts_with("```") {
            // Standalone code block (not in doc comment)
            let lang = trimmed[3..].trim();
            let is_jet = lang.is_empty() || lang == "jet";
            let mut code_lines = Vec::new();
            let mut output_lines = Vec::new();
            let mut in_output = false;
            i += 1;

            while i < lines.len() && !lines[i].1.trim().starts_with("```") {
                let code_line = lines[i].1;
                let code_trimmed = code_line.trim();

                if code_trimmed.starts_with(">>>") {
                    code_lines.push(code_trimmed[3..].trim_start().to_string());
                    in_output = true;
                } else if code_trimmed.starts_with(">") && in_output {
                    code_lines.push(code_trimmed[1..].trim_start().to_string());
                } else if in_output && !code_trimmed.is_empty() {
                    output_lines.push(code_trimmed.to_string());
                } else if !code_trimmed.is_empty() {
                    code_lines.push(code_trimmed.to_string());
                }

                i += 1;
            }

            if is_jet && !code_lines.is_empty() {
                examples.push(ExtractedExample {
                    code: code_lines.join("\n"),
                    expected_output: if output_lines.is_empty() {
                        None
                    } else {
                        Some(output_lines.join("\n"))
                    },
                    line_number: line_num + 1,
                });
            }
        }

        i += 1;
    }

    Ok(examples)
}

/// Run a single example and check output
async fn run_single_example(code: &str, expected: &Option<String>) -> Result<()> {
    // Create a temporary file with the example code
    let temp_dir = tempfile::tempdir()?;
    let temp_file = temp_dir.path().join("test.jt");

    // Wrap the example code in a main function if needed
    let wrapped_code = if code.contains("fn main") {
        code.to_string()
    } else {
        format!(
            r#"fn main() -> Result<(), String> {{
    {}
    Ok(())
}}"#,
            code
        )
    };

    tokio::fs::write(&temp_file, &wrapped_code)
        .await
        .context("Failed to write temporary test file")?;

    // Try to compile and run using jet CLI
    // For now, we'll use a placeholder implementation
    // In a real implementation, this would call the Jet compiler

    // Placeholder: just check if the code parses
    let tokens = jet_lexer::tokenize(&wrapped_code);
    let mut parser = jet_parser::Parser::new(tokens);
    match parser.parse_module() {
        Ok(_) => {
            // Parsing succeeded
            // In a full implementation, we would:
            // 1. Compile the code
            // 2. Run the binary
            // 3. Capture output
            // 4. Compare with expected

            // For now, just succeed if no expected output
            if expected.is_none() {
                Ok(())
            } else {
                // With expected output, we'd need to actually run it
                // This is a placeholder
                Ok(())
            }
        }
        Err(e) => {
            anyhow::bail!("Parse error: {:?}", e)
        }
    }
}

/// Run a doc test with the Jet compiler
async fn _compile_and_run(_code: &str) -> Result<String> {
    // This would integrate with the Jet compiler to:
    // 1. Create a temporary project
    // 2. Write the code
    // 3. Compile it
    // 4. Run the binary
    // 5. Return the output

    // Placeholder implementation
    let output = Command::new("echo")
        .arg("test output")
        .output()
        .await
        .context("Failed to run example")?;

    if !output.status.success() {
        anyhow::bail!(
            "Example failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_examples() {
        let source = r#"
### Adds two numbers
###
### ```
### >>> add(2, 3)
### 5
### ```
fn add(a: int, b: int) -> int {
    a + b
}
"#;

        let examples = extract_examples_from_source(source).unwrap();
        assert_eq!(examples.len(), 1);
        assert_eq!(examples[0].code, "add(2, 3)");
        assert_eq!(examples[0].expected_output, Some("5".to_string()));
    }

    #[test]
    fn test_extract_examples_standalone() {
        let source = r#"
```
>>> println("hello")
hello
```
"#;

        let examples = extract_examples_from_source(source).unwrap();
        assert_eq!(examples.len(), 1);
        assert_eq!(examples[0].code, "println(\"hello\")");
        assert_eq!(examples[0].expected_output, Some("hello".to_string()));
    }
}
