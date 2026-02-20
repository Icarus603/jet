//! Jet Documentation Generator
//!
//! This crate provides documentation generation for the Jet programming language,
//! including:
//!
//! - Extracting doc comments from source files
//! - Running executable examples in doc comments
//! - Generating beautiful HTML documentation with search
//! - Displaying AI annotations (confidence, generated_by)
//! - Type-driven documentation generation
#![allow(dead_code, unused_imports)]
#![allow(clippy::manual_strip, clippy::manual_split_once)]

pub mod extract;
pub mod html;
pub mod test_examples;

use anyhow::Result;
use extract::DocModule;
use std::path::{Path, PathBuf};

/// Configuration for documentation generation
#[derive(Debug, Clone)]
pub struct DocConfig {
    /// Output directory for generated docs
    pub output_dir: PathBuf,
    /// Whether to include private items
    pub include_private: bool,
    /// Whether to document dependencies
    pub document_deps: bool,
    /// Project name
    pub project_name: String,
    /// Project version
    pub project_version: String,
}

impl Default for DocConfig {
    fn default() -> Self {
        Self {
            output_dir: PathBuf::from("target/doc"),
            include_private: false,
            document_deps: true,
            project_name: String::from("Unnamed Project"),
            project_version: String::from("0.1.0"),
        }
    }
}

/// A documentation item with metadata
#[derive(Debug, Clone)]
pub struct DocItem {
    /// The kind of documentation item
    pub kind: DocItemKind,
    /// The name of the item
    pub name: String,
    /// The module path
    pub module_path: Vec<String>,
    /// Doc comment content (markdown)
    pub doc_comment: Option<String>,
    /// AI annotations if present
    pub ai_annotations: Option<AiAnnotations>,
    /// Source location
    pub source_file: PathBuf,
    /// Line number in source
    pub line_number: usize,
    /// Whether the item is public
    pub is_public: bool,
    /// Type signature (if available)
    pub type_signature: Option<String>,
}

/// The kind of documentation item
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DocItemKind {
    Function,
    Struct,
    Enum,
    Trait,
    TypeAlias,
    Constant,
    Module,
    Effect,
}

/// AI annotations extracted from doc comments
#[derive(Debug, Clone, Default)]
pub struct AiAnnotations {
    /// Confidence level (0.0 - 1.0)
    pub confidence: Option<f32>,
    /// Tool that generated the code
    pub generated_by: Option<String>,
    /// Prompt used for generation
    pub prompt: Option<String>,
    /// Number of human edits
    pub human_edit_count: Option<u32>,
}

/// An executable example from a doc comment
#[derive(Debug, Clone)]
pub struct DocExample {
    /// The code to execute
    pub code: String,
    /// Expected output (if specified)
    pub expected_output: Option<String>,
    /// Whether the example should fail
    pub should_panic: bool,
    /// Line number in source
    pub line_number: usize,
}

/// Documentation database for a project
#[derive(Debug, Clone)]
pub struct DocDatabase {
    /// All documented items
    pub items: Vec<DocItem>,
    /// Configuration used
    pub config: DocConfig,
}

impl DocDatabase {
    /// Create a new empty documentation database
    pub fn new(config: DocConfig) -> Self {
        Self {
            items: Vec::new(),
            config,
        }
    }

    /// Add a documented item
    pub fn add_item(&mut self, item: DocItem) {
        self.items.push(item);
    }

    /// Get all items of a specific kind
    pub fn items_of_kind(&self, kind: DocItemKind) -> impl Iterator<Item = &DocItem> {
        self.items.iter().filter(move |item| item.kind == kind)
    }

    /// Search for items by name
    pub fn search(&self, query: &str) -> Vec<&DocItem> {
        let query_lower = query.to_lowercase();
        self.items
            .iter()
            .filter(|item| {
                item.name.to_lowercase().contains(&query_lower)
                    || item
                        .doc_comment
                        .as_ref()
                        .map(|d| d.to_lowercase().contains(&query_lower))
                        .unwrap_or(false)
            })
            .collect()
    }

    /// Get items in a specific module
    pub fn items_in_module(&self, module_path: &[String]) -> Vec<&DocItem> {
        self.items
            .iter()
            .filter(|item| item.module_path == module_path)
            .collect()
    }
}

/// Generate documentation for a project
pub async fn generate_docs(source_paths: &[PathBuf], config: DocConfig) -> Result<DocDatabase> {
    let mut database = DocDatabase::new(config.clone());

    for path in source_paths {
        if path.is_file() && path.extension().map(|e| e == "jet").unwrap_or(false) {
            let module = extract::extract_docs_from_file(path).await?;
            process_module(&mut database, module, &[])?;
        } else if path.is_dir() {
            process_directory(&mut database, path, &[]).await?;
        }
    }

    Ok(database)
}

async fn process_directory(
    database: &mut DocDatabase,
    dir: &Path,
    module_path: &[String],
) -> Result<()> {
    let mut entries = tokio::fs::read_dir(dir).await?;

    while let Some(entry) = entries.next_entry().await? {
        let path = entry.path();
        let file_name = path.file_stem().and_then(|s| s.to_str()).map(String::from);

        if path.is_file() && path.extension().map(|e| e == "jet").unwrap_or(false) {
            if let Some(name) = file_name {
                let mut new_path = module_path.to_vec();
                new_path.push(name);
                let module = extract::extract_docs_from_file(&path).await?;
                process_module(database, module, &new_path)?;
            }
        } else if path.is_dir() {
            if let Some(name) = file_name {
                let mut new_path = module_path.to_vec();
                new_path.push(name);
                Box::pin(process_directory(database, &path, &new_path)).await?;
            }
        }
    }

    Ok(())
}

fn process_module(
    database: &mut DocDatabase,
    module: DocModule,
    module_path: &[String],
) -> Result<()> {
    for item in module.items {
        let doc_item = DocItem {
            kind: item.kind,
            name: item.name,
            module_path: module_path.to_vec(),
            doc_comment: item.doc_comment,
            ai_annotations: item.ai_annotations,
            source_file: module.source_file.clone(),
            line_number: item.line_number,
            is_public: item.is_public,
            type_signature: item.type_signature,
        };

        // Filter private items if not including them
        if !doc_item.is_public && !database.config.include_private {
            continue;
        }

        database.add_item(doc_item);
    }

    Ok(())
}

/// Run doc tests for the given source files
pub async fn run_doc_tests(source_paths: &[PathBuf]) -> Result<DocTestResults> {
    let mut results = DocTestResults::default();

    for path in source_paths {
        if path.is_file() && path.extension().map(|e| e == "jet").unwrap_or(false) {
            let file_results = test_examples::run_doc_tests_in_file(path).await?;
            results.total += file_results.total;
            results.passed += file_results.passed;
            results.failed += file_results.failed;
            results.failures.extend(file_results.failures);
        }
    }

    Ok(results)
}

/// Results from running doc tests
#[derive(Debug, Clone, Default)]
pub struct DocTestResults {
    /// Total number of tests
    pub total: usize,
    /// Number of passed tests
    pub passed: usize,
    /// Number of failed tests
    pub failed: usize,
    /// Failed test details
    pub failures: Vec<DocTestFailure>,
}

impl DocTestResults {
    /// Check if all tests passed
    pub fn all_passed(&self) -> bool {
        self.failed == 0
    }
}

/// A failed doc test
#[derive(Debug, Clone)]
pub struct DocTestFailure {
    /// Source file path
    pub file: PathBuf,
    /// Line number
    pub line: usize,
    /// Test code
    pub code: String,
    /// Error message
    pub error: String,
}
