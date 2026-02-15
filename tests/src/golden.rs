//! Golden File Testing
//!
//! Golden file tests compare actual output against expected output stored in files.
//! If the output changes, the test fails and can be blessed to update the golden file.

use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};

/// A golden file test
pub struct GoldenFile {
    /// Path to the golden file
    path: PathBuf,
    /// The actual content
    actual: String,
    /// Whether to update the golden file on mismatch
    bless: bool,
}

impl GoldenFile {
    /// Create a new golden file test
    pub fn new(path: impl AsRef<Path>, actual: impl Into<String>) -> Self {
        Self {
            path: path.as_ref().to_path_buf(),
            actual: actual.into(),
            bless: std::env::var("BLESS_GOLDEN").is_ok(),
        }
    }

    /// Set whether to bless (update) the golden file
    pub fn with_bless(mut self, bless: bool) -> Self {
        self.bless = bless;
        self
    }

    /// Assert that the actual content matches the golden file
    pub fn assert(&self) -> Result<()> {
        if !self.path.exists() {
            if self.bless {
                // Create the golden file
                if let Some(parent) = self.path.parent() {
                    fs::create_dir_all(parent)?;
                }
                fs::write(&self.path, &self.actual).with_context(|| {
                    format!("Failed to write golden file: {}", self.path.display())
                })?;
                println!("Created golden file: {}", self.path.display());
                return Ok(());
            } else {
                anyhow::bail!(
                    "Golden file does not exist: {}\nRun with BLESS_GOLDEN=1 to create it.",
                    self.path.display()
                );
            }
        }

        let expected = fs::read_to_string(&self.path)
            .with_context(|| format!("Failed to read golden file: {}", self.path.display()))?;

        if expected != self.actual {
            if self.bless {
                fs::write(&self.path, &self.actual).with_context(|| {
                    format!("Failed to update golden file: {}", self.path.display())
                })?;
                println!("Updated golden file: {}", self.path.display());
                return Ok(());
            } else {
                anyhow::bail!(
                    "Golden file mismatch: {}\n\nExpected:\n{}\n\nActual:\n{}\n\nRun with BLESS_GOLDEN=1 to update.",
                    self.path.display(),
                    expected,
                    self.actual
                );
            }
        }

        Ok(())
    }
}

/// Trait for types that can be golden tested
pub trait GoldenTest {
    /// Get the content to compare against the golden file
    fn golden_content(&self) -> String;

    /// Run the golden file test
    fn golden_test(&self, path: impl AsRef<Path>) -> Result<()> {
        GoldenFile::new(path, self.golden_content()).assert()
    }
}

/// Compare actual output against a golden file
pub fn assert_golden(path: impl AsRef<Path>, actual: impl Into<String>) -> Result<()> {
    GoldenFile::new(path, actual).assert()
}

/// Create or update a golden file
pub fn bless_golden(path: impl AsRef<Path>, content: impl Into<String>) -> Result<()> {
    GoldenFile::new(path, content).with_bless(true).assert()
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_golden_file_match() {
        let temp_dir = TempDir::new().unwrap();
        let golden_path = temp_dir.path().join("test.golden");

        fs::write(&golden_path, "expected content").unwrap();

        let golden = GoldenFile::new(&golden_path, "expected content");
        golden.assert().unwrap();
    }

    #[test]
    fn test_golden_file_mismatch() {
        let temp_dir = TempDir::new().unwrap();
        let golden_path = temp_dir.path().join("test.golden");

        fs::write(&golden_path, "expected content").unwrap();

        // Explicitly disable bless mode to test mismatch behavior
        let golden = GoldenFile::new(&golden_path, "actual content").with_bless(false);
        assert!(golden.assert().is_err());
    }

    #[test]
    fn test_golden_file_bless() {
        let temp_dir = TempDir::new().unwrap();
        let golden_path = temp_dir.path().join("test.golden");

        fs::write(&golden_path, "old content").unwrap();

        let golden = GoldenFile::new(&golden_path, "new content").with_bless(true);
        golden.assert().unwrap();

        let updated = fs::read_to_string(&golden_path).unwrap();
        assert_eq!(updated, "new content");
    }

    #[test]
    fn test_golden_file_create() {
        let temp_dir = TempDir::new().unwrap();
        let golden_path = temp_dir.path().join("new/test.golden");

        let golden = GoldenFile::new(&golden_path, "new content").with_bless(true);
        golden.assert().unwrap();

        assert!(golden_path.exists());
        let content = fs::read_to_string(&golden_path).unwrap();
        assert_eq!(content, "new content");
    }
}
