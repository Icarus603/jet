//! Command history management for the REPL
//!
//! Handles persistent storage of command history.

use anyhow::{Context, Result};
use std::fs::{File, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::path::Path;

/// Manages command history
pub struct HistoryManager {
    file_path: std::path::PathBuf,
    max_size: usize,
    entries: Vec<String>,
}

impl HistoryManager {
    /// Create a new history manager
    pub fn new(file_path: &Path, max_size: usize) -> Result<Self> {
        let entries = if file_path.exists() {
            Self::load_entries(file_path, max_size)?
        } else {
            Vec::new()
        };

        Ok(Self {
            file_path: file_path.to_path_buf(),
            max_size,
            entries,
        })
    }

    /// Add an entry to the history
    pub fn add(&mut self, entry: &str) {
        // Don't add empty entries or duplicates of the last entry
        if entry.trim().is_empty() {
            return;
        }

        if let Some(last) = self.entries.last() {
            if last == entry {
                return;
            }
        }

        self.entries.push(entry.to_string());

        // Trim to max size
        if self.entries.len() > self.max_size {
            self.entries.remove(0);
        }
    }

    /// Get all history entries
    pub fn entries(&self) -> Result<&[String]> {
        Ok(&self.entries)
    }

    /// Search history for entries matching a pattern
    pub fn search(&self, pattern: &str) -> Vec<&String> {
        self.entries
            .iter()
            .filter(|e| e.contains(pattern))
            .collect()
    }

    /// Save history to file
    pub fn save(&self) -> Result<()> {
        // Ensure parent directory exists
        if let Some(parent) = self.file_path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create directory {:?}", parent))?;
        }

        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&self.file_path)
            .with_context(|| format!("Failed to open history file {:?}", self.file_path))?;

        for entry in &self.entries {
            writeln!(file, "{}", entry).with_context(|| "Failed to write history entry")?;
        }

        Ok(())
    }

    /// Clear all history
    pub fn clear(&mut self) {
        self.entries.clear();
    }

    /// Load entries from file
    fn load_entries(file_path: &Path, max_size: usize) -> Result<Vec<String>> {
        let file = File::open(file_path)
            .with_context(|| format!("Failed to open history file {:?}", file_path))?;
        let reader = BufReader::new(file);

        let mut entries: Vec<String> = reader
            .lines()
            .map_while(Result::ok)
            .filter(|line| !line.trim().is_empty())
            .collect();

        // Keep only the most recent entries
        if entries.len() > max_size {
            entries = entries.split_off(entries.len() - max_size);
        }

        Ok(entries)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_add_entry() {
        let temp_file = NamedTempFile::new().unwrap();
        let mut history = HistoryManager::new(temp_file.path(), 100).unwrap();

        history.add("let x = 1");
        history.add("let y = 2");

        assert_eq!(history.entries().unwrap().len(), 2);
    }

    #[test]
    fn test_no_duplicate_consecutive() {
        let temp_file = NamedTempFile::new().unwrap();
        let mut history = HistoryManager::new(temp_file.path(), 100).unwrap();

        history.add("let x = 1");
        history.add("let x = 1");

        assert_eq!(history.entries().unwrap().len(), 1);
    }

    #[test]
    fn test_max_size() {
        let temp_file = NamedTempFile::new().unwrap();
        let mut history = HistoryManager::new(temp_file.path(), 3).unwrap();

        history.add("entry 1");
        history.add("entry 2");
        history.add("entry 3");
        history.add("entry 4");

        assert_eq!(history.entries().unwrap().len(), 3);
        assert_eq!(history.entries().unwrap()[0], "entry 2");
    }

    #[test]
    fn test_search() {
        let temp_file = NamedTempFile::new().unwrap();
        let mut history = HistoryManager::new(temp_file.path(), 100).unwrap();

        history.add("let x = 1");
        history.add("let y = 2");
        history.add("x + y");

        let results = history.search("x");
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_save_and_load() {
        let temp_file = NamedTempFile::new().unwrap();
        {
            let mut history = HistoryManager::new(temp_file.path(), 100).unwrap();
            history.add("let x = 1");
            history.add("let y = 2");
            history.save().unwrap();
        }

        let history = HistoryManager::new(temp_file.path(), 100).unwrap();
        assert_eq!(history.entries().unwrap().len(), 2);
    }
}
