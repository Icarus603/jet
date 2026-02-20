//! REPL configuration
//!
//! Handles loading and saving of REPL configuration from ~/.jet/repl.toml

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// REPL configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReplConfig {
    /// Path to history file
    #[serde(default = "default_history_file")]
    pub history_file: PathBuf,

    /// Maximum number of history entries
    #[serde(default = "default_history_size")]
    pub history_size: usize,

    /// Editor mode (emacs or vi)
    #[serde(default = "default_editor")]
    pub editor: String,

    /// Enable colors
    #[serde(default = "default_colors")]
    pub colors: bool,

    /// Primary prompt
    #[serde(default = "default_prompt")]
    pub prompt: String,

    /// Multiline prompt
    #[serde(default = "default_multiline_prompt")]
    pub multiline_prompt: String,

    /// Auto-import modules
    #[serde(default)]
    pub auto_import: Vec<String>,
}

impl Default for ReplConfig {
    fn default() -> Self {
        Self {
            history_file: default_history_file(),
            history_size: default_history_size(),
            editor: default_editor(),
            colors: default_colors(),
            prompt: default_prompt(),
            multiline_prompt: default_multiline_prompt(),
            auto_import: Vec::new(),
        }
    }
}

impl ReplConfig {
    /// Load configuration from file
    pub fn load() -> Result<Self> {
        let config_path = get_config_path()?;

        if config_path.exists() {
            let content = std::fs::read_to_string(&config_path)
                .with_context(|| format!("Failed to read config from {:?}", config_path))?;
            let config: ReplConfig = toml::from_str(&content)
                .with_context(|| format!("Failed to parse config from {:?}", config_path))?;
            Ok(config)
        } else {
            // Create default config
            let config = ReplConfig::default();
            config.save()?;
            Ok(config)
        }
    }

    /// Save configuration to file
    pub fn save(&self) -> Result<()> {
        let config_path = get_config_path()?;

        // Ensure parent directory exists
        if let Some(parent) = config_path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create directory {:?}", parent))?;
        }

        let content = toml::to_string_pretty(self).context("Failed to serialize config")?;
        std::fs::write(&config_path, content)
            .with_context(|| format!("Failed to write config to {:?}", config_path))?;

        Ok(())
    }
}

fn get_config_path() -> Result<PathBuf> {
    let home = dirs::home_dir().context("Failed to determine home directory")?;
    Ok(home.join(".jet").join("repl.toml"))
}

fn default_history_file() -> PathBuf {
    dirs::home_dir()
        .map(|h| h.join(".jet").join("repl_history"))
        .unwrap_or_else(|| PathBuf::from(".repl_history"))
}

fn default_history_size() -> usize {
    1000
}

fn default_editor() -> String {
    "emacs".to_string()
}

fn default_colors() -> bool {
    true
}

fn default_prompt() -> String {
    "> ".to_string()
}

fn default_multiline_prompt() -> String {
    "... ".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = ReplConfig::default();
        assert_eq!(config.history_size, 1000);
        assert_eq!(config.editor, "emacs");
        assert!(config.colors);
        assert_eq!(config.prompt, "> ");
        assert_eq!(config.multiline_prompt, "... ");
    }

    #[test]
    fn test_config_serialization() {
        let config = ReplConfig::default();
        let toml = toml::to_string(&config).unwrap();
        assert!(toml.contains("history_size"));
        assert!(toml.contains("editor"));
    }
}
