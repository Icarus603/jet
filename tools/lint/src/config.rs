//! Lint configuration
//!
//! Configuration for the linter, including lint levels and overrides.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Lint level - controls the severity of a lint
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum LintLevel {
    /// Error - will fail the build
    Error,
    /// Deny - treat as error but can be overridden
    Deny,
    /// Warning - report but don't fail
    #[default]
    Warn,
    /// Info - informational only
    Info,
    /// Allow - suppress the lint
    Allow,
    /// Forbid - treat as error, cannot be overridden
    Forbid,
}

impl LintLevel {
    /// Parse a lint level from a string
    /// Note: This is intentionally not implementing std::str::FromStr to avoid
    /// the complexity of error types for this simple internal parser.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "error" => Some(LintLevel::Error),
            "deny" => Some(LintLevel::Deny),
            "warn" | "warning" => Some(LintLevel::Warn),
            "info" => Some(LintLevel::Info),
            "allow" => Some(LintLevel::Allow),
            "forbid" => Some(LintLevel::Forbid),
            _ => None,
        }
    }

    /// Check if this level is an error
    pub fn is_error(&self) -> bool {
        matches!(self, LintLevel::Error | LintLevel::Deny | LintLevel::Forbid)
    }

    /// Check if this level allows the lint
    pub fn is_allowed(&self) -> bool {
        *self == LintLevel::Allow
    }
}

/// Override for a specific lint
#[derive(Debug, Clone)]
pub struct LintOverride {
    /// The lint name (or "warnings" for all warnings)
    pub name: String,
    /// The level to set
    pub level: LintLevel,
    /// Whether this is a global override (from command line)
    pub is_global: bool,
}

/// Linter configuration
#[derive(Debug, Clone)]
pub struct LintConfig {
    /// Default lint level
    pub default_level: LintLevel,
    /// Per-lint overrides
    pub lint_overrides: HashMap<String, LintLevel>,
    /// Whether to treat warnings as errors
    pub deny_warnings: bool,
    /// Whether to auto-fix issues where possible
    pub auto_fix: bool,
    /// Output format
    pub output_format: OutputFormat,
}

/// Output format for lint results
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputFormat {
    /// Human-readable format
    #[default]
    Human,
    /// JSON format
    Json,
    /// Short format (one line per issue)
    Short,
}

impl OutputFormat {
    /// Parse from string
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "human" => Some(OutputFormat::Human),
            "json" => Some(OutputFormat::Json),
            "short" => Some(OutputFormat::Short),
            _ => None,
        }
    }
}

impl LintConfig {
    /// Create a new default configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the default lint level
    pub fn with_default_level(mut self, level: LintLevel) -> Self {
        self.default_level = level;
        self
    }

    /// Add a lint override
    pub fn with_override(mut self, name: impl Into<String>, level: LintLevel) -> Self {
        self.lint_overrides.insert(name.into(), level);
        self
    }

    /// Set whether to deny warnings
    pub fn with_deny_warnings(mut self, deny: bool) -> Self {
        self.deny_warnings = deny;
        self
    }

    /// Set whether to auto-fix
    pub fn with_auto_fix(mut self, auto_fix: bool) -> Self {
        self.auto_fix = auto_fix;
        self
    }

    /// Set the output format
    pub fn with_output_format(mut self, format: OutputFormat) -> Self {
        self.output_format = format;
        self
    }

    /// Check if a lint is enabled (not allowed)
    pub fn is_enabled(&self, lint_name: &str) -> bool {
        if let Some(level) = self.lint_overrides.get(lint_name) {
            return !level.is_allowed();
        }

        if self.deny_warnings && self.default_level == LintLevel::Warn {
            return true;
        }

        !self.default_level.is_allowed()
    }

    /// Get the effective level for a lint
    pub fn effective_level(&self, lint_name: &str, default: LintLevel) -> LintLevel {
        // Check for explicit override
        if let Some(level) = self.lint_overrides.get(lint_name) {
            return *level;
        }

        // Check for "warnings" override
        if default == LintLevel::Warn || default == LintLevel::Info {
            if let Some(level) = self.lint_overrides.get("warnings") {
                return *level;
            }
        }

        // Apply deny_warnings
        if self.deny_warnings && default == LintLevel::Warn {
            return LintLevel::Deny;
        }

        default
    }

    /// Load configuration from TOML
    pub fn from_toml(content: &str) -> Result<Self, String> {
        let table: toml::Table = content
            .parse()
            .map_err(|e| format!("TOML parse error: {}", e))?;

        let mut config = Self::default();

        // Parse [lints] section
        if let Some(lints) = table.get("lints").and_then(|v| v.as_table()) {
            for (key, value) in lints {
                let level = match value {
                    toml::Value::String(s) => LintLevel::from_str(s)
                        .ok_or_else(|| format!("Invalid lint level: {}", s))?,
                    _ => continue,
                };
                config.lint_overrides.insert(key.clone(), level);
            }
        }

        Ok(config)
    }

    /// Merge another config into this one (overriding values)
    pub fn merge(&mut self, other: LintConfig) {
        self.lint_overrides.extend(other.lint_overrides);
        if other.deny_warnings {
            self.deny_warnings = true;
        }
        if other.auto_fix {
            self.auto_fix = true;
        }
    }
}

impl Default for LintConfig {
    fn default() -> Self {
        Self {
            default_level: LintLevel::Warn,
            lint_overrides: HashMap::new(),
            deny_warnings: false,
            auto_fix: false,
            output_format: OutputFormat::Human,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lint_level_parsing() {
        assert_eq!(LintLevel::from_str("error"), Some(LintLevel::Error));
        assert_eq!(LintLevel::from_str("deny"), Some(LintLevel::Deny));
        assert_eq!(LintLevel::from_str("warn"), Some(LintLevel::Warn));
        assert_eq!(LintLevel::from_str("warning"), Some(LintLevel::Warn));
        assert_eq!(LintLevel::from_str("allow"), Some(LintLevel::Allow));
        assert_eq!(LintLevel::from_str("forbid"), Some(LintLevel::Forbid));
        assert_eq!(LintLevel::from_str("unknown"), None);
    }

    #[test]
    fn test_lint_level_is_error() {
        assert!(LintLevel::Error.is_error());
        assert!(LintLevel::Deny.is_error());
        assert!(LintLevel::Forbid.is_error());
        assert!(!LintLevel::Warn.is_error());
        assert!(!LintLevel::Info.is_error());
        assert!(!LintLevel::Allow.is_error());
    }

    #[test]
    fn test_config_builder() {
        let config = LintConfig::new()
            .with_default_level(LintLevel::Warn)
            .with_override("unused_variables", LintLevel::Deny)
            .with_deny_warnings(true);

        assert_eq!(config.default_level, LintLevel::Warn);
        assert_eq!(
            config.lint_overrides.get("unused_variables"),
            Some(&LintLevel::Deny)
        );
        assert!(config.deny_warnings);
    }

    #[test]
    fn test_is_enabled() {
        let config = LintConfig::new()
            .with_default_level(LintLevel::Warn)
            .with_override("allowed_lint", LintLevel::Allow);

        assert!(config.is_enabled("some_lint"));
        assert!(!config.is_enabled("allowed_lint"));
    }

    #[test]
    fn test_effective_level() {
        let config = LintConfig::new()
            .with_override("specific_lint", LintLevel::Error)
            .with_deny_warnings(true);

        assert_eq!(
            config.effective_level("specific_lint", LintLevel::Warn),
            LintLevel::Error
        );
        assert_eq!(
            config.effective_level("other_lint", LintLevel::Warn),
            LintLevel::Deny
        );
    }

    #[test]
    fn test_from_toml() {
        let toml = r#"
[lints]
unused = "deny"
dead_code = "warn"
shadowing = "allow"
"#;

        let config = LintConfig::from_toml(toml).unwrap();
        assert_eq!(config.lint_overrides.get("unused"), Some(&LintLevel::Deny));
        assert_eq!(
            config.lint_overrides.get("dead_code"),
            Some(&LintLevel::Warn)
        );
        assert_eq!(
            config.lint_overrides.get("shadowing"),
            Some(&LintLevel::Allow)
        );
    }
}
