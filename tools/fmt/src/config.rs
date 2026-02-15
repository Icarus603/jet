//! Formatter configuration
//!
//! While jet-fmt is opinionated, some basic configuration is available
//! for integration with different environments.

/// Configuration for the Jet code formatter
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatConfig {
    /// Number of spaces per indentation level (default: 4)
    pub indent_size: usize,
    /// String to use for indentation (default: "    ")
    pub indent_string: String,
    /// Maximum line length before wrapping (default: 100)
    pub max_line_length: usize,
    /// Hard maximum line length (default: 120)
    pub hard_max_line_length: usize,
    /// Use Unix line endings (default: true)
    pub unix_line_endings: bool,
    /// Ensure trailing newline at end of file (default: true)
    pub trailing_newline: bool,
    /// Maximum consecutive blank lines (default: 1)
    pub max_blank_lines: usize,
    /// Format imports (default: true)
    pub format_imports: bool,
    /// Group imports by: stdlib, external, local (default: true)
    pub group_imports: bool,
    /// Sort imports within groups (default: true)
    pub sort_imports: bool,
    /// Wrap function arguments if they exceed line length (default: true)
    pub wrap_function_args: bool,
    /// Wrap struct fields if they exceed line length (default: true)
    pub wrap_struct_fields: bool,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent_size: 4,
            indent_string: "    ".to_string(),
            max_line_length: 100,
            hard_max_line_length: 120,
            unix_line_endings: true,
            trailing_newline: true,
            max_blank_lines: 1,
            format_imports: true,
            group_imports: true,
            sort_imports: true,
            wrap_function_args: true,
            wrap_struct_fields: true,
        }
    }
}

impl FormatConfig {
    /// Create a new configuration with default settings
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the indentation size
    pub fn with_indent_size(mut self, size: usize) -> Self {
        self.indent_size = size;
        self.indent_string = " ".repeat(size);
        self
    }

    /// Set the maximum line length
    pub fn with_max_line_length(mut self, length: usize) -> Self {
        self.max_line_length = length;
        self
    }

    /// Set the hard maximum line length
    pub fn with_hard_max_line_length(mut self, length: usize) -> Self {
        self.hard_max_line_length = length;
        self
    }

    /// Set whether to use Unix line endings
    pub fn with_unix_line_endings(mut self, unix: bool) -> Self {
        self.unix_line_endings = unix;
        self
    }

    /// Set whether to ensure trailing newline
    pub fn with_trailing_newline(mut self, trailing: bool) -> Self {
        self.trailing_newline = trailing;
        self
    }

    /// Set the maximum number of consecutive blank lines
    pub fn with_max_blank_lines(mut self, max: usize) -> Self {
        self.max_blank_lines = max;
        self
    }

    /// Disable import formatting
    pub fn without_import_formatting(mut self) -> Self {
        self.format_imports = false;
        self
    }

    /// Disable import grouping
    pub fn without_import_grouping(mut self) -> Self {
        self.group_imports = false;
        self
    }

    /// Disable import sorting
    pub fn without_import_sorting(mut self) -> Self {
        self.sort_imports = false;
        self
    }

    /// Disable function argument wrapping
    pub fn without_arg_wrapping(mut self) -> Self {
        self.wrap_function_args = false;
        self
    }

    /// Disable struct field wrapping
    pub fn without_field_wrapping(mut self) -> Self {
        self.wrap_struct_fields = false;
        self
    }

    /// Load configuration from a TOML file
    pub fn from_toml(_content: &str) -> Result<Self, String> {
        // For now, just return default config
        // In a full implementation, this would parse TOML
        Ok(Self::default())
    }

    /// Serialize configuration to TOML
    pub fn to_toml(&self) -> String {
        format!(
            r#"# Jet formatter configuration
# This is an opinionated formatter, most options are not configurable

indent_size = {}
max_line_length = {}
hard_max_line_length = {}
unix_line_endings = {}
trailing_newline = {}
max_blank_lines = {}
format_imports = {}
group_imports = {}
sort_imports = {}
wrap_function_args = {}
wrap_struct_fields = {}
"#,
            self.indent_size,
            self.max_line_length,
            self.hard_max_line_length,
            self.unix_line_endings,
            self.trailing_newline,
            self.max_blank_lines,
            self.format_imports,
            self.group_imports,
            self.sort_imports,
            self.wrap_function_args,
            self.wrap_struct_fields,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = FormatConfig::default();
        assert_eq!(config.indent_size, 4);
        assert_eq!(config.max_line_length, 100);
        assert_eq!(config.hard_max_line_length, 120);
        assert!(config.unix_line_endings);
        assert!(config.trailing_newline);
    }

    #[test]
    fn test_builder_methods() {
        let config = FormatConfig::new()
            .with_indent_size(2)
            .with_max_line_length(80)
            .without_import_formatting();

        assert_eq!(config.indent_size, 2);
        assert_eq!(config.indent_string, "  ");
        assert_eq!(config.max_line_length, 80);
        assert!(!config.format_imports);
    }

    #[test]
    fn test_toml_roundtrip() {
        let config = FormatConfig::default();
        let toml = config.to_toml();
        assert!(toml.contains("indent_size = 4"));
        assert!(toml.contains("max_line_length = 100"));
    }
}
