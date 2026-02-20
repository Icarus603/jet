//! Rename symbol refactoring with AI tracking annotations
//!
//! This module provides semantic-preserving rename operations with support
//! for @rename tracking annotations to document automated renames.

use crate::{RefactoringConfig, RefactoringMetadata, RefactoringResult};
use jet_diagnostics::{Diagnostic, Span};
use jet_parser::ast::{Attribute, AttributeArg, AttributeValue, Ident, Module};
use std::collections::HashSet;

/// A tracked rename operation
#[derive(Debug, Clone)]
pub struct TrackedRename {
    /// The original name
    pub from: String,
    /// The new name
    pub to: String,
    /// Who/what performed the rename
    pub applied_by: String,
    /// When the rename was applied (timestamp)
    pub timestamp: String,
    /// Reason for the rename
    pub reason: Option<String>,
}

impl TrackedRename {
    /// Create a new tracked rename
    pub fn new(from: String, to: String, applied_by: String) -> Self {
        Self {
            from,
            to,
            applied_by,
            timestamp: "2026-02-19T00:00:00Z".to_string(), // Placeholder - would use actual timestamp
            reason: None,
        }
    }

    /// Set the reason for the rename
    pub fn with_reason(mut self, reason: String) -> Self {
        self.reason = Some(reason);
        self
    }

    /// Generate the @rename attribute text
    pub fn to_attribute(&self) -> String {
        let mut parts = vec![
            format!("from=\"{}\"", self.from),
            format!("to=\"{}\"", self.to),
            format!("applied_by=\"{}\"", self.applied_by),
        ];

        if let Some(ref reason) = self.reason {
            parts.push(format!("reason=\"{}\"", reason));
        }

        format!("@rename({})", parts.join(", "))
    }
}

/// Check if a symbol can be renamed at the given span
pub fn can_rename_symbol(ast: &Module, span: Span) -> Result<bool, Vec<Diagnostic>> {
    // Find the identifier at the span
    if let Some(ident) = find_identifier_at_span(ast, span) {
        // Check if it's a valid symbol that can be renamed
        if is_valid_symbol_name(&ident.name) {
            return Ok(true);
        }
    }
    Ok(false)
}

/// Rename a symbol throughout the codebase
pub fn rename_symbol(
    source: &str,
    ast: &Module,
    span: Span,
    config: &RefactoringConfig,
) -> Result<RefactoringResult, Vec<Diagnostic>> {
    rename_symbol_to(source, ast, span, None, config)
}

/// Rename a symbol to a specific new name
pub fn rename_symbol_to(
    source: &str,
    ast: &Module,
    span: Span,
    new_name: Option<&str>,
    config: &RefactoringConfig,
) -> Result<RefactoringResult, Vec<Diagnostic>> {
    // Find the identifier at the span
    let ident = find_identifier_at_span(ast, span)
        .ok_or_else(|| vec![Diagnostic::error("No symbol found at selection", span)])?;

    let old_name = ident.name.clone();

    // Generate or use provided new name
    let new_name = new_name.map(|s| s.to_string()).unwrap_or_else(|| {
        generate_new_name(&old_name)
    });

    // Validate the new name
    if !is_valid_symbol_name(&new_name) {
        return Err(vec![Diagnostic::error(
            format!("'{}' is not a valid symbol name", new_name),
            span,
        )]);
    }

    // Check for name conflicts
    if would_cause_name_conflict(ast, &old_name, &new_name, span) {
        return Err(vec![Diagnostic::error(
            format!("Renaming '{}' to '{}' would cause a name conflict", old_name, new_name),
            span,
        )]);
    }

    // Find all occurrences of the symbol
    let occurrences = find_all_occurrences(ast, &old_name, span);

    if occurrences.is_empty() {
        return Err(vec![Diagnostic::error(
            format!("No occurrences of '{}' found", old_name),
            span,
        )]);
    }

    // Build the new source with replacements
    let mut new_source = source.to_string();
    let mut offset: isize = 0;
    let mut change_locations = vec![];

    for occurrence in &occurrences {
        let adjusted_start = (occurrence.start as isize + offset) as usize;
        let adjusted_end = (occurrence.end as isize + offset) as usize;

        new_source.replace_range(adjusted_start..adjusted_end, &new_name);
        change_locations.push(*occurrence);

        // Update offset for subsequent replacements
        offset += new_name.len() as isize - (occurrence.end - occurrence.start) as isize;
    }

    // Generate tracking annotation if enabled
    let tracking_annotation = if config.generate_tracking_annotations {
        let tracked_rename = TrackedRename::new(
            old_name.clone(),
            new_name.clone(),
            config.applied_by.clone(),
        );
        Some(tracked_rename.to_attribute())
    } else {
        None
    };

    // If this is a function/struct/enum definition, add the @rename attribute
    if let Some(def_span) = find_definition_span(ast, &old_name, span) {
        if config.generate_tracking_annotations {
            // Insert the @rename attribute before the definition
            let line_start = find_line_start(source, def_span.start);
            let indent = get_indentation(source, line_start);
            let attr = format!("{}\n", tracking_annotation.clone().unwrap());

            let adjusted_pos = (line_start as isize + offset) as usize;
            new_source.insert_str(adjusted_pos, &attr);
        }
    }

    Ok(RefactoringResult {
        source: new_source,
        diagnostics: vec![],
        kind: crate::RefactoringKind::RenameSymbol,
        metadata: RefactoringMetadata {
            semantic_preserving: true,
            requires_verification: true,
            new_symbol: Some(new_name.clone()),
            change_locations,
            tracking_annotation,
        },
    })
}

/// Parse a @rename attribute from an attribute AST node
pub fn parse_rename_attribute(attr: &Attribute) -> Option<TrackedRename> {
    if attr.name.name != "rename" {
        return None;
    }

    let mut from = None;
    let mut to = None;
    let mut applied_by = None;
    let mut reason = None;

    for arg in &attr.arguments {
        match arg {
            AttributeArg::Named { name, value } => {
                match name.name.as_str() {
                    "from" => from = get_string_value(value),
                    "to" => to = get_string_value(value),
                    "applied_by" => applied_by = get_string_value(value),
                    "reason" => reason = get_string_value(value),
                    _ => {}
                }
            }
            _ => {}
        }
    }

    Some(TrackedRename {
        from: from?,
        to: to?,
        applied_by: applied_by.unwrap_or_else(|| "unknown".to_string()),
        timestamp: "2026-02-19T00:00:00Z".to_string(), // Placeholder
        reason,
    })
}

/// Get all tracked renames in a module
pub fn find_tracked_renames(ast: &Module) -> Vec<(TrackedRename, Span)> {
    let mut renames = vec![];

    for item in &ast.items {
        let attrs = match item {
            jet_parser::ast::ModuleItem::Function(f) => &f.attributes,
            jet_parser::ast::ModuleItem::Struct(s) => &s.attributes,
            jet_parser::ast::ModuleItem::Enum(e) => &e.attributes,
            jet_parser::ast::ModuleItem::Trait(t) => &t.attributes,
            _ => continue,
        };

        for attr in attrs {
            if let Some(rename) = parse_rename_attribute(attr) {
                renames.push((rename, attr.span));
            }
        }
    }

    renames
}

/// Generate a new name from an old name (for automatic renaming)
fn generate_new_name(old_name: &str) -> String {
    // Try to improve the name based on common patterns

    // Pattern: single letter -> descriptive name
    if old_name.len() == 1 {
        match old_name {
            "x" | "i" | "n" => "index".to_string(),
            "s" => "string".to_string(),
            "f" => "file".to_string(),
            "c" => "count".to_string(),
            "v" => "value".to_string(),
            "k" => "key".to_string(),
            _ => format!("{}_value", old_name),
        }
    }
    // Pattern: abbreviated -> expanded
    else if old_name == "num" {
        "number".to_string()
    } else if old_name == "str" {
        "string".to_string()
    } else if old_name == "idx" {
        "index".to_string()
    }
    // Pattern: add suffix for clarity
    else if old_name.ends_with("_") {
        format!("{}value", old_name)
    }
    // Default: add "new_" prefix
    else {
        format!("new_{}", old_name)
    }
}

/// Check if a name is a valid symbol name
fn is_valid_symbol_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }

    let mut chars = name.chars();

    // First character must be alphabetic or underscore
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }

    // Rest must be alphanumeric or underscore
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// Check if renaming would cause a name conflict
fn would_cause_name_conflict(
    ast: &Module,
    old_name: &str,
    new_name: &str,
    span: Span,
) -> bool {
    // Find all symbols with the new name in scope
    let existing_symbols = find_all_symbols_named(ast, new_name);

    // Check if any would conflict with our rename
    for symbol_span in existing_symbols {
        // If it's not the same symbol we're renaming
        if !is_same_symbol(ast, old_name, span, symbol_span) {
            // Check if they're in the same scope
            if are_in_same_scope(ast, span, symbol_span) {
                return true;
            }
        }
    }

    false
}

/// Find an identifier at the given span
fn find_identifier_at_span(ast: &Module, span: Span) -> Option<&Ident> {
    // Traverse the AST to find the identifier at this span
    // This is a placeholder - needs proper AST traversal
    None
}

/// Find all occurrences of a symbol
fn find_all_occurrences(ast: &Module, name: &str, definition_span: Span) -> Vec<Span> {
    // Find all references to this symbol throughout the module
    // This includes the definition and all uses
    vec![] // Placeholder - needs proper AST traversal
}

/// Find the span of the definition of a symbol
fn find_definition_span(ast: &Module, name: &str, usage_span: Span) -> Option<Span> {
    // Find where this symbol is defined
    None // Placeholder - needs proper AST traversal
}

/// Find all symbols with a given name
fn find_all_symbols_named(ast: &Module, name: &str) -> Vec<Span> {
    // Find all definitions with this name
    vec![] // Placeholder - needs proper AST traversal
}

/// Check if two spans refer to the same symbol
fn is_same_symbol(ast: &Module, name: &str, span1: Span, span2: Span) -> bool {
    // Check if both spans refer to the same symbol definition
    false // Placeholder - needs proper AST traversal
}

/// Check if two spans are in the same scope
fn are_in_same_scope(ast: &Module, span1: Span, span2: Span) -> bool {
    // Check if both spans are in the same lexical scope
    true // Conservative: assume same scope
}

/// Get a string value from an attribute value
fn get_string_value(value: &AttributeValue) -> Option<String> {
    match value {
        AttributeValue::String(s) => Some(s.clone()),
        _ => None,
    }
}

/// Find the start of the line containing the position
fn find_line_start(source: &str, pos: usize) -> usize {
    source[..pos].rfind('\n').map(|i| i + 1).unwrap_or(0)
}

/// Get indentation at a position
fn get_indentation(source: &str, pos: usize) -> String {
    let line_start = find_line_start(source, pos);
    let line_end = source[line_start..]
        .find('\n')
        .map(|i| line_start + i)
        .unwrap_or(source.len());

    let line = &source[line_start..line_end];
    line.chars().take_while(|c| c.is_whitespace()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_new_name() {
        assert_eq!(generate_new_name("x"), "index");
        assert_eq!(generate_new_name("num"), "number");
        assert_eq!(generate_new_name("foo"), "new_foo");
    }

    #[test]
    fn test_is_valid_symbol_name() {
        assert!(is_valid_symbol_name("foo"));
        assert!(is_valid_symbol_name("foo_bar"));
        assert!(is_valid_symbol_name("_foo"));
        assert!(!is_valid_symbol_name("123"));
        assert!(!is_valid_symbol_name("foo-bar"));
        assert!(!is_valid_symbol_name(""));
    }

    #[test]
    fn test_tracked_rename_attribute() {
        let rename = TrackedRename::new(
            "old_name".to_string(),
            "new_name".to_string(),
            "ai".to_string(),
        );

        let attr = rename.to_attribute();
        assert!(attr.contains("from=\"old_name\""));
        assert!(attr.contains("to=\"new_name\""));
        assert!(attr.contains("applied_by=\"ai\""));
    }

    #[test]
    fn test_tracked_rename_with_reason() {
        let rename = TrackedRename::new(
            "old_name".to_string(),
            "new_name".to_string(),
            "ai".to_string(),
        ).with_reason("better clarity".to_string());

        let attr = rename.to_attribute();
        assert!(attr.contains("reason=\"better clarity\""));
    }
}
