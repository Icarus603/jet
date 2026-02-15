//! Code actions (quick fixes) for the Jet LSP server
//!
//! Provides automated fixes for common issues and refactoring operations.

use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, Diagnostic, Position, Range, TextEdit, Url,
    WorkspaceEdit,
};

/// Get available code actions for a given diagnostic
pub fn get_code_actions(
    uri: &Url,
    diagnostic: &Diagnostic,
    _source: &str,
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Check if we can provide a quick fix for this diagnostic
    if let Some(tower_lsp::lsp_types::NumberOrString::String(code_str)) = &diagnostic.code {
        match code_str.as_str() {
            "E0200" => {
                // Unresolved name - could suggest importing
                actions.push(create_import_suggestion(uri, diagnostic));
            }
            "unused_variables" | "unused_imports" => {
                // Unused code - suggest removal
                actions.push(create_remove_unused_code(uri, diagnostic));
            }
            _ => {}
        }
    }

    actions
}

/// Create an import suggestion code action
fn create_import_suggestion(_uri: &Url, diagnostic: &Diagnostic) -> CodeActionOrCommand {
    CodeActionOrCommand::CodeAction(CodeAction {
        title: "Import missing module".to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(std::collections::HashMap::new()),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(false),
        disabled: None,
        data: None,
    })
}

/// Create a code action to remove unused code
fn create_remove_unused_code(uri: &Url, diagnostic: &Diagnostic) -> CodeActionOrCommand {
    let edit = TextEdit {
        range: diagnostic.range,
        new_text: "".to_string(),
    };

    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), vec![edit]);

    CodeActionOrCommand::CodeAction(CodeAction {
        title: "Remove unused code".to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(true),
        disabled: None,
        data: None,
    })
}

/// Get refactoring code actions for a range
pub fn get_refactoring_actions(_uri: &Url, range: Range, source: &str) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Extract variable refactoring
    if can_extract_variable(range, source) {
        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
            title: "Extract variable".to_string(),
            kind: Some(CodeActionKind::REFACTOR_EXTRACT),
            diagnostics: None,
            edit: None,
            command: None,
            is_preferred: None,
            disabled: None,
            data: None,
        }));
    }

    // Extract function refactoring
    if can_extract_function(range, source) {
        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
            title: "Extract function".to_string(),
            kind: Some(CodeActionKind::REFACTOR_EXTRACT),
            diagnostics: None,
            edit: None,
            command: None,
            is_preferred: None,
            disabled: None,
            data: None,
        }));
    }

    actions
}

/// Check if we can extract a variable from the selected range
fn can_extract_variable(range: Range, source: &str) -> bool {
    // For now, just check if the range is non-empty
    let start = position_to_offset(range.start, source);
    let end = position_to_offset(range.end, source);

    if start >= end {
        return false;
    }

    let selected = &source[start..end];

    // Don't extract if it's just whitespace or a simple identifier
    if selected.trim().is_empty() {
        return false;
    }

    true
}

/// Check if we can extract a function from the selected range
fn can_extract_function(range: Range, source: &str) -> bool {
    // Check if the range contains multiple statements or complex expressions
    let start = position_to_offset(range.start, source);
    let end = position_to_offset(range.end, source);

    if start >= end {
        return false;
    }

    let selected = &source[start..end];

    // Check for multiple lines or complex expressions
    selected.contains('\n') || selected.contains("let ") || selected.contains("if ")
}

/// Convert a Position to a byte offset in the source
fn position_to_offset(position: Position, source: &str) -> usize {
    let line = position.line as usize;
    let character = position.character as usize;

    let mut current_line = 0;
    let mut offset = 0;
    let mut current_char_on_line = 0;

    for c in source.chars() {
        if current_line == line && current_char_on_line == character {
            return offset;
        }
        if c == '\n' {
            if current_line == line {
                return offset;
            }
            current_line += 1;
            current_char_on_line = 0;
        } else {
            current_char_on_line += 1;
        }
        offset += c.len_utf8();
    }

    offset
}

/// Get source code actions (organize imports, etc.)
pub fn get_source_actions(_uri: &Url, source: &str) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Organize imports
    if source.contains("import ") {
        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
            title: "Organize imports".to_string(),
            kind: Some(CodeActionKind::SOURCE_ORGANIZE_IMPORTS),
            diagnostics: None,
            edit: None, // Would apply import organization
            command: None,
            is_preferred: None,
            disabled: None,
            data: None,
        }));
    }

    // Format document
    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
        title: "Format document".to_string(),
        kind: Some(CodeActionKind::SOURCE),
        diagnostics: None,
        edit: None,
        command: None,
        is_preferred: None,
        disabled: None,
        data: None,
    }));

    actions
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_can_extract_variable() {
        let source = "fn main():\n    let x = 1 + 2\n";
        let range = Range {
            start: Position {
                line: 1,
                character: 12,
            },
            end: Position {
                line: 1,
                character: 17,
            },
        };

        assert!(can_extract_variable(range, source));
    }

    #[test]
    fn test_position_to_offset() {
        let source = "line1\nline2\nline3";
        let pos = Position {
            line: 1,
            character: 2,
        };

        let offset = position_to_offset(pos, source);
        assert_eq!(&source[offset..offset + 3], "ne2");
    }
}
