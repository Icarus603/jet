//! Code actions (quick fixes) for the Jet LSP server
//!
//! Provides automated fixes for common issues and refactoring operations.
//! Integrates with the jet-refactor crate for AI-safe refactoring operations.

use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, Diagnostic, Position, Range, TextEdit, Url,
    WorkspaceEdit,
};

// Integration with jet-refactor for semantic refactoring operations
// Note: Full integration requires contract verification (task #5)
// For now, we use text-based refactorings with placeholder semantic checks

/// Get available code actions for a given diagnostic
pub fn get_code_actions(
    uri: &Url,
    diagnostic: &Diagnostic,
    source: &str,
) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Check if we can provide a quick fix for this diagnostic
    if let Some(tower_lsp::lsp_types::NumberOrString::String(code_str)) = &diagnostic.code {
        match code_str.as_str() {
            "E0200" => {
                // Unresolved name - could suggest importing
                actions.push(create_import_suggestion(uri, diagnostic));
            }
            "E0308" => {
                // Type mismatch - suggest type annotation fix
                actions.push(create_type_fix_suggestion(uri, diagnostic));
            }
            "unused_variables" | "unused_imports" => {
                // Unused code - suggest removal
                actions.push(create_remove_unused_code(uri, diagnostic));
                // Also suggest prefixing with underscore
                if should_offer_prefix_underscore(diagnostic, source) {
                    actions.push(create_prefix_underscore(uri, diagnostic));
                }
            }
            "E0425" => {
                // Cannot find value - suggest similar names
                actions.push(create_suggest_similar_name(uri, diagnostic, source));
            }
            "E0001" => {
                // Unreachable pattern - suggest removing
                actions.push(create_remove_unreachable(uri, diagnostic));
            }
            _ => {}
        }
    }

    // Check message content for common patterns
    let message = diagnostic.message.to_lowercase();
    if message.contains("missing") && message.contains("return") {
        actions.push(create_add_return_type(uri, diagnostic));
    }
    if message.contains("expected") && message.contains("found") {
        actions.push(create_type_fix_suggestion(uri, diagnostic));
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

/// Create a type fix suggestion code action
fn create_type_fix_suggestion(uri: &Url, diagnostic: &Diagnostic) -> CodeActionOrCommand {
    // Try to extract expected and found types from the diagnostic message
    let message = &diagnostic.message;

    // Parse common type mismatch patterns
    // Pattern: "expected `Type`, found `Type`" or "expected Type, found Type"
    let expected_type = extract_type_from_message(message, "expected");
    let found_type = extract_type_from_message(message, "found");

    let (title, new_text) = if let (Some(expected), Some(_found)) = (&expected_type, &found_type) {
        // Suggest adding a type annotation with the expected type
        let suggestion = format!("/* expected: {} */", expected);
        (format!("Add type annotation: {}", expected), suggestion)
    } else if let Some(expected) = expected_type {
        (
            format!("Expected type: {}", expected),
            format!("/* expected: {} */", expected),
        )
    } else {
        (
            "Add type annotation comment".to_string(),
            "/* fix type mismatch */".to_string(),
        )
    };

    let edit = TextEdit {
        range: diagnostic.range,
        new_text,
    };

    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), vec![edit]);

    CodeActionOrCommand::CodeAction(CodeAction {
        title,
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(false),
        disabled: None,
        data: None,
    })
}

/// Extract a type name from a diagnostic message
fn extract_type_from_message(message: &str, prefix: &str) -> Option<String> {
    // Look for patterns like "expected `Type`" or "expected Type,"
    let patterns = [format!("{} `", prefix), format!("{} ", prefix)];

    for pattern in &patterns {
        if let Some(start) = message.find(pattern) {
            let after_prefix = &message[start + pattern.len()..];

            // Find the end of the type name
            let end = if let Some(backtick) = after_prefix.find('`') {
                backtick
            } else if let Some(comma) = after_prefix.find(',') {
                comma
            } else if let Some(newline) = after_prefix.find('\n') {
                newline
            } else {
                after_prefix.len()
            };

            let type_name = after_prefix[..end].trim();
            if !type_name.is_empty() && type_name != "type" {
                return Some(type_name.to_string());
            }
        }
    }

    None
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

/// Create a code action to prefix unused variable with underscore
fn create_prefix_underscore(uri: &Url, diagnostic: &Diagnostic) -> CodeActionOrCommand {
    let mut changes = std::collections::HashMap::new();

    let edit = TextEdit {
        range: Range {
            start: diagnostic.range.start,
            end: diagnostic.range.start,
        },
        new_text: "_".to_string(),
    };
    changes.insert(uri.clone(), vec![edit]);

    CodeActionOrCommand::CodeAction(CodeAction {
        title: "Prefix with underscore to ignore".to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(false),
        disabled: None,
        data: None,
    })
}

fn should_offer_prefix_underscore(diagnostic: &Diagnostic, source: &str) -> bool {
    let start = position_to_offset(diagnostic.range.start, source);
    let end = position_to_offset(diagnostic.range.end, source);
    if start >= end || end > source.len() {
        return false;
    }
    let selected = &source[start..end];
    let trimmed = selected.trim();
    !trimmed.is_empty() && !trimmed.starts_with('_')
}

/// Create a code action to suggest a similar name
fn create_suggest_similar_name(
    uri: &Url,
    diagnostic: &Diagnostic,
    source: &str,
) -> CodeActionOrCommand {
    let start = position_to_offset(diagnostic.range.start, source);
    let end = position_to_offset(diagnostic.range.end, source);
    if start >= end || end > source.len() {
        return CodeActionOrCommand::CodeAction(CodeAction {
            title: "Show similar names".to_string(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![diagnostic.clone()]),
            edit: None,
            command: None,
            is_preferred: Some(false),
            disabled: None,
            data: None,
        });
    }

    let unresolved = source[start..end].trim();
    let best_match = best_similar_identifier(unresolved, source);
    if let Some(replacement) = best_match {
        let edit = TextEdit {
            range: diagnostic.range,
            new_text: replacement.clone(),
        };
        let mut changes = std::collections::HashMap::new();
        changes.insert(uri.clone(), vec![edit]);
        return CodeActionOrCommand::CodeAction(CodeAction {
            title: format!("Replace with '{}'", replacement),
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
        });
    }

    CodeActionOrCommand::CodeAction(CodeAction {
        title: "Show similar names".to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: None,
        command: None,
        is_preferred: Some(false),
        disabled: None,
        data: None,
    })
}

fn best_similar_identifier(unresolved: &str, source: &str) -> Option<String> {
    if unresolved.is_empty() {
        return None;
    }
    let mut candidates = collect_identifiers(source);
    candidates.retain(|name| name != unresolved && !is_keyword(name));
    if candidates.is_empty() {
        return None;
    }

    let unresolved_lower = unresolved.to_lowercase();
    candidates
        .into_iter()
        .map(|candidate| {
            let dist = levenshtein(&unresolved_lower, &candidate.to_lowercase());
            (candidate, dist)
        })
        .filter(|(_, dist)| *dist <= unresolved.len().max(3) / 2 + 1)
        .min_by(|(a_name, a_dist), (b_name, b_dist)| {
            a_dist.cmp(b_dist).then_with(|| a_name.cmp(b_name))
        })
        .map(|(name, _)| name)
}

fn collect_identifiers(source: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut current = String::new();
    let mut in_ident = false;
    for ch in source.chars() {
        if in_ident {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                current.push(ch);
            } else {
                if current.len() > 1 {
                    out.push(current.clone());
                }
                current.clear();
                in_ident = false;
            }
        } else if ch.is_ascii_alphabetic() || ch == '_' {
            current.push(ch);
            in_ident = true;
        }
    }
    if in_ident && current.len() > 1 {
        out.push(current);
    }
    out.sort();
    out.dedup();
    out
}

fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "and"
            | "async"
            | "await"
            | "break"
            | "chan"
            | "concurrent"
            | "const"
            | "continue"
            | "elif"
            | "else"
            | "enum"
            | "false"
            | "fn"
            | "for"
            | "from"
            | "if"
            | "impl"
            | "import"
            | "in"
            | "let"
            | "loop"
            | "match"
            | "mod"
            | "mut"
            | "not"
            | "or"
            | "pub"
            | "raise"
            | "return"
            | "self"
            | "spawn"
            | "struct"
            | "trait"
            | "true"
            | "type"
            | "unit"
            | "use"
            | "where"
            | "while"
    )
}

fn levenshtein(a: &str, b: &str) -> usize {
    if a == b {
        return 0;
    }
    if a.is_empty() {
        return b.chars().count();
    }
    if b.is_empty() {
        return a.chars().count();
    }

    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let mut prev: Vec<usize> = (0..=b_chars.len()).collect();
    let mut curr = vec![0; b_chars.len() + 1];

    for (i, ac) in a_chars.iter().enumerate() {
        curr[0] = i + 1;
        for (j, bc) in b_chars.iter().enumerate() {
            let cost = if ac == bc { 0 } else { 1 };
            curr[j + 1] = (curr[j] + 1).min(prev[j + 1] + 1).min(prev[j] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }

    prev[b_chars.len()]
}

/// Create a code action to remove unreachable code
fn create_remove_unreachable(uri: &Url, diagnostic: &Diagnostic) -> CodeActionOrCommand {
    let edit = TextEdit {
        range: diagnostic.range,
        new_text: "".to_string(),
    };

    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), vec![edit]);

    CodeActionOrCommand::CodeAction(CodeAction {
        title: "Remove unreachable code".to_string(),
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

/// Create a code action to add return type annotation
fn create_add_return_type(_uri: &Url, diagnostic: &Diagnostic) -> CodeActionOrCommand {
    CodeActionOrCommand::CodeAction(CodeAction {
        title: "Add return type annotation".to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: None,
        command: None,
        is_preferred: Some(false),
        disabled: None,
        data: None,
    })
}

/// Get refactoring code actions for a range
pub fn get_refactoring_actions(uri: &Url, range: Range, source: &str) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Extract variable refactoring
    if can_extract_variable(range, source) {
        if let Some(action) = create_extract_variable_action(uri, range, source) {
            actions.push(action);
        }
    }

    // Extract function refactoring
    if can_extract_function(range, source) {
        if let Some(action) = create_extract_function_action(uri, range, source) {
            actions.push(action);
        }
    }

    // Inline variable refactoring
    if can_inline_variable(range, source) {
        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
            title: "Inline variable".to_string(),
            kind: Some(CodeActionKind::REFACTOR_INLINE),
            diagnostics: None,
            edit: None,
            command: None,
            is_preferred: None,
            disabled: None,
            data: None,
        }));
    }

    // Rename symbol refactoring
    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
        title: "Rename symbol".to_string(),
        kind: Some(CodeActionKind::REFACTOR),
        diagnostics: None,
        edit: None,
        command: None,
        is_preferred: None,
        disabled: None,
        data: None,
    }));

    // AI-safe refactoring suggestions (from jet-refactor integration)
    // These will be fully enabled once contract verification is complete
    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
        title: "AI: Suggest refactorings".to_string(),
        kind: Some(CodeActionKind::REFACTOR),
        diagnostics: None,
        edit: None,
        command: Some(tower_lsp::lsp_types::Command {
            title: "Analyze for refactoring opportunities".to_string(),
            command: "jet.analyzeRefactorings".to_string(),
            arguments: None,
        }),
        is_preferred: None,
        disabled: None,
        data: None,
    }));

    actions
}

/// Create extract variable refactoring action
fn create_extract_variable_action(
    uri: &Url,
    range: Range,
    source: &str,
) -> Option<CodeActionOrCommand> {
    let start = position_to_offset(range.start, source);
    let end = position_to_offset(range.end, source);

    if start >= end {
        return None;
    }

    let selected = &source[start..end];
    let trimmed = selected.trim();

    if trimmed.is_empty() {
        return None;
    }

    // Generate a variable name based on the expression
    let var_name = generate_variable_name(trimmed);

    // Create the edit to extract the variable
    let mut changes = std::collections::HashMap::new();

    // Replace the selected text with the variable name
    let replace_edit = TextEdit {
        range,
        new_text: var_name.clone(),
    };

    // Add the let binding before the current line
    let line_start = find_line_start(start, source);
    let indent = get_indent_at_position(line_start, source);
    let let_binding = format!("{}let {} = {}\n", indent, var_name, trimmed);

    let insert_edit = TextEdit {
        range: Range {
            start: offset_to_position(line_start, source),
            end: offset_to_position(line_start, source),
        },
        new_text: let_binding,
    };

    changes.insert(uri.clone(), vec![insert_edit, replace_edit]);

    Some(CodeActionOrCommand::CodeAction(CodeAction {
        title: format!("Extract variable '{}'", var_name),
        kind: Some(CodeActionKind::REFACTOR_EXTRACT),
        diagnostics: None,
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: None,
        disabled: None,
        data: None,
    }))
}

/// Create extract function refactoring action
fn create_extract_function_action(
    uri: &Url,
    range: Range,
    source: &str,
) -> Option<CodeActionOrCommand> {
    let start = position_to_offset(range.start, source);
    let end = position_to_offset(range.end, source);

    if start >= end {
        return None;
    }

    let selected = &source[start..end];

    // Generate a function name
    let func_name = generate_function_name(selected);

    let mut changes = std::collections::HashMap::new();

    // Replace selected text with function call
    let call_edit = TextEdit {
        range,
        new_text: format!("{}()", func_name),
    };

    // Add function definition after the current function
    let func_def = format!("\n\nfn {}():\n    {}", func_name, selected.trim());

    // Find a good position to insert the function (end of current function)
    let insert_pos = find_function_end(end, source);

    let insert_edit = TextEdit {
        range: Range {
            start: offset_to_position(insert_pos, source),
            end: offset_to_position(insert_pos, source),
        },
        new_text: func_def,
    };

    changes.insert(uri.clone(), vec![call_edit, insert_edit]);

    Some(CodeActionOrCommand::CodeAction(CodeAction {
        title: format!("Extract function '{}'", func_name),
        kind: Some(CodeActionKind::REFACTOR_EXTRACT),
        diagnostics: None,
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: None,
        disabled: None,
        data: None,
    }))
}

/// Generate a variable name from an expression
fn generate_variable_name(expr: &str) -> String {
    // Simple heuristic: use the first identifier in the expression
    let cleaned: String = expr
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                ' '
            }
        })
        .collect();

    let words: Vec<&str> = cleaned.split_whitespace().collect();

    if let Some(first_word) = words.first() {
        if first_word.len() > 1 {
            // Convert to snake_case
            let mut result = String::new();
            for (i, c) in first_word.chars().enumerate() {
                if i == 0 {
                    result.push(c.to_ascii_lowercase());
                } else if c.is_uppercase() {
                    result.push('_');
                    result.push(c.to_ascii_lowercase());
                } else {
                    result.push(c);
                }
            }
            return result;
        }
    }

    "extracted".to_string()
}

/// Generate a function name from selected code
fn generate_function_name(code: &str) -> String {
    // Simple heuristic based on content
    if code.contains("if ") || code.contains("match ") {
        "check_condition".to_string()
    } else if code.contains("for ") || code.contains("while ") {
        "process_items".to_string()
    } else if code.contains("return ") {
        "compute_value".to_string()
    } else {
        "new_function".to_string()
    }
}

/// Find the start of the line containing the offset
fn find_line_start(offset: usize, source: &str) -> usize {
    source[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0)
}

/// Get the indentation at a position
fn get_indent_at_position(pos: usize, source: &str) -> String {
    let line_start = find_line_start(pos, source);
    let line_end = source[line_start..]
        .find('\n')
        .map(|i| line_start + i)
        .unwrap_or(source.len());

    let line = &source[line_start..line_end];
    line.chars().take_while(|c| c.is_whitespace()).collect()
}

/// Find the end of the current function
fn find_function_end(offset: usize, source: &str) -> usize {
    // Simple heuristic: find the next line that starts at column 0 (not indented)
    // after the current position
    let rest = &source[offset..];

    for (i, line) in rest.lines().enumerate() {
        if i > 0 && !line.starts_with(' ') && !line.starts_with('\t') && !line.is_empty() {
            // Found a non-indented line, return position before it
            let pos = offset + rest.lines().take(i).map(|l| l.len() + 1).sum::<usize>();
            return pos;
        }
    }

    source.len()
}

/// Convert offset to Position
fn offset_to_position(offset: usize, source: &str) -> Position {
    let mut line = 0u32;
    let mut character = 0u32;

    for (i, c) in source.chars().enumerate() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }

    Position { line, character }
}

/// Check if we can extract a variable from the selected range
fn can_extract_variable(range: Range, source: &str) -> bool {
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

    // Don't extract if it's just a single identifier
    let trimmed = selected.trim();
    if trimmed.chars().all(|c| c.is_alphanumeric() || c == '_') {
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

/// Check if we can inline a variable at the given range
fn can_inline_variable(range: Range, source: &str) -> bool {
    let start = position_to_offset(range.start, source);
    let end = position_to_offset(range.end, source);

    if start >= end {
        return false;
    }

    let selected = &source[start..end];
    let trimmed = selected.trim();

    // Check if it's a simple variable reference
    trimmed.chars().all(|c| c.is_alphanumeric() || c == '_')
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
pub fn get_source_actions(uri: &Url, source: &str) -> Vec<CodeActionOrCommand> {
    let mut actions = Vec::new();

    // Organize imports
    if source.contains("import ") {
        actions.push(create_organize_imports_action(uri, source));
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

    // Sort lines
    actions.push(CodeActionOrCommand::CodeAction(CodeAction {
        title: "Sort lines".to_string(),
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

/// Create organize imports action
fn create_organize_imports_action(uri: &Url, source: &str) -> CodeActionOrCommand {
    let mut changes = std::collections::HashMap::new();
    if let Some(edit) = build_organize_imports_edit(source) {
        changes.insert(uri.clone(), vec![edit]);
    }

    CodeActionOrCommand::CodeAction(CodeAction {
        title: "Organize imports".to_string(),
        kind: Some(CodeActionKind::SOURCE_ORGANIZE_IMPORTS),
        diagnostics: None,
        edit: if changes.is_empty() {
            None
        } else {
            Some(WorkspaceEdit {
                changes: Some(changes),
                document_changes: None,
                change_annotations: None,
            })
        },
        command: None,
        is_preferred: Some(true),
        disabled: None,
        data: None,
    })
}

fn build_organize_imports_edit(source: &str) -> Option<TextEdit> {
    let lines: Vec<&str> = source.lines().collect();
    if lines.is_empty() {
        return None;
    }

    let mut block_start = None;
    let mut block_end = None;
    for (idx, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        let is_import = trimmed.starts_with("import ") || trimmed.starts_with("from ");
        if block_start.is_none() {
            if is_import {
                block_start = Some(idx);
                block_end = Some(idx);
                continue;
            }
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }
            break;
        }

        if is_import || trimmed.is_empty() {
            block_end = Some(idx);
            continue;
        }
        break;
    }

    let (start, end) = match (block_start, block_end) {
        (Some(s), Some(e)) if s <= e => (s, e),
        _ => return None,
    };

    let mut imports = lines[start..=end]
        .iter()
        .map(|line| line.trim())
        .filter(|line| line.starts_with("import ") || line.starts_with("from "))
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();
    if imports.is_empty() {
        return None;
    }

    imports.sort_by_key(|s| (s.starts_with("from "), s.to_lowercase()));
    imports.dedup();

    let mut new_text = imports.join("\n");
    if end + 1 < lines.len() {
        new_text.push('\n');
    }

    let old_text = lines[start..=end].join("\n");
    if old_text == new_text.trim_end_matches('\n') {
        return None;
    }

    Some(TextEdit {
        range: Range {
            start: Position {
                line: start as u32,
                character: 0,
            },
            end: Position {
                line: end as u32,
                character: lines[end].len() as u32,
            },
        },
        new_text,
    })
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
    fn test_can_extract_variable_simple_identifier() {
        let source = "fn main():\n    let x = 1 + 2\n";
        let range = Range {
            start: Position {
                line: 1,
                character: 8,
            },
            end: Position {
                line: 1,
                character: 9,
            },
        };

        // Should not extract a simple identifier
        assert!(!can_extract_variable(range, source));
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

    #[test]
    fn test_generate_variable_name() {
        assert_eq!(generate_variable_name("fooBar"), "foo_bar");
        assert_eq!(generate_variable_name("hello world"), "hello");
        assert_eq!(generate_variable_name("x + y"), "extracted");
    }

    #[test]
    fn test_generate_function_name() {
        assert_eq!(generate_function_name("if x > 0:"), "check_condition");
        assert_eq!(generate_function_name("for i in items:"), "process_items");
        assert_eq!(generate_function_name("return x + y"), "compute_value");
        assert_eq!(generate_function_name("x + y"), "new_function");
    }

    #[test]
    fn test_find_line_start() {
        let source = "line1\nline2\nline3";
        assert_eq!(find_line_start(0, source), 0);
        assert_eq!(find_line_start(8, source), 6);
        assert_eq!(find_line_start(15, source), 12);
    }

    #[test]
    fn test_get_indent_at_position() {
        let source = "line1\n    line2\n\tline3";
        assert_eq!(get_indent_at_position(0, source), "");
        assert_eq!(get_indent_at_position(6, source), "    ");
        assert_eq!(get_indent_at_position(16, source), "\t");
    }

    #[test]
    fn test_should_offer_prefix_underscore() {
        let source = "fn main():\n    let _unused = 1\n    let unused = 2\n";
        let already_prefixed = Diagnostic {
            range: Range {
                start: Position {
                    line: 1,
                    character: 8,
                },
                end: Position {
                    line: 1,
                    character: 15,
                },
            },
            ..Default::default()
        };
        let plain = Diagnostic {
            range: Range {
                start: Position {
                    line: 2,
                    character: 8,
                },
                end: Position {
                    line: 2,
                    character: 14,
                },
            },
            ..Default::default()
        };
        assert!(!should_offer_prefix_underscore(&already_prefixed, source));
        assert!(should_offer_prefix_underscore(&plain, source));
    }

    #[test]
    fn test_build_organize_imports_edit_sorts_and_dedups() {
        let source = "from z import c\nimport b\nimport a\nimport b\n\nfn main():\n    pass\n";
        let edit = build_organize_imports_edit(source).expect("expected organize imports edit");
        assert_eq!(edit.range.start.line, 0);
        assert_eq!(edit.new_text, "import a\nimport b\nfrom z import c\n");
    }

    #[test]
    fn test_similar_name_action_produces_edit() {
        let source = "fn main():\n    let value = 1\n    let x = valu\n";
        let diagnostic = Diagnostic {
            range: Range {
                start: Position {
                    line: 2,
                    character: 12,
                },
                end: Position {
                    line: 2,
                    character: 16,
                },
            },
            code: Some(tower_lsp::lsp_types::NumberOrString::String(
                "E0425".to_string(),
            )),
            message: "cannot find value `valu` in this scope".to_string(),
            ..Default::default()
        };
        let uri = Url::parse("file:///test.jet").unwrap();
        let actions = get_code_actions(&uri, &diagnostic, source);
        let replace_action = actions.into_iter().find_map(|a| match a {
            CodeActionOrCommand::CodeAction(action)
                if action.title.contains("Replace with 'value'") =>
            {
                Some(action)
            }
            _ => None,
        });
        assert!(replace_action.is_some());
    }
}
