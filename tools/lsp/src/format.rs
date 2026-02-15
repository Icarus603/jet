//! Document formatting for the Jet LSP server
//!
//! Provides code formatting for Jet source files using the jet-fmt crate.

use ropey::Rope;
use tower_lsp::lsp_types::{Position, Range, TextEdit};

/// Format a document
pub fn format_document(text: &Rope) -> Vec<TextEdit> {
    let source = text.to_string();

    // Use the jet-fmt crate for formatting
    match jet_fmt::format_source(&source) {
        Ok(formatted) => {
            // Create a single text edit replacing the entire document
            let end_line = text.len_lines().saturating_sub(1) as u32;
            let last_line_len = if text.len_lines() > 0 {
                text.line(text.len_lines() - 1).len_chars() as u32
            } else {
                0
            };

            vec![TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: end_line,
                        character: last_line_len,
                    },
                },
                new_text: formatted,
            }]
        }
        Err(_) => {
            // If formatting fails (e.g., due to parse errors), return no edits
            vec![]
        }
    }
}

/// Format a range within a document
pub fn format_range(text: &Rope, start_line: usize, end_line: usize) -> Vec<TextEdit> {
    let start_line = start_line.min(text.len_lines());
    let end_line = end_line.min(text.len_lines());

    if start_line >= end_line {
        return vec![];
    }

    // Extract the range
    let start_char = text.line_to_char(start_line);
    let end_char = text.line_to_char(end_line);

    let range_text = text.slice(start_char..end_char).to_string();

    // Try to format just this range
    // For now, we just trim trailing whitespace from each line
    let formatted: String = range_text
        .lines()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n");

    if formatted == range_text {
        return vec![];
    }

    vec![TextEdit {
        range: Range {
            start: Position {
                line: start_line as u32,
                character: 0,
            },
            end: Position {
                line: end_line as u32,
                character: 0,
            },
        },
        new_text: formatted + "\n",
    }]
}

/// Format on type (after specific characters)
pub fn format_on_type(text: &Rope, line: u32, ch: char) -> Vec<TextEdit> {
    match ch {
        ';' | '}' | ')' => {
            // Reformat the current line
            let line_idx = line as usize;
            if line_idx >= text.len_lines() {
                return vec![];
            }

            let line_start = text.line_to_char(line_idx);
            let line_end = line_start + text.line(line_idx).len_chars();
            let line_text = text.slice(line_start..line_end).to_string();

            let trimmed = line_text.trim();
            if trimmed != line_text {
                return vec![TextEdit {
                    range: Range {
                        start: Position { line, character: 0 },
                        end: Position {
                            line,
                            character: line_text.len() as u32,
                        },
                    },
                    new_text: trimmed.to_string(),
                }];
            }
        }
        _ => {}
    }

    vec![]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_document_basic() {
        let text = Rope::from_str("fn main():\n    let x = 1\n");
        let edits = format_document(&text);

        // Should return at least one edit
        assert!(!edits.is_empty());
    }

    #[test]
    fn test_format_range() {
        let text = Rope::from_str("line1\n  line2  \nline3\n");
        let edits = format_range(&text, 1, 2);

        assert_eq!(edits.len(), 1);
        assert!(edits[0].new_text.contains("line2"));
    }

    #[test]
    fn test_format_on_type_semicolon() {
        let text = Rope::from_str("  let x = 1  \n");
        let edits = format_on_type(&text, 0, ';');

        assert!(!edits.is_empty());
    }
}
