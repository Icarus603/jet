//! Document state management for the Jet LSP server
//!
//! This module handles document synchronization, parsing, and caching of ASTs.

use jet_diagnostics::{Diagnostic as JetDiagnostic, Level, Span};
use jet_lexer::Lexer;
use jet_parser::Parser;
use ropey::Rope;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};

/// A document in the LSP server
#[derive(Debug)]
pub struct Document {
    /// The document URI
    pub uri: Url,
    /// The document version
    pub version: i32,
    /// The document text as a rope for efficient editing
    pub text: Rope,
    /// Cached AST
    pub ast: Option<jet_parser::ast::Module>,
    /// Cached diagnostics
    pub diagnostics: Vec<Diagnostic>,
}

impl Document {
    /// Create a new document
    pub fn new(uri: Url, version: i32, text: String) -> Self {
        Self {
            uri,
            version,
            text: Rope::from_str(&text),
            ast: None,
            diagnostics: Vec::new(),
        }
    }

    /// Parse the document and update the cached AST and diagnostics
    pub fn parse(&mut self) {
        let text = self.text.to_string();
        let mut lexer = Lexer::new(&text);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(tokens);

        match parser.parse_module() {
            Ok(ast) => {
                self.ast = Some(ast);
                self.diagnostics = Vec::new();
            }
            Err(e) => {
                self.ast = None;
                // Convert parse error to LSP diagnostic
                let diagnostic = Diagnostic {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 0,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("jet".to_string()),
                    message: e.to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                };
                self.diagnostics = vec![diagnostic];
            }
        }
    }

    /// Convert a Position to a byte offset in the rope
    pub fn position_to_offset(&self, position: Position) -> usize {
        let line = position.line as usize;
        let character = position.character as usize;

        if line >= self.text.len_lines() {
            return self.text.len_chars();
        }

        let line_start = self.text.line_to_char(line);
        let line_len = self.text.line(line).len_chars();

        if character >= line_len {
            line_start + line_len
        } else {
            line_start + character
        }
    }

    /// Convert a byte offset to a Position
    #[allow(dead_code)]
    pub fn offset_to_position(&self, offset: usize) -> Position {
        let line = self.text.char_to_line(offset);
        let line_start = self.text.line_to_char(line);
        let character = offset - line_start;

        Position {
            line: line as u32,
            character: character as u32,
        }
    }

    /// Convert a jet-diagnostics Span to an LSP Range
    #[allow(dead_code)]
    pub fn span_to_range(&self, span: Span) -> Range {
        Range {
            start: self.offset_to_position(span.start),
            end: self.offset_to_position(span.end),
        }
    }

    /// Convert a Jet diagnostic to an LSP diagnostic
    #[allow(dead_code)]
    pub fn convert_diagnostic(&self, diag: &JetDiagnostic) -> Diagnostic {
        let severity = match diag.level {
            Level::Error => Some(DiagnosticSeverity::ERROR),
            Level::Warning => Some(DiagnosticSeverity::WARNING),
            Level::Note => Some(DiagnosticSeverity::INFORMATION),
            Level::Help => Some(DiagnosticSeverity::HINT),
        };

        let mut message = diag.message.clone();

        // Add notes to the message
        for note in &diag.notes {
            message.push_str(&format!("\n\nNote: {}", note));
        }

        // Add labels to the message
        for label in &diag.labels {
            let style_str = match label.style {
                jet_diagnostics::LabelStyle::Primary => "Primary",
                jet_diagnostics::LabelStyle::Secondary => "Secondary",
            };
            message.push_str(&format!("\n\n{}: {}", style_str, label.message));
        }

        Diagnostic {
            range: self.span_to_range(diag.span),
            severity,
            code: diag
                .error_code
                .map(|c| tower_lsp::lsp_types::NumberOrString::String(format!("{:?}", c))),
            code_description: None,
            source: Some("jet".to_string()),
            message,
            related_information: None,
            tags: None,
            data: None,
        }
    }

    /// Get the word at a position
    pub fn word_at_position(&self, position: Position) -> Option<String> {
        let offset = self.position_to_offset(position);
        let char_idx = offset;

        if char_idx >= self.text.len_chars() {
            return None;
        }

        // Find word boundaries
        let line = self.text.char_to_line(char_idx);
        let line_start = self.text.line_to_char(line);
        let line_end = line_start + self.text.line(line).len_chars().saturating_sub(1);

        // Find start of word
        let mut word_start = char_idx;
        while word_start > line_start {
            let c = self.text.char(word_start - 1);
            if !is_word_char(c) {
                break;
            }
            word_start -= 1;
        }

        // Find end of word
        let mut word_end = char_idx;
        while word_end < line_end {
            let c = self.text.char(word_end);
            if !is_word_char(c) {
                break;
            }
            word_end += 1;
        }

        if word_start >= word_end {
            return None;
        }

        Some(self.text.slice(word_start..word_end).to_string())
    }

    /// Apply an incremental change to the document
    pub fn apply_change(&mut self, change: &tower_lsp::lsp_types::TextDocumentContentChangeEvent) {
        if let Some(range) = change.range {
            let start_offset = self.position_to_offset(range.start);
            let end_offset = self.position_to_offset(range.end);

            // Remove the old text and insert the new text
            self.text.remove(start_offset..end_offset);
            self.text.insert(start_offset, &change.text);
        } else {
            // Full document replace
            self.text = Rope::from_str(&change.text);
        }
    }
}

/// Check if a character is a valid word character
fn is_word_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[allow(dead_code)]
trait LabelStyleExt {
    fn style_str(&self) -> &'static str;
}

impl LabelStyleExt for jet_diagnostics::LabelStyle {
    fn style_str(&self) -> &'static str {
        match self {
            jet_diagnostics::LabelStyle::Primary => "Primary",
            jet_diagnostics::LabelStyle::Secondary => "Secondary",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_document_new() {
        let uri = Url::parse("file:///test.jet").unwrap();
        let doc = Document::new(uri, 1, "fn main() {}".to_string());

        assert_eq!(doc.version, 1);
        assert_eq!(doc.text.to_string(), "fn main() {}");
    }

    #[test]
    fn test_position_to_offset() {
        let uri = Url::parse("file:///test.jet").unwrap();
        let doc = Document::new(uri, 1, "line1\nline2\nline3".to_string());

        // Position at start of line 0
        let pos = Position {
            line: 0,
            character: 0,
        };
        assert_eq!(doc.position_to_offset(pos), 0);

        // Position at start of line 1 (after "line1\n")
        let pos = Position {
            line: 1,
            character: 0,
        };
        assert_eq!(doc.position_to_offset(pos), 6);

        // Position with character offset
        let pos = Position {
            line: 1,
            character: 3,
        };
        assert_eq!(doc.position_to_offset(pos), 9);
    }

    #[test]
    fn test_offset_to_position() {
        let uri = Url::parse("file:///test.jet").unwrap();
        let doc = Document::new(uri, 1, "line1\nline2\nline3".to_string());

        // Offset 0 should be at line 0, char 0
        let pos = doc.offset_to_position(0);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);

        // Offset 6 should be at line 1, char 0 (after "line1\n")
        let pos = doc.offset_to_position(6);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);
    }

    #[test]
    fn test_word_at_position() {
        let uri = Url::parse("file:///test.jet").unwrap();
        let doc = Document::new(uri, 1, "fn main() {}".to_string());

        // Position on "main"
        let pos = Position {
            line: 0,
            character: 4,
        };
        assert_eq!(doc.word_at_position(pos), Some("main".to_string()));

        // Position on "fn"
        let pos = Position {
            line: 0,
            character: 1,
        };
        assert_eq!(doc.word_at_position(pos), Some("fn".to_string()));
    }
}
