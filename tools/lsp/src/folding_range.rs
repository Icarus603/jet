//! Folding range provider for the Jet LSP server
//!
//! This module provides code folding capabilities based on AST structure.

use jet_parser::ast::{Function, Module, ModuleItem, StructDef};
use tower_lsp::lsp_types::{FoldingRange, FoldingRangeKind, FoldingRangeParams};

use crate::document::Document;

/// Get folding ranges for a document
pub fn get_folding_ranges(doc: &Document, ast: &Module) -> Vec<FoldingRange> {
    let mut ranges = Vec::new();

    for item in &ast.items {
        match item {
            ModuleItem::Function(func) => {
                if let Some(range) = folding_range_for_function(func, &doc.text) {
                    ranges.push(range);
                }
            }
            ModuleItem::Struct(struct_def) => {
                if let Some(range) = folding_range_for_struct(struct_def, &doc.text) {
                    ranges.push(range);
                }
            }
            ModuleItem::Enum(enum_def) => {
                if let Some(range) = folding_range_for_enum(enum_def, &doc.text) {
                    ranges.push(range);
                }
            }
            ModuleItem::Impl(impl_def) => {
                if let Some(range) = folding_range_for_impl(impl_def, &doc.text) {
                    ranges.push(range);
                }
            }
            ModuleItem::Trait(trait_def) => {
                if let Some(range) = folding_range_for_trait(trait_def, &doc.text) {
                    ranges.push(range);
                }
            }
            _ => {}
        }
    }

    // Also add folding ranges for multiline comments
    ranges.extend(folding_ranges_for_comments(&doc.text.to_string()));

    ranges
}

/// Convert byte offset to line number (0-indexed for LSP)
fn byte_offset_to_line(text: &ropey::Rope, offset: usize) -> Option<u32> {
    let offset = offset.min(text.len_chars());
    let line = text.char_to_line(offset);
    Some(line as u32)
}

fn folding_range_for_function(func: &Function, text: &ropey::Rope) -> Option<FoldingRange> {
    // Use function span for folding range
    let start_line = byte_offset_to_line(text, func.span.start)?;
    let end_line = byte_offset_to_line(text, func.span.end)?;

    if start_line < end_line {
        Some(FoldingRange {
            start_line,
            start_character: None,
            end_line,
            end_character: None,
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: Some(func.name.name.clone()),
        })
    } else {
        None
    }
}

fn folding_range_for_struct(struct_def: &StructDef, text: &ropey::Rope) -> Option<FoldingRange> {
    // Use struct span for folding
    let start_line = byte_offset_to_line(text, struct_def.span.start)?;
    let end_line = byte_offset_to_line(text, struct_def.span.end)?;

    if start_line < end_line {
        return Some(FoldingRange {
            start_line,
            start_character: None,
            end_line,
            end_character: None,
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: Some(struct_def.name.name.clone()),
        });
    }
    None
}

fn folding_range_for_enum(
    enum_def: &jet_parser::ast::EnumDef,
    text: &ropey::Rope,
) -> Option<FoldingRange> {
    let start_line = byte_offset_to_line(text, enum_def.span.start)?;
    let end_line = byte_offset_to_line(text, enum_def.span.end)?;

    if start_line < end_line {
        Some(FoldingRange {
            start_line,
            start_character: None,
            end_line,
            end_character: None,
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: Some(enum_def.name.name.clone()),
        })
    } else {
        None
    }
}

fn folding_range_for_impl(
    impl_def: &jet_parser::ast::ImplDef,
    text: &ropey::Rope,
) -> Option<FoldingRange> {
    let start_line = byte_offset_to_line(text, impl_def.span.start)?;
    let end_line = byte_offset_to_line(text, impl_def.span.end)?;

    if start_line < end_line {
        let name = impl_def
            .trait_path
            .as_ref()
            .map(|t| {
                let trait_name = t
                    .segments
                    .last()
                    .map(|s| s.name.clone())
                    .unwrap_or_default();
                format!("impl {} for {:?}", trait_name, impl_def.ty)
            })
            .unwrap_or_else(|| format!("impl {:?}", impl_def.ty));

        Some(FoldingRange {
            start_line,
            start_character: None,
            end_line,
            end_character: None,
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: Some(name),
        })
    } else {
        None
    }
}

fn folding_range_for_trait(
    trait_def: &jet_parser::ast::TraitDef,
    text: &ropey::Rope,
) -> Option<FoldingRange> {
    let start_line = byte_offset_to_line(text, trait_def.span.start)?;
    let end_line = byte_offset_to_line(text, trait_def.span.end)?;

    if start_line < end_line {
        Some(FoldingRange {
            start_line,
            start_character: None,
            end_line,
            end_character: None,
            kind: Some(FoldingRangeKind::Region),
            collapsed_text: Some(trait_def.name.name.clone()),
        })
    } else {
        None
    }
}

fn folding_ranges_for_comments(text: &str) -> Vec<FoldingRange> {
    let mut ranges = Vec::new();
    let lines: Vec<&str> = text.lines().collect();

    let mut i = 0;
    while i < lines.len() {
        let line = lines[i].trim();

        // Check for doc comment blocks (/// or #)
        if line.starts_with("///") || line.starts_with("# ") {
            let start_line = i as u32;
            let mut end_line = i;

            // Find the end of the comment block
            while end_line + 1 < lines.len() {
                let next_line = lines[end_line + 1].trim();
                if next_line.starts_with("///") || next_line.starts_with("# ") {
                    end_line += 1;
                } else {
                    break;
                }
            }

            if end_line > i {
                ranges.push(FoldingRange {
                    start_line,
                    start_character: None,
                    end_line: end_line as u32,
                    end_character: None,
                    kind: Some(FoldingRangeKind::Comment),
                    collapsed_text: Some("/// ...".to_string()),
                });
            }

            i = end_line + 1;
        } else {
            i += 1;
        }
    }

    ranges
}

/// Handle folding range request
pub fn handle_folding_range(
    doc: &Document,
    ast: &Module,
    _params: FoldingRangeParams,
) -> Vec<FoldingRange> {
    get_folding_ranges(doc, ast)
}
