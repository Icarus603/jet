//! Documentation extraction from Jet source files
//!
//! This module extracts doc comments and metadata from Jet source files,
//! parsing AI annotations and executable examples.

use crate::{AiAnnotations, DocExample, DocItemKind};
use anyhow::{Context, Result};
use jet_lexer::{tokenize, Token};
use jet_parser::ast::ModuleItem;
use jet_parser::Parser;
use std::path::{Path, PathBuf};

/// A documented module extracted from source
#[derive(Debug, Clone)]
pub struct DocModule {
    /// Source file path
    pub source_file: PathBuf,
    /// Module-level documentation
    pub module_doc: Option<String>,
    /// Documented items in the module
    pub items: Vec<DocItem>,
}

/// A documented item within a module
#[derive(Debug, Clone)]
pub struct DocItem {
    /// The kind of documentation item
    pub kind: DocItemKind,
    /// The name of the item
    pub name: String,
    /// Doc comment content (markdown)
    pub doc_comment: Option<String>,
    /// AI annotations if present
    pub ai_annotations: Option<AiAnnotations>,
    /// Executable examples from the doc comment
    pub examples: Vec<DocExample>,
    /// Line number in source
    pub line_number: usize,
    /// Whether the item is public
    pub is_public: bool,
    /// Type signature (if available)
    pub type_signature: Option<String>,
}

/// Extract documentation from a source file
pub async fn extract_docs_from_file(path: &Path) -> Result<DocModule> {
    let content = tokio::fs::read_to_string(path)
        .await
        .with_context(|| format!("Failed to read file: {}", path.display()))?;

    extract_docs_from_source(&content, path)
}

/// Extract documentation from source code
pub fn extract_docs_from_source(source: &str, source_file: &Path) -> Result<DocModule> {
    // Tokenize to extract doc comments
    let tokens = tokenize(source);

    // Parse the module to get structure
    let mut parser = Parser::new(tokens.clone());
    let module = parser.parse_module()?;

    // Extract doc comments by analyzing tokens
    let doc_comments = extract_doc_comments(&tokens, source);

    // Build documented items from AST and doc comments
    let mut items = Vec::new();
    let mut module_doc = None;

    // Check for module-level doc comment at the start
    if let Some(first_doc) = doc_comments.first() {
        if first_doc.line_number == 1 {
            module_doc = Some(first_doc.content.clone());
        }
    }

    // Process each module item
    for item in &module.items {
        if let Some(doc_item) = process_module_item(item, &doc_comments, source) {
            items.push(doc_item);
        }
    }

    Ok(DocModule {
        source_file: source_file.to_path_buf(),
        module_doc,
        items,
    })
}

/// A raw doc comment extracted from tokens
#[derive(Debug, Clone)]
pub struct RawDocComment {
    /// Content of the doc comment (without ###)
    pub content: String,
    /// Line number in source
    pub line_number: usize,
}

/// Extract doc comments from token stream
fn extract_doc_comments(tokens: &[jet_lexer::SpannedToken], source: &str) -> Vec<RawDocComment> {
    let mut comments = Vec::new();
    let lines: Vec<&str> = source.lines().collect();

    for spanned in tokens.iter() {
        if let Token::DocComment(content) = &spanned.token {
            // Find line number by counting newlines before this token
            let line_number = source[..spanned.span.start].lines().count();
            comments.push(RawDocComment {
                content: content.clone(),
                line_number,
            });
        }
    }

    // Also handle ### style comments that might not be tokenized as DocComment
    for (line_num, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("###") {
            // Check if we already have this comment
            let content = trimmed[3..].trim_start().to_string();
            if !comments
                .iter()
                .any(|c| c.line_number == line_num + 1 && c.content == content)
            {
                comments.push(RawDocComment {
                    content,
                    line_number: line_num + 1,
                });
            }
        }
    }

    comments
}

/// Process a module item to extract documentation
fn process_module_item(
    item: &ModuleItem,
    doc_comments: &[RawDocComment],
    source: &str,
) -> Option<DocItem> {
    match item {
        ModuleItem::Function(func) => {
            let line_number = find_item_line_number(source, &func.name.name);
            let doc_comment = find_doc_comment_for_line(doc_comments, line_number);
            let (doc_text, ai_annotations) = parse_doc_comment_internal(&doc_comment);
            let examples = extract_examples_internal(&doc_text);

            Some(DocItem {
                kind: DocItemKind::Function,
                name: func.name.name.clone(),
                doc_comment: doc_text,
                ai_annotations,
                examples,
                line_number,
                is_public: func.public,
                type_signature: Some(format_function_signature(func)),
            })
        }
        ModuleItem::Struct(struct_def) => {
            let line_number = find_item_line_number(source, &struct_def.name.name);
            let doc_comment = find_doc_comment_for_line(doc_comments, line_number);
            let (doc_text, ai_annotations) = parse_doc_comment_internal(&doc_comment);
            let examples = extract_examples_internal(&doc_text);

            Some(DocItem {
                kind: DocItemKind::Struct,
                name: struct_def.name.name.clone(),
                doc_comment: doc_text,
                ai_annotations,
                examples,
                line_number,
                is_public: struct_def.public,
                type_signature: Some(format_struct_signature(struct_def)),
            })
        }
        ModuleItem::Enum(enum_def) => {
            let line_number = find_item_line_number(source, &enum_def.name.name);
            let doc_comment = find_doc_comment_for_line(doc_comments, line_number);
            let (doc_text, ai_annotations) = parse_doc_comment_internal(&doc_comment);
            let examples = extract_examples_internal(&doc_text);

            Some(DocItem {
                kind: DocItemKind::Enum,
                name: enum_def.name.name.clone(),
                doc_comment: doc_text,
                ai_annotations,
                examples,
                line_number,
                is_public: enum_def.public,
                type_signature: Some(format_enum_signature(enum_def)),
            })
        }
        ModuleItem::Trait(trait_def) => {
            let line_number = find_item_line_number(source, &trait_def.name.name);
            let doc_comment = find_doc_comment_for_line(doc_comments, line_number);
            let (doc_text, ai_annotations) = parse_doc_comment_internal(&doc_comment);
            let examples = extract_examples_internal(&doc_text);

            Some(DocItem {
                kind: DocItemKind::Trait,
                name: trait_def.name.name.clone(),
                doc_comment: doc_text,
                ai_annotations,
                examples,
                line_number,
                is_public: trait_def.public,
                type_signature: Some(format_trait_signature(trait_def)),
            })
        }
        ModuleItem::TypeAlias(type_alias) => {
            let line_number = find_item_line_number(source, &type_alias.name.name);
            let doc_comment = find_doc_comment_for_line(doc_comments, line_number);
            let (doc_text, ai_annotations) = parse_doc_comment_internal(&doc_comment);
            let examples = extract_examples_internal(&doc_text);

            Some(DocItem {
                kind: DocItemKind::TypeAlias,
                name: type_alias.name.name.clone(),
                doc_comment: doc_text,
                ai_annotations,
                examples,
                line_number,
                is_public: type_alias.public,
                type_signature: Some(format_type_alias_signature(type_alias)),
            })
        }
        ModuleItem::Const(const_def) => {
            let line_number = find_item_line_number(source, &const_def.name.name);
            let doc_comment = find_doc_comment_for_line(doc_comments, line_number);
            let (doc_text, ai_annotations) = parse_doc_comment_internal(&doc_comment);
            let examples = extract_examples_internal(&doc_text);

            Some(DocItem {
                kind: DocItemKind::Constant,
                name: const_def.name.name.clone(),
                doc_comment: doc_text,
                ai_annotations,
                examples,
                line_number,
                is_public: const_def.public,
                type_signature: Some(format_const_signature(const_def)),
            })
        }
        ModuleItem::Effect(effect_def) => {
            let line_number = find_item_line_number(source, &effect_def.name.name);
            let doc_comment = find_doc_comment_for_line(doc_comments, line_number);
            let (doc_text, ai_annotations) = parse_doc_comment_internal(&doc_comment);
            let examples = extract_examples_internal(&doc_text);

            Some(DocItem {
                kind: DocItemKind::Effect,
                name: effect_def.name.name.clone(),
                doc_comment: doc_text,
                ai_annotations,
                examples,
                line_number,
                is_public: effect_def.public,
                type_signature: Some(format_effect_signature(effect_def)),
            })
        }
        _ => None,
    }
}

/// Find the line number for an item by searching for its name
fn find_item_line_number(source: &str, name: &str) -> usize {
    for (i, line) in source.lines().enumerate() {
        if line.contains(&format!("fn {}", name))
            || line.contains(&format!("struct {}", name))
            || line.contains(&format!("enum {}", name))
            || line.contains(&format!("trait {}", name))
            || line.contains(&format!("type {}", name))
            || line.contains(&format!("const {}", name))
            || line.contains(&format!("effect {}", name))
        {
            return i + 1;
        }
    }
    0
}

/// Find the doc comment associated with a line
fn find_doc_comment_for_line(doc_comments: &[RawDocComment], item_line: usize) -> Option<String> {
    // Collect consecutive doc comments immediately before the item
    let mut result = Vec::new();

    for comment in doc_comments {
        if comment.line_number < item_line {
            // Check if this is immediately before the item (no blank lines)
            let distance = item_line - comment.line_number;
            if distance <= 10 {
                // Reasonable threshold
                result.push(comment.content.clone());
            }
        }
    }

    if result.is_empty() {
        None
    } else {
        Some(result.join("\n"))
    }
}

/// Parse a doc comment to extract AI annotations (internal use)
fn parse_doc_comment_internal(comment: &Option<String>) -> (Option<String>, Option<AiAnnotations>) {
    parse_doc_comment_inner(comment)
}

/// Extract executable examples from doc comment (internal use)
fn extract_examples_internal(doc_text: &Option<String>) -> Vec<DocExample> {
    extract_examples_inner(doc_text)
}

/// Parse an example with >>> input and expected output
fn parse_example(code: &str, line_number: usize) -> DocExample {
    let lines: Vec<&str> = code.lines().collect();
    let mut input_lines = Vec::new();
    let mut output_lines = Vec::new();
    let mut in_output = false;

    for line in lines {
        let trimmed = line.trim();

        if trimmed.starts_with(">>>") {
            input_lines.push(trimmed[3..].trim_start());
            in_output = true;
        } else if trimmed.starts_with(">") && in_output {
            // Continuation of input
            input_lines.push(trimmed[1..].trim_start());
        } else if in_output && !trimmed.is_empty() {
            // This is expected output
            output_lines.push(line);
        } else if trimmed.is_empty() {
            in_output = false;
        }
    }

    DocExample {
        code: input_lines.join("\n"),
        expected_output: if output_lines.is_empty() {
            None
        } else {
            Some(output_lines.join("\n"))
        },
        should_panic: false,
        line_number,
    }
}

// Signature formatting functions

fn format_function_signature(func: &jet_parser::ast::Function) -> String {
    let mut sig = String::new();

    if func.public {
        sig.push_str("pub ");
    }

    sig.push_str(&format!("fn {}", func.name.name));

    if !func.generics.is_empty() {
        let params: Vec<_> = func.generics.iter().map(|g| g.name.name.clone()).collect();
        sig.push_str(&format!("[{}]", params.join(", ")));
    }

    sig.push('(');
    let params: Vec<_> = func
        .params
        .iter()
        .map(|p| format!("{:?}: {:?}", p.pattern, p.ty))
        .collect();
    sig.push_str(&params.join(", "));
    sig.push(')');

    if let Some(ret) = &func.return_type {
        sig.push_str(&format!(" -> {:?}", ret));
    }

    if !func.effects.is_empty() {
        let effects: Vec<_> = func.effects.iter().map(|e| format!("{:?}", e)).collect();
        sig.push_str(&format!(" ! {}", effects.join(", ")));
    }

    sig
}

fn format_struct_signature(struct_def: &jet_parser::ast::StructDef) -> String {
    let mut sig = String::new();

    if struct_def.public {
        sig.push_str("pub ");
    }

    sig.push_str(&format!("struct {}", struct_def.name.name));

    if !struct_def.generics.is_empty() {
        let params: Vec<_> = struct_def
            .generics
            .iter()
            .map(|g| g.name.name.clone())
            .collect();
        sig.push_str(&format!("[{}]", params.join(", ")));
    }

    sig.push_str(" {");
    let fields: Vec<_> = struct_def
        .fields
        .iter()
        .map(|f| format!("{}: {:?}", f.name.name, f.ty))
        .collect();
    sig.push_str(&fields.join(", "));
    sig.push('}');

    sig
}

fn format_enum_signature(enum_def: &jet_parser::ast::EnumDef) -> String {
    let mut sig = String::new();

    if enum_def.public {
        sig.push_str("pub ");
    }

    sig.push_str(&format!("enum {}", enum_def.name.name));

    if !enum_def.generics.is_empty() {
        let params: Vec<_> = enum_def
            .generics
            .iter()
            .map(|g| g.name.name.clone())
            .collect();
        sig.push_str(&format!("[{}]", params.join(", ")));
    }

    sig
}

fn format_trait_signature(trait_def: &jet_parser::ast::TraitDef) -> String {
    let mut sig = String::new();

    if trait_def.public {
        sig.push_str("pub ");
    }

    sig.push_str(&format!("trait {}", trait_def.name.name));

    if !trait_def.generics.is_empty() {
        let params: Vec<_> = trait_def
            .generics
            .iter()
            .map(|g| g.name.name.clone())
            .collect();
        sig.push_str(&format!("[{}]", params.join(", ")));
    }

    sig
}

fn format_type_alias_signature(type_alias: &jet_parser::ast::TypeAlias) -> String {
    let mut sig = String::new();

    if type_alias.public {
        sig.push_str("pub ");
    }

    sig.push_str(&format!("type {}", type_alias.name.name));

    if !type_alias.generics.is_empty() {
        let params: Vec<_> = type_alias
            .generics
            .iter()
            .map(|g| g.name.name.clone())
            .collect();
        sig.push_str(&format!("[{}]", params.join(", ")));
    }

    sig.push_str(&format!(" = {:?}", type_alias.ty));

    sig
}

fn format_const_signature(const_def: &jet_parser::ast::ConstDef) -> String {
    let mut sig = String::new();

    if const_def.public {
        sig.push_str("pub ");
    }

    sig.push_str(&format!(
        "const {}: {:?}",
        const_def.name.name, const_def.ty
    ));

    sig
}

fn format_effect_signature(effect_def: &jet_parser::ast::EffectDef) -> String {
    let mut sig = String::new();

    if effect_def.public {
        sig.push_str("pub ");
    }

    sig.push_str(&format!("effect {}", effect_def.name.name));

    if !effect_def.generics.is_empty() {
        let params: Vec<_> = effect_def
            .generics
            .iter()
            .map(|g| g.name.name.clone())
            .collect();
        sig.push_str(&format!("[{}]", params.join(", ")));
    }

    sig
}

/// Public API for extracting doc comments from source code
pub fn extract_doc_comments_from_source(source: &str) -> Vec<RawDocComment> {
    let tokens = tokenize(source);
    extract_doc_comments(&tokens, source)
}

/// Public API for parsing a doc comment string
pub fn parse_doc_comment(comment: &str) -> (Option<String>, Option<AiAnnotations>) {
    let comment_opt = if comment.is_empty() {
        None
    } else {
        Some(comment.to_string())
    };
    parse_doc_comment_inner(&comment_opt)
}

/// Public API for extracting examples from doc text
pub fn extract_examples(doc_text: &str) -> Vec<DocExample> {
    let doc_opt = if doc_text.is_empty() {
        None
    } else {
        Some(doc_text.to_string())
    };
    extract_examples_inner(&doc_opt)
}

// Internal version that takes Option<String>
fn parse_doc_comment_inner(comment: &Option<String>) -> (Option<String>, Option<AiAnnotations>) {
    let comment = match comment {
        Some(c) => c,
        None => return (None, None),
    };

    let mut annotations = AiAnnotations::default();
    let mut lines = Vec::new();

    for line in comment.lines() {
        let trimmed = line.trim();

        // Parse AI annotations
        if trimmed.starts_with("@confidence") {
            if let Some(value) = trimmed.split_whitespace().nth(1) {
                annotations.confidence = value.parse().ok();
            }
        } else if trimmed.starts_with("@generated_by") {
            annotations.generated_by = trimmed.splitn(2, ' ').nth(1).map(|s| s.trim().to_string());
        } else if trimmed.starts_with("@prompt") {
            annotations.prompt = trimmed.splitn(2, ' ').nth(1).map(|s| s.trim().to_string());
        } else if trimmed.starts_with("@human_edit_count") {
            if let Some(value) = trimmed.split_whitespace().nth(1) {
                annotations.human_edit_count = value.parse().ok();
            }
        } else {
            lines.push(line);
        }
    }

    let doc_text = if lines.is_empty() {
        None
    } else {
        Some(lines.join("\n"))
    };

    let ai_annos = if annotations.confidence.is_some()
        || annotations.generated_by.is_some()
        || annotations.prompt.is_some()
        || annotations.human_edit_count.is_some()
    {
        Some(annotations)
    } else {
        None
    };

    (doc_text, ai_annos)
}

// Internal version that takes Option<String>
fn extract_examples_inner(doc_text: &Option<String>) -> Vec<DocExample> {
    let text = match doc_text {
        Some(t) => t,
        None => return Vec::new(),
    };

    let mut examples = Vec::new();
    let lines: Vec<&str> = text.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Look for code block start
        if line.starts_with("```") {
            let lang = line[3..].trim();
            let is_jet = lang.is_empty() || lang == "jet";
            let mut code_lines = Vec::new();
            let mut output_lines = Vec::new();
            let mut in_output = false;
            let start_line = i;
            i += 1;

            // Collect code until closing ```
            while i < lines.len() && !lines[i].trim().starts_with("```") {
                let code_line = lines[i];
                let code_trimmed = code_line.trim();

                if code_trimmed.starts_with(">>>") {
                    code_lines.push(code_trimmed[3..].trim_start().to_string());
                    in_output = true;
                } else if code_trimmed.starts_with(">") && in_output {
                    code_lines.push(code_trimmed[1..].trim_start().to_string());
                } else if in_output && !code_trimmed.is_empty() {
                    output_lines.push(code_trimmed.to_string());
                } else if !code_trimmed.is_empty() {
                    code_lines.push(code_trimmed.to_string());
                }
                i += 1;
            }

            if is_jet && !code_lines.is_empty() {
                examples.push(DocExample {
                    code: code_lines.join("\n"),
                    expected_output: if output_lines.is_empty() {
                        None
                    } else {
                        Some(output_lines.join("\n"))
                    },
                    should_panic: false,
                    line_number: start_line + 1,
                });
            }
        }

        i += 1;
    }

    examples
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_doc_comment() {
        let comment = "This is a function.\n@confidence 0.95\n@generated_by claude";

        let (doc, ai) = parse_doc_comment(comment);

        assert_eq!(doc, Some("This is a function.".to_string()));
        assert!(ai.is_some());
        let ai = ai.unwrap();
        assert_eq!(ai.confidence, Some(0.95));
        assert_eq!(ai.generated_by, Some("claude".to_string()));
    }

    #[test]
    fn test_extract_examples() {
        let doc = r#"Adds two numbers.

```
>>> add(2, 3)
5
```
"#;

        let examples = extract_examples(doc);

        assert_eq!(examples.len(), 1);
        assert_eq!(examples[0].code, "add(2, 3)");
        assert_eq!(examples[0].expected_output, Some("5".to_string()));
    }
}
