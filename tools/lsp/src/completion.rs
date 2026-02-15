//! Completion logic for the Jet LSP server
//!
//! Provides code completion suggestions based on context.

use jet_parser::ast::{Function, Module, ModuleItem, Pattern, StructDef};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Position};

/// Keywords in the Jet language
const KEYWORDS: &[(&str, &str)] = &[
    ("fn", "Define a function"),
    ("let", "Define a variable"),
    ("if", "Conditional expression"),
    ("else", "Else branch"),
    ("elif", "Else-if branch"),
    ("match", "Pattern matching"),
    ("return", "Return from function"),
    ("struct", "Define a struct"),
    ("enum", "Define an enum"),
    ("trait", "Define a trait"),
    ("impl", "Implement a trait"),
    ("import", "Import a module"),
    ("from", "Import from module"),
    ("pub", "Make item public"),
    ("mut", "Mutable binding"),
    ("async", "Async expression/block"),
    ("await", "Await an async expression"),
    ("spawn", "Spawn a concurrent task"),
    ("concurrent", "Concurrent block"),
    ("for", "For loop"),
    ("while", "While loop"),
    ("loop", "Infinite loop"),
    ("break", "Break from loop"),
    ("continue", "Continue to next iteration"),
    ("in", "Used in for loops"),
    ("and", "Logical AND"),
    ("or", "Logical OR"),
    ("not", "Logical NOT"),
    ("true", "Boolean true"),
    ("false", "Boolean false"),
    ("unit", "Unit type/value"),
    ("type", "Type alias"),
    ("mod", "Define a module"),
    ("use", "Use a trait/item"),
    ("where", "Type constraint clause"),
    ("chan", "Channel type"),
    ("raise", "Raise an error/effect"),
    ("self", "Self reference"),
    ("Self", "Self type"),
];

/// Built-in types
const BUILT_IN_TYPES: &[(&str, &str)] = &[
    ("int", "Signed integer type"),
    ("uint", "Unsigned integer type"),
    ("float", "Floating-point type"),
    ("bool", "Boolean type"),
    ("string", "String type"),
    ("char", "Character type"),
    ("unit", "Unit type"),
];

/// Get completions based on the AST and position
pub fn get_completions(ast: &Module, _position: Position) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Always add keywords
    items.extend(get_keyword_completions());

    // Add built-in types
    items.extend(get_type_completions());

    // Add symbols from the module
    items.extend(get_module_completions(ast));

    items
}

/// Get keyword completions
pub fn get_keyword_completions() -> Vec<CompletionItem> {
    KEYWORDS
        .iter()
        .map(|(kw, doc)| CompletionItem {
            label: kw.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(doc.to_string()),
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: Some(format!("1_{}", kw)),
            filter_text: None,
            insert_text: Some(kw.to_string()),
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
            label_details: None,
        })
        .collect()
}

/// Get type completions
fn get_type_completions() -> Vec<CompletionItem> {
    BUILT_IN_TYPES
        .iter()
        .map(|(ty, doc)| CompletionItem {
            label: ty.to_string(),
            kind: Some(CompletionItemKind::TYPE_PARAMETER),
            detail: Some(doc.to_string()),
            documentation: None,
            deprecated: None,
            preselect: None,
            sort_text: Some(format!("2_{}", ty)),
            filter_text: None,
            insert_text: Some(ty.to_string()),
            insert_text_format: None,
            insert_text_mode: None,
            text_edit: None,
            additional_text_edits: None,
            command: None,
            commit_characters: None,
            data: None,
            tags: None,
            label_details: None,
        })
        .collect()
}

/// Get completions from module items
fn get_module_completions(ast: &Module) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for item in &ast.items {
        match item {
            ModuleItem::Function(func) => {
                items.push(function_to_completion(func));
            }
            ModuleItem::Struct(struct_def) => {
                items.push(struct_to_completion(struct_def));
            }
            ModuleItem::Enum(enum_def) => {
                items.push(CompletionItem {
                    label: enum_def.name.name.clone(),
                    kind: Some(CompletionItemKind::ENUM),
                    detail: Some(format!("enum {}", enum_def.name.name)),
                    documentation: None,
                    deprecated: None,
                    preselect: None,
                    sort_text: Some(format!("3_{}", enum_def.name.name)),
                    filter_text: None,
                    insert_text: Some(enum_def.name.name.clone()),
                    insert_text_format: None,
                    insert_text_mode: None,
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: None,
                    data: None,
                    tags: None,
                    label_details: None,
                });
            }
            ModuleItem::Trait(trait_def) => {
                items.push(CompletionItem {
                    label: trait_def.name.name.clone(),
                    kind: Some(CompletionItemKind::INTERFACE),
                    detail: Some(format!("trait {}", trait_def.name.name)),
                    documentation: None,
                    deprecated: None,
                    preselect: None,
                    sort_text: Some(format!("3_{}", trait_def.name.name)),
                    filter_text: None,
                    insert_text: Some(trait_def.name.name.clone()),
                    insert_text_format: None,
                    insert_text_mode: None,
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: None,
                    data: None,
                    tags: None,
                    label_details: None,
                });
            }
            ModuleItem::Const(const_def) => {
                items.push(CompletionItem {
                    label: const_def.name.name.clone(),
                    kind: Some(CompletionItemKind::CONSTANT),
                    detail: Some(format!("const {}", const_def.name.name)),
                    documentation: None,
                    deprecated: None,
                    preselect: None,
                    sort_text: Some(format!("4_{}", const_def.name.name)),
                    filter_text: None,
                    insert_text: Some(const_def.name.name.clone()),
                    insert_text_format: None,
                    insert_text_mode: None,
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: None,
                    data: None,
                    tags: None,
                    label_details: None,
                });
            }
            ModuleItem::TypeAlias(type_alias) => {
                items.push(CompletionItem {
                    label: type_alias.name.name.clone(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some(format!("type {}", type_alias.name.name)),
                    documentation: None,
                    deprecated: None,
                    preselect: None,
                    sort_text: Some(format!("3_{}", type_alias.name.name)),
                    filter_text: None,
                    insert_text: Some(type_alias.name.name.clone()),
                    insert_text_format: None,
                    insert_text_mode: None,
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: None,
                    data: None,
                    tags: None,
                    label_details: None,
                });
            }
            _ => {}
        }
    }

    items
}

/// Convert a function to a completion item
fn function_to_completion(func: &Function) -> CompletionItem {
    let name = func.name.name.clone();
    let detail = format_function_signature(func);

    CompletionItem {
        label: name.clone(),
        kind: Some(CompletionItemKind::FUNCTION),
        detail: Some(detail),
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: Some(format!("3_{}", name)),
        filter_text: None,
        insert_text: Some(format_function_snippet(func)),
        insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
        label_details: None,
    }
}

/// Convert a struct to a completion item
fn struct_to_completion(struct_def: &StructDef) -> CompletionItem {
    let name = struct_def.name.name.clone();

    CompletionItem {
        label: name.clone(),
        kind: Some(CompletionItemKind::STRUCT),
        detail: Some(format!("struct {}", name)),
        documentation: None,
        deprecated: None,
        preselect: None,
        sort_text: Some(format!("3_{}", name)),
        filter_text: None,
        insert_text: Some(name),
        insert_text_format: None,
        insert_text_mode: None,
        text_edit: None,
        additional_text_edits: None,
        command: None,
        commit_characters: None,
        data: None,
        tags: None,
        label_details: None,
    }
}

/// Format a function signature for display
fn format_function_signature(func: &Function) -> String {
    let mut result = String::new();

    if func.public {
        result.push_str("pub ");
    }

    result.push_str("fn ");
    result.push_str(&func.name.name);

    // Generic parameters
    if !func.generics.is_empty() {
        result.push('[');
        for (i, param) in func.generics.iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }
            result.push_str(&param.name.name);
        }
        result.push(']');
    }

    // Parameters
    result.push('(');
    for (i, param) in func.params.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(&format_pattern(&param.pattern));
        result.push_str(": ");
        result.push_str(&format_type(&param.ty));
    }
    result.push(')');

    // Return type
    if let Some(ret) = &func.return_type {
        result.push_str(" -> ");
        result.push_str(&format_type(ret));
    }

    // Effects
    if !func.effects.is_empty() {
        result.push_str(" ! ");
        for (i, effect) in func.effects.iter().enumerate() {
            if i > 0 {
                result.push_str(" | ");
            }
            result.push_str(&format_type(effect));
        }
    }

    result
}

/// Format a function as a snippet for insertion
fn format_function_snippet(func: &Function) -> String {
    let mut result = func.name.name.clone();
    result.push('(');

    // Add placeholders for parameters
    for (i, _param) in func.params.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(&format!(
            "${{{}:{}}}",
            i + 1,
            "arg".to_string() + &i.to_string()
        ));
    }

    result.push(')');
    result
}

/// Format a pattern for display
fn format_pattern(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Ident { mutable, name } => {
            if *mutable {
                format!("mut {}", name.name)
            } else {
                name.name.clone()
            }
        }
        Pattern::Wildcard(_) => "_".to_string(),
        Pattern::Tuple(patterns) => {
            let inner: Vec<String> = patterns.iter().map(format_pattern).collect();
            format!("({})", inner.join(", "))
        }
        _ => "...".to_string(),
    }
}

/// Format a type for display
fn format_type(ty: &jet_parser::ast::Type) -> String {
    use jet_parser::ast::Type;

    match ty {
        Type::Path(path) => path
            .segments
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<_>>()
            .join("::"),
        Type::Generic(base, args) => {
            let base_str = format_type(base);
            let args_str: Vec<String> = args.iter().map(format_type).collect();
            format!("{}[{}]", base_str, args_str.join(", "))
        }
        Type::Tuple(types) => {
            let inner: Vec<String> = types.iter().map(format_type).collect();
            format!("({})", inner.join(", "))
        }
        Type::Array(inner, size) => {
            let inner_str = format_type(inner);
            if size.is_some() {
                format!("[{}; ...]", inner_str)
            } else {
                format!("[{}]", inner_str)
            }
        }
        Type::Function {
            params,
            return_type,
            effects,
        } => {
            let params_str: Vec<String> = params.iter().map(format_type).collect();
            let mut result = format!("fn({})", params_str.join(", "));
            if let Some(ret) = return_type {
                result.push_str(&format!(" -> {}", format_type(ret)));
            }
            if !effects.is_empty() {
                result.push_str(" ! ");
                let effects_str: Vec<String> = effects.iter().map(format_type).collect();
                result.push_str(&effects_str.join(" | "));
            }
            result
        }
        Type::Reference { mutable, inner } => {
            if *mutable {
                format!("&mut {}", format_type(inner))
            } else {
                format!("&{}", format_type(inner))
            }
        }
        Type::Channel(inner) => format!("chan[{}]", format_type(inner)),
        Type::Async(inner) => format!("async {}", format_type(inner)),
        Type::Infer => "_".to_string(),
        Type::SelfType => "Self".to_string(),
    }
}

/// Get completions triggered by a specific character
pub fn get_trigger_completions(
    ast: &Module,
    position: Position,
    trigger_char: char,
) -> Vec<CompletionItem> {
    match trigger_char {
        '.' => get_dot_completions(ast, position),
        ':' => get_colon_completions(ast, position),
        _ => Vec::new(),
    }
}

/// Get completions after a dot (field/method access)
fn get_dot_completions(_ast: &Module, _position: Position) -> Vec<CompletionItem> {
    // TODO: Analyze expression before dot to provide field/method completions
    Vec::new()
}

/// Get completions after a colon (type annotations, paths)
fn get_colon_completions(_ast: &Module, _position: Position) -> Vec<CompletionItem> {
    // TODO: Provide type completions based on context
    get_type_completions()
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::Module;

    #[test]
    fn test_keyword_completions() {
        let completions = get_keyword_completions();
        assert!(!completions.is_empty());

        // Check that "fn" is included
        let fn_kw = completions.iter().find(|c| c.label == "fn");
        assert!(fn_kw.is_some());
        assert_eq!(fn_kw.unwrap().kind, Some(CompletionItemKind::KEYWORD));
    }

    #[test]
    fn test_type_completions() {
        let completions = get_type_completions();
        assert!(!completions.is_empty());

        // Check that "int" is included
        let int_ty = completions.iter().find(|c| c.label == "int");
        assert!(int_ty.is_some());
        assert_eq!(
            int_ty.unwrap().kind,
            Some(CompletionItemKind::TYPE_PARAMETER)
        );
    }

    #[test]
    fn test_get_completions_empty_module() {
        let ast = Module::new(Span::new(0, 0));
        let completions = get_completions(
            &ast,
            Position {
                line: 0,
                character: 0,
            },
        );
        // Should at least have keywords
        assert!(!completions.is_empty());
    }
}
