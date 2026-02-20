//! Completion logic for the Jet LSP server
//!
//! Provides code completion suggestions based on context.

use jet_parser::ast::{
    Function, ImplItem, Module, ModuleItem, Pattern, StructDef, TraitDef, Type as AstType,
};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Documentation, Position};

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

    for (i, param) in func.params.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        let placeholder_name = snippet_placeholder_name(&param.pattern, i + 1);
        result.push_str(&format!("${{{}:{}}}", i + 1, placeholder_name));
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
fn get_dot_completions(ast: &Module, _position: Position) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Collect all struct definitions for field completions
    let structs = collect_structs(ast);
    let impls = collect_impls(ast);
    let traits = collect_traits(ast);

    // Add struct fields as completions
    for (struct_name, struct_def) in &structs {
        for field in &struct_def.fields {
            let detail = format!("{}: {}", field.name.name, format_type(&field.ty));
            items.push(CompletionItem {
                label: field.name.name.clone(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(detail),
                documentation: Some(Documentation::String(format!(
                    "Field of struct `{}`",
                    struct_name
                ))),
                deprecated: None,
                preselect: None,
                sort_text: Some(format!("1_{}", field.name.name)),
                filter_text: None,
                insert_text: Some(field.name.name.clone()),
                insert_text_format: None,
                insert_text_mode: None,
                text_edit: None,
                additional_text_edits: None,
                command: None,
                commit_characters: None,
                data: None,
                tags: None,
                label_details: Some(tower_lsp::lsp_types::CompletionItemLabelDetails {
                    detail: Some(format!(" ({})", struct_name)),
                    description: Some(format_type(&field.ty)),
                }),
            });
        }
    }

    // Add methods from impl blocks
    for (type_name, methods) in &impls {
        for method in methods {
            let detail = format_method_signature(method);
            items.push(CompletionItem {
                label: method.name.name.clone(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some(detail.clone()),
                documentation: Some(Documentation::String(format!("Method of `{}`", type_name))),
                deprecated: None,
                preselect: None,
                sort_text: Some(format!("2_{}", method.name.name)),
                filter_text: None,
                insert_text: Some(format_method_snippet(method)),
                insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
                insert_text_mode: None,
                text_edit: None,
                additional_text_edits: None,
                command: None,
                commit_characters: None,
                data: None,
                tags: None,
                label_details: Some(tower_lsp::lsp_types::CompletionItemLabelDetails {
                    detail: Some(format!(" ({})", type_name)),
                    description: Some(detail),
                }),
            });
        }
    }

    // Add trait methods
    for (trait_name, trait_def) in &traits {
        for item in &trait_def.items {
            if let jet_parser::ast::TraitItem::Method {
                name,
                params,
                return_type,
                ..
            } = item
            {
                let detail =
                    format_trait_method_signature(name.name.as_str(), params, return_type.as_ref());
                items.push(CompletionItem {
                    label: name.name.clone(),
                    kind: Some(CompletionItemKind::METHOD),
                    detail: Some(detail.clone()),
                    documentation: Some(Documentation::String(format!(
                        "Trait method from `{}`",
                        trait_name
                    ))),
                    deprecated: None,
                    preselect: None,
                    sort_text: Some(format!("3_{}", name.name)),
                    filter_text: None,
                    insert_text: Some(format_trait_method_snippet(name.name.as_str(), params)),
                    insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
                    insert_text_mode: None,
                    text_edit: None,
                    additional_text_edits: None,
                    command: None,
                    commit_characters: None,
                    data: None,
                    tags: None,
                    label_details: Some(tower_lsp::lsp_types::CompletionItemLabelDetails {
                        detail: Some(format!(" (trait {})", trait_name)),
                        description: None,
                    }),
                });
            }
        }
    }

    items
}

/// Collect all struct definitions from the module
fn collect_structs(ast: &Module) -> Vec<(String, &StructDef)> {
    let mut structs = Vec::new();
    for item in &ast.items {
        if let ModuleItem::Struct(struct_def) = item {
            structs.push((struct_def.name.name.clone(), struct_def));
        }
    }
    structs
}

/// Collect all impl blocks and their methods from the module
fn collect_impls(ast: &Module) -> Vec<(String, Vec<&Function>)> {
    let mut impls: Vec<(String, Vec<&Function>)> = Vec::new();

    for item in &ast.items {
        if let ModuleItem::Impl(impl_def) = item {
            let type_name = format_type(&impl_def.ty);
            let methods: Vec<&Function> = impl_def
                .items
                .iter()
                .filter_map(|item| {
                    if let ImplItem::Method(func) = item {
                        Some(func)
                    } else {
                        None
                    }
                })
                .collect();

            if !methods.is_empty() {
                // Check if we already have an entry for this type
                if let Some(existing) = impls.iter_mut().find(|(name, _)| name == &type_name) {
                    existing.1.extend(methods);
                } else {
                    impls.push((type_name, methods));
                }
            }
        }
    }

    impls
}

/// Collect all trait definitions from the module
fn collect_traits(ast: &Module) -> Vec<(String, &TraitDef)> {
    let mut traits = Vec::new();
    for item in &ast.items {
        if let ModuleItem::Trait(trait_def) = item {
            traits.push((trait_def.name.name.clone(), trait_def));
        }
    }
    traits
}

/// Format a method signature for display
fn format_method_signature(func: &Function) -> String {
    let mut result = String::new();
    result.push_str("fn ");
    result.push_str(&func.name.name);
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
    if let Some(ret) = &func.return_type {
        result.push_str(" -> ");
        result.push_str(&format_type(ret));
    }
    result
}

/// Format a method as a snippet for insertion
fn format_method_snippet(func: &Function) -> String {
    let mut result = func.name.name.clone();
    result.push('(');
    for (i, param) in func.params.iter().skip(1).enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        let placeholder_name = snippet_placeholder_name(&param.pattern, i + 1);
        result.push_str(&format!("${{{}:{}}}", i + 1, placeholder_name));
    }
    result.push(')');
    result
}

/// Format a trait method signature
fn format_trait_method_signature(
    name: &str,
    params: &[jet_parser::ast::Param],
    return_type: Option<&AstType>,
) -> String {
    let mut result = String::new();
    result.push_str("fn ");
    result.push_str(name);
    result.push('(');
    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(&format_pattern(&param.pattern));
        result.push_str(": ");
        result.push_str(&format_type(&param.ty));
    }
    result.push(')');
    if let Some(ret) = return_type {
        result.push_str(" -> ");
        result.push_str(&format_type(ret));
    }
    result
}

/// Format a trait method as a snippet
fn format_trait_method_snippet(name: &str, params: &[jet_parser::ast::Param]) -> String {
    let mut result = name.to_string();
    result.push('(');
    // Skip first param if it's self
    let start_idx = if params
        .first()
        .map(|p| format_pattern(&p.pattern).contains("self"))
        .unwrap_or(false)
    {
        1
    } else {
        0
    };
    for (i, param) in params.iter().skip(start_idx).enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        let idx = i + 1;
        let placeholder_name = snippet_placeholder_name(&param.pattern, idx);
        result.push_str(&format!("${{{}:{}}}", idx, placeholder_name));
    }
    result.push(')');
    result
}

fn snippet_placeholder_name(pattern: &Pattern, idx: usize) -> String {
    let fallback = format!("arg{}", idx);
    let raw = first_pattern_name(pattern).unwrap_or(fallback.clone());
    let sanitized: String = raw
        .chars()
        .filter(|c: &char| c.is_ascii_alphanumeric() || *c == '_')
        .collect();
    if sanitized.is_empty() {
        return fallback;
    }
    if sanitized
        .chars()
        .next()
        .map(|c: char| c.is_ascii_digit())
        .unwrap_or(false)
    {
        return format!("arg_{}", sanitized);
    }
    sanitized
}

fn first_pattern_name(pattern: &Pattern) -> Option<String> {
    match pattern {
        Pattern::Ident { name, .. } => Some(name.name.clone()),
        Pattern::Bind { name, .. } => Some(name.name.clone()),
        Pattern::Mut(inner) => first_pattern_name(inner),
        Pattern::Ref { pattern, .. } => first_pattern_name(pattern),
        Pattern::Rest(Some(name)) => Some(name.name.clone()),
        Pattern::Tuple(patterns) | Pattern::Array(patterns) => {
            patterns.iter().find_map(first_pattern_name)
        }
        _ => None,
    }
}

/// Get completions after a colon (type annotations, paths)
fn get_colon_completions(ast: &Module, _position: Position) -> Vec<CompletionItem> {
    let mut items = get_type_completions();

    // Add type constructors for context-aware completion
    items.extend(get_type_constructors(ast));

    // Add user-defined types from the module
    items.extend(get_user_defined_types(ast));

    items
}

/// Get type constructors (struct and enum constructors)
fn get_type_constructors(ast: &Module) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for item in &ast.items {
        match item {
            ModuleItem::Struct(struct_def) => {
                // Add struct constructor if it has fields
                if !struct_def.fields.is_empty() {
                    let name = &struct_def.name.name;
                    let snippet = format_struct_constructor_snippet(struct_def);
                    items.push(CompletionItem {
                        label: format!("{} {{...}}", name),
                        kind: Some(CompletionItemKind::CONSTRUCTOR),
                        detail: Some(format!("Constructor for struct {}", name)),
                        documentation: Some(Documentation::String(format!(
                            "Create a new {} instance",
                            name
                        ))),
                        deprecated: None,
                        preselect: None,
                        sort_text: Some(format!("1_{}", name)),
                        filter_text: Some(name.clone()),
                        insert_text: Some(snippet),
                        insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
                        insert_text_mode: None,
                        text_edit: None,
                        additional_text_edits: None,
                        command: None,
                        commit_characters: None,
                        data: None,
                        tags: None,
                        label_details: Some(tower_lsp::lsp_types::CompletionItemLabelDetails {
                            detail: Some(" (constructor)".to_string()),
                            description: Some(format!("struct {}", name)),
                        }),
                    });
                }
            }
            ModuleItem::Enum(enum_def) => {
                // Add enum variant constructors
                for variant in &enum_def.variants {
                    let name = &variant.name.name;
                    let snippet = format_enum_variant_snippet(variant);
                    items.push(CompletionItem {
                        label: format!("{}::{}", enum_def.name.name, name),
                        kind: Some(CompletionItemKind::CONSTRUCTOR),
                        detail: Some(format!("Variant {} of enum {}", name, enum_def.name.name)),
                        documentation: None,
                        deprecated: None,
                        preselect: None,
                        sort_text: Some(format!("2_{}", name)),
                        filter_text: Some(format!("{}::{}", enum_def.name.name, name)),
                        insert_text: Some(snippet),
                        insert_text_format: Some(tower_lsp::lsp_types::InsertTextFormat::SNIPPET),
                        insert_text_mode: None,
                        text_edit: None,
                        additional_text_edits: None,
                        command: None,
                        commit_characters: None,
                        data: None,
                        tags: None,
                        label_details: Some(tower_lsp::lsp_types::CompletionItemLabelDetails {
                            detail: Some(format!(" ({}::{})", enum_def.name.name, name)),
                            description: None,
                        }),
                    });
                }
            }
            _ => {}
        }
    }

    items
}

/// Format a struct constructor as a snippet
fn format_struct_constructor_snippet(struct_def: &StructDef) -> String {
    let mut result = struct_def.name.name.clone();
    result.push_str(" {\n");
    for (i, field) in struct_def.fields.iter().enumerate() {
        result.push_str("    ");
        result.push_str(&field.name.name);
        result.push_str(": ");
        result.push_str(&format!("${{{}:{}}}", i + 1, format_type(&field.ty)));
        result.push_str(",\n");
    }
    result.push('}');
    result
}

/// Format an enum variant as a snippet
fn format_enum_variant_snippet(variant: &jet_parser::ast::EnumVariant) -> String {
    use jet_parser::ast::VariantBody;

    let mut result = variant.name.name.clone();
    match &variant.body {
        VariantBody::Unit => {}
        VariantBody::Tuple(types) => {
            result.push('(');
            for (i, ty) in types.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&format!("${{{}:{}}}", i + 1, format_type(ty)));
            }
            result.push(')');
        }
        VariantBody::Struct(fields) => {
            result.push_str(" {\n");
            for (i, field) in fields.iter().enumerate() {
                result.push_str("    ");
                result.push_str(&field.name.name);
                result.push_str(": ");
                result.push_str(&format!("${{{}:{}}}", i + 1, format_type(&field.ty)));
                result.push_str(",\n");
            }
            result.push('}');
        }
        VariantBody::Discriminant(_) => {}
    }
    result
}

/// Get user-defined types from the module
fn get_user_defined_types(ast: &Module) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    for item in &ast.items {
        match item {
            ModuleItem::Struct(struct_def) => {
                items.push(CompletionItem {
                    label: struct_def.name.name.clone(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some(format!("struct {}", struct_def.name.name)),
                    documentation: None,
                    deprecated: None,
                    preselect: None,
                    sort_text: Some(format!("2_{}", struct_def.name.name)),
                    filter_text: None,
                    insert_text: Some(struct_def.name.name.clone()),
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
            ModuleItem::Enum(enum_def) => {
                items.push(CompletionItem {
                    label: enum_def.name.name.clone(),
                    kind: Some(CompletionItemKind::ENUM),
                    detail: Some(format!("enum {}", enum_def.name.name)),
                    documentation: None,
                    deprecated: None,
                    preselect: None,
                    sort_text: Some(format!("2_{}", enum_def.name.name)),
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
            ModuleItem::TypeAlias(type_alias) => {
                items.push(CompletionItem {
                    label: type_alias.name.name.clone(),
                    kind: Some(CompletionItemKind::TYPE_PARAMETER),
                    detail: Some(format!(
                        "type {} = {}",
                        type_alias.name.name,
                        format_type(&type_alias.ty)
                    )),
                    documentation: None,
                    deprecated: None,
                    preselect: None,
                    sort_text: Some(format!("2_{}", type_alias.name.name)),
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

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::{Expr, Function as AstFunction, Ident, Module, Param, Type};

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

    #[test]
    fn test_dot_completions_with_struct() {
        use jet_lexer::Span;
        use jet_parser::ast::{FieldDef, Ident, StructDef};

        let mut ast = Module::new(Span::new(0, 0));
        let struct_def = StructDef {
            public: true,
            attributes: Vec::new(),
            name: Ident::new("Person", Span::new(0, 0)),
            generics: vec![],
            fields: vec![
                FieldDef {
                    public: true,
                    name: Ident::new("name", Span::new(0, 0)),
                    ty: jet_parser::ast::Type::Path(jet_parser::ast::Path::single(Ident::new(
                        "string",
                        Span::new(0, 0),
                    ))),
                },
                FieldDef {
                    public: true,
                    name: Ident::new("age", Span::new(0, 0)),
                    ty: jet_parser::ast::Type::Path(jet_parser::ast::Path::single(Ident::new(
                        "int",
                        Span::new(0, 0),
                    ))),
                },
            ],
            span: Span::new(0, 0),
        };
        ast.items.push(ModuleItem::Struct(struct_def));

        let completions = get_dot_completions(
            &ast,
            Position {
                line: 0,
                character: 0,
            },
        );

        // Should have field completions
        assert!(!completions.is_empty());
        let name_field = completions.iter().find(|c| c.label == "name");
        assert!(name_field.is_some());
        assert_eq!(name_field.unwrap().kind, Some(CompletionItemKind::FIELD));
    }

    #[test]
    fn test_colon_completions_with_types() {
        use jet_lexer::Span;
        use jet_parser::ast::{FieldDef, Ident, StructDef};

        let mut ast = Module::new(Span::new(0, 0));
        let struct_def = StructDef {
            public: true,
            attributes: Vec::new(),
            name: Ident::new("Point", Span::new(0, 0)),
            generics: vec![],
            fields: vec![
                FieldDef {
                    public: true,
                    name: Ident::new("x", Span::new(0, 0)),
                    ty: jet_parser::ast::Type::Path(jet_parser::ast::Path::single(Ident::new(
                        "int",
                        Span::new(0, 0),
                    ))),
                },
                FieldDef {
                    public: true,
                    name: Ident::new("y", Span::new(0, 0)),
                    ty: jet_parser::ast::Type::Path(jet_parser::ast::Path::single(Ident::new(
                        "int",
                        Span::new(0, 0),
                    ))),
                },
            ],
            span: Span::new(0, 0),
        };
        ast.items.push(ModuleItem::Struct(struct_def));

        let completions = get_colon_completions(
            &ast,
            Position {
                line: 0,
                character: 0,
            },
        );

        // Should have type completions including the struct constructor
        assert!(!completions.is_empty());
        let constructor = completions.iter().find(|c| c.label == "Point {...}");
        assert!(constructor.is_some());
        assert_eq!(
            constructor.unwrap().kind,
            Some(CompletionItemKind::CONSTRUCTOR)
        );
    }

    #[test]
    fn test_function_snippet_uses_param_names() {
        let func = AstFunction {
            public: false,
            attributes: vec![],
            name: Ident::new("sum", Span::new(0, 0)),
            generics: vec![],
            params: vec![
                Param {
                    pattern: Pattern::Ident {
                        mutable: false,
                        name: Ident::new("left", Span::new(0, 0)),
                    },
                    ty: Type::Path(jet_parser::ast::Path::single(Ident::new(
                        "int",
                        Span::new(0, 0),
                    ))),
                },
                Param {
                    pattern: Pattern::Ident {
                        mutable: false,
                        name: Ident::new("right", Span::new(0, 0)),
                    },
                    ty: Type::Path(jet_parser::ast::Path::single(Ident::new(
                        "int",
                        Span::new(0, 0),
                    ))),
                },
            ],
            return_type: None,
            effects: vec![],
            where_clause: vec![],
            contract: None,
            body: Expr::Pass,
            span: Span::new(0, 0),
        };
        assert_eq!(format_function_snippet(&func), "sum(${1:left}, ${2:right})");
    }

    #[test]
    fn test_method_snippet_skips_self_and_uses_name() {
        let params = vec![
            Param {
                pattern: Pattern::Ident {
                    mutable: false,
                    name: Ident::new("self", Span::new(0, 0)),
                },
                ty: Type::Path(jet_parser::ast::Path::single(Ident::new(
                    "Self",
                    Span::new(0, 0),
                ))),
            },
            Param {
                pattern: Pattern::Ident {
                    mutable: false,
                    name: Ident::new("value", Span::new(0, 0)),
                },
                ty: Type::Path(jet_parser::ast::Path::single(Ident::new(
                    "int",
                    Span::new(0, 0),
                ))),
            },
        ];
        assert_eq!(
            format_trait_method_snippet("set", &params),
            "set(${1:value})"
        );
    }
}
