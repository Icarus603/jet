//! LSP method handlers for the Jet language server
//!
//! This module implements the core LSP functionality including hover,
//! goto definition, find references, and document symbols.

use crate::document::Document;
use jet_parser::ast::{
    Expr, Function, Module, ModuleItem, Pattern, Stmt, StructDef, Type, VariantBody,
};
use tower_lsp::lsp_types::{DocumentSymbol, Location, Position, Range, SymbolKind};

/// Context for LSP operations that need position conversion
pub struct HandlerContext<'a> {
    pub doc: &'a Document,
}

impl<'a> HandlerContext<'a> {
    pub fn new(doc: &'a Document) -> Self {
        Self { doc }
    }

    /// Convert a byte span to an LSP Range using the document's text
    pub fn span_to_range(&self, span: jet_lexer::Span) -> Range {
        Range {
            start: self.doc.offset_to_position(span.start),
            end: self.doc.offset_to_position(span.end),
        }
    }

    /// Get the word at a position
    pub fn word_at_position(&self, position: Position) -> Option<String> {
        self.doc.word_at_position(position)
    }
}

/// Find the definition of a symbol at a position
pub fn find_definition(doc: &Document, ast: &Module, position: Position) -> Option<Location> {
    let ctx = HandlerContext::new(doc);
    let word = ctx.word_at_position(position)?;

    // Search through module items for the definition
    for item in &ast.items {
        match item {
            ModuleItem::Function(func) => {
                if func.name.name == word {
                    return Some(Location {
                        uri: doc.uri.clone(),
                        range: ctx.span_to_range(func.name.span),
                    });
                }
                // Check if the position is within the function body
                if is_position_in_function(func, position, &ctx) {
                    return find_definition_in_function(doc, func, position, &word, &ctx);
                }
            }
            ModuleItem::Struct(struct_def) => {
                if struct_def.name.name == word {
                    return Some(Location {
                        uri: doc.uri.clone(),
                        range: ctx.span_to_range(struct_def.name.span),
                    });
                }
            }
            ModuleItem::Enum(enum_def) => {
                if enum_def.name.name == word {
                    return Some(Location {
                        uri: doc.uri.clone(),
                        range: ctx.span_to_range(enum_def.span),
                    });
                }
                // Check variants
                for variant in &enum_def.variants {
                    if variant.name.name == word {
                        return Some(Location {
                            uri: doc.uri.clone(),
                            range: ctx.span_to_range(variant.name.span),
                        });
                    }
                }
            }
            ModuleItem::Trait(trait_def) => {
                if trait_def.name.name == word {
                    return Some(Location {
                        uri: doc.uri.clone(),
                        range: ctx.span_to_range(trait_def.name.span),
                    });
                }
            }
            ModuleItem::Const(const_def) => {
                if const_def.name.name == word {
                    return Some(Location {
                        uri: doc.uri.clone(),
                        range: ctx.span_to_range(const_def.name.span),
                    });
                }
            }
            ModuleItem::TypeAlias(type_alias) => {
                if type_alias.name.name == word {
                    return Some(Location {
                        uri: doc.uri.clone(),
                        range: ctx.span_to_range(type_alias.name.span),
                    });
                }
            }
            _ => {}
        }
    }

    None
}

/// Check if a position is within a function
fn is_position_in_function(func: &Function, position: Position, ctx: &HandlerContext) -> bool {
    let func_range = ctx.span_to_range(func.span);
    position.line >= func_range.start.line && position.line <= func_range.end.line
}

/// Find a definition within a function scope
fn find_definition_in_function(
    _doc: &Document,
    func: &Function,
    _position: Position,
    word: &str,
    ctx: &HandlerContext,
) -> Option<Location> {
    // Check parameters
    for param in &func.params {
        if pattern_contains_ident(&param.pattern, word) {
            return Some(Location {
                uri: ctx.doc.uri.clone(),
                range: ctx.span_to_range(param.pattern.span()),
            });
        }
    }

    // Check let bindings in function body
    find_definition_in_expr(&func.body, word, ctx)
}

/// Find definition within an expression
fn find_definition_in_expr(expr: &Expr, word: &str, ctx: &HandlerContext) -> Option<Location> {
    match expr {
        Expr::Block(block) => {
            for stmt in &block.stmts {
                if let Some(name) = stmt_defines_name(stmt, word) {
                    return Some(Location {
                        uri: ctx.doc.uri.clone(),
                        range: ctx.span_to_range(name.span),
                    });
                }
            }
            None
        }
        _ => None,
    }
}

/// Check if a statement defines a name
fn stmt_defines_name(stmt: &Stmt, name: &str) -> Option<jet_parser::ast::Ident> {
    match stmt {
        Stmt::Let { pattern, .. } => pattern_defines_name(pattern, name),
        Stmt::Expr(_) => None,
        Stmt::Assign { .. } => None,
        Stmt::Return(_) => None,
        Stmt::Break { .. } => None,
        Stmt::Continue { .. } => None,
        Stmt::Handle { .. } => None,
    }
}

/// Check if a pattern defines a name
fn pattern_defines_name(pattern: &Pattern, name: &str) -> Option<jet_parser::ast::Ident> {
    match pattern {
        Pattern::Ident { name: ident, .. } if ident.name == name => Some(ident.clone()),
        Pattern::Tuple(patterns) => patterns.iter().find_map(|p| pattern_defines_name(p, name)),
        Pattern::Bind { name: ident, .. } if ident.name == name => Some(ident.clone()),
        _ => None,
    }
}

/// Check if a pattern contains an identifier
fn pattern_contains_ident(pattern: &Pattern, name: &str) -> bool {
    match pattern {
        Pattern::Ident { name: ident, .. } => ident.name == name,
        Pattern::Tuple(patterns) => patterns.iter().any(|p| pattern_contains_ident(p, name)),
        Pattern::Struct { fields, .. } => fields.iter().any(|f| {
            f.pattern
                .as_ref()
                .map(|p| pattern_contains_ident(p, name))
                .unwrap_or(false)
        }),
        Pattern::Array(patterns) => patterns.iter().any(|p| pattern_contains_ident(p, name)),
        Pattern::Bind {
            name: ident,
            pattern,
        } => ident.name == name || pattern_contains_ident(pattern, name),
        Pattern::Mut(inner) => pattern_contains_ident(inner, name),
        Pattern::Ref { pattern, .. } => pattern_contains_ident(pattern, name),
        _ => false,
    }
}

/// Get hover information for a symbol at a position
pub fn get_hover_info(doc: &Document, ast: &Module, position: Position) -> Option<String> {
    let ctx = HandlerContext::new(doc);
    let word = ctx.word_at_position(position)?;

    // Search for the symbol in the module
    for item in &ast.items {
        match item {
            ModuleItem::Function(func) => {
                if func.name.name == word {
                    return Some(format_function_hover(func));
                }
            }
            ModuleItem::Struct(struct_def) => {
                if struct_def.name.name == word {
                    return Some(format_struct_hover(struct_def));
                }
            }
            ModuleItem::Enum(enum_def) => {
                if enum_def.name.name == word {
                    return Some(format_enum_hover(enum_def));
                }
                // Check variants
                for variant in &enum_def.variants {
                    if variant.name.name == word {
                        return Some(format_variant_hover(enum_def, variant));
                    }
                }
            }
            ModuleItem::Trait(trait_def) => {
                if trait_def.name.name == word {
                    return Some(format_trait_hover(trait_def));
                }
            }
            ModuleItem::Const(const_def) => {
                if const_def.name.name == word {
                    return Some(format_const_hover(const_def));
                }
            }
            ModuleItem::TypeAlias(type_alias) => {
                if type_alias.name.name == word {
                    return Some(format_type_alias_hover(type_alias));
                }
            }
            _ => {}
        }
    }

    // Check if it's a keyword
    if jet_lexer::is_keyword(&word) {
        return Some(format_keyword_hover(&word));
    }

    // Check if it's a built-in type
    if let Some(hover) = format_builtin_type_hover(&word) {
        return Some(hover);
    }

    None
}

/// Format function information for hover
fn format_function_hover(func: &Function) -> String {
    let mut result = String::new();

    result.push_str("```jet\n");
    result.push_str(&format_function_signature(func));
    result.push_str("\n```");

    if func.public {
        result.push_str("\n\n*Public function*");
    }

    result
}

/// Format struct information for hover
fn format_struct_hover(struct_def: &StructDef) -> String {
    let mut result = String::new();

    result.push_str("```jet\n");
    if struct_def.public {
        result.push_str("pub ");
    }
    result.push_str("struct ");
    result.push_str(&struct_def.name.name);

    // Generic parameters
    if !struct_def.generics.is_empty() {
        result.push('[');
        for (i, param) in struct_def.generics.iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }
            result.push_str(&param.name.name);
        }
        result.push(']');
    }

    // Fields
    result.push_str(":\n");
    for field in &struct_def.fields {
        result.push_str("    ");
        if field.public {
            result.push_str("pub ");
        }
        result.push_str(&field.name.name);
        result.push_str(": ");
        result.push_str(&format_type(&field.ty));
        result.push('\n');
    }

    result.push_str("\n```");
    result
}

/// Format enum information for hover
fn format_enum_hover(enum_def: &jet_parser::ast::EnumDef) -> String {
    let mut result = String::new();

    result.push_str("```jet\n");
    if enum_def.public {
        result.push_str("pub ");
    }
    result.push_str("enum ");
    result.push_str(&enum_def.name.name);

    // Generic parameters
    if !enum_def.generics.is_empty() {
        result.push('[');
        for (i, param) in enum_def.generics.iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }
            result.push_str(&param.name.name);
        }
        result.push(']');
    }

    // Variants
    result.push_str(":\n");
    for variant in &enum_def.variants {
        result.push_str("    | ");
        result.push_str(&variant.name.name);
        match &variant.body {
            VariantBody::Tuple(types) => {
                result.push('(');
                let type_strs: Vec<String> = types.iter().map(format_type).collect();
                result.push_str(&type_strs.join(", "));
                result.push(')');
            }
            VariantBody::Struct(fields) => {
                result.push_str(" { ");
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name.name, format_type(&f.ty)))
                    .collect();
                result.push_str(&field_strs.join(", "));
                result.push_str(" }");
            }
            _ => {}
        }
        result.push('\n');
    }

    result.push_str("\n```");
    result
}

/// Format enum variant information for hover
fn format_variant_hover(
    enum_def: &jet_parser::ast::EnumDef,
    variant: &jet_parser::ast::EnumVariant,
) -> String {
    let mut result = String::new();

    result.push_str("```jet\n");
    result.push_str(&enum_def.name.name);
    result.push_str("::");
    result.push_str(&variant.name.name);

    match &variant.body {
        VariantBody::Tuple(types) => {
            result.push('(');
            let type_strs: Vec<String> = types.iter().map(format_type).collect();
            result.push_str(&type_strs.join(", "));
            result.push(')');
        }
        VariantBody::Struct(fields) => {
            result.push_str(" { ");
            let field_strs: Vec<String> = fields
                .iter()
                .map(|f| format!("{}: {}", f.name.name, format_type(&f.ty)))
                .collect();
            result.push_str(&field_strs.join(", "));
            result.push_str(" }");
        }
        VariantBody::Unit => {}
        VariantBody::Discriminant(_) => {}
    }

    result.push_str("\n```\n\n");
    result.push_str("Variant of enum `");
    result.push_str(&enum_def.name.name);
    result.push('`');

    result
}

/// Format trait information for hover
fn format_trait_hover(trait_def: &jet_parser::ast::TraitDef) -> String {
    let mut result = String::new();

    result.push_str("```jet\n");
    if trait_def.public {
        result.push_str("pub ");
    }
    result.push_str("trait ");
    result.push_str(&trait_def.name.name);

    // Generic parameters
    if !trait_def.generics.is_empty() {
        result.push('[');
        for (i, param) in trait_def.generics.iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }
            result.push_str(&param.name.name);
        }
        result.push(']');
    }

    // Super traits
    if !trait_def.super_traits.is_empty() {
        result.push_str(": ");
        let super_traits: Vec<String> = trait_def.super_traits.iter().map(format_type).collect();
        result.push_str(&super_traits.join(" + "));
    }

    result.push_str("\n```");
    result
}

/// Format const information for hover
fn format_const_hover(const_def: &jet_parser::ast::ConstDef) -> String {
    let mut result = String::new();

    result.push_str("```jet\n");
    if const_def.public {
        result.push_str("pub ");
    }
    result.push_str("let ");
    result.push_str(&const_def.name.name);
    result.push_str(": ");
    result.push_str(&format_type(&const_def.ty));
    result.push_str("\n```");

    result
}

/// Format type alias information for hover
fn format_type_alias_hover(type_alias: &jet_parser::ast::TypeAlias) -> String {
    let mut result = String::new();

    result.push_str("```jet\n");
    if type_alias.public {
        result.push_str("pub ");
    }
    result.push_str("type ");
    result.push_str(&type_alias.name.name);

    // Generic parameters
    if !type_alias.generics.is_empty() {
        result.push('[');
        for (i, param) in type_alias.generics.iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }
            result.push_str(&param.name.name);
        }
        result.push(']');
    }

    result.push_str(" = ");
    result.push_str(&format_type(&type_alias.ty));
    result.push_str("\n```");

    result
}

/// Format keyword hover information
fn format_keyword_hover(keyword: &str) -> String {
    let description = match keyword {
        "fn" => "Define a function",
        "let" => "Define a variable or constant",
        "if" => "Conditional expression",
        "else" => "Else branch of conditional",
        "elif" => "Else-if branch of conditional",
        "match" => "Pattern matching expression",
        "struct" => "Define a struct type",
        "enum" => "Define an enum type",
        "trait" => "Define a trait",
        "impl" => "Implement a trait or inherent methods",
        "import" => "Import a module",
        "return" => "Return from function",
        "pub" => "Make item public",
        "mut" => "Mutable binding",
        "async" => "Async expression or block",
        "await" => "Await an async expression",
        "for" => "For loop",
        "while" => "While loop",
        "loop" => "Infinite loop",
        "break" => "Break from loop",
        "continue" => "Continue to next iteration",
        "spawn" => "Spawn a concurrent task",
        "concurrent" => "Concurrent block",
        "raise" => "Raise an error/effect",
        "chan" => "Channel type",
        "and" => "Logical AND",
        "or" => "Logical OR",
        "not" => "Logical NOT",
        _ => "Jet keyword",
    };

    format!("**`{}`** - {}", keyword, description)
}

/// Format built-in type hover information
fn format_builtin_type_hover(type_name: &str) -> Option<String> {
    let description = match type_name {
        "int" => "Signed integer type (platform-dependent size)",
        "uint" => "Unsigned integer type (platform-dependent size)",
        "float" => "Floating-point type (64-bit)",
        "bool" => "Boolean type (`true` or `false`)",
        "string" => "UTF-8 encoded string type",
        "char" => "Unicode character type",
        "unit" => "Unit type (empty tuple)",
        _ => return None,
    };

    Some(format!("**`{}`** - {}", type_name, description))
}

/// Format a function signature
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
        let effects: Vec<String> = func.effects.iter().map(format_type).collect();
        result.push_str(&effects.join(" | "));
    }

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
fn format_type(ty: &Type) -> String {
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

/// Get document symbols for the outline view
#[allow(deprecated)]
pub fn get_document_symbols(doc: &Document, ast: &Module) -> Vec<DocumentSymbol> {
    let ctx = HandlerContext::new(doc);
    let mut symbols = Vec::new();

    for item in &ast.items {
        match item {
            ModuleItem::Function(func) => {
                symbols.push(DocumentSymbol {
                    name: func.name.name.clone(),
                    detail: Some(format_function_signature(func)),
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    deprecated: None,
                    range: ctx.span_to_range(func.span),
                    selection_range: ctx.span_to_range(func.name.span),
                    children: None,
                });
            }
            ModuleItem::Struct(struct_def) => {
                let children: Vec<DocumentSymbol> = struct_def
                    .fields
                    .iter()
                    .map(|field| DocumentSymbol {
                        name: field.name.name.clone(),
                        detail: Some(format_type(&field.ty)),
                        kind: SymbolKind::FIELD,
                        tags: None,
                        deprecated: None,
                        range: ctx.span_to_range(field.name.span),
                        selection_range: ctx.span_to_range(field.name.span),
                        children: None,
                    })
                    .collect();

                symbols.push(DocumentSymbol {
                    name: struct_def.name.name.clone(),
                    detail: None,
                    kind: SymbolKind::STRUCT,
                    tags: None,
                    deprecated: None,
                    range: ctx.span_to_range(struct_def.span),
                    selection_range: ctx.span_to_range(struct_def.name.span),
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                });
            }
            ModuleItem::Enum(enum_def) => {
                let children: Vec<DocumentSymbol> = enum_def
                    .variants
                    .iter()
                    .map(|variant| DocumentSymbol {
                        name: variant.name.name.clone(),
                        detail: match &variant.body {
                            VariantBody::Tuple(types) => {
                                Some(types.iter().map(format_type).collect::<Vec<_>>().join(", "))
                            }
                            VariantBody::Struct(fields) => Some(
                                fields
                                    .iter()
                                    .map(|f| format_type(&f.ty))
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            ),
                            _ => None,
                        },
                        kind: SymbolKind::ENUM_MEMBER,
                        tags: None,
                        deprecated: None,
                        range: ctx.span_to_range(variant.name.span),
                        selection_range: ctx.span_to_range(variant.name.span),
                        children: None,
                    })
                    .collect();

                symbols.push(DocumentSymbol {
                    name: enum_def.name.name.clone(),
                    detail: None,
                    kind: SymbolKind::ENUM,
                    tags: None,
                    deprecated: None,
                    range: ctx.span_to_range(enum_def.span),
                    selection_range: ctx.span_to_range(enum_def.name.span),
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                });
            }
            ModuleItem::Trait(trait_def) => {
                let children: Vec<DocumentSymbol> = trait_def
                    .items
                    .iter()
                    .filter_map(|item| match item {
                        jet_parser::ast::TraitItem::Method { name, .. } => Some(DocumentSymbol {
                            name: name.name.clone(),
                            detail: None,
                            kind: SymbolKind::METHOD,
                            tags: None,
                            deprecated: None,
                            range: ctx.span_to_range(name.span),
                            selection_range: ctx.span_to_range(name.span),
                            children: None,
                        }),
                        _ => None,
                    })
                    .collect();

                symbols.push(DocumentSymbol {
                    name: trait_def.name.name.clone(),
                    detail: None,
                    kind: SymbolKind::INTERFACE,
                    tags: None,
                    deprecated: None,
                    range: ctx.span_to_range(trait_def.span),
                    selection_range: ctx.span_to_range(trait_def.name.span),
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                });
            }
            ModuleItem::Const(const_def) => {
                symbols.push(DocumentSymbol {
                    name: const_def.name.name.clone(),
                    detail: Some(format_type(&const_def.ty)),
                    kind: SymbolKind::CONSTANT,
                    tags: None,
                    deprecated: None,
                    range: ctx.span_to_range(const_def.span),
                    selection_range: ctx.span_to_range(const_def.name.span),
                    children: None,
                });
            }
            ModuleItem::TypeAlias(type_alias) => {
                symbols.push(DocumentSymbol {
                    name: type_alias.name.name.clone(),
                    detail: Some(format_type(&type_alias.ty)),
                    kind: SymbolKind::TYPE_PARAMETER,
                    tags: None,
                    deprecated: None,
                    range: ctx.span_to_range(type_alias.span),
                    selection_range: ctx.span_to_range(type_alias.name.span),
                    children: None,
                });
            }
            _ => {}
        }
    }

    symbols
}

/// Find all references to a symbol
pub fn find_references(doc: &Document, ast: &Module, position: Position) -> Vec<Location> {
    let ctx = HandlerContext::new(doc);
    let mut locations = Vec::new();

    let Some(word) = ctx.word_at_position(position) else {
        return locations;
    };

    // Search through the AST for references to this symbol
    for item in &ast.items {
        match item {
            ModuleItem::Function(func) => {
                // Check function name
                if func.name.name == word {
                    locations.push(Location {
                        uri: doc.uri.clone(),
                        range: ctx.span_to_range(func.name.span),
                    });
                }
                // Search function body for references
                find_references_in_expr(&func.body, &word, &ctx, &mut locations);
            }
            _ => {
                // Check other items for the name
                if let Some(name) = get_item_name(item) {
                    if name == word {
                        locations.push(Location {
                            uri: doc.uri.clone(),
                            range: ctx.span_to_range(get_item_span(item)),
                        });
                    }
                }
            }
        }
    }

    locations
}

/// Find references within a statement
fn find_references_in_stmt(
    stmt: &Stmt,
    word: &str,
    ctx: &HandlerContext,
    locations: &mut Vec<Location>,
) {
    match stmt {
        Stmt::Expr(expr) => {
            find_references_in_expr(expr, word, ctx, locations);
        }
        Stmt::Let { value, .. } => {
            find_references_in_expr(value, word, ctx, locations);
        }
        Stmt::Assign { target, value, .. } => {
            find_references_in_expr(target, word, ctx, locations);
            find_references_in_expr(value, word, ctx, locations);
        }
        Stmt::Return(expr) => {
            if let Some(e) = expr {
                find_references_in_expr(e, word, ctx, locations);
            }
        }
        Stmt::Break { value, .. } => {
            if let Some(val) = value {
                find_references_in_expr(val, word, ctx, locations);
            }
        }
        Stmt::Continue { .. } => {}
        Stmt::Handle { body, handlers } => {
            find_references_in_expr(body, word, ctx, locations);
            for handler in handlers {
                find_references_in_expr(&handler.body, word, ctx, locations);
            }
        }
    }
}

/// Find references within an expression
fn find_references_in_expr(
    expr: &Expr,
    word: &str,
    ctx: &HandlerContext,
    locations: &mut Vec<Location>,
) {
    match expr {
        Expr::Variable(ident) if ident.name == word => {
            locations.push(Location {
                uri: ctx.doc.uri.clone(),
                range: ctx.span_to_range(ident.span),
            });
        }
        Expr::Path(path) => {
            if let Some(last) = path.segments.last() {
                if last.name == word {
                    locations.push(Location {
                        uri: ctx.doc.uri.clone(),
                        range: ctx.span_to_range(last.span),
                    });
                }
            }
        }
        Expr::Binary { left, right, .. } => {
            find_references_in_expr(left, word, ctx, locations);
            find_references_in_expr(right, word, ctx, locations);
        }
        Expr::Unary { expr: inner, .. } => {
            find_references_in_expr(inner, word, ctx, locations);
        }
        Expr::Call { func, args } => {
            find_references_in_expr(func, word, ctx, locations);
            for arg in args {
                find_references_in_expr(arg, word, ctx, locations);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            find_references_in_expr(receiver, word, ctx, locations);
            for arg in args {
                find_references_in_expr(arg, word, ctx, locations);
            }
        }
        Expr::FieldAccess { object, .. } => {
            find_references_in_expr(object, word, ctx, locations);
        }
        Expr::Index { object, index } => {
            find_references_in_expr(object, word, ctx, locations);
            find_references_in_expr(index, word, ctx, locations);
        }
        Expr::Tuple(exprs) => {
            for e in exprs {
                find_references_in_expr(e, word, ctx, locations);
            }
        }
        Expr::Array(exprs) => {
            for e in exprs {
                find_references_in_expr(e, word, ctx, locations);
            }
        }
        Expr::Block(block) => {
            for stmt in &block.stmts {
                find_references_in_stmt(stmt, word, ctx, locations);
            }
            if let Some(expr) = &block.expr {
                find_references_in_expr(expr, word, ctx, locations);
            }
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            find_references_in_expr(cond, word, ctx, locations);
            find_references_in_expr(then_branch, word, ctx, locations);
            if let Some(else_branch) = else_branch {
                find_references_in_expr(else_branch, word, ctx, locations);
            }
        }
        Expr::Match { expr, arms } => {
            find_references_in_expr(expr, word, ctx, locations);
            for arm in arms {
                find_references_in_expr(&arm.body, word, ctx, locations);
                if let Some(guard) = &arm.guard {
                    find_references_in_expr(guard, word, ctx, locations);
                }
            }
        }
        Expr::While { cond, body, .. } => {
            find_references_in_expr(cond, word, ctx, locations);
            find_references_in_expr(body, word, ctx, locations);
        }
        Expr::For { iterable, body, .. } => {
            find_references_in_expr(iterable, word, ctx, locations);
            find_references_in_expr(body, word, ctx, locations);
        }
        Expr::Loop { body, .. } => {
            find_references_in_expr(body, word, ctx, locations);
        }
        Expr::Assign { target, value, .. } => {
            find_references_in_expr(target, word, ctx, locations);
            find_references_in_expr(value, word, ctx, locations);
        }
        Expr::Return(expr) => {
            if let Some(e) = expr {
                find_references_in_expr(e, word, ctx, locations);
            }
        }
        Expr::Break { value, .. } => {
            if let Some(val) = value {
                find_references_in_expr(val, word, ctx, locations);
            }
        }
        Expr::Continue { .. } => {}
        Expr::Lambda { body, .. } => {
            find_references_in_expr(body, word, ctx, locations);
        }
        Expr::Await(expr) => {
            find_references_in_expr(expr, word, ctx, locations);
        }
        Expr::Try(expr) => {
            find_references_in_expr(expr, word, ctx, locations);
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                if let Some(value) = &field.value {
                    find_references_in_expr(value, word, ctx, locations);
                }
            }
        }
        Expr::Spawn(expr) => {
            find_references_in_expr(expr, word, ctx, locations);
        }
        Expr::Async(block) => {
            for stmt in &block.stmts {
                find_references_in_stmt(stmt, word, ctx, locations);
            }
            if let Some(expr) = &block.expr {
                find_references_in_expr(expr, word, ctx, locations);
            }
        }
        Expr::Concurrent(block) => {
            for stmt in &block.stmts {
                find_references_in_stmt(stmt, word, ctx, locations);
            }
            if let Some(expr) = &block.expr {
                find_references_in_expr(expr, word, ctx, locations);
            }
        }
        Expr::SelfExpr(_) => {}
        Expr::Pass => {}
        Expr::Raise(raise_expr) => {
            for arg in &raise_expr.args {
                find_references_in_expr(arg, word, ctx, locations);
            }
        }
        Expr::Handle(handle_expr) => {
            find_references_in_expr(&handle_expr.body, word, ctx, locations);
            for handler in &handle_expr.handlers {
                find_references_in_expr(&handler.body, word, ctx, locations);
            }
        }
        Expr::Resume(resume_expr) => {
            if let Some(value) = &resume_expr.value {
                find_references_in_expr(value, word, ctx, locations);
            }
        }
        Expr::Literal(_) => {}
        Expr::Variable(ident) if ident.name == word => {
            locations.push(Location {
                uri: ctx.doc.uri.clone(),
                range: ctx.span_to_range(ident.span),
            });
        }
        Expr::Variable(_) => {}
    }
}

/// Get the name of a module item
fn get_item_name(item: &ModuleItem) -> Option<&str> {
    match item {
        ModuleItem::Function(f) => Some(&f.name.name),
        ModuleItem::Struct(s) => Some(&s.name.name),
        ModuleItem::Enum(e) => Some(&e.name.name),
        ModuleItem::Trait(t) => Some(&t.name.name),
        ModuleItem::Const(c) => Some(&c.name.name),
        ModuleItem::TypeAlias(t) => Some(&t.name.name),
        ModuleItem::Effect(e) => Some(&e.name.name),
        _ => None,
    }
}

/// Get the span of a module item
fn get_item_span(item: &ModuleItem) -> jet_lexer::Span {
    match item {
        ModuleItem::Function(f) => f.span,
        ModuleItem::Struct(s) => s.span,
        ModuleItem::Enum(e) => e.span,
        ModuleItem::Trait(t) => t.span,
        ModuleItem::Const(c) => c.span,
        ModuleItem::TypeAlias(t) => t.span,
        ModuleItem::Import(_) => jet_lexer::Span::new(0, 0),
        ModuleItem::Impl(i) => i.span,
        ModuleItem::Effect(e) => e.span,
    }
}

/// Trait to get span from an expression
#[allow(dead_code)]
trait ExprSpan {
    fn span(&self) -> jet_lexer::Span;
}

impl ExprSpan for Expr {
    fn span(&self) -> jet_lexer::Span {
        match self {
            Expr::Literal(_) => jet_lexer::Span::new(0, 0),
            Expr::Variable(ident) => ident.span,
            Expr::Path(path) => path.span,
            Expr::Call { func, args } => {
                let func_span = func.span();
                if let Some(last_arg) = args.last() {
                    jet_lexer::Span::new(func_span.start, last_arg.span().end)
                } else {
                    func_span
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let recv_span = receiver.span();
                if let Some(last_arg) = args.last() {
                    jet_lexer::Span::new(recv_span.start, last_arg.span().end)
                } else {
                    jet_lexer::Span::new(recv_span.start, method.span.end)
                }
            }
            Expr::FieldAccess { object, field } => {
                jet_lexer::Span::new(object.span().start, field.span.end)
            }
            Expr::Index { object, index } => {
                jet_lexer::Span::new(object.span().start, index.span().end)
            }
            Expr::Unary { expr, .. } => expr.span(),
            Expr::Binary { left, right, .. } => {
                jet_lexer::Span::new(left.span().start, right.span().end)
            }
            Expr::Block(block) => block.span,
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let start = cond.span().start;
                let end = if let Some(else_branch) = else_branch {
                    else_branch.span().end
                } else {
                    then_branch.span().end
                };
                jet_lexer::Span::new(start, end)
            }
            Expr::Match { expr, arms } => {
                let start = expr.span().start;
                let end = if let Some(last_arm) = arms.last() {
                    last_arm.body.span().end
                } else {
                    expr.span().end
                };
                jet_lexer::Span::new(start, end)
            }
            Expr::While { cond, body, .. } => {
                jet_lexer::Span::new(cond.span().start, body.span().end)
            }
            Expr::For { iterable, body, .. } => {
                jet_lexer::Span::new(iterable.span().start, body.span().end)
            }
            Expr::Loop { body, .. } => body.span(),
            Expr::Lambda { body, .. } => body.span(),
            Expr::Await(expr) => expr.span(),
            Expr::Try(expr) => expr.span(),
            Expr::Assign { target, value, .. } => {
                jet_lexer::Span::new(target.span().start, value.span().end)
            }
            Expr::Break { value, .. } => {
                if let Some(val) = value {
                    val.span()
                } else {
                    jet_lexer::Span::new(0, 0)
                }
            }
            Expr::Continue { .. } => jet_lexer::Span::new(0, 0),
            Expr::Return(expr) => {
                if let Some(e) = expr {
                    e.span()
                } else {
                    jet_lexer::Span::new(0, 0)
                }
            }
            Expr::Tuple(exprs) => {
                if let (Some(first), Some(last)) = (exprs.first(), exprs.last()) {
                    jet_lexer::Span::new(first.span().start, last.span().end)
                } else {
                    jet_lexer::Span::new(0, 0)
                }
            }
            Expr::Array(exprs) => {
                if let (Some(first), Some(last)) = (exprs.first(), exprs.last()) {
                    jet_lexer::Span::new(first.span().start, last.span().end)
                } else {
                    jet_lexer::Span::new(0, 0)
                }
            }
            Expr::StructLiteral { path, fields } => {
                let start = path.span.start;
                let end = if let Some(last_field) = fields.last() {
                    if let Some(value) = &last_field.value {
                        value.span().end
                    } else {
                        last_field.name.span.end
                    }
                } else {
                    path.span.end
                };
                jet_lexer::Span::new(start, end)
            }
            Expr::Spawn(expr) => expr.span(),
            Expr::Async(block) => block.span,
            Expr::Concurrent(block) => block.span,
            Expr::SelfExpr(span) => *span,
            Expr::Pass => jet_lexer::Span::new(0, 0),
            Expr::Raise(raise_expr) => raise_expr.span,
            Expr::Handle(handle_expr) => handle_expr.span,
            Expr::Resume(resume_expr) => resume_expr.span,
        }
    }
}

#[allow(dead_code)]
trait PatternSpan {
    fn span(&self) -> jet_lexer::Span;
}

impl PatternSpan for Pattern {
    fn span(&self) -> jet_lexer::Span {
        match self {
            Pattern::Ident { name, .. } => name.span,
            Pattern::Wildcard(span) => *span,
            Pattern::Tuple(patterns) => {
                if let (Some(first), Some(last)) = (patterns.first(), patterns.last()) {
                    first.span().merge(&last.span())
                } else {
                    jet_lexer::Span::new(0, 0)
                }
            }
            Pattern::Struct { path, .. } => path.span,
            Pattern::Enum { path, variant, .. } => path.span.merge(&variant.span),
            Pattern::Array(patterns) => {
                if let (Some(first), Some(last)) = (patterns.first(), patterns.last()) {
                    first.span().merge(&last.span())
                } else {
                    jet_lexer::Span::new(0, 0)
                }
            }
            Pattern::Rest(Some(ident)) => ident.span,
            Pattern::Rest(None) => jet_lexer::Span::new(0, 0),
            Pattern::Or(left, right) => left.span().merge(&right.span()),
            Pattern::Bind { name, .. } => name.span,
            Pattern::Mut(inner) => inner.span(),
            Pattern::Ref { pattern, .. } => pattern.span(),
            Pattern::Literal(_) => jet_lexer::Span::new(0, 0),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::Module;
    use tower_lsp::lsp_types::Url;

    #[test]
    fn test_get_document_symbols_empty() {
        let ast = Module::new(Span::new(0, 0));
        // Create a mock document for testing
        let doc = Document::new(Url::parse("file:///test.jet").unwrap(), 1, "".to_string());
        let symbols = get_document_symbols(&doc, &ast);
        assert!(symbols.is_empty());
    }
}
