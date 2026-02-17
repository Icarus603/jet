//! Semantic tokens for syntax highlighting in the Jet LSP server
//!
//! This module provides semantic token analysis for rich syntax highlighting.
//! It maps Jet AST nodes to LSP semantic token types and modifiers.

use jet_lexer::{Span, SpannedToken, Token};
use jet_parser::ast::{
    Block, ConstDef, EffectDef, EnumDef, EnumVariant, Expr, FieldDef, FieldInit, Function, GenericParam,
    HandlerArm, Ident, ImplDef, ImplItem, Import, ImportItem, MatchArm, Module, ModuleItem,
    OperationDef, Param, Path, Pattern, Stmt, StructDef, TraitDef, TraitItem, Type, TypeAlias,
    VariantBody, WhereBound,
};
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenModifier, SemanticTokenType};

/// Token type indices (must match the order in get_semantic_token_types)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
#[allow(dead_code)]
pub enum TokenType {
    Namespace = 0,
    Type = 1,
    Class = 2,
    Enum = 3,
    Interface = 4,
    Struct = 5,
    TypeParameter = 6,
    Parameter = 7,
    Variable = 8,
    Property = 9,
    EnumMember = 10,
    Function = 11,
    Method = 12,
    Keyword = 13,
    Modifier = 14,
    Comment = 15,
    String = 16,
    Number = 17,
    Operator = 18,
}

/// Token modifier indices (must match the order in get_semantic_token_modifiers)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
#[allow(dead_code)]
pub enum TokenModifier {
    Declaration = 0,
    Definition = 1,
    Readonly = 2,
    Static = 3,
    Abstract = 4,
    Deprecated = 5,
    Async = 6,
    Modification = 7,
}

impl TokenModifier {
    #[allow(dead_code)]
    fn to_bitset(self) -> u32 {
        1 << (self as u32)
    }
}

/// Line information for a source position
#[derive(Debug)]
struct LineInfo {
    line: u32,
    character: u32,
}

/// Compute line and character from a byte offset
fn offset_to_position(source: &str, offset: usize) -> LineInfo {
    let mut line = 0u32;
    let mut last_line_start = 0usize;

    for (i, ch) in source.char_indices() {
        if i >= offset {
            return LineInfo {
                line,
                character: (i - last_line_start) as u32,
            };
        }
        if ch == '\n' {
            line += 1;
            last_line_start = i + 1;
        }
    }

    LineInfo {
        line,
        character: (source.len().saturating_sub(last_line_start)) as u32,
    }
}

/// A builder for semantic tokens
pub struct SemanticTokensBuilder {
    tokens: Vec<SemanticToken>,
    prev_line: u32,
    prev_char: u32,
}

impl SemanticTokensBuilder {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            prev_line: 0,
            prev_char: 0,
        }
    }

    /// Add a token
    pub fn push(&mut self, line: u32, character: u32, length: u32, token_type: u32, modifier: u32) {
        let delta_line = line - self.prev_line;
        let delta_start = if delta_line == 0 {
            character - self.prev_char
        } else {
            character
        };

        self.tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: modifier,
        });

        self.prev_line = line;
        self.prev_char = character;
    }

    /// Build the final token list
    pub fn build(self) -> Vec<SemanticToken> {
        self.tokens
    }
}

impl Default for SemanticTokensBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Compute semantic tokens for a module
pub fn compute_semantic_tokens(
    source: &str,
    module: &Module,
    tokens: &[SpannedToken],
) -> Vec<SemanticToken> {
    let mut builder = SemanticTokensBuilder::new();

    // First pass: token-based highlighting (keywords, literals, operators)
    for token in tokens {
        if let Some((token_type, modifier)) = classify_token(&token.token) {
            let start_pos = offset_to_position(source, token.span.start);
            let length = (token.span.end - token.span.start) as u32;

            builder.push(
                start_pos.line,
                start_pos.character,
                length,
                token_type,
                modifier,
            );
        }
    }

    // Second pass: AST-based highlighting (identifiers with semantic meaning)
    let mut visitor = SemanticTokenVisitor::new(source, &mut builder);
    visitor.visit_module(module);

    builder.build()
}

/// Compute semantic tokens for a specific range
pub fn compute_semantic_tokens_range(
    source: &str,
    module: &Module,
    tokens: &[SpannedToken],
    start_line: u32,
    end_line: u32,
) -> Vec<SemanticToken> {
    let mut builder = SemanticTokensBuilder::new();

    for token in tokens {
        let start_pos = offset_to_position(source, token.span.start);

        // Skip tokens outside the requested range
        if start_pos.line < start_line || start_pos.line > end_line {
            continue;
        }

        if let Some((token_type, modifier)) = classify_token(&token.token) {
            let length = (token.span.end - token.span.start) as u32;

            builder.push(
                start_pos.line,
                start_pos.character,
                length,
                token_type,
                modifier,
            );
        }
    }

    // Second pass: AST-based highlighting for identifiers in range
    let mut visitor =
        SemanticTokenVisitor::new_with_range(source, &mut builder, start_line, end_line);
    visitor.visit_module(module);

    builder.build()
}

/// Classify a lexer token
fn classify_token(token: &Token) -> Option<(u32, u32)> {
    match token {
        // Keywords
        Token::Fn
        | Token::Let
        | Token::If
        | Token::Else
        | Token::Elif
        | Token::Match
        | Token::Return
        | Token::Struct
        | Token::Enum
        | Token::Trait
        | Token::Impl
        | Token::Import
        | Token::From
        | Token::Pub
        | Token::Mut
        | Token::Async
        | Token::Await
        | Token::Spawn
        | Token::Concurrent
        | Token::For
        | Token::While
        | Token::Loop
        | Token::Break
        | Token::Continue
        | Token::In
        | Token::And
        | Token::Or
        | Token::Not
        | Token::True
        | Token::False
        | Token::Unit
        | Token::Type
        | Token::Mod
        | Token::Use
        | Token::Where
        | Token::Chan
        | Token::Raise
        | Token::Handle
        | Token::Resume
        | Token::Effect
        | Token::Self_
        | Token::Pass
        | Token::Const
        | Token::As
        | Token::With => Some((TokenType::Keyword as u32, 0)),

        // Literals
        Token::Integer(_) | Token::Float(_) => Some((TokenType::Number as u32, 0)),
        Token::String(_) => Some((TokenType::String as u32, 0)),
        Token::Char(_) => Some((TokenType::String as u32, 0)),
        Token::Bool(_) => Some((TokenType::Keyword as u32, 0)), // true/false as keywords

        // Operators - using correct Token variant names from jet_lexer
        Token::Plus
        | Token::Minus
        | Token::Star
        | Token::Slash
        | Token::Percent
        | Token::Power
        | Token::Eq
        | Token::Ne
        | Token::Lt
        | Token::Gt
        | Token::Le
        | Token::Ge
        | Token::AndBit
        | Token::OrBit
        | Token::Xor
        | Token::NotBit
        | Token::Shl
        | Token::Shr
        | Token::Arrow
        | Token::FatArrow
        | Token::Colon
        | Token::ColonColon
        | Token::Dot
        | Token::DotDot
        | Token::DotDotDot
        | Token::Comma
        | Token::Semi
        | Token::Question
        | Token::At
        | Token::Underscore
        | Token::Bang
        | Token::Assign
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::StarAssign
        | Token::SlashAssign
        | Token::PercentAssign
        | Token::AndAssign
        | Token::OrAssign
        | Token::XorAssign
        | Token::ShlAssign
        | Token::ShrAssign
        | Token::LParen
        | Token::RParen
        | Token::LBracket
        | Token::RBracket
        | Token::LBrace
        | Token::RBrace => Some((TokenType::Operator as u32, 0)),

        // Identifiers - we'll handle these via AST
        Token::Ident(_) => None,
        Token::Label(_) => None,

        // Other tokens - skip
        Token::Eof => None,
        Token::Error(_) => None,
        Token::Newline => None,
        Token::Indent => None,
        Token::Dedent => None,
        Token::DocComment(_) => None,
    }
}

/// AST visitor for semantic token generation
struct SemanticTokenVisitor<'a> {
    source: &'a str,
    builder: &'a mut SemanticTokensBuilder,
    start_line: Option<u32>,
    end_line: Option<u32>,
}

impl<'a> SemanticTokenVisitor<'a> {
    fn new(source: &'a str, builder: &'a mut SemanticTokensBuilder) -> Self {
        Self {
            source,
            builder,
            start_line: None,
            end_line: None,
        }
    }

    fn new_with_range(
        source: &'a str,
        builder: &'a mut SemanticTokensBuilder,
        start_line: u32,
        end_line: u32,
    ) -> Self {
        Self {
            source,
            builder,
            start_line: Some(start_line),
            end_line: Some(end_line),
        }
    }

    /// Check if a span is within the requested range (if any)
    fn is_in_range(&self, span: Span) -> bool {
        if self.start_line.is_none() {
            return true;
        }

        let start_pos = offset_to_position(self.source, span.start);
        let start_line = self.start_line.unwrap();
        let end_line = self.end_line.unwrap();

        start_pos.line >= start_line && start_pos.line <= end_line
    }

    /// Push a semantic token for an identifier
    fn push_ident(&mut self, ident: &Ident, token_type: TokenType, modifier: u32) {
        if !self.is_in_range(ident.span) {
            return;
        }

        let pos = offset_to_position(self.source, ident.span.start);
        let length = (ident.span.end - ident.span.start) as u32;

        self.builder.push(
            pos.line,
            pos.character,
            length,
            token_type as u32,
            modifier,
        );
    }

    fn visit_module(&mut self, module: &Module) {
        for item in &module.items {
            self.visit_module_item(item);
        }
    }

    fn visit_module_item(&mut self, item: &ModuleItem) {
        match item {
            ModuleItem::Function(func) => self.visit_function(func),
            ModuleItem::Struct(s) => self.visit_struct_def(s),
            ModuleItem::Enum(e) => self.visit_enum_def(e),
            ModuleItem::Trait(t) => self.visit_trait_def(t),
            ModuleItem::Impl(i) => self.visit_impl_def(i),
            ModuleItem::TypeAlias(t) => self.visit_type_alias(t),
            ModuleItem::Const(c) => self.visit_const_def(c),
            ModuleItem::Effect(e) => self.visit_effect_def(e),
            ModuleItem::Import(i) => self.visit_import(i),
        }
    }

    fn visit_function(&mut self, func: &Function) {
        // Function name is a declaration
        let modifier = TokenModifier::Declaration.to_bitset();
        self.push_ident(&func.name, TokenType::Function, modifier);

        // Visit generic parameters
        for generic in &func.generics {
            self.visit_generic_param(generic);
        }

        // Visit parameters
        for param in &func.params {
            self.visit_param(param);
        }

        // Visit return type
        if let Some(ret_type) = &func.return_type {
            self.visit_type(ret_type);
        }

        // Visit effects
        for effect in &func.effects {
            self.visit_type(effect);
        }

        // Visit where clause
        for bound in &func.where_clause {
            self.visit_where_bound(bound);
        }

        // Visit body
        self.visit_expr(&func.body);
    }

    fn visit_struct_def(&mut self, s: &StructDef) {
        // Struct name is a declaration
        let modifier = TokenModifier::Declaration.to_bitset();
        self.push_ident(&s.name, TokenType::Struct, modifier);

        // Visit generic parameters
        for generic in &s.generics {
            self.visit_generic_param(generic);
        }

        // Visit fields
        for field in &s.fields {
            self.visit_field_def(field);
        }
    }

    fn visit_enum_def(&mut self, e: &EnumDef) {
        // Enum name is a declaration
        let modifier = TokenModifier::Declaration.to_bitset();
        self.push_ident(&e.name, TokenType::Enum, modifier);

        // Visit generic parameters
        for generic in &e.generics {
            self.visit_generic_param(generic);
        }

        // Visit variants
        for variant in &e.variants {
            self.visit_enum_variant(variant);
        }
    }

    fn visit_enum_variant(&mut self, variant: &EnumVariant) {
        // Variant name is a declaration
        let modifier = TokenModifier::Declaration.to_bitset();
        self.push_ident(&variant.name, TokenType::EnumMember, modifier);

        // Visit variant body types
        match &variant.body {
            VariantBody::Unit => {}
            VariantBody::Tuple(types) => {
                for ty in types {
                    self.visit_type(ty);
                }
            }
            VariantBody::Struct(fields) => {
                for field in fields {
                    self.visit_field_def(field);
                }
            }
            VariantBody::Discriminant(expr) => {
                self.visit_expr(expr);
            }
        }
    }

    fn visit_trait_def(&mut self, t: &TraitDef) {
        // Trait name is a declaration (mapped to Interface)
        let modifier = TokenModifier::Declaration.to_bitset();
        self.push_ident(&t.name, TokenType::Interface, modifier);

        // Visit generic parameters
        for generic in &t.generics {
            self.visit_generic_param(generic);
        }

        // Visit super traits
        for super_trait in &t.super_traits {
            self.visit_type(super_trait);
        }

        // Visit trait items
        for item in &t.items {
            match item {
                TraitItem::Method {
                    name,
                    generics,
                    params,
                    return_type,
                    effects,
                    where_clause,
                } => {
                    // Method declaration
                    let modifier = TokenModifier::Declaration.to_bitset();
                    self.push_ident(name, TokenType::Method, modifier);

                    for generic in generics {
                        self.visit_generic_param(generic);
                    }

                    for param in params {
                        self.visit_param(param);
                    }

                    if let Some(ret_type) = return_type {
                        self.visit_type(ret_type);
                    }

                    for effect in effects {
                        self.visit_type(effect);
                    }

                    for bound in where_clause {
                        self.visit_where_bound(bound);
                    }
                }
                TraitItem::TypeDecl {
                    name,
                    generics,
                    bounds,
                } => {
                    // Type declaration
                    let modifier = TokenModifier::Declaration.to_bitset();
                    self.push_ident(name, TokenType::TypeParameter, modifier);

                    for generic in generics {
                        self.visit_generic_param(generic);
                    }

                    for bound in bounds {
                        self.visit_type(bound);
                    }
                }
                TraitItem::ConstDecl { name, ty } => {
                    // Const declaration
                    let modifier = TokenModifier::Declaration.to_bitset()
                        | TokenModifier::Readonly.to_bitset();
                    self.push_ident(name, TokenType::Variable, modifier);
                    self.visit_type(ty);
                }
            }
        }
    }

    fn visit_impl_def(&mut self, i: &ImplDef) {
        // Visit generic parameters
        for generic in &i.generics {
            self.visit_generic_param(generic);
        }

        // Visit trait path if this is a trait impl
        if let Some(trait_path) = &i.trait_path {
            self.visit_path(trait_path, TokenType::Interface);
        }

        // Visit implemented type
        self.visit_type(&i.ty);

        // Visit where clause
        for bound in &i.where_clause {
            self.visit_where_bound(bound);
        }

        // Visit impl items
        for item in &i.items {
            match item {
                ImplItem::Method(func) => {
                    // Method implementation
                    let modifier = TokenModifier::Definition.to_bitset();
                    self.push_ident(&func.name, TokenType::Method, modifier);

                    for generic in &func.generics {
                        self.visit_generic_param(generic);
                    }

                    for param in &func.params {
                        self.visit_param(param);
                    }

                    if let Some(ret_type) = &func.return_type {
                        self.visit_type(ret_type);
                    }

                    for effect in &func.effects {
                        self.visit_type(effect);
                    }

                    for bound in &func.where_clause {
                        self.visit_where_bound(bound);
                    }

                    self.visit_expr(&func.body);
                }
                ImplItem::Const { name, ty, value } => {
                    let modifier = TokenModifier::Definition.to_bitset()
                        | TokenModifier::Readonly.to_bitset();
                    self.push_ident(name, TokenType::Variable, modifier);
                    self.visit_type(ty);
                    self.visit_expr(value);
                }
                ImplItem::TypeAlias(ta) => {
                    let modifier = TokenModifier::Definition.to_bitset();
                    self.push_ident(&ta.name, TokenType::Type, modifier);

                    for generic in &ta.generics {
                        self.visit_generic_param(generic);
                    }

                    self.visit_type(&ta.ty);
                }
            }
        }
    }

    fn visit_type_alias(&mut self, t: &TypeAlias) {
        let modifier = TokenModifier::Declaration.to_bitset();
        self.push_ident(&t.name, TokenType::Type, modifier);

        for generic in &t.generics {
            self.visit_generic_param(generic);
        }

        self.visit_type(&t.ty);
    }

    fn visit_const_def(&mut self, c: &ConstDef) {
        let modifier = TokenModifier::Declaration.to_bitset()
            | TokenModifier::Readonly.to_bitset();
        self.push_ident(&c.name, TokenType::Variable, modifier);
        self.visit_type(&c.ty);
        self.visit_expr(&c.value);
    }

    fn visit_effect_def(&mut self, e: &EffectDef) {
        let modifier = TokenModifier::Declaration.to_bitset();
        self.push_ident(&e.name, TokenType::Type, modifier);

        for generic in &e.generics {
            self.visit_generic_param(generic);
        }

        for op in &e.operations {
            self.visit_operation_def(op);
        }
    }

    fn visit_operation_def(&mut self, op: &OperationDef) {
        let modifier = TokenModifier::Declaration.to_bitset();
        self.push_ident(&op.name, TokenType::Method, modifier);

        for param in &op.params {
            self.visit_param(param);
        }

        if let Some(ret_type) = &op.return_type {
            self.visit_type(ret_type);
        }
    }

    fn visit_import(&mut self, import: &Import) {
        match import {
            Import::Simple { path, alias } => {
                self.visit_path(path, TokenType::Namespace);
                if let Some(alias) = alias {
                    let modifier = TokenModifier::Declaration.to_bitset();
                    self.push_ident(alias, TokenType::Namespace, modifier);
                }
            }
            Import::From { path, items } => {
                self.visit_path(path, TokenType::Namespace);
                for item in items {
                    self.visit_import_item(item);
                }
            }
        }
    }

    fn visit_import_item(&mut self, item: &ImportItem) {
        match item {
            ImportItem::Single { name, alias } => {
                // The imported name itself
                self.push_ident(name, TokenType::Variable, 0);
                if let Some(alias) = alias {
                    let modifier = TokenModifier::Declaration.to_bitset();
                    self.push_ident(alias, TokenType::Variable, modifier);
                }
            }
            ImportItem::Group(items) => {
                for item in items {
                    self.visit_import_item(item);
                }
            }
        }
    }

    fn visit_generic_param(&mut self, generic: &GenericParam) {
        let modifier = TokenModifier::Declaration.to_bitset();
        self.push_ident(&generic.name, TokenType::TypeParameter, modifier);

        for bound in &generic.bounds {
            self.visit_type(bound);
        }
    }

    fn visit_where_bound(&mut self, bound: &WhereBound) {
        self.visit_type(&bound.ty);
        for b in &bound.bounds {
            self.visit_type(b);
        }
    }

    fn visit_field_def(&mut self, field: &FieldDef) {
        let modifier = if field.public {
            TokenModifier::Declaration.to_bitset()
        } else {
            TokenModifier::Declaration.to_bitset() | TokenModifier::Readonly.to_bitset()
        };
        self.push_ident(&field.name, TokenType::Property, modifier);
        self.visit_type(&field.ty);
    }

    fn visit_param(&mut self, param: &Param) {
        self.visit_pattern(&param.pattern);
        self.visit_type(&param.ty);
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard(_) => {}
            Pattern::Ident { mutable, name } => {
                let modifier = if *mutable {
                    TokenModifier::Declaration.to_bitset()
                } else {
                    TokenModifier::Declaration.to_bitset() | TokenModifier::Readonly.to_bitset()
                };
                self.push_ident(name, TokenType::Variable, modifier);
            }
            Pattern::Literal(_) => {}
            Pattern::Tuple(patterns) => {
                for p in patterns {
                    self.visit_pattern(p);
                }
            }
            Pattern::Struct { path, fields, .. } => {
                self.visit_path(path, TokenType::Struct);
                for field in fields {
                    self.push_ident(&field.name, TokenType::Property, 0);
                    if let Some(pat) = &field.pattern {
                        self.visit_pattern(pat);
                    }
                }
            }
            Pattern::Enum { path, variant, inner } => {
                self.visit_path(path, TokenType::Enum);
                self.push_ident(variant, TokenType::EnumMember, 0);
                if let Some(inner) = inner {
                    self.visit_pattern(inner);
                }
            }
            Pattern::Array(patterns) => {
                for p in patterns {
                    self.visit_pattern(p);
                }
            }
            Pattern::Rest(ident) => {
                if let Some(ident) = ident {
                    self.push_ident(ident, TokenType::Variable, TokenModifier::Declaration.to_bitset());
                }
            }
            Pattern::Or(left, right) => {
                self.visit_pattern(left);
                self.visit_pattern(right);
            }
            Pattern::Bind { name, pattern } => {
                let modifier = TokenModifier::Declaration.to_bitset();
                self.push_ident(name, TokenType::Variable, modifier);
                self.visit_pattern(pattern);
            }
            Pattern::Mut(pattern) => {
                self.visit_pattern(pattern);
            }
            Pattern::Ref { pattern, .. } => {
                self.visit_pattern(pattern);
            }
        }
    }

    fn visit_type(&mut self, ty: &Type) {
        match ty {
            Type::Path(path) => {
                self.visit_path(path, TokenType::Type);
            }
            Type::Generic(base, args) => {
                self.visit_type(base);
                for arg in args {
                    self.visit_type(arg);
                }
            }
            Type::Tuple(types) => {
                for t in types {
                    self.visit_type(t);
                }
            }
            Type::Array(inner, size) => {
                self.visit_type(inner);
                if let Some(size) = size {
                    self.visit_expr(size);
                }
            }
            Type::Function { params, return_type, effects } => {
                for param in params {
                    self.visit_type(param);
                }
                if let Some(ret) = return_type {
                    self.visit_type(ret);
                }
                for effect in effects {
                    self.visit_type(effect);
                }
            }
            Type::Reference { inner, .. } => {
                self.visit_type(inner);
            }
            Type::Channel(inner) => {
                self.visit_type(inner);
            }
            Type::Async(inner) => {
                self.visit_type(inner);
            }
            Type::Infer | Type::SelfType => {}
        }
    }

    fn visit_path(&mut self, path: &Path, token_type: TokenType) {
        // Visit all segments of the path
        for segment in &path.segments {
            self.push_ident(segment, token_type, 0);
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(_) => {}
            Expr::Variable(ident) => {
                self.push_ident(ident, TokenType::Variable, 0);
            }
            Expr::Path(path) => {
                self.visit_path(path, TokenType::Variable);
            }
            Expr::Call { func, args } => {
                self.visit_expr(func);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::MethodCall { receiver, method, args } => {
                self.visit_expr(receiver);
                self.push_ident(method, TokenType::Method, 0);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::FieldAccess { object, field } => {
                self.visit_expr(object);
                self.push_ident(field, TokenType::Property, 0);
            }
            Expr::Index { object, index } => {
                self.visit_expr(object);
                self.visit_expr(index);
            }
            Expr::Unary { expr, .. } => {
                self.visit_expr(expr);
            }
            Expr::Binary { left, right, .. } => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            Expr::Block(block) => {
                self.visit_block(block);
            }
            Expr::If { cond, then_branch, else_branch } => {
                self.visit_expr(cond);
                self.visit_expr(then_branch);
                if let Some(else_branch) = else_branch {
                    self.visit_expr(else_branch);
                }
            }
            Expr::Match { expr, arms } => {
                self.visit_expr(expr);
                for arm in arms {
                    self.visit_match_arm(arm);
                }
            }
            Expr::While { cond, body, .. } => {
                self.visit_expr(cond);
                self.visit_expr(body);
            }
            Expr::For { pattern, iterable, body, .. } => {
                self.visit_pattern(pattern);
                self.visit_expr(iterable);
                self.visit_expr(body);
            }
            Expr::Loop { body, .. } => {
                self.visit_expr(body);
            }
            Expr::Lambda { params, return_type, effects, body } => {
                for param in params {
                    self.visit_param(param);
                }
                if let Some(ret) = return_type {
                    self.visit_type(ret);
                }
                for effect in effects {
                    self.visit_type(effect);
                }
                self.visit_expr(body);
            }
            Expr::Await(expr) => {
                self.visit_expr(expr);
            }
            Expr::Try(expr) => {
                self.visit_expr(expr);
            }
            Expr::Assign { target, value, .. } => {
                self.visit_expr(target);
                self.visit_expr(value);
            }
            Expr::Break { value, .. } => {
                if let Some(value) = value {
                    self.visit_expr(value);
                }
            }
            Expr::Continue { .. } => {}
            Expr::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
            }
            Expr::Tuple(exprs) => {
                for expr in exprs {
                    self.visit_expr(expr);
                }
            }
            Expr::Array(exprs) => {
                for expr in exprs {
                    self.visit_expr(expr);
                }
            }
            Expr::StructLiteral { path, fields } => {
                self.visit_path(path, TokenType::Struct);
                for field in fields {
                    self.visit_field_init(field);
                }
            }
            Expr::Spawn(expr) => {
                self.visit_expr(expr);
            }
            Expr::Async(block) => {
                self.visit_block(block);
            }
            Expr::Concurrent(block) => {
                self.visit_block(block);
            }
            Expr::SelfExpr(_) => {
                // Self is a keyword, handled by lexer
            }
            Expr::Pass => {}
            Expr::Raise(raise) => {
                if let Some(effect) = &raise.effect {
                    self.visit_path(effect, TokenType::Type);
                }
                self.push_ident(&raise.operation, TokenType::Method, 0);
                for arg in &raise.args {
                    self.visit_expr(arg);
                }
            }
            Expr::Handle(handle) => {
                self.visit_expr(&handle.body);
                for handler in &handle.handlers {
                    self.visit_handler_arm(handler);
                }
            }
            Expr::Resume(resume) => {
                if let Some(value) = &resume.value {
                    self.visit_expr(value);
                }
            }
        }
    }

    fn visit_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
        if let Some(expr) = &block.expr {
            self.visit_expr(expr);
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { pattern, ty, value } => {
                self.visit_pattern(pattern);
                if let Some(ty) = ty {
                    self.visit_type(ty);
                }
                self.visit_expr(value);
            }
            Stmt::Expr(expr) => {
                self.visit_expr(expr);
            }
            Stmt::Assign { target, value, .. } => {
                self.visit_expr(target);
                self.visit_expr(value);
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
            }
            Stmt::Break { value, .. } => {
                if let Some(value) = value {
                    self.visit_expr(value);
                }
            }
            Stmt::Continue { .. } => {}
            Stmt::Handle { body, handlers } => {
                self.visit_expr(body);
                for handler in handlers {
                    self.visit_handler_arm(handler);
                }
            }
        }
    }

    fn visit_match_arm(&mut self, arm: &MatchArm) {
        self.visit_pattern(&arm.pattern);
        if let Some(guard) = &arm.guard {
            self.visit_expr(guard);
        }
        self.visit_expr(&arm.body);
    }

    fn visit_handler_arm(&mut self, arm: &HandlerArm) {
        self.push_ident(&arm.operation, TokenType::Method, 0);
        for param in &arm.params {
            self.visit_pattern(param);
        }
        if let Some(resume) = &arm.resume_name {
            let modifier = TokenModifier::Declaration.to_bitset();
            self.push_ident(resume, TokenType::Function, modifier);
        }
        self.visit_expr(&arm.body);
    }

    fn visit_field_init(&mut self, field: &FieldInit) {
        self.push_ident(&field.name, TokenType::Property, 0);
        if let Some(value) = &field.value {
            self.visit_expr(value);
        }
    }
}

/// Get semantic token types for LSP server capabilities
pub fn get_semantic_token_types() -> Vec<SemanticTokenType> {
    vec![
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::TYPE,
        SemanticTokenType::CLASS,
        SemanticTokenType::ENUM,
        SemanticTokenType::INTERFACE,
        SemanticTokenType::STRUCT,
        SemanticTokenType::TYPE_PARAMETER,
        SemanticTokenType::PARAMETER,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::METHOD,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::MODIFIER,
        SemanticTokenType::COMMENT,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::OPERATOR,
    ]
}

/// Get semantic token modifiers for LSP server capabilities
pub fn get_semantic_token_modifiers() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DECLARATION,
        SemanticTokenModifier::DEFINITION,
        SemanticTokenModifier::READONLY,
        SemanticTokenModifier::STATIC,
        SemanticTokenModifier::ABSTRACT,
        SemanticTokenModifier::DEPRECATED,
        SemanticTokenModifier::ASYNC,
        SemanticTokenModifier::MODIFICATION,
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position() {
        let source = "line1\nline2\nline3";

        // Start of file
        let pos = offset_to_position(source, 0);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);

        // After "line1\n" (position 6)
        let pos = offset_to_position(source, 6);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);

        // Middle of line 2
        let pos = offset_to_position(source, 8);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 2);
    }

    #[test]
    fn test_semantic_tokens_builder() {
        let mut builder = SemanticTokensBuilder::new();

        // Add tokens at increasing positions
        builder.push(0, 0, 2, TokenType::Keyword as u32, 0); // "fn"
        builder.push(
            0,
            3,
            4,
            TokenType::Function as u32,
            TokenModifier::Declaration.to_bitset(),
        ); // "main"
        builder.push(1, 4, 5, TokenType::Keyword as u32, 0); // "let"

        let tokens = builder.build();
        assert_eq!(tokens.len(), 3);

        // Check first token
        assert_eq!(tokens[0].delta_line, 0);
        assert_eq!(tokens[0].delta_start, 0);
        assert_eq!(tokens[0].length, 2);

        // Check second token (same line)
        assert_eq!(tokens[1].delta_line, 0);
        assert_eq!(tokens[1].delta_start, 3);
        assert_eq!(tokens[1].length, 4);

        // Check third token (new line)
        assert_eq!(tokens[2].delta_line, 1);
        assert_eq!(tokens[2].delta_start, 4);
    }

    #[test]
    fn test_classify_token() {
        assert!(classify_token(&Token::Fn).is_some());
        assert!(classify_token(&Token::Let).is_some());
        assert!(classify_token(&Token::Integer("42".to_string())).is_some());
        assert!(classify_token(&Token::String("hello".to_string())).is_some());
        assert!(classify_token(&Token::Plus).is_some());
        assert!(classify_token(&Token::Ident("foo".to_string())).is_none());
    }
}
