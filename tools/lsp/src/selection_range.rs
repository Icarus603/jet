//! Selection ranges for the Jet LSP server
//!
//! Provides semantic selection range expansion based on AST structure.

use crate::document::Document;
use jet_parser::ast::{
    Block, EnumDef, EnumVariant, Expr, FieldDef, Function, ImplDef, MatchArm, Module, ModuleItem,
    Param, Pattern, Stmt, StructDef, TraitDef, TypeAlias, VariantBody,
};
use tower_lsp::lsp_types::{Position, Range, SelectionRange};

/// Get selection ranges for given positions
pub fn get_selection_ranges(
    doc: &Document,
    ast: &Module,
    positions: Vec<Position>,
) -> Vec<SelectionRange> {
    positions
        .into_iter()
        .filter_map(|pos| get_selection_range_at_position(doc, ast, pos))
        .collect()
}

/// Get selection range hierarchy at a specific position
fn get_selection_range_at_position(
    doc: &Document,
    ast: &Module,
    position: Position,
) -> Option<SelectionRange> {
    // Build a hierarchy of ranges from innermost to outermost
    let mut ranges: Vec<Range> = Vec::new();

    // Find innermost expression/statement at position
    find_ranges_at_position(doc, ast, position, &mut ranges);

    if ranges.is_empty() {
        return None;
    }

    // Build the linked list of selection ranges
    let mut current: Option<Box<SelectionRange>> = None;

    for range in ranges.into_iter().rev() {
        current = Some(Box::new(SelectionRange {
            range,
            parent: current,
        }));
    }

    current.map(|boxed| *boxed)
}

/// Find all AST node ranges at the given position, from innermost to outermost
fn find_ranges_at_position(
    doc: &Document,
    ast: &Module,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    for item in &ast.items {
        if let Some(range) = get_module_item_range(doc, item) {
            if is_position_in_range(position, range) {
                // Check for more specific ranges within this item
                find_ranges_in_module_item(doc, item, position, ranges);

                // Add the module item range itself
                ranges.push(range);
            }
        }
    }
}

/// Get the range for a module item
fn get_module_item_range(doc: &Document, item: &ModuleItem) -> Option<Range> {
    let span = match item {
        ModuleItem::Function(f) => f.span,
        ModuleItem::Struct(s) => s.span,
        ModuleItem::Enum(e) => e.span,
        ModuleItem::Trait(t) => t.span,
        ModuleItem::Const(c) => c.span,
        ModuleItem::TypeAlias(t) => t.span,
        ModuleItem::Import(_) => return None, // Import is an enum, not a struct with span
        ModuleItem::Impl(i) => i.span,
        ModuleItem::Effect(e) => e.span,
    };
    Some(doc.span_to_range(span))
}

/// Find ranges within a module item
fn find_ranges_in_module_item(
    doc: &Document,
    item: &ModuleItem,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    match item {
        ModuleItem::Function(func) => {
            find_ranges_in_function(doc, func, position, ranges);
        }
        ModuleItem::Struct(struct_def) => {
            find_ranges_in_struct(doc, struct_def, position, ranges);
        }
        ModuleItem::Enum(enum_def) => {
            find_ranges_in_enum(doc, enum_def, position, ranges);
        }
        ModuleItem::Trait(trait_def) => {
            find_ranges_in_trait(doc, trait_def, position, ranges);
        }
        ModuleItem::Const(const_def) => {
            find_ranges_in_const(doc, const_def, position, ranges);
        }
        ModuleItem::TypeAlias(type_alias) => {
            find_ranges_in_type_alias(doc, type_alias, position, ranges);
        }
        ModuleItem::Impl(impl_def) => {
            find_ranges_in_impl(doc, impl_def, position, ranges);
        }
        _ => {}
    }
}

/// Find ranges within a function
fn find_ranges_in_function(
    doc: &Document,
    func: &Function,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    // Function name
    let name_range = doc.span_to_range(func.name.span);
    if is_position_in_range(position, name_range) {
        ranges.push(name_range);
    }

    // Parameters
    for param in &func.params {
        let param_range = doc.span_to_range(get_param_span(param));
        if is_position_in_range(position, param_range) {
            find_ranges_in_pattern(doc, &param.pattern, position, ranges);
            ranges.push(param_range);
        }
    }

    // Return type
    if let Some(ret_ty) = &func.return_type {
        let ret_range = doc.span_to_range(get_type_span(ret_ty));
        if is_position_in_range(position, ret_range) {
            ranges.push(ret_range);
        }
    }

    // Function body
    find_ranges_in_expr(doc, &func.body, position, ranges);
}

/// Find ranges within a struct definition
fn find_ranges_in_struct(
    doc: &Document,
    struct_def: &StructDef,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    // Struct name
    let name_range = doc.span_to_range(struct_def.name.span);
    if is_position_in_range(position, name_range) {
        ranges.push(name_range);
    }

    // Fields
    for field in &struct_def.fields {
        let field_range = doc.span_to_range(get_field_span(field));
        if is_position_in_range(position, field_range) {
            // Field name
            let field_name_range = doc.span_to_range(field.name.span);
            if is_position_in_range(position, field_name_range) {
                ranges.push(field_name_range);
            }

            // Field type
            let field_type_range = doc.span_to_range(get_type_span(&field.ty));
            if is_position_in_range(position, field_type_range) {
                ranges.push(field_type_range);
            }

            ranges.push(field_range);
        }
    }
}

/// Find ranges within an enum definition
fn find_ranges_in_enum(
    doc: &Document,
    enum_def: &EnumDef,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    // Enum name
    let name_range = doc.span_to_range(enum_def.name.span);
    if is_position_in_range(position, name_range) {
        ranges.push(name_range);
    }

    // Variants
    for variant in &enum_def.variants {
        let variant_range = doc.span_to_range(get_variant_span(variant));
        if is_position_in_range(position, variant_range) {
            // Variant name
            let variant_name_range = doc.span_to_range(variant.name.span);
            if is_position_in_range(position, variant_name_range) {
                ranges.push(variant_name_range);
            }

            // Variant body (if any)
            match &variant.body {
                VariantBody::Tuple(types) => {
                    for ty in types {
                        let ty_range = doc.span_to_range(get_type_span(ty));
                        if is_position_in_range(position, ty_range) {
                            ranges.push(ty_range);
                        }
                    }
                }
                VariantBody::Struct(fields) => {
                    for field in fields {
                        let field_range = doc.span_to_range(get_field_span(field));
                        if is_position_in_range(position, field_range) {
                            ranges.push(field_range);
                        }
                    }
                }
                _ => {}
            }

            ranges.push(variant_range);
        }
    }
}

/// Find ranges within a trait definition
fn find_ranges_in_trait(
    doc: &Document,
    trait_def: &TraitDef,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    // Trait name
    let name_range = doc.span_to_range(trait_def.name.span);
    if is_position_in_range(position, name_range) {
        ranges.push(name_range);
    }

    // Trait items
    for item in &trait_def.items {
        match item {
            jet_parser::ast::TraitItem::Method {
                name,
                params,
                return_type,
                ..
            } => {
                let method_range = doc.span_to_range(name.span);
                if is_position_in_range(position, method_range) {
                    ranges.push(method_range);
                }

                // Parameters
                for param in params {
                    let param_range = doc.span_to_range(get_param_span(param));
                    if is_position_in_range(position, param_range) {
                        ranges.push(param_range);
                    }
                }

                // Return type
                if let Some(ret_ty) = return_type {
                    let ret_range = doc.span_to_range(get_type_span(ret_ty));
                    if is_position_in_range(position, ret_range) {
                        ranges.push(ret_range);
                    }
                }
            }
            jet_parser::ast::TraitItem::TypeDecl { name, .. } => {
                let type_range = doc.span_to_range(name.span);
                if is_position_in_range(position, type_range) {
                    ranges.push(type_range);
                }
            }
            jet_parser::ast::TraitItem::ConstDecl { name, .. } => {
                let const_range = doc.span_to_range(name.span);
                if is_position_in_range(position, const_range) {
                    ranges.push(const_range);
                }
            }
        }
    }
}

/// Find ranges within a const definition
fn find_ranges_in_const(
    doc: &Document,
    const_def: &jet_parser::ast::ConstDef,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    // Const name
    let name_range = doc.span_to_range(const_def.name.span);
    if is_position_in_range(position, name_range) {
        ranges.push(name_range);
    }

    // Const type
    let ty_range = doc.span_to_range(get_type_span(&const_def.ty));
    if is_position_in_range(position, ty_range) {
        ranges.push(ty_range);
    }

    // Const value
    find_ranges_in_expr(doc, &const_def.value, position, ranges);
}

/// Find ranges within a type alias
fn find_ranges_in_type_alias(
    doc: &Document,
    type_alias: &TypeAlias,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    // Type name
    let name_range = doc.span_to_range(type_alias.name.span);
    if is_position_in_range(position, name_range) {
        ranges.push(name_range);
    }

    // Type definition
    let ty_range = doc.span_to_range(get_type_span(&type_alias.ty));
    if is_position_in_range(position, ty_range) {
        ranges.push(ty_range);
    }
}

/// Find ranges within an impl block
fn find_ranges_in_impl(
    doc: &Document,
    impl_def: &ImplDef,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    // Self type (impl_def.ty)
    let self_range = doc.span_to_range(get_type_span(&impl_def.ty));
    if is_position_in_range(position, self_range) {
        ranges.push(self_range);
    }

    // Trait (if any) - impl_def.trait_path
    if let Some(trait_path) = &impl_def.trait_path {
        let trait_range = doc.span_to_range(trait_path.span);
        if is_position_in_range(position, trait_range) {
            ranges.push(trait_range);
        }
    }

    // Impl items
    for item in &impl_def.items {
        match item {
            jet_parser::ast::ImplItem::Method(func) => {
                find_ranges_in_function(doc, func, position, ranges);
            }
            jet_parser::ast::ImplItem::TypeAlias(type_alias) => {
                let name_range = doc.span_to_range(type_alias.name.span);
                if is_position_in_range(position, name_range) {
                    ranges.push(name_range);
                }

                let ty_range = doc.span_to_range(get_type_span(&type_alias.ty));
                if is_position_in_range(position, ty_range) {
                    ranges.push(ty_range);
                }
            }
            jet_parser::ast::ImplItem::Const {
                name, ty, value, ..
            } => {
                let name_range = doc.span_to_range(name.span);
                if is_position_in_range(position, name_range) {
                    ranges.push(name_range);
                }

                let ty_range = doc.span_to_range(get_type_span(ty));
                if is_position_in_range(position, ty_range) {
                    ranges.push(ty_range);
                }

                find_ranges_in_expr(doc, value, position, ranges);
            }
        }
    }
}

/// Find ranges within an expression
fn find_ranges_in_expr(doc: &Document, expr: &Expr, position: Position, ranges: &mut Vec<Range>) {
    let expr_range = doc.span_to_range(get_expr_span(expr));

    if !is_position_in_range(position, expr_range) {
        return;
    }

    // Add the expression itself
    let should_add = match expr {
        Expr::Variable(_) | Expr::Literal(_) | Expr::SelfExpr(_) | Expr::Pass => false,
        _ => true,
    };

    // Handle specific expression types
    match expr {
        Expr::Call { func, args } => {
            // Function being called
            find_ranges_in_expr(doc, func, position, ranges);

            // Arguments
            for arg in args {
                find_ranges_in_expr(doc, arg, position, ranges);
            }
        }
        Expr::MethodCall {
            receiver,
            args,
            method,
            ..
        } => {
            // Receiver
            find_ranges_in_expr(doc, receiver, position, ranges);

            // Method name
            let method_range = doc.span_to_range(method.span);
            if is_position_in_range(position, method_range) {
                ranges.push(method_range);
            }

            // Arguments
            for arg in args {
                find_ranges_in_expr(doc, arg, position, ranges);
            }
        }
        Expr::FieldAccess { object, field } => {
            find_ranges_in_expr(doc, object, position, ranges);

            let field_range = doc.span_to_range(field.span);
            if is_position_in_range(position, field_range) {
                ranges.push(field_range);
            }
        }
        Expr::Index { object, index } => {
            find_ranges_in_expr(doc, object, position, ranges);
            find_ranges_in_expr(doc, index, position, ranges);
        }
        Expr::Binary { left, right, .. } => {
            find_ranges_in_expr(doc, left, position, ranges);
            find_ranges_in_expr(doc, right, position, ranges);
        }
        Expr::Unary { expr: inner, .. } => {
            find_ranges_in_expr(doc, inner, position, ranges);
        }
        Expr::Block(block) => {
            find_ranges_in_block(doc, block, position, ranges);
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            find_ranges_in_expr(doc, cond, position, ranges);
            find_ranges_in_expr(doc, then_branch, position, ranges);
            if let Some(else_branch) = else_branch {
                find_ranges_in_expr(doc, else_branch, position, ranges);
            }
        }
        Expr::Match { expr, arms } => {
            find_ranges_in_expr(doc, expr, position, ranges);
            for arm in arms {
                find_ranges_in_match_arm(doc, arm, position, ranges);
            }
        }
        Expr::While { cond, body, .. } => {
            find_ranges_in_expr(doc, cond, position, ranges);
            find_ranges_in_expr(doc, body, position, ranges);
        }
        Expr::For {
            pattern,
            iterable,
            body,
            ..
        } => {
            find_ranges_in_pattern(doc, pattern, position, ranges);
            find_ranges_in_expr(doc, iterable, position, ranges);
            find_ranges_in_expr(doc, body, position, ranges);
        }
        Expr::Loop { body, .. } => {
            find_ranges_in_expr(doc, body, position, ranges);
        }
        Expr::Lambda { params, body, .. } => {
            for param in params {
                find_ranges_in_pattern(doc, &param.pattern, position, ranges);
            }
            find_ranges_in_expr(doc, body, position, ranges);
        }
        Expr::Assign { target, value, .. } => {
            find_ranges_in_expr(doc, target, position, ranges);
            find_ranges_in_expr(doc, value, position, ranges);
        }
        Expr::Tuple(exprs) => {
            for e in exprs {
                find_ranges_in_expr(doc, e, position, ranges);
            }
        }
        Expr::Array(exprs) => {
            for e in exprs {
                find_ranges_in_expr(doc, e, position, ranges);
            }
        }
        Expr::StructLiteral { path, fields } => {
            let path_range = doc.span_to_range(path.span);
            if is_position_in_range(position, path_range) {
                ranges.push(path_range);
            }

            for field in fields {
                let field_name_range = doc.span_to_range(field.name.span);
                if is_position_in_range(position, field_name_range) {
                    ranges.push(field_name_range);
                }

                if let Some(value) = &field.value {
                    find_ranges_in_expr(doc, value, position, ranges);
                }
            }
        }
        Expr::Await(inner) => {
            find_ranges_in_expr(doc, inner, position, ranges);
        }
        Expr::Try(inner) => {
            find_ranges_in_expr(doc, inner, position, ranges);
        }
        Expr::Return(inner) => {
            if let Some(e) = inner {
                find_ranges_in_expr(doc, e, position, ranges);
            }
        }
        Expr::Break { value, .. } => {
            if let Some(val) = value {
                find_ranges_in_expr(doc, val, position, ranges);
            }
        }
        Expr::Spawn(inner) => {
            find_ranges_in_expr(doc, inner, position, ranges);
        }
        Expr::Async(block) => {
            find_ranges_in_block(doc, block, position, ranges);
        }
        Expr::Concurrent(block) => {
            find_ranges_in_block(doc, block, position, ranges);
        }
        Expr::Handle(handle) => {
            find_ranges_in_expr(doc, &handle.body, position, ranges);
            for handler in &handle.handlers {
                find_ranges_in_expr(doc, &handler.body, position, ranges);
            }
        }
        Expr::Raise(raise) => {
            for arg in &raise.args {
                find_ranges_in_expr(doc, arg, position, ranges);
            }
        }
        Expr::Resume(resume) => {
            if let Some(value) = &resume.value {
                find_ranges_in_expr(doc, value, position, ranges);
            }
        }
        _ => {}
    }

    if should_add {
        ranges.push(expr_range);
    }
}

/// Find ranges within a block
fn find_ranges_in_block(
    doc: &Document,
    block: &Block,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    for stmt in &block.stmts {
        find_ranges_in_stmt(doc, stmt, position, ranges);
    }

    if let Some(expr) = &block.expr {
        find_ranges_in_expr(doc, expr, position, ranges);
    }

    // Add the block itself
    let block_range = doc.span_to_range(block.span);
    if is_position_in_range(position, block_range) {
        ranges.push(block_range);
    }
}

/// Find ranges within a statement
fn find_ranges_in_stmt(doc: &Document, stmt: &Stmt, position: Position, ranges: &mut Vec<Range>) {
    let stmt_range = doc.span_to_range(get_stmt_span(stmt));

    if !is_position_in_range(position, stmt_range) {
        return;
    }

    match stmt {
        Stmt::Expr(expr) => {
            find_ranges_in_expr(doc, expr, position, ranges);
        }
        Stmt::Let { pattern, ty, value } => {
            find_ranges_in_pattern(doc, pattern, position, ranges);
            find_ranges_in_expr(doc, value, position, ranges);

            // Type annotation
            if let Some(type_ann) = ty {
                let ty_range = doc.span_to_range(get_type_span(type_ann));
                if is_position_in_range(position, ty_range) {
                    ranges.push(ty_range);
                }
            }
        }
        Stmt::Assign { target, value, .. } => {
            find_ranges_in_expr(doc, target, position, ranges);
            find_ranges_in_expr(doc, value, position, ranges);
        }
        Stmt::Return(Some(expr)) => {
            find_ranges_in_expr(doc, expr, position, ranges);
        }
        Stmt::Break {
            value: Some(expr), ..
        } => {
            find_ranges_in_expr(doc, expr, position, ranges);
        }
        Stmt::Handle { body, handlers, .. } => {
            find_ranges_in_expr(doc, body, position, ranges);
            for handler in handlers {
                find_ranges_in_expr(doc, &handler.body, position, ranges);
            }
        }
        _ => {}
    }

    ranges.push(stmt_range);
}

/// Find ranges within a pattern
fn find_ranges_in_pattern(
    doc: &Document,
    pattern: &Pattern,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    let pattern_range = doc.span_to_range(pattern.span());

    if !is_position_in_range(position, pattern_range) {
        return;
    }

    match pattern {
        Pattern::Ident { name, .. } => {
            let name_range = doc.span_to_range(name.span);
            if is_position_in_range(position, name_range) {
                ranges.push(name_range);
            }
        }
        Pattern::Tuple(patterns) => {
            for p in patterns {
                find_ranges_in_pattern(doc, p, position, ranges);
            }
        }
        Pattern::Struct { path, fields, .. } => {
            let path_range = doc.span_to_range(path.span);
            if is_position_in_range(position, path_range) {
                ranges.push(path_range);
            }

            for field in fields {
                let field_name_range = doc.span_to_range(field.name.span);
                if is_position_in_range(position, field_name_range) {
                    ranges.push(field_name_range);
                }

                if let Some(pat) = &field.pattern {
                    find_ranges_in_pattern(doc, pat, position, ranges);
                }
            }
        }
        Pattern::Enum {
            path,
            variant,
            inner,
        } => {
            let path_range = doc.span_to_range(path.span);
            if is_position_in_range(position, path_range) {
                ranges.push(path_range);
            }

            let variant_range = doc.span_to_range(variant.span);
            if is_position_in_range(position, variant_range) {
                ranges.push(variant_range);
            }

            if let Some(inner_pat) = inner {
                find_ranges_in_pattern(doc, inner_pat, position, ranges);
            }
        }
        Pattern::Array(patterns) => {
            for p in patterns {
                find_ranges_in_pattern(doc, p, position, ranges);
            }
        }
        Pattern::Bind { name, pattern } => {
            let name_range = doc.span_to_range(name.span);
            if is_position_in_range(position, name_range) {
                ranges.push(name_range);
            }

            find_ranges_in_pattern(doc, pattern, position, ranges);
        }
        Pattern::Mut(inner) => {
            find_ranges_in_pattern(doc, inner, position, ranges);
        }
        Pattern::Ref { pattern, .. } => {
            find_ranges_in_pattern(doc, pattern, position, ranges);
        }
        Pattern::Or(left, right) => {
            find_ranges_in_pattern(doc, left, position, ranges);
            find_ranges_in_pattern(doc, right, position, ranges);
        }
        _ => {}
    }

    ranges.push(pattern_range);
}

/// Find ranges within a match arm
fn find_ranges_in_match_arm(
    doc: &Document,
    arm: &MatchArm,
    position: Position,
    ranges: &mut Vec<Range>,
) {
    find_ranges_in_pattern(doc, &arm.pattern, position, ranges);

    if let Some(guard) = &arm.guard {
        find_ranges_in_expr(doc, guard, position, ranges);
    }

    find_ranges_in_expr(doc, &arm.body, position, ranges);

    // MatchArm doesn't have a span field, compute from pattern and body
    let arm_span = jet_lexer::Span::new(arm.pattern.span().start, get_expr_span(&arm.body).end);
    let arm_range = doc.span_to_range(arm_span);
    if is_position_in_range(position, arm_range) {
        ranges.push(arm_range);
    }
}

// Helper functions to get spans

fn get_param_span(param: &Param) -> jet_lexer::Span {
    param.pattern.span()
}

fn get_field_span(field: &FieldDef) -> jet_lexer::Span {
    // Span from field name start to type end
    jet_lexer::Span::new(field.name.span.start, get_type_span(&field.ty).end)
}

fn get_variant_span(variant: &EnumVariant) -> jet_lexer::Span {
    match &variant.body {
        VariantBody::Unit => variant.name.span,
        VariantBody::Discriminant(_) => variant.name.span,
        VariantBody::Tuple(types) => {
            if let Some(last) = types.last() {
                jet_lexer::Span::new(variant.name.span.start, get_type_span(last).end)
            } else {
                variant.name.span
            }
        }
        VariantBody::Struct(fields) => {
            if let Some(last) = fields.last() {
                jet_lexer::Span::new(variant.name.span.start, get_type_span(&last.ty).end)
            } else {
                variant.name.span
            }
        }
    }
}

fn get_type_span(ty: &jet_parser::ast::Type) -> jet_lexer::Span {
    use jet_parser::ast::Type;

    match ty {
        Type::Path(path) => path.span,
        Type::Generic(base, args) => {
            let base_span = get_type_span(base);
            if let Some(last) = args.last() {
                jet_lexer::Span::new(base_span.start, get_type_span(last).end)
            } else {
                base_span
            }
        }
        Type::Tuple(types) => {
            if let (Some(first), Some(last)) = (types.first(), types.last()) {
                jet_lexer::Span::new(get_type_span(first).start, get_type_span(last).end)
            } else {
                jet_lexer::Span::new(0, 0)
            }
        }
        Type::Array(inner, _) => get_type_span(inner),
        Type::Function {
            params,
            return_type,
            ..
        } => {
            let start = if let Some(first) = params.first() {
                get_type_span(first).start
            } else {
                0
            };
            let end = if let Some(ret) = return_type {
                get_type_span(ret).end
            } else if let Some(last) = params.last() {
                get_type_span(last).end
            } else {
                start
            };
            jet_lexer::Span::new(start, end)
        }
        Type::Reference { inner, .. } => get_type_span(inner),
        Type::Channel(inner) => get_type_span(inner),
        Type::Async(inner) => get_type_span(inner),
        Type::Infer => jet_lexer::Span::new(0, 0),
        Type::SelfType => jet_lexer::Span::new(0, 0),
    }
}

fn get_expr_span(expr: &Expr) -> jet_lexer::Span {
    match expr {
        Expr::Literal(_) => jet_lexer::Span::new(0, 0),
        Expr::Variable(ident) => ident.span,
        Expr::Path(path) => path.span,
        Expr::Block(block) => block.span,
        Expr::Call { func, args } => {
            let func_span = get_expr_span(func);
            if let Some(last_arg) = args.last() {
                jet_lexer::Span::new(func_span.start, get_expr_span(last_arg).end)
            } else {
                func_span
            }
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            let recv_span = get_expr_span(receiver);
            if let Some(last_arg) = args.last() {
                jet_lexer::Span::new(recv_span.start, get_expr_span(last_arg).end)
            } else {
                jet_lexer::Span::new(recv_span.start, method.span.end)
            }
        }
        Expr::FieldAccess { object, field } => {
            jet_lexer::Span::new(get_expr_span(object).start, field.span.end)
        }
        Expr::Index { object, index } => {
            jet_lexer::Span::new(get_expr_span(object).start, get_expr_span(index).end)
        }
        Expr::Binary { left, right, .. } => {
            jet_lexer::Span::new(get_expr_span(left).start, get_expr_span(right).end)
        }
        Expr::Unary { expr, .. } => get_expr_span(expr),
        Expr::Assign { target, value, .. } => {
            jet_lexer::Span::new(get_expr_span(target).start, get_expr_span(value).end)
        }
        Expr::Tuple(exprs) => {
            if let (Some(first), Some(last)) = (exprs.first(), exprs.last()) {
                jet_lexer::Span::new(get_expr_span(first).start, get_expr_span(last).end)
            } else {
                jet_lexer::Span::new(0, 0)
            }
        }
        Expr::Array(exprs) => {
            if let (Some(first), Some(last)) = (exprs.first(), exprs.last()) {
                jet_lexer::Span::new(get_expr_span(first).start, get_expr_span(last).end)
            } else {
                jet_lexer::Span::new(0, 0)
            }
        }
        Expr::If {
            cond, else_branch, ..
        } => {
            let start = get_expr_span(cond).start;
            let end = if let Some(else_branch) = else_branch {
                get_expr_span(else_branch).end
            } else {
                get_expr_span(cond).end
            };
            jet_lexer::Span::new(start, end)
        }
        Expr::Match { expr, arms } => {
            let start = get_expr_span(expr).start;
            let end = if let Some(last_arm) = arms.last() {
                get_expr_span(&*last_arm.body).end
            } else {
                get_expr_span(expr).end
            };
            jet_lexer::Span::new(start, end)
        }
        Expr::While { cond, body, .. } => {
            jet_lexer::Span::new(get_expr_span(cond).start, get_expr_span(body).end)
        }
        Expr::For { iterable, body, .. } => {
            jet_lexer::Span::new(get_expr_span(iterable).start, get_expr_span(body).end)
        }
        Expr::Loop { body, .. } => get_expr_span(body),
        Expr::Lambda { body, .. } => get_expr_span(body),
        Expr::Await(expr) => get_expr_span(expr),
        Expr::Try(expr) => get_expr_span(expr),
        Expr::Return(expr) => expr
            .as_ref()
            .map(|e| get_expr_span(e))
            .unwrap_or(jet_lexer::Span::new(0, 0)),
        Expr::Break { value, .. } => value
            .as_ref()
            .map(|e| get_expr_span(e))
            .unwrap_or(jet_lexer::Span::new(0, 0)),
        Expr::Continue { .. } => jet_lexer::Span::new(0, 0),
        Expr::Spawn(expr) => get_expr_span(expr),
        Expr::Async(block) => block.span,
        Expr::Concurrent(block) => block.span,
        Expr::SelfExpr(span) => *span,
        Expr::Pass => jet_lexer::Span::new(0, 0),
        Expr::Raise(raise) => raise.span,
        Expr::Handle(handle) => handle.span,
        Expr::Resume(resume) => resume.span,
        Expr::StructLiteral { path, fields } => {
            let start = path.span.start;
            let end = if let Some(last_field) = fields.last() {
                if let Some(ref value) = last_field.value {
                    get_expr_span(value).end
                } else {
                    last_field.name.span.end
                }
            } else {
                path.span.end
            };
            jet_lexer::Span::new(start, end)
        }
    }
}

fn get_stmt_span(stmt: &Stmt) -> jet_lexer::Span {
    match stmt {
        Stmt::Expr(expr) => get_expr_span(expr),
        Stmt::Let { pattern, value, .. } => {
            let pattern_span = pattern.span();
            let value_span = get_expr_span(value);
            jet_lexer::Span::new(pattern_span.start, value_span.end)
        }
        Stmt::Assign { target, value, .. } => {
            jet_lexer::Span::new(get_expr_span(target).start, get_expr_span(value).end)
        }
        Stmt::Return(expr) => expr
            .as_ref()
            .map(|e| get_expr_span(e))
            .unwrap_or(jet_lexer::Span::new(0, 0)),
        Stmt::Break { value, .. } => value
            .as_ref()
            .map(|e| get_expr_span(e))
            .unwrap_or(jet_lexer::Span::new(0, 0)),
        Stmt::Continue { .. } => jet_lexer::Span::new(0, 0),
        Stmt::Handle { body, .. } => get_expr_span(body),
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

fn is_position_in_range(position: Position, range: Range) -> bool {
    let pos_line = position.line;
    let pos_char = position.character;

    let after_start = pos_line > range.start.line
        || (pos_line == range.start.line && pos_char >= range.start.character);

    let before_end = pos_line < range.end.line
        || (pos_line == range.end.line && pos_char <= range.end.character);

    after_start && before_end
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_position_in_range() {
        let range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 10,
                character: 0,
            },
        };

        assert!(is_position_in_range(
            Position {
                line: 5,
                character: 0
            },
            range
        ));
        assert!(!is_position_in_range(
            Position {
                line: 15,
                character: 0
            },
            range
        ));
    }
}
