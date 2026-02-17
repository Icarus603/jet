//! Inlay hints for the Jet LSP server
//!
//! Provides inline type annotations and parameter name hints.

use crate::document::Document;
use jet_parser::ast::{Expr, Function, Module, ModuleItem, Param, Pattern, Stmt};
use tower_lsp::lsp_types::{
    InlayHint, InlayHintKind, InlayHintLabel, InlayHintTooltip, MarkupContent, MarkupKind,
    Position, Range,
};

/// Get inlay hints for a document range
pub fn get_inlay_hints(doc: &Document, ast: &Module, range: Range) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    for item in &ast.items {
        match item {
            ModuleItem::Function(func) => {
                hints.extend(get_function_inlay_hints(doc, func, range));
            }
            ModuleItem::Const(const_def) => {
                // Type hint for const declarations without explicit type
                let const_range = doc.span_to_range(const_def.span);
                if ranges_overlap(const_range, range) {
                    // Check if the const has an explicit type annotation
                    if let Expr::Block(block) = const_def.value.as_ref() {
                        if let Some(first_stmt) = block.stmts.first() {
                            if let Stmt::Let { pattern, ty, .. } = first_stmt {
                                if ty.is_none() {
                                    // Add type hint after the pattern
                                    if let Some(hint_pos) = find_pattern_end_position(doc, pattern)
                                    {
                                        hints.push(InlayHint {
                                            position: hint_pos,
                                            label: InlayHintLabel::String(format!(
                                                ": {}",
                                                format_type(&const_def.ty)
                                            )),
                                            kind: Some(InlayHintKind::TYPE),
                                            text_edits: None,
                                            tooltip: Some(InlayHintTooltip::MarkupContent(
                                                MarkupContent {
                                                    kind: MarkupKind::Markdown,
                                                    value: format!(
                                                        "Inferred type: `{}`",
                                                        format_type(&const_def.ty)
                                                    ),
                                                },
                                            )),
                                            padding_left: Some(false),
                                            padding_right: Some(false),
                                            data: None,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    hints
}

/// Get inlay hints for a function
fn get_function_inlay_hints(doc: &Document, func: &Function, range: Range) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    let func_range = doc.span_to_range(func.span);

    if !ranges_overlap(func_range, range) {
        return hints;
    }

    // Parameter name hints at call sites
    hints.extend(get_call_site_hints(doc, &func.body, range, &func.params));

    // Type hints for let bindings without explicit types
    hints.extend(get_let_type_hints(doc, &func.body, range));

    hints
}

/// Get parameter name hints at function call sites
fn get_call_site_hints(
    doc: &Document,
    expr: &Expr,
    range: Range,
    func_params: &[Param],
) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    match expr {
        Expr::Call { func, args } => {
            let call_range = get_expr_range(doc, expr);
            if ranges_overlap(call_range, range) {
                // Try to find the function definition to get parameter names
                if let Expr::Variable(_ident) = func.as_ref() {
                    // Use parameter names from the function signature
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(pos) = find_arg_position(doc, arg) {
                            let param_name = func_params
                                .get(i)
                                .map(|p| get_param_name(&p.pattern))
                                .unwrap_or_else(|| format!("arg{}", i));
                            hints.push(InlayHint {
                                position: pos,
                                label: InlayHintLabel::String(format!("{}:", param_name)),
                                kind: Some(InlayHintKind::PARAMETER),
                                text_edits: None,
                                tooltip: Some(InlayHintTooltip::MarkupContent(MarkupContent {
                                    kind: MarkupKind::Markdown,
                                    value: format!("Parameter `{}`", param_name),
                                })),
                                padding_left: Some(false),
                                padding_right: Some(true),
                                data: None,
                            });
                        }
                    }
                } else {
                    // For other call expressions, use generic parameter hints
                    for (i, arg) in args.iter().enumerate() {
                        if let Some(pos) = find_arg_position(doc, arg) {
                            hints.push(InlayHint {
                                position: pos,
                                label: InlayHintLabel::String(format!("arg{}:", i)),
                                kind: Some(InlayHintKind::PARAMETER),
                                text_edits: None,
                                tooltip: Some(InlayHintTooltip::String(format!(
                                    "Argument {}",
                                    i + 1
                                ))),
                                padding_left: Some(false),
                                padding_right: Some(true),
                                data: None,
                            });
                        }
                    }
                }

                // Recurse into arguments
                for arg in args {
                    hints.extend(get_call_site_hints(doc, arg, range, func_params));
                }
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            let call_range = get_expr_range(doc, expr);
            if ranges_overlap(call_range, range) {
                // Add parameter hints for method calls
                for (i, arg) in args.iter().enumerate() {
                    if let Some(pos) = find_arg_position(doc, arg) {
                        hints.push(InlayHint {
                            position: pos,
                            label: InlayHintLabel::String(format!("arg{}:", i)),
                            kind: Some(InlayHintKind::PARAMETER),
                            text_edits: None,
                            tooltip: Some(InlayHintTooltip::String(format!("Argument {}", i + 1))),
                            padding_left: Some(false),
                            padding_right: Some(true),
                            data: None,
                        });
                    }
                }

                // Recurse
                hints.extend(get_call_site_hints(doc, receiver, range, func_params));
                for arg in args {
                    hints.extend(get_call_site_hints(doc, arg, range, func_params));
                }
            }
        }
        Expr::Block(block) => {
            for stmt in &block.stmts {
                hints.extend(get_stmt_hints(doc, stmt, range, func_params));
            }
            if let Some(expr) = &block.expr {
                hints.extend(get_call_site_hints(doc, expr, range, func_params));
            }
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            hints.extend(get_call_site_hints(doc, cond, range, func_params));
            hints.extend(get_call_site_hints(doc, then_branch, range, func_params));
            if let Some(else_branch) = else_branch {
                hints.extend(get_call_site_hints(doc, else_branch, range, func_params));
            }
        }
        Expr::Match { expr, arms } => {
            hints.extend(get_call_site_hints(doc, expr, range, func_params));
            for arm in arms {
                hints.extend(get_call_site_hints(doc, &arm.body, range, func_params));
            }
        }
        Expr::While { cond, body, .. } => {
            hints.extend(get_call_site_hints(doc, cond, range, func_params));
            hints.extend(get_call_site_hints(doc, body, range, func_params));
        }
        Expr::For { iterable, body, .. } => {
            hints.extend(get_call_site_hints(doc, iterable, range, func_params));
            hints.extend(get_call_site_hints(doc, body, range, func_params));
        }
        Expr::Loop { body, .. } => {
            hints.extend(get_call_site_hints(doc, body, range, func_params));
        }
        Expr::Binary { left, right, .. } => {
            hints.extend(get_call_site_hints(doc, left, range, func_params));
            hints.extend(get_call_site_hints(doc, right, range, func_params));
        }
        Expr::Unary { expr: inner, .. } => {
            hints.extend(get_call_site_hints(doc, inner, range, func_params));
        }
        Expr::Assign { target, value, .. } => {
            hints.extend(get_call_site_hints(doc, target, range, func_params));
            hints.extend(get_call_site_hints(doc, value, range, func_params));
        }
        Expr::FieldAccess { object, .. } => {
            hints.extend(get_call_site_hints(doc, object, range, func_params));
        }
        Expr::Index { object, index } => {
            hints.extend(get_call_site_hints(doc, object, range, func_params));
            hints.extend(get_call_site_hints(doc, index, range, func_params));
        }
        Expr::Tuple(exprs) => {
            for e in exprs {
                hints.extend(get_call_site_hints(doc, e, range, func_params));
            }
        }
        Expr::Array(exprs) => {
            for e in exprs {
                hints.extend(get_call_site_hints(doc, e, range, func_params));
            }
        }
        Expr::Lambda { body, params, .. } => {
            hints.extend(get_call_site_hints(doc, body, range, params));
        }
        Expr::Await(inner) => {
            hints.extend(get_call_site_hints(doc, inner, range, func_params));
        }
        Expr::Try(inner) => {
            hints.extend(get_call_site_hints(doc, inner, range, func_params));
        }
        Expr::Return(Some(inner)) => {
            hints.extend(get_call_site_hints(doc, inner, range, func_params));
        }
        Expr::Break {
            value: Some(inner), ..
        } => {
            hints.extend(get_call_site_hints(doc, inner, range, func_params));
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                if let Some(value) = &field.value {
                    hints.extend(get_call_site_hints(doc, value, range, func_params));
                }
            }
        }
        Expr::Spawn(inner) => {
            hints.extend(get_call_site_hints(doc, inner, range, func_params));
        }
        Expr::Async(block) => {
            for stmt in &block.stmts {
                hints.extend(get_stmt_hints(doc, stmt, range, func_params));
            }
            if let Some(expr) = &block.expr {
                hints.extend(get_call_site_hints(doc, expr, range, func_params));
            }
        }
        Expr::Concurrent(block) => {
            for stmt in &block.stmts {
                hints.extend(get_stmt_hints(doc, stmt, range, func_params));
            }
            if let Some(expr) = &block.expr {
                hints.extend(get_call_site_hints(doc, expr, range, func_params));
            }
        }
        Expr::Handle(handle) => {
            hints.extend(get_call_site_hints(doc, &handle.body, range, func_params));
            for handler in &handle.handlers {
                hints.extend(get_call_site_hints(doc, &handler.body, range, func_params));
            }
        }
        Expr::Raise(raise) => {
            for arg in &raise.args {
                hints.extend(get_call_site_hints(doc, arg, range, func_params));
            }
        }
        Expr::Resume(resume) => {
            if let Some(value) = &resume.value {
                hints.extend(get_call_site_hints(doc, value, range, func_params));
            }
        }
        _ => {}
    }

    hints
}

/// Get hints from statements
fn get_stmt_hints(
    doc: &Document,
    stmt: &Stmt,
    range: Range,
    func_params: &[Param],
) -> Vec<InlayHint> {
    match stmt {
        Stmt::Expr(expr) => get_call_site_hints(doc, expr, range, func_params),
        Stmt::Let { pattern, ty, value } => {
            let mut hints = Vec::new();
            hints.extend(get_call_site_hints(doc, value, range, func_params));
            // Type hint for let without explicit type
            if ty.is_none() {
                // Use the pattern's span for the statement range
                let pattern_span = pattern.span();
                let stmt_range = doc.span_to_range(pattern_span);
                if ranges_overlap(stmt_range, range) {
                    if let Some(hint_pos) = find_pattern_end_position(doc, pattern) {
                        hints.push(InlayHint {
                            position: hint_pos,
                            label: InlayHintLabel::String(": <inferred>".to_string()),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: Some(InlayHintTooltip::String(
                                "Type will be inferred by the compiler".to_string(),
                            )),
                            padding_left: Some(false),
                            padding_right: Some(false),
                            data: None,
                        });
                    }
                }
            }
            hints
        }
        Stmt::Assign { target, value, .. } => {
            let mut hints = Vec::new();
            hints.extend(get_call_site_hints(doc, target, range, func_params));
            hints.extend(get_call_site_hints(doc, value, range, func_params));
            hints
        }
        Stmt::Return(Some(expr)) => get_call_site_hints(doc, expr, range, func_params),
        Stmt::Return(None) => Vec::new(),
        Stmt::Break {
            value: Some(expr), ..
        } => get_call_site_hints(doc, expr, range, func_params),
        Stmt::Break { value: None, .. } => Vec::new(),
        Stmt::Continue { .. } => Vec::new(),
        Stmt::Handle { body, handlers, .. } => {
            let mut hints = Vec::new();
            hints.extend(get_call_site_hints(doc, body, range, func_params));
            for handler in handlers {
                hints.extend(get_call_site_hints(doc, &handler.body, range, func_params));
            }
            hints
        }
    }
}

/// Get type hints for let bindings
fn get_let_type_hints(doc: &Document, expr: &Expr, range: Range) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    match expr {
        Expr::Block(block) => {
            for stmt in &block.stmts {
                if let Stmt::Let { pattern, ty, value } = stmt {
                    if ty.is_none() {
                        let pattern_span = pattern.span();
                        let stmt_range = doc.span_to_range(pattern_span);
                        if ranges_overlap(stmt_range, range) {
                            if let Some(hint_pos) = find_pattern_end_position(doc, pattern) {
                                hints.push(InlayHint {
                                    position: hint_pos,
                                    label: InlayHintLabel::String(": <inferred>".to_string()),
                                    kind: Some(InlayHintKind::TYPE),
                                    text_edits: None,
                                    tooltip: Some(InlayHintTooltip::String(
                                        "Type will be inferred by the compiler".to_string(),
                                    )),
                                    padding_left: Some(false),
                                    padding_right: Some(false),
                                    data: None,
                                });
                            }
                        }
                    }
                    // Recurse into the value
                    hints.extend(get_call_site_hints(doc, value, range, &[]));
                } else {
                    hints.extend(get_stmt_hints(doc, stmt, range, &[]));
                }
            }
            if let Some(expr) = &block.expr {
                hints.extend(get_let_type_hints(doc, expr, range));
            }
        }
        _ => {}
    }

    hints
}

/// Get parameter name from pattern
fn get_param_name(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Ident { name, .. } => name.name.clone(),
        Pattern::Bind { name, .. } => name.name.clone(),
        Pattern::Mut(inner) => get_param_name(inner),
        Pattern::Ref { pattern, .. } => get_param_name(pattern),
        _ => "arg".to_string(),
    }
}

/// Find the position after a pattern for placing type hint
fn find_pattern_end_position(doc: &Document, pattern: &Pattern) -> Option<Position> {
    let span = pattern.span();
    let range = doc.span_to_range(span);
    Some(Position {
        line: range.end.line,
        character: range.end.character,
    })
}

/// Find the position at the start of an argument for parameter hint
fn find_arg_position(doc: &Document, expr: &Expr) -> Option<Position> {
    let span = get_expr_span(expr);
    let range = doc.span_to_range(span);
    Some(Position {
        line: range.start.line,
        character: range.start.character,
    })
}

/// Get the range of an expression
fn get_expr_range(doc: &Document, expr: &Expr) -> Range {
    let span = get_expr_span(expr);
    doc.span_to_range(span)
}

/// Get span from expression
fn get_expr_span(expr: &Expr) -> jet_lexer::Span {
    match expr {
        Expr::Literal(_) => jet_lexer::Span::new(0, 0),
        Expr::Variable(ident) => ident.span,
        Expr::Path(path) => path.span,
        Expr::Block(block) => block.span,
        Expr::Call { func, args } => {
            if args.is_empty() {
                get_expr_span(func)
            } else {
                let func_span = get_expr_span(func);
                let last_arg_span = get_expr_span(args.last().unwrap());
                jet_lexer::Span::new(func_span.start, last_arg_span.end)
            }
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
        } => {
            let recv_span = get_expr_span(receiver);
            if args.is_empty() {
                jet_lexer::Span::new(recv_span.start, method.span.end)
            } else {
                let last_arg_span = get_expr_span(args.last().unwrap());
                jet_lexer::Span::new(recv_span.start, last_arg_span.end)
            }
        }
        Expr::If { cond, .. } => get_expr_span(cond),
        Expr::Match { expr, .. } => get_expr_span(expr),
        Expr::While { cond, .. } => get_expr_span(cond),
        Expr::For { iterable, .. } => get_expr_span(iterable),
        Expr::Loop { body, .. } => get_expr_span(body),
        Expr::Lambda { body, .. } => get_expr_span(body),
        Expr::FieldAccess { object, field } => {
            let obj_span = get_expr_span(object);
            jet_lexer::Span::new(obj_span.start, field.span.end)
        }
        Expr::Index { object, index } => {
            let obj_span = get_expr_span(object);
            let idx_span = get_expr_span(index);
            jet_lexer::Span::new(obj_span.start, idx_span.end)
        }
        Expr::Binary { left, right, .. } => {
            let left_span = get_expr_span(left);
            let right_span = get_expr_span(right);
            jet_lexer::Span::new(left_span.start, right_span.end)
        }
        Expr::Unary { expr, .. } => get_expr_span(expr),
        Expr::Assign { target, value, .. } => {
            let target_span = get_expr_span(target);
            let value_span = get_expr_span(value);
            jet_lexer::Span::new(target_span.start, value_span.end)
        }
        Expr::Tuple(exprs) => {
            if let (Some(first), Some(last)) = (exprs.first(), exprs.last()) {
                let first_span = get_expr_span(first);
                let last_span = get_expr_span(last);
                jet_lexer::Span::new(first_span.start, last_span.end)
            } else {
                jet_lexer::Span::new(0, 0)
            }
        }
        Expr::Array(exprs) => {
            if let (Some(first), Some(last)) = (exprs.first(), exprs.last()) {
                let first_span = get_expr_span(first);
                let last_span = get_expr_span(last);
                jet_lexer::Span::new(first_span.start, last_span.end)
            } else {
                jet_lexer::Span::new(0, 0)
            }
        }
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
        Expr::StructLiteral { path, .. } => path.span,
    }
}

/// Check if two ranges overlap
fn ranges_overlap(a: Range, b: Range) -> bool {
    // Check if range a overlaps with range b
    let a_start = (a.start.line, a.start.character);
    let a_end = (a.end.line, a.end.character);
    let b_start = (b.start.line, b.start.character);
    let b_end = (b.end.line, b.end.character);

    a_start <= b_end && a_end >= b_start
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
        Type::Array(inner, _) => {
            format!("[{}]", format_type(inner))
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

#[cfg(test)]
mod tests {
    use super::*;
    use jet_lexer::Span;
    use jet_parser::ast::Ident;

    #[test]
    fn test_ranges_overlap() {
        let range_a = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 10,
                character: 0,
            },
        };
        let range_b = Range {
            start: Position {
                line: 5,
                character: 0,
            },
            end: Position {
                line: 15,
                character: 0,
            },
        };
        assert!(ranges_overlap(range_a, range_b));

        let range_c = Range {
            start: Position {
                line: 20,
                character: 0,
            },
            end: Position {
                line: 30,
                character: 0,
            },
        };
        assert!(!ranges_overlap(range_a, range_c));
    }

    #[test]
    fn test_get_param_name() {
        // Test simple identifier pattern
        let ident = Ident::new("x", Span::new(0, 1));
        let pattern = Pattern::Ident {
            mutable: false,
            name: ident,
        };
        assert_eq!(get_param_name(&pattern), "x");

        // Test mutable identifier pattern
        let ident = Ident::new("y", Span::new(0, 1));
        let pattern = Pattern::Mut(Box::new(Pattern::Ident {
            mutable: false,
            name: ident,
        }));
        assert_eq!(get_param_name(&pattern), "y");
    }
}
