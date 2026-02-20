//! Signature help for the Jet LSP server
//!
//! Provides function signature information with active parameter highlighting.

use crate::document::Document;
use jet_parser::ast::{Expr, Function, Module, ModuleItem};
use tower_lsp::lsp_types::{
    ParameterInformation, ParameterLabel, Position, SignatureHelp, SignatureInformation,
};

/// Get signature help at a position
pub fn get_signature_help(
    doc: &Document,
    ast: &Module,
    position: Position,
) -> Option<SignatureHelp> {
    // Find the call expression at the position
    let call_info = find_call_at_position(doc, ast, position)?;

    // Build signature information
    let signatures = vec![call_info.signature];

    Some(SignatureHelp {
        signatures,
        active_signature: Some(0),
        active_parameter: call_info.active_parameter,
    })
}

/// Information about a function call
struct CallInfo {
    signature: SignatureInformation,
    active_parameter: Option<u32>,
}

/// Find a function call at the given position
fn find_call_at_position(doc: &Document, ast: &Module, position: Position) -> Option<CallInfo> {
    // Convert position to offset for searching
    let offset = doc.position_to_offset(position);

    // Search through the AST for a call expression at this position
    for item in &ast.items {
        if let ModuleItem::Function(func) = item {
            if let Some(call_info) = find_call_in_function(doc, func, position, offset) {
                return Some(call_info);
            }
        }
    }

    None
}

/// Find a call within a function
fn find_call_in_function(
    doc: &Document,
    func: &Function,
    position: Position,
    offset: usize,
) -> Option<CallInfo> {
    find_call_in_expr(doc, &func.body, position, offset, &func.params)
}

/// Find a call within an expression
fn find_call_in_expr(
    doc: &Document,
    expr: &Expr,
    position: Position,
    offset: usize,
    _params: &[jet_parser::ast::Param],
) -> Option<CallInfo> {
    match expr {
        Expr::Call { func, args } => {
            let call_span = get_expr_span(expr);
            let call_range = doc.span_to_range(call_span);

            // Check if position is within this call
            if is_position_in_range(position, call_range) {
                // Get function name
                let func_name = match func.as_ref() {
                    Expr::Variable(ident) => ident.name.clone(),
                    Expr::Path(path) => path
                        .segments
                        .last()
                        .map(|s| s.name.clone())
                        .unwrap_or_default(),
                    _ => "function".to_string(),
                };

                // Find which parameter is active
                let active_param = find_active_parameter(doc, args, offset);

                // Build signature information
                let signature = build_function_signature(&func_name, args);

                return Some(CallInfo {
                    signature,
                    active_parameter: active_param.map(|i| i as u32),
                });
            }

            // Recurse into arguments
            for arg in args {
                if let Some(info) = find_call_in_expr(doc, arg, position, offset, _params) {
                    return Some(info);
                }
            }

            // Recurse into the function expression
            find_call_in_expr(doc, func, position, offset, _params)
        }
        Expr::MethodCall {
            receiver,
            method,
            args,
            ..
        } => {
            let call_span = get_expr_span(expr);
            let call_range = doc.span_to_range(call_span);

            if is_position_in_range(position, call_range) {
                let func_name = format!("{}.{}", get_receiver_type_hint(receiver), method.name);
                let active_param = find_active_parameter(doc, args, offset);
                let signature = build_method_signature(&func_name, receiver, args);

                return Some(CallInfo {
                    signature,
                    active_parameter: active_param.map(|i| i as u32),
                });
            }

            // Recurse
            for arg in args {
                if let Some(info) = find_call_in_expr(doc, arg, position, offset, _params) {
                    return Some(info);
                }
            }
            find_call_in_expr(doc, receiver, position, offset, _params)
        }
        Expr::Block(block) => {
            for stmt in &block.stmts {
                if let Some(info) = find_call_in_stmt(doc, stmt, position, offset, _params) {
                    return Some(info);
                }
            }
            if let Some(expr) = &block.expr {
                find_call_in_expr(doc, expr, position, offset, _params)
            } else {
                None
            }
        }
        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            if let Some(info) = find_call_in_expr(doc, cond, position, offset, _params) {
                return Some(info);
            }
            if let Some(info) = find_call_in_expr(doc, then_branch, position, offset, _params) {
                return Some(info);
            }
            if let Some(else_branch) = else_branch {
                find_call_in_expr(doc, else_branch, position, offset, _params)
            } else {
                None
            }
        }
        Expr::Match { expr, arms } => {
            if let Some(info) = find_call_in_expr(doc, expr, position, offset, _params) {
                return Some(info);
            }
            for arm in arms {
                if let Some(info) = find_call_in_expr(doc, &arm.body, position, offset, _params) {
                    return Some(info);
                }
            }
            None
        }
        Expr::While { cond, body, .. } => {
            if let Some(info) = find_call_in_expr(doc, cond, position, offset, _params) {
                return Some(info);
            }
            find_call_in_expr(doc, body, position, offset, _params)
        }
        Expr::For { iterable, body, .. } => {
            if let Some(info) = find_call_in_expr(doc, iterable, position, offset, _params) {
                return Some(info);
            }
            find_call_in_expr(doc, body, position, offset, _params)
        }
        Expr::Loop { body, .. } => find_call_in_expr(doc, body, position, offset, _params),
        Expr::Binary { left, right, .. } => {
            if let Some(info) = find_call_in_expr(doc, left, position, offset, _params) {
                return Some(info);
            }
            find_call_in_expr(doc, right, position, offset, _params)
        }
        Expr::Unary { expr: inner, .. } => find_call_in_expr(doc, inner, position, offset, _params),
        Expr::Assign { target, value, .. } => {
            if let Some(info) = find_call_in_expr(doc, target, position, offset, _params) {
                return Some(info);
            }
            find_call_in_expr(doc, value, position, offset, _params)
        }
        Expr::FieldAccess { object, .. } => {
            find_call_in_expr(doc, object, position, offset, _params)
        }
        Expr::Index { object, index } => {
            if let Some(info) = find_call_in_expr(doc, object, position, offset, _params) {
                return Some(info);
            }
            find_call_in_expr(doc, index, position, offset, _params)
        }
        Expr::Tuple(exprs) => {
            for e in exprs {
                if let Some(info) = find_call_in_expr(doc, e, position, offset, _params) {
                    return Some(info);
                }
            }
            None
        }
        Expr::Array(exprs) => {
            for e in exprs {
                if let Some(info) = find_call_in_expr(doc, e, position, offset, _params) {
                    return Some(info);
                }
            }
            None
        }
        Expr::Lambda { body, .. } => find_call_in_expr(doc, body, position, offset, _params),
        Expr::Await(inner) => find_call_in_expr(doc, inner, position, offset, _params),
        Expr::Try(inner) => find_call_in_expr(doc, inner, position, offset, _params),
        Expr::Return(expr) => {
            if let Some(e) = expr {
                find_call_in_expr(doc, e, position, offset, _params)
            } else {
                None
            }
        }
        Expr::Break { value, .. } => {
            if let Some(val) = value {
                find_call_in_expr(doc, val, position, offset, _params)
            } else {
                None
            }
        }
        Expr::StructLiteral { fields, .. } => {
            for field in fields {
                if let Some(value) = &field.value {
                    if let Some(info) = find_call_in_expr(doc, value, position, offset, _params) {
                        return Some(info);
                    }
                }
            }
            None
        }
        Expr::Spawn(inner) => find_call_in_expr(doc, inner, position, offset, _params),
        Expr::Async(block) => {
            for stmt in &block.stmts {
                if let Some(info) = find_call_in_stmt(doc, stmt, position, offset, _params) {
                    return Some(info);
                }
            }
            if let Some(expr) = &block.expr {
                find_call_in_expr(doc, expr, position, offset, _params)
            } else {
                None
            }
        }
        Expr::Concurrent(block) => {
            for stmt in &block.stmts {
                if let Some(info) = find_call_in_stmt(doc, stmt, position, offset, _params) {
                    return Some(info);
                }
            }
            if let Some(expr) = &block.expr {
                find_call_in_expr(doc, expr, position, offset, _params)
            } else {
                None
            }
        }
        Expr::Handle(handle) => {
            if let Some(info) = find_call_in_expr(doc, &handle.body, position, offset, _params) {
                return Some(info);
            }
            for handler in &handle.handlers {
                if let Some(info) = find_call_in_expr(doc, &handler.body, position, offset, _params)
                {
                    return Some(info);
                }
            }
            None
        }
        Expr::Raise(raise) => {
            for arg in &raise.args {
                if let Some(info) = find_call_in_expr(doc, arg, position, offset, _params) {
                    return Some(info);
                }
            }
            None
        }
        Expr::Resume(resume) => {
            if let Some(value) = &resume.value {
                find_call_in_expr(doc, value, position, offset, _params)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Find a call within a statement
fn find_call_in_stmt(
    doc: &Document,
    stmt: &jet_parser::ast::Stmt,
    position: Position,
    offset: usize,
    params: &[jet_parser::ast::Param],
) -> Option<CallInfo> {
    use jet_parser::ast::Stmt;

    match stmt {
        Stmt::Expr(expr) => find_call_in_expr(doc, expr, position, offset, params),
        Stmt::Let { value, .. } => find_call_in_expr(doc, value, position, offset, params),
        Stmt::Assign { target, value, .. } => {
            if let Some(info) = find_call_in_expr(doc, target, position, offset, params) {
                return Some(info);
            }
            find_call_in_expr(doc, value, position, offset, params)
        }
        Stmt::Return(Some(expr)) => find_call_in_expr(doc, expr, position, offset, params),
        Stmt::Return(None) => None,
        Stmt::Break {
            value: Some(expr), ..
        } => find_call_in_expr(doc, expr, position, offset, params),
        Stmt::Break { value: None, .. } => None,
        Stmt::Continue { .. } => None,
        Stmt::Handle { body, handlers, .. } => {
            if let Some(info) = find_call_in_expr(doc, body, position, offset, params) {
                return Some(info);
            }
            for handler in handlers {
                if let Some(info) = find_call_in_expr(doc, &handler.body, position, offset, params)
                {
                    return Some(info);
                }
            }
            None
        }
    }
}

/// Find which parameter is active based on cursor position
fn find_active_parameter(_doc: &Document, args: &[Expr], offset: usize) -> Option<usize> {
    if args.is_empty() {
        return Some(0);
    }

    for (i, arg) in args.iter().enumerate() {
        let arg_span = get_expr_span(arg);
        let arg_start = arg_span.start;
        let arg_end = arg_span.end;

        // If cursor is before this argument, we're at the previous parameter
        if offset < arg_start {
            return Some(i);
        }

        // If cursor is within this argument, we're at this parameter
        if offset >= arg_start && offset <= arg_end {
            return Some(i);
        }
    }

    // Cursor is after all arguments
    Some(args.len())
}

/// Build signature information for a function call
fn build_function_signature(name: &str, args: &[Expr]) -> SignatureInformation {
    let mut label = format!("{}(", name);
    let mut parameters = Vec::new();

    for (i, _arg) in args.iter().enumerate() {
        if i > 0 {
            label.push_str(", ");
        }
        let param_label = format!("arg{}", i);
        let param_start = label.len();
        label.push_str(&param_label);
        let param_end = label.len();

        parameters.push(ParameterInformation {
            label: ParameterLabel::LabelOffsets([param_start as u32, param_end as u32]),
            documentation: None,
        });
    }

    label.push(')');

    SignatureInformation {
        label,
        documentation: None,
        parameters: Some(parameters),
        active_parameter: None,
    }
}

/// Build signature information for a method call
fn build_method_signature(name: &str, _receiver: &Expr, args: &[Expr]) -> SignatureInformation {
    let mut label = format!("{}(", name);
    let mut parameters = Vec::new();

    // Add self parameter
    parameters.push(ParameterInformation {
        label: ParameterLabel::Simple("self".to_string()),
        documentation: None,
    });

    for (i, _arg) in args.iter().enumerate() {
        if i > 0 {
            label.push_str(", ");
        }
        let param_label = format!("arg{}", i);
        let param_start = label.len();
        label.push_str(&param_label);
        let param_end = label.len();

        parameters.push(ParameterInformation {
            label: ParameterLabel::LabelOffsets([param_start as u32, param_end as u32]),
            documentation: None,
        });
    }

    label.push(')');

    SignatureInformation {
        label,
        documentation: None,
        parameters: Some(parameters),
        active_parameter: None,
    }
}

/// Get a type hint for a receiver expression
fn get_receiver_type_hint(receiver: &Expr) -> String {
    match receiver {
        Expr::Variable(ident) => ident.name.clone(),
        Expr::Path(path) => path
            .segments
            .last()
            .map(|s| s.name.clone())
            .unwrap_or_default(),
        _ => "Self".to_string(),
    }
}

/// Check if a position is within a range
fn is_position_in_range(position: Position, range: tower_lsp::lsp_types::Range) -> bool {
    let pos_line = position.line;
    let pos_char = position.character;

    let after_start = pos_line > range.start.line
        || (pos_line == range.start.line && pos_char >= range.start.character);

    let before_end = pos_line < range.end.line
        || (pos_line == range.end.line && pos_char <= range.end.character);

    after_start && before_end
}

/// Get span from expression
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
                get_expr_span(&last_arm.body).end
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
                if let Some(value) = &last_field.value {
                    get_expr_span(value).end
                } else {
                    last_field.name.span.end
                }
            } else {
                path.span.end
            };
            jet_lexer::Span::new(start, end)
        }
        Expr::Hole(span) => *span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_position_in_range() {
        let range = tower_lsp::lsp_types::Range {
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
