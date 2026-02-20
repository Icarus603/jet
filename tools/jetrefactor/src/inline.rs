//! Inline variable and function refactoring operations
//!
//! This module provides semantic-preserving inline operations:
//! - Inline Variable: Replace variable uses with its definition
//! - Inline Function: Replace function call with function body

use crate::{RefactoringConfig, RefactoringMetadata, RefactoringResult};
use jet_diagnostics::{Diagnostic, Span};
use jet_parser::ast::{Expr, Function, Ident, LetBinding, Module, Pattern, Stmt};
use std::collections::HashMap;

/// Check if a variable can be inlined at the given span
pub fn can_inline_variable(ast: &Module, span: Span) -> Result<bool, Vec<Diagnostic>> {
    // Find the variable reference at the span
    if let Some(expr) = find_expression_at_span(ast, span) {
        // Must be a variable reference
        if !matches!(expr, Expr::Variable(_)) {
            return Ok(false);
        }

        // Check if we can find the definition
        if let Some(var_name) = get_variable_name(expr) {
            if find_variable_definition(ast, &var_name, span).is_some() {
                return Ok(true);
            }
        }
    }
    Ok(false)
}

/// Inline a variable (replace all uses or a single use with its definition)
pub fn inline_variable(
    source: &str,
    ast: &Module,
    span: Span,
    config: &RefactoringConfig,
) -> Result<RefactoringResult, Vec<Diagnostic>> {
    // Find the variable at the span
    let var_expr = find_expression_at_span(ast, span)
        .ok_or_else(|| vec![Diagnostic::error("No variable found at selection", span)])?;

    let var_name = get_variable_name(var_expr)
        .ok_or_else(|| vec![Diagnostic::error("Selection is not a variable", span)])?;

    // Find the variable definition
    let (def_span, def_expr) = find_variable_definition(ast, &var_name, span)
        .ok_or_else(|| vec![Diagnostic::error(
            format!("Cannot find definition for variable '{}'", var_name),
            span,
        )])?;

    // Get the definition text
    let def_text = &source[def_span.start..def_span.end];

    // Check complexity - don't inline complex expressions unless forced
    let complexity = calculate_complexity(def_expr);
    if complexity > config.max_inline_complexity {
        return Err(vec![Diagnostic::error(
            format!(
                "Variable '{}' has complexity {} which exceeds max inline complexity {}",
                var_name, complexity, config.max_inline_complexity
            ),
            span,
        )]);
    }

    // Check for side effects - don't inline if definition has side effects
    // and is used multiple times
    let has_side_effects = expr_has_side_effects(def_expr);
    let usages = find_variable_usages(ast, &var_name);

    if has_side_effects && usages.len() > 1 {
        return Err(vec![Diagnostic::error(
            format!(
                "Cannot inline variable '{}' with side effects used multiple times",
                var_name
            ),
            span,
        )]);
    }

    // Build the new source
    let mut new_source = source.to_string();
    let mut change_locations = vec![];

    // Remove the original let binding if inlining all uses
    // For now, we inline all uses
    let let_stmt_span = find_let_statement_span(ast, def_span)
        .unwrap_or(def_span);

    // Find and replace all usages
    let mut offset: isize = 0;
    for usage_span in &usages {
        let adjusted_start = (*usage_span.start as isize + offset) as usize;
        let adjusted_end = (*usage_span.end as isize + offset) as usize;

        // Check if this is the last usage - we can remove the let binding
        let is_last_usage = usage_span == usages.last().unwrap();

        // Replace the variable with its definition
        // Wrap in parentheses if needed for precedence
        let replacement = if needs_parentheses(def_expr, usage_span, ast) {
            format!("({})", def_text)
        } else {
            def_text.to_string()
        };

        new_source.replace_range(adjusted_start..adjusted_end, &replacement);
        change_locations.push(*usage_span);

        // Update offset for subsequent replacements
        offset += replacement.len() as isize - (usage_span.end - usage_span.start) as isize;

        // Remove the let binding after the last usage is inlined
        if is_last_usage {
            let adjusted_let_start = (let_stmt_span.start as isize + offset) as usize;
            let adjusted_let_end = (let_stmt_span.end as isize + offset) as usize;

            // Include the newline after the let statement
            let adjusted_let_end = if adjusted_let_end < new_source.len()
                && new_source[adjusted_let_end..].starts_with('\n')
            {
                adjusted_let_end + 1
            } else {
                adjusted_let_end
            };

            new_source.replace_range(adjusted_let_start..adjusted_let_end, "");
            change_locations.push(let_stmt_span);
        }
    }

    // Generate tracking annotation
    let tracking_annotation = if config.generate_tracking_annotations {
        Some(format!("@inlined(by=\"{}\", variable=\"{}\")", config.applied_by, var_name))
    } else {
        None
    };

    Ok(RefactoringResult {
        source: new_source,
        diagnostics: vec![],
        kind: crate::RefactoringKind::InlineVariable,
        metadata: RefactoringMetadata {
            semantic_preserving: true,
            requires_verification: true,
            new_symbol: None,
            change_locations,
            tracking_annotation,
        },
    })
}

/// Check if a function can be inlined at the given span
pub fn can_inline_function(ast: &Module, span: Span) -> Result<bool, Vec<Diagnostic>> {
    // Find the function call at the span
    if let Some(expr) = find_expression_at_span(ast, span) {
        // Must be a function call
        if !matches!(expr, Expr::Call { .. }) {
            return Ok(false);
        }

        // Check if we can find the function definition
        if let Some(func_name) = get_called_function_name(expr) {
            if find_function_definition(ast, &func_name).is_some() {
                return Ok(true);
            }
        }
    }
    Ok(false)
}

/// Inline a function call (replace with function body)
pub fn inline_function(
    source: &str,
    ast: &Module,
    span: Span,
    config: &RefactoringConfig,
) -> Result<RefactoringResult, Vec<Diagnostic>> {
    // Find the function call at the span
    let call_expr = find_expression_at_span(ast, span)
        .ok_or_else(|| vec![Diagnostic::error("No function call found at selection", span)])?;

    let (func_name, args) = get_call_info(call_expr)
        .ok_or_else(|| vec![Diagnostic::error("Selection is not a function call", span)])?;

    // Find the function definition
    let func = find_function_definition(ast, &func_name)
        .ok_or_else(|| vec![Diagnostic::error(
            format!("Cannot find definition for function '{}'", func_name),
            span,
        )])?;

    // Check complexity
    let complexity = calculate_function_complexity(func);
    if complexity > config.max_inline_complexity {
        return Err(vec![Diagnostic::error(
            format!(
                "Function '{}' has complexity {} which exceeds max inline complexity {}",
                func_name, complexity, config.max_inline_complexity
            ),
            span,
        )]);
    }

    // Check for recursion - don't inline recursive functions
    if is_recursive_function(func) {
        return Err(vec![Diagnostic::error(
            format!("Cannot inline recursive function '{}'", func_name),
            span,
        )]);
    }

    // Build parameter substitution map
    let param_substitutions = build_param_substitutions(&func.params, &args)
        .map_err(|e| vec![Diagnostic::error(e, span)])?;

    // Get the function body text
    let body_text = extract_function_body(source, func);

    // Substitute parameters with arguments
    let inlined_body = substitute_params(&body_text, &param_substitutions);

    // Build the new source
    let mut new_source = source.to_string();

    // Replace the call with the inlined body
    // Wrap in block if needed
    let replacement = if needs_block_wrapper(call_expr) {
        format!("{{ {} }}", inlined_body)
    } else {
        inlined_body
    };

    new_source.replace_range(span.start..span.end, &replacement);

    // Generate tracking annotation
    let tracking_annotation = if config.generate_tracking_annotations {
        Some(format!("@inlined(by=\"{}\", function=\"{}\")", config.applied_by, func_name))
    } else {
        None
    };

    Ok(RefactoringResult {
        source: new_source,
        diagnostics: vec![],
        kind: crate::RefactoringKind::InlineFunction,
        metadata: RefactoringMetadata {
            semantic_preserving: true,
            requires_verification: true,
            new_symbol: None,
            change_locations: vec![span],
            tracking_annotation,
        },
    })
}

/// Get the name of a variable from an expression
fn get_variable_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Variable(ident) => Some(ident.name.clone()),
        _ => None,
    }
}

/// Find the definition of a variable
fn find_variable_definition(
    ast: &Module,
    var_name: &str,
    before_span: Span,
) -> Option<(Span, &Expr)> {
    // Traverse the AST to find the let binding for this variable
    // that comes before the given span
    None // Placeholder - needs proper AST traversal
}

/// Find all usages of a variable in the AST
fn find_variable_usages(ast: &Module, var_name: &str) -> Vec<Span> {
    // Traverse the AST to find all references to this variable
    vec![] // Placeholder - needs proper AST traversal
}

/// Find the span of the let statement containing the definition
fn find_let_statement_span(ast: &Module, def_span: Span) -> Option<Span> {
    // Find the full let statement that contains this definition
    None // Placeholder - needs proper AST traversal
}

/// Check if an expression has side effects
fn expr_has_side_effects(expr: &Expr) -> bool {
    match expr {
        Expr::Assign { .. } => true,
        Expr::Call { .. } => true, // Conservative: assume function calls have side effects
        Expr::MethodCall { .. } => true,
        Expr::Binary { left, right, .. } => {
            expr_has_side_effects(left) || expr_has_side_effects(right)
        }
        Expr::Unary { expr, .. } => expr_has_side_effects(expr),
        Expr::Block(block) => block.stmts.iter().any(|s| stmt_has_side_effects(s)),
        Expr::If { cond, then_branch, else_branch } => {
            expr_has_side_effects(cond)
                || expr_has_side_effects(then_branch)
                || else_branch.as_ref().map(|e| expr_has_side_effects(e)).unwrap_or(false)
        }
        _ => false,
    }
}

/// Check if a statement has side effects
fn stmt_has_side_effects(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(expr) => expr_has_side_effects(expr),
        Stmt::Assign { .. } => true,
        Stmt::Return(Some(expr)) => expr_has_side_effects(expr),
        _ => false,
    }
}

/// Calculate the complexity of an expression
fn calculate_complexity(expr: &Expr) -> usize {
    match expr {
        Expr::Literal(_) => 1,
        Expr::Variable(_) => 1,
        Expr::Binary { left, right, .. } => {
            1 + calculate_complexity(left) + calculate_complexity(right)
        }
        Expr::Unary { expr, .. } => 1 + calculate_complexity(expr),
        Expr::Call { func, args } => {
            1 + calculate_complexity(func)
                + args.iter().map(|a| calculate_complexity(a)).sum::<usize>()
        }
        Expr::MethodCall { receiver, args, .. } => {
            1 + calculate_complexity(receiver)
                + args.iter().map(|a| calculate_complexity(a)).sum::<usize>()
        }
        Expr::FieldAccess { object, .. } => 1 + calculate_complexity(object),
        Expr::Index { object, index } => {
            1 + calculate_complexity(object) + calculate_complexity(index)
        }
        Expr::Block(block) => {
            block.stmts.iter().map(|s| stmt_complexity(s)).sum::<usize>()
                + block.expr.as_ref().map(|e| calculate_complexity(e)).unwrap_or(0)
        }
        _ => 5, // Default for other expressions
    }
}

/// Calculate the complexity of a statement
fn stmt_complexity(stmt: &Stmt) -> usize {
    match stmt {
        Stmt::Let { value, .. } => 1 + calculate_complexity(value),
        Stmt::Expr(expr) => calculate_complexity(expr),
        Stmt::Assign { value, .. } => 1 + calculate_complexity(value),
        Stmt::Return(Some(expr)) => calculate_complexity(expr),
        _ => 1,
    }
}

/// Calculate the complexity of a function
fn calculate_function_complexity(func: &Function) -> usize {
    calculate_complexity(&func.body)
}

/// Check if a function is recursive
fn is_recursive_function(func: &Function) -> bool {
    // Check if the function body contains a call to itself
    contains_call_to(&func.body, &func.name.name)
}

/// Check if an expression contains a call to a specific function
fn contains_call_to(expr: &Expr, func_name: &str) -> bool {
    match expr {
        Expr::Call { func, .. } => {
            if let Expr::Variable(ident) = func.as_ref() {
                if ident.name == func_name {
                    return true;
                }
            }
            contains_call_to(func, func_name)
        }
        Expr::Binary { left, right, .. } => {
            contains_call_to(left, func_name) || contains_call_to(right, func_name)
        }
        Expr::Unary { expr, .. } => contains_call_to(expr, func_name),
        Expr::Block(block) => {
            block.stmts.iter().any(|s| stmt_contains_call_to(s, func_name))
                || block.expr.as_ref().map(|e| contains_call_to(e, func_name)).unwrap_or(false)
        }
        _ => false,
    }
}

/// Check if a statement contains a call to a specific function
fn stmt_contains_call_to(stmt: &Stmt, func_name: &str) -> bool {
    match stmt {
        Stmt::Expr(expr) => contains_call_to(expr, func_name),
        Stmt::Let { value, .. } => contains_call_to(value, func_name),
        Stmt::Assign { value, .. } => contains_call_to(value, func_name),
        Stmt::Return(Some(expr)) => contains_call_to(expr, func_name),
        _ => false,
    }
}

/// Get the name of the called function from a call expression
fn get_called_function_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Call { func, .. } => {
            if let Expr::Variable(ident) = func.as_ref() {
                Some(ident.name.clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Get call information (function name and arguments)
fn get_call_info(expr: &Expr) -> Option<(String, Vec<&Expr>)> {
    match expr {
        Expr::Call { func, args } => {
            if let Expr::Variable(ident) = func.as_ref() {
                Some((ident.name.clone(), args.iter().collect()))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Find a function definition by name
fn find_function_definition<'a>(ast: &'a Module, func_name: &str) -> Option<&'a Function> {
    for item in &ast.items {
        if let jet_parser::ast::ModuleItem::Function(func) = item {
            if func.name.name == func_name {
                return Some(func);
            }
        }
    }
    None
}

/// Build parameter substitutions from function params and call arguments
fn build_param_substitutions(
    params: &[jet_parser::ast::Param],
    args: &[&Expr],
) -> Result<HashMap<String, String>, String> {
    if params.len() != args.len() {
        return Err(format!(
            "Parameter count mismatch: expected {}, got {}",
            params.len(),
            args.len()
        ));
    }

    let mut substitutions = HashMap::new();
    for (param, arg) in params.iter().zip(args.iter()) {
        let param_name = pattern_to_name(&param.pattern)?;
        // In a real implementation, we'd serialize the argument expression
        substitutions.insert(param_name, "arg".to_string());
    }

    Ok(substitutions)
}

/// Convert a pattern to a name
fn pattern_to_name(pattern: &Pattern) -> Result<String, String> {
    match pattern {
        Pattern::Ident { name, .. } => Ok(name.name.clone()),
        _ => Err("Complex patterns not supported for inlining".to_string()),
    }
}

/// Extract the function body as text
fn extract_function_body(source: &str, func: &Function) -> String {
    // Extract the body from the source
    // This is a simplified version
    let body_start = func.body.span().start;
    let body_end = func.body.span().end;
    source[body_start..body_end].to_string()
}

/// Substitute parameters with arguments in the body text
fn substitute_params(body: &str, substitutions: &HashMap<String, String>) -> String {
    let mut result = body.to_string();
    for (param, arg) in substitutions {
        // Simple text replacement - in practice, this needs to be more sophisticated
        // to avoid replacing substrings of other identifiers
        result = result.replace(param, arg);
    }
    result
}

/// Check if parentheses are needed around inlined expression
fn needs_parentheses(expr: &Expr, _usage_span: &Span, _ast: &Module) -> bool {
    match expr {
        Expr::Binary { .. } => true,
        _ => false,
    }
}

/// Check if a block wrapper is needed for inlined function
fn needs_block_wrapper(_expr: &Expr) -> bool {
    // Check if the inlined body needs to be wrapped in a block
    false
}

/// Find an expression at the given span
fn find_expression_at_span(ast: &Module, span: Span) -> Option<&Expr> {
    // This is a simplified version - in practice, we'd traverse the AST
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_variable_name() {
        let ident = Ident::new("foo".to_string(), Span::default());
        let expr = Expr::Variable(ident);
        assert_eq!(get_variable_name(&expr), Some("foo".to_string()));
    }

    #[test]
    fn test_expr_has_side_effects() {
        // Literal has no side effects
        let lit = Expr::Literal(jet_parser::ast::Literal::Integer(42));
        assert!(!expr_has_side_effects(&lit));
    }

    #[test]
    fn test_calculate_complexity() {
        let lit = Expr::Literal(jet_parser::ast::Literal::Integer(42));
        assert_eq!(calculate_complexity(&lit), 1);
    }
}
