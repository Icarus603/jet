//! Extract variable and function refactoring operations
//!
//! This module provides semantic-preserving extraction operations:
//! - Extract Variable: Move an expression into a new let binding
//! - Extract Function: Move a block of code into a new function

use crate::{RefactoringConfig, RefactoringMetadata, RefactoringResult};
use jet_diagnostics::{Diagnostic, Span};
use jet_parser::ast::{Expr, Function, Ident, LetBinding, Module, Pattern, Stmt};
use std::collections::HashSet;

/// Check if a variable can be extracted from the given span
pub fn can_extract_variable(ast: &Module, span: Span) -> Result<bool, Vec<Diagnostic>> {
    // Find the expression at the given span
    if let Some(expr) = find_expression_at_span(ast, span) {
        // Can't extract if it's already just a variable
        if matches!(expr, Expr::Variable(_)) {
            return Ok(false);
        }
        // Can't extract if it's a simple literal that's not worth extracting
        if matches!(expr, Expr::Literal(_)) {
            // Allow extracting complex literals like strings or arrays
            return Ok(true);
        }
        return Ok(true);
    }
    Ok(false)
}

/// Extract an expression into a variable
pub fn extract_variable(
    source: &str,
    ast: &Module,
    span: Span,
    config: &RefactoringConfig,
) -> Result<RefactoringResult, Vec<Diagnostic>> {
    // Find the expression to extract
    let expr = find_expression_at_span(ast, span)
        .ok_or_else(|| vec![Diagnostic::error("No expression found at selection", span)])?;

    // Get the expression text
    let expr_text = &source[span.start..span.end];

    // Generate a variable name
    let var_name = generate_variable_name(expr, expr_text);

    // Find the enclosing function to determine scope
    let enclosing_func = find_enclosing_function(ast, span);

    // Find all usages of this expression (for potential deduplication)
    let _usages = find_similar_expressions(ast, expr);

    // Determine if the variable should be mutable
    let is_mutable = expr_contains_mutation(expr);

    // Build the let binding
    let let_keyword = if config.prefer_immutable && !is_mutable {
        "let"
    } else {
        "let mut"
    };

    // Find the insertion point (start of the statement containing the expression)
    let insertion_point = find_statement_start(ast, span)
        .unwrap_or(span.start);

    // Get the indentation at the insertion point
    let indent = get_indentation(source, insertion_point);

    // Build the new source
    let mut new_source = source.to_string();

    // Create the let binding
    let let_binding = if config.generate_tracking_annotations {
        format!(
            "{}@extracted(by=\"{}\")\n{}{} {} = {}\n",
            indent, config.applied_by, indent, let_keyword, var_name, expr_text
        )
    } else {
        format!(
            "{}{} {} = {}\n",
            indent, let_keyword, var_name, expr_text
        )
    };

    // Insert the let binding before the statement
    new_source.insert_str(insertion_point, &let_binding);

    // Replace the expression with the variable name
    // Need to adjust for the inserted text
    let adjusted_span = Span::new(
        span.start + let_binding.len(),
        span.end + let_binding.len(),
    );
    new_source.replace_range(adjusted_span.start..adjusted_span.end, &var_name);

    // Generate tracking annotation if enabled
    let tracking_annotation = if config.generate_tracking_annotations {
        Some(format!("@extracted(by=\"{}\")", config.applied_by))
    } else {
        None
    };

    Ok(RefactoringResult {
        source: new_source,
        diagnostics: vec![],
        kind: crate::RefactoringKind::ExtractVariable,
        metadata: RefactoringMetadata {
            semantic_preserving: true,
            requires_verification: true,
            new_symbol: Some(var_name),
            change_locations: vec![span],
            tracking_annotation,
        },
    })
}

/// Check if a function can be extracted from the given span
pub fn can_extract_function(ast: &Module, span: Span) -> Result<bool, Vec<Diagnostic>> {
    // Find statements in the span
    let statements = find_statements_in_span(ast, span);

    // Need at least one statement to extract
    if statements.is_empty() {
        return Ok(false);
    }

    // Check if the selection is within a function
    if find_enclosing_function(ast, span).is_none() {
        return Ok(false);
    }

    Ok(true)
}

/// Extract statements into a new function
pub fn extract_function(
    source: &str,
    ast: &Module,
    span: Span,
    config: &RefactoringConfig,
) -> Result<RefactoringResult, Vec<Diagnostic>> {
    // Find the statements to extract
    let statements = find_statements_in_span(ast, span);

    if statements.is_empty() {
        return Err(vec![Diagnostic::error(
            "No statements found in selection",
            span,
        )]);
    }

    // Find the enclosing function
    let enclosing_func = find_enclosing_function(ast, span)
        .ok_or_else(|| vec![Diagnostic::error(
            "Cannot extract function outside of a function",
            span,
        )])?;

    // Get the selected text
    let selected_text = &source[span.start..span.end];

    // Generate a function name
    let func_name = generate_function_name(&statements, selected_text);

    // Analyze what variables are used (for parameters) and defined (for return)
    let (params, return_expr) = analyze_extraction_boundary(ast, &statements, span);

    // Build the function
    let func_indent = get_indentation(source, enclosing_func.span.start);
    let body_indent = format!("{}    ", func_indent);

    // Format the function body
    let body_text = format_body_for_extraction(selected_text, &body_indent);

    // Build parameter list
    let param_list = params
        .iter()
        .map(|(name, ty)| format!("{}: {}", name, ty))
        .collect::<Vec<_>>()
        .join(", ");

    // Determine return type
    let return_type = return_expr.as_ref().map(|_| "-> auto"); // Type inference

    // Build the function definition
    let func_def = if config.generate_tracking_annotations {
        format!(
            "\n{}@extracted(by=\"{}\", from=\"{}\")\n{}fn {}({}) {}:\n{}\n",
            func_indent,
            config.applied_by,
            enclosing_func.name.name,
            func_indent,
            func_name,
            param_list,
            return_type.unwrap_or(""),
            body_text
        )
    } else {
        format!(
            "\n{}fn {}({}) {}:\n{}\n",
            func_indent,
            func_name,
            param_list,
            return_type.unwrap_or(""),
            body_text
        )
    };

    // Build the function call to replace the extracted code
    let arg_list = params
        .iter()
        .map(|(name, _)| name.clone())
        .collect::<Vec<_>>()
        .join(", ");

    let func_call = if arg_list.is_empty() {
        format!("{}()", func_name)
    } else {
        format!("{}({})", func_name, arg_list)
    };

    // Build the new source
    let mut new_source = source.to_string();

    // Find insertion point (after the enclosing function)
    let insert_pos = find_function_end(source, enclosing_func.span.end);

    // Insert the new function
    new_source.insert_str(insert_pos, &func_def);

    // Replace the extracted code with the function call
    // Need to adjust for the inserted function
    let adjusted_span = Span::new(span.start, span.end);
    new_source.replace_range(adjusted_span.start..adjusted_span.end, &func_call);

    // Generate tracking annotation
    let tracking_annotation = if config.generate_tracking_annotations {
        Some(format!(
            "@extracted(by=\"{}\", from=\"{}\")",
            config.applied_by, enclosing_func.name.name
        ))
    } else {
        None
    };

    Ok(RefactoringResult {
        source: new_source,
        diagnostics: vec![],
        kind: crate::RefactoringKind::ExtractFunction,
        metadata: RefactoringMetadata {
            semantic_preserving: true,
            requires_verification: true,
            new_symbol: Some(func_name),
            change_locations: vec![span],
            tracking_annotation,
        },
    })
}

/// Generate a variable name from an expression
fn generate_variable_name(expr: &Expr, expr_text: &str) -> String {
    // Try to derive name from the expression structure
    let suggested = match expr {
        Expr::Call { func, .. } => {
            if let Expr::Variable(ident) = func.as_ref() {
                // Convert function call to variable name
                camel_to_snake(&ident.name)
            } else {
                "result".to_string()
            }
        }
        Expr::MethodCall { method, .. } => {
            camel_to_snake(&method.name)
        }
        Expr::FieldAccess { field, .. } => {
            field.name.clone()
        }
        Expr::Binary { op, .. } => {
            match op {
                jet_parser::ast::BinaryOp::Add => "sum",
                jet_parser::ast::BinaryOp::Sub => "difference",
                jet_parser::ast::BinaryOp::Mul => "product",
                jet_parser::ast::BinaryOp::Div => "quotient",
                _ => "result",
            }
            .to_string()
        }
        _ => "extracted".to_string(),
    };

    // If suggestion is too generic, try to use the expression text
    if suggested == "result" || suggested == "extracted" {
        // Extract first identifier from expression text
        expr_text
            .split_whitespace()
            .next()
            .and_then(|word| word.chars().filter(|c| c.is_alphanumeric() || *c == '_').collect::<String>().parse().ok())
            .unwrap_or_else(|| "extracted".to_string())
    } else {
        suggested
    }
}

/// Generate a function name from statements
fn generate_function_name(statements: &[&Stmt], selected_text: &str) -> String {
    // Analyze the statements to suggest a good name
    for stmt in statements {
        match stmt {
            Stmt::Expr(expr) | Stmt::Return(Some(expr)) => {
                if let Expr::Call { func, .. } = expr.as_ref() {
                    if let Expr::Variable(ident) = func.as_ref() {
                        return format!("process_{}", camel_to_snake(&ident.name));
                    }
                }
            }
            _ => {}
        }
    }

    // Check for common patterns
    if selected_text.contains("if ") || selected_text.contains("match ") {
        "check_condition".to_string()
    } else if selected_text.contains("for ") || selected_text.contains("while ") {
        "process_items".to_string()
    } else if selected_text.contains("return ") {
        "compute_result".to_string()
    } else {
        "extracted_function".to_string()
    }
}

/// Convert camelCase to snake_case
fn camel_to_snake(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if i > 0 && c.is_uppercase() {
            result.push('_');
            result.push(c.to_ascii_lowercase());
        } else {
            result.push(c.to_ascii_lowercase());
        }
    }
    result
}

/// Check if an expression contains mutation
fn expr_contains_mutation(expr: &Expr) -> bool {
    match expr {
        Expr::Assign { .. } => true,
        Expr::Call { .. } => {
            // Conservative: assume function calls might mutate
            true
        }
        Expr::MethodCall { .. } => true,
        Expr::Binary { left, right, .. } => {
            expr_contains_mutation(left) || expr_contains_mutation(right)
        }
        Expr::Unary { expr, .. } => expr_contains_mutation(expr),
        _ => false,
    }
}

/// Find an expression at the given span
fn find_expression_at_span(ast: &Module, span: Span) -> Option<&Expr> {
    // This is a simplified version - in practice, we'd traverse the AST
    // For now, return None to indicate we need proper AST traversal
    None
}

/// Find all similar expressions in the AST
fn find_similar_expressions<'a>(ast: &'a Module, expr: &Expr) -> Vec<&'a Expr> {
    // This would find duplicate expressions that could be replaced
    // with the new variable
    vec![]
}

/// Find the enclosing function for a span
fn find_enclosing_function<'a>(ast: &'a Module, span: Span) -> Option<&'a Function> {
    for item in &ast.items {
        if let jet_parser::ast::ModuleItem::Function(func) = item {
            if func.span.start <= span.start && func.span.end >= span.end {
                return Some(func);
            }
        }
    }
    None
}

/// Find the start of the statement containing the span
fn find_statement_start(ast: &Module, span: Span) -> Option<usize> {
    // This would traverse the AST to find the containing statement
    None
}

/// Find statements within a span
fn find_statements_in_span<'a>(ast: &'a Module, span: Span) -> Vec<&'a Stmt> {
    // This would extract all statements that fall within the given span
    vec![]
}

/// Analyze what variables cross the extraction boundary
fn analyze_extraction_boundary(
    ast: &Module,
    statements: &[&Stmt],
    span: Span,
) -> (Vec<(String, String)>, Option<String>) {
    // Returns (parameters, return_expression)
    // This would analyze variable usage to determine:
    // - What variables are used but defined outside (parameters)
    // - What variable is the result (return value)

    let mut used_vars = HashSet::new();
    let mut defined_vars = HashSet::new();

    for stmt in statements {
        match stmt {
            Stmt::Let { pattern, .. } => {
                collect_pattern_idents(pattern, &mut defined_vars);
            }
            _ => {
                // Collect used variables from the statement
            }
        }
    }

    // Parameters are used but not defined in the extracted code
    let params: Vec<(String, String)> = used_vars
        .difference(&defined_vars)
        .map(|name| (name.clone(), "auto".to_string())) // Type inference
        .collect();

    (params, None)
}

/// Collect identifiers from a pattern
fn collect_pattern_idents(pattern: &Pattern, idents: &mut HashSet<String>) {
    match pattern {
        Pattern::Ident { name, .. } => {
            idents.insert(name.name.clone());
        }
        Pattern::Tuple(patterns) => {
            for p in patterns {
                collect_pattern_idents(p, idents);
            }
        }
        Pattern::Struct { fields, .. } => {
            for field in fields {
                if let Some(ref p) = field.pattern {
                    collect_pattern_idents(p, idents);
                }
            }
        }
        _ => {}
    }
}

/// Get indentation at a position
fn get_indentation(source: &str, pos: usize) -> String {
    let line_start = source[..pos].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line_end = source[line_start..]
        .find('\n')
        .map(|i| line_start + i)
        .unwrap_or(source.len());

    let line = &source[line_start..line_end];
    line.chars().take_while(|c| c.is_whitespace()).collect()
}

/// Format body text for extraction into a function
fn format_body_for_extraction(text: &str, indent: &str) -> String {
    text.lines()
        .map(|line| format!("{}{}", indent, line.trim_start()))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Find the end of a function in source text
fn find_function_end(source: &str, func_end: usize) -> usize {
    // Find the next line that starts at column 0 (not indented)
    // after the current function
    let rest = &source[func_end..];

    for (i, line) in rest.lines().enumerate() {
        if i > 0 && !line.starts_with(' ') && !line.starts_with('\t') && !line.is_empty() {
            // Found a non-indented line
            return func_end + rest.lines().take(i).map(|l| l.len() + 1).sum::<usize>();
        }
    }

    source.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_camel_to_snake() {
        assert_eq!(camel_to_snake("fooBar"), "foo_bar");
        assert_eq!(camel_to_snake("getUserName"), "get_user_name");
        assert_eq!(camel_to_snake("HTMLParser"), "h_t_m_l_parser");
    }

    #[test]
    fn test_generate_variable_name() {
        // Test with a simple expression text
        assert_eq!(
            generate_variable_name(&Expr::Literal(jet_parser::ast::Literal::Integer(42)), "fooBar"),
            "fooBar"
        );
    }

    #[test]
    fn test_get_indentation() {
        let source = "line1\n    line2\n        line3";
        assert_eq!(get_indentation(source, 0), "");
        assert_eq!(get_indentation(source, 10), "    ");
    }
}
