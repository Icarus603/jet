//! Tests for expression parsing
//!
//! Covers all expression types and operator precedence.

use jet_lexer::Lexer;
use jet_parser::Parser;

fn parse_expr(input: &str) -> Result<jet_parser::ast::Expr, String> {
    let wrapped = format!("fn __test__():\n    {}", input);
    let mut lexer = Lexer::new(&wrapped);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);

    match parser.parse_module() {
        Ok(module) => {
            if let Some(jet_parser::ast::ModuleItem::Function(func)) = module.items.first() {
                Ok(func.body.clone())
            } else {
                Err("Failed to extract expression".to_string())
            }
        }
        Err(e) => Err(format!("{:?}", e)),
    }
}

fn assert_parses(input: &str) {
    match parse_expr(input) {
        Ok(_) => {}
        Err(e) => panic!("Expected '{}' to parse, but got error: {}", input, e),
    }
}

fn assert_parse_fails(input: &str) {
    if parse_expr(input).is_ok() {
        panic!("Expected '{}' to fail parsing, but it succeeded", input);
    }
}

// =============================================================================
// Literal Expressions
// =============================================================================

#[test]
fn test_integer_literal() {
    assert_parses("42");
    assert_parses("0");
    assert_parses("-42");
}

#[test]
fn test_float_literal() {
    assert_parses("3.14");
    assert_parses("0.0");
    assert_parses("1e10");
}

#[test]
fn test_string_literal() {
    assert_parses("\"hello\"");
    assert_parses("\"\"");
    assert_parses("\"hello world\"");
}

#[test]
fn test_bool_literals() {
    assert_parses("true");
    assert_parses("false");
}

#[test]
fn test_unit_literal() {
    assert_parses("()");
}

// =============================================================================
// Identifier Expressions
// =============================================================================

#[test]
fn test_simple_identifier() {
    assert_parses("x");
    assert_parses("foo");
}

#[test]
fn test_hole_expression() {
    // Hole expression for hole-driven development
    let result = parse_expr("_");
    assert!(result.is_ok(), "Expected '_' to parse as hole expression");
    match result {
        Ok(jet_parser::ast::Expr::Hole(_)) => {}
        Ok(jet_parser::ast::Expr::Block(block))
            if matches!(
                block.stmts.first(),
                Some(jet_parser::ast::Stmt::Expr(expr))
                    if matches!(expr.as_ref(), jet_parser::ast::Expr::Hole(_))
            ) => {}
        Ok(other) => panic!("Expected Hole expression, got {:?}", other),
        Err(e) => panic!("Parse error: {}", e),
    }

    // Holes can appear in various expression contexts
    assert_parses("let x = _");
    assert_parses("foo(_)");
    assert_parses("_ + 1");
    assert_parses("(1, _)");
}

// =============================================================================
// Arithmetic Expressions
// =============================================================================

#[test]
fn test_addition() {
    assert_parses("1 + 2");
}

#[test]
fn test_subtraction() {
    assert_parses("5 - 3");
}

#[test]
fn test_multiplication() {
    assert_parses("4 * 5");
}

#[test]
fn test_division() {
    assert_parses("10 / 2");
}

#[test]
fn test_modulo() {
    assert_parses("10 % 3");
}

#[test]
fn test_exponentiation() {
    assert_parses("2 ** 3");
}

#[test]
fn test_arithmetic_precedence() {
    assert_parses("1 + 2 * 3");
    assert_parses("(1 + 2) * 3");
    assert_parses("10 - 3 + 2");
    assert_parses("2 * 3 ** 2");
}

#[test]
fn test_parentheses() {
    assert_parses("(1 + 2)");
    assert_parses("((x))");
}

// =============================================================================
// Comparison Expressions
// =============================================================================

#[test]
fn test_equality() {
    assert_parses("x == y");
}

#[test]
fn test_not_equal() {
    assert_parses("x != y");
}

#[test]
fn test_less_than() {
    assert_parses("x < y");
}

#[test]
fn test_greater_than() {
    assert_parses("x > y");
}

#[test]
fn test_less_than_or_equal() {
    assert_parses("x <= y");
}

#[test]
fn test_greater_than_or_equal() {
    assert_parses("x >= y");
}

// =============================================================================
// Logical Expressions
// =============================================================================

#[test]
fn test_logical_and() {
    assert_parses("a and b");
}

#[test]
fn test_logical_or() {
    assert_parses("a or b");
}

#[test]
fn test_logical_not() {
    assert_parses("not x");
}

// =============================================================================
// Bitwise Expressions
// =============================================================================

#[test]
fn test_bitwise_and() {
    assert_parses("a & b");
}

#[test]
fn test_bitwise_or() {
    assert_parses("a | b");
}

#[test]
fn test_bitwise_xor() {
    assert_parses("a ^ b");
}

#[test]
fn test_bitwise_not() {
    assert_parses("~a");
}

#[test]
fn test_bitwise_shifts() {
    assert_parses("a << 2");
    assert_parses("a >> 2");
}

// =============================================================================
// Function Calls
// =============================================================================

#[test]
fn test_simple_function_call() {
    assert_parses("foo()");
}

#[test]
fn test_function_call_with_args() {
    assert_parses("foo(a, b, c)");
}

#[test]
fn test_nested_function_calls() {
    assert_parses("foo(bar())");
}

// =============================================================================
// Method Calls
// =============================================================================

#[test]
fn test_simple_method_call() {
    assert_parses("obj.method()");
}

#[test]
fn test_method_call_with_args() {
    assert_parses("obj.method(arg1, arg2)");
}

#[test]
fn test_chained_method_calls() {
    assert_parses("obj.method1().method2()");
}

// =============================================================================
// Field Access
// =============================================================================

#[test]
fn test_simple_field_access() {
    assert_parses("obj.field");
}

#[test]
fn test_chained_field_access() {
    assert_parses("obj.field1.field2");
}

// =============================================================================
// Index Expressions
// =============================================================================

#[test]
fn test_simple_index() {
    assert_parses("arr[0]");
}

#[test]
fn test_chained_index() {
    assert_parses("arr[0][1]");
}

// =============================================================================
// Lambda Expressions
// =============================================================================

#[test]
fn test_lambda_single_param() {
    assert_parses("|x| x + 1");
}

#[test]
fn test_lambda_multiple_params() {
    assert_parses("|x, y| x + y");
}

#[test]
fn test_lambda_no_params() {
    assert_parses("|| 42");
}

#[test]
fn test_lambda_typed_params() {
    assert_parses("|x: i32| x + 1");
    assert_parses("|x: i32, y: i32| x + y");
}

#[test]
fn test_lambda_return_type() {
    assert_parses("|x: i32| -> i32 x + 1");
    assert_parses("|x, y| -> i32 x + y");
}

#[test]
fn test_lambda_block_body() {
    assert_parses("|x| { x + 1 }");
    assert_parses("|x: i32| -> i32 { x + 1 }");
}

#[test]
fn test_lambda_full_syntax() {
    // Full syntax: |params| -> return_type { body }
    assert_parses("|x: i32, y: i32| -> i32 { x + y }");
}

// =============================================================================
// Tuple Expressions
// =============================================================================

#[test]
fn test_empty_tuple() {
    assert_parses("()");
}

#[test]
fn test_single_element_tuple() {
    assert_parses("(x,)");
}

#[test]
fn test_two_element_tuple() {
    assert_parses("(a, b)");
}

// =============================================================================
// Array Expressions
// =============================================================================

#[test]
fn test_empty_array() {
    assert_parses("[]");
}

#[test]
fn test_array_literal() {
    assert_parses("[1, 2, 3]");
}

// =============================================================================
// Range Expressions
// =============================================================================

#[test]
fn test_exclusive_range() {
    assert_parses("0..10");
}

#[test]
fn test_inclusive_range() {
    assert_parses("0...10");
}

// =============================================================================
// Try Operator
// =============================================================================

#[test]
fn test_try_operator() {
    assert_parses("result?");
}

// =============================================================================
// Await Expression
// =============================================================================

#[test]
fn test_await_expression() {
    assert_parses("future.await");
}

// =============================================================================
// Complex Expressions
// =============================================================================

#[test]
fn test_complex_expression_1() {
    assert_parses("arr[i].field.method(arg1, arg2)");
}

#[test]
fn test_complex_expression_2() {
    assert_parses("(a + b) * (c - d) / e");
}

// =============================================================================
// Error Cases
// =============================================================================

#[test]
fn test_unclosed_parenthesis() {
    assert_parse_fails("(1 + 2");
}

#[test]
fn test_unclosed_bracket() {
    assert_parse_fails("[1, 2, 3");
}
