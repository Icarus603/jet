//! Additional type inference edge case tests

use jet_lexer::Lexer;
use jet_parser::Parser;
use jet_typeck::type_check_module;

fn type_check_source(source: &str) -> Result<(), Vec<String>> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    let ast = parser
        .parse_module()
        .map_err(|e| vec![format!("{:?}", e)])?;

    match type_check_module(&ast) {
        Ok(_) => Ok(()),
        Err(diagnostics) => Err(diagnostics.iter().map(|d| d.message.clone()).collect()),
    }
}

fn assert_type_checks(source: &str) {
    match type_check_source(source) {
        Ok(_) => {}
        Err(errors) => panic!(
            "Expected type checking to succeed, but got errors: {:?}",
            errors
        ),
    }
}

#[test]
fn test_integer_arithmetic_inference() {
    // Test that integer arithmetic correctly infers int type
    assert_type_checks(
        r#"fn test():
    let a = 5
    let b = 10
    let c = a + b
    let d = a - b
    let e = a * b
    let f = a / b"#,
    );
}

#[test]
fn test_float_arithmetic_inference() {
    // Test that float arithmetic correctly infers float type
    assert_type_checks(
        r#"fn test():
    let a = 5.0
    let b = 10.0
    let c = a + b
    let d = a - b
    let e = a * b
    let f = a / b"#,
    );
}

#[test]
fn test_mixed_literal_expressions() {
    // Test chained arithmetic operations
    assert_type_checks(
        r#"fn test():
    let x = 1 + 2 + 3
    let y = 10 - 5 - 2
    let z = 2 * 3 * 4
    let w = 100 / 10 / 2"#,
    );
}

#[test]
fn test_parenthesized_expressions() {
    // Test that parenthesized expressions preserve types
    assert_type_checks(
        r#"fn test():
    let x = (1 + 2) * 3
    let y = 10 / (2 + 3)
    let z = ((4 + 5) * 2) - 1"#,
    );
}

#[test]
fn test_comparison_operations() {
    // Test that comparison operations work with inferred types
    assert_type_checks(
        r#"fn test():
    let x = 5
    let y = 10
    let a = x < y
    let b = x > y
    let c = x <= y
    let d = x >= y
    let e = x == y
    let f = x != y"#,
    );
}

#[test]
fn test_boolean_operations() {
    // Test that boolean operations work correctly
    assert_type_checks(
        r#"fn test():
    let a = true
    let b = false
    let c = a and b
    let d = a or b
    let e = not a"#,
    );
}

#[test]
fn test_variable_reuse_in_arithmetic() {
    // Test that variables can be used multiple times
    assert_type_checks(
        r#"fn test():
    let x = 5
    let a = x + x
    let b = x * x
    let c = x - x
    let d = x / x"#,
    );
}

#[test]
fn test_function_return_type_inference() {
    // Test that return types are correctly inferred
    assert_type_checks(
        r#"fn add(x: int, y: int) -> int:
    return x + y

fn test():
    let res = add(5, 10)
    let doubled = res * 2"#,
    );
}

#[test]
fn test_nested_function_calls() {
    // Test nested function calls with type inference
    assert_type_checks(
        r#"fn double(x: int) -> int:
    return x * 2

fn square(x: int) -> int:
    return x * x

fn test():
    let a = double(5)
    let b = square(a)
    let c = double(square(3))"#,
    );
}

#[test]
fn test_unary_operations() {
    // Test unary operations
    assert_type_checks(
        r#"fn test():
    let x = 5
    let y = -x
    let z = -(-x)
    let a = not true
    let b = not false"#,
    );
}
