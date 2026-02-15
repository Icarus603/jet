//! Tests for control flow parsing

use jet_lexer::Lexer;
use jet_parser::Parser;

fn parse_module(input: &str) -> Result<jet_parser::ast::Module, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    parser.parse_module().map_err(|e| format!("{:?}", e))
}

fn assert_parses(input: &str) {
    match parse_module(input) {
        Ok(_) => {}
        Err(e) => panic!("Expected parsing to succeed, but got error: {}", e),
    }
}

fn assert_parse_fails(input: &str) {
    if parse_module(input).is_ok() {
        panic!("Expected parsing to fail, but it succeeded");
    }
}

#[test]
fn test_if_simple() {
    assert_parses(
        r#"
fn test():
    if x:
        do_something()
"#,
    );
}

#[test]
fn test_if_else() {
    assert_parses(
        r#"
fn test():
    if x:
        do_this()
    else:
        do_that()
"#,
    );
}

#[test]
fn test_if_elif() {
    assert_parses(
        r#"
fn test():
    if a:
        do_a()
    elif b:
        do_b()
"#,
    );
}

#[test]
fn test_if_elif_else() {
    assert_parses(
        r#"
fn test():
    if a:
        do_a()
    elif b:
        do_b()
    else:
        do_c()
"#,
    );
}

#[test]
fn test_while() {
    assert_parses(
        r#"
fn test():
    while x < 10:
        x = x + 1
"#,
    );
}

#[test]
fn test_while_with_label() {
    assert_parses(
        r#"
fn test():
    'outer: while true:
        do_something()
"#,
    );
}

#[test]
fn test_for() {
    assert_parses(
        r#"
fn test():
    for x in items:
        process(x)
"#,
    );
}

#[test]
fn test_for_with_range() {
    assert_parses(
        r#"
fn test():
    for i in 0..10:
        process(i)
"#,
    );
}

#[test]
fn test_match() {
    assert_parses(
        r#"
fn test():
    match x:
        | A => do_a()
        | B => do_b()
"#,
    );
}

#[test]
fn test_loop() {
    assert_parses(
        r#"
fn test():
    loop:
        do_something()
"#,
    );
}

#[test]
fn test_if_missing_condition() {
    assert_parse_fails(
        r#"
fn test():
    if:
        do_something()
"#,
    );
}

#[test]
fn test_if_missing_body() {
    assert_parse_fails(
        r#"
fn test():
    if x:
"#,
    );
}

#[test]
fn test_while_missing_condition() {
    assert_parse_fails(
        r#"
fn test():
    while:
        do_something()
"#,
    );
}
