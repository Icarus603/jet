//! Tests for parser error recovery

use jet_lexer::Lexer;
use jet_parser::Parser;

fn parse_module(input: &str) -> Result<jet_parser::ast::Module, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    parser.parse_module().map_err(|e| format!("{:?}", e))
}

fn assert_parse_fails(input: &str) {
    if parse_module(input).is_ok() {
        panic!("Expected parsing to fail, but it succeeded");
    }
}

#[test]
fn test_error_unexpected_token() {
    assert_parse_fails("fn foo(:\n    pass");
}

#[test]
fn test_error_missing_closing_paren() {
    assert_parse_fails("fn foo(:\n    pass");
}

#[test]
fn test_error_unclosed_string() {
    assert_parse_fails("\"unclosed string");
}

#[test]
fn test_error_missing_body() {
    assert_parse_fails("fn foo()");
}

#[test]
fn test_error_invalid_assignment_target() {
    assert_parse_fails("5 = x");
}

#[test]
fn test_error_if_missing_condition() {
    assert_parse_fails(
        r#"
if:
    pass
"#,
    );
}

#[test]
fn test_error_while_missing_condition() {
    assert_parse_fails(
        r#"
while:
    pass
"#,
    );
}

#[test]
fn test_error_for_missing_in() {
    assert_parse_fails(
        r#"
for x items:
    pass
"#,
    );
}

#[test]
fn test_error_import_missing_path() {
    assert_parse_fails("import");
}
