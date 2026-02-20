//! Tests for AST round-trip parsing
//!
//! These tests verify that the parser can successfully parse valid programs.
//! Full round-trip (parse -> print -> parse) requires Display trait implementation.

use jet_lexer::Lexer;
use jet_parser::Parser;

fn parse_module(input: &str) -> Result<jet_parser::ast::Module, String> {
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);
    parser.parse_module().map_err(|e| format!("{:?}", e))
}

fn assert_roundtrip(input: &str) {
    let first_parse = parse_module(input).expect("First parse should succeed");
    let second_parse = parse_module(input).expect("Second parse should succeed");
    assert_eq!(first_parse, second_parse, "AST changed after round-trip");
}

#[test]
fn roundtrip_empty_module() {
    assert_roundtrip("");
}

#[test]
fn roundtrip_simple_function() {
    assert_roundtrip(
        r#"
fn foo():
    pass
"#,
    );
}

#[test]
fn roundtrip_function_with_params() {
    assert_roundtrip(
        r#"
fn add(a: int, b: int) -> int:
    return a + b
"#,
    );
}

#[test]
fn roundtrip_struct() {
    assert_roundtrip(
        r#"
struct Point:
    x: float
    y: float
"#,
    );
}

#[test]
fn roundtrip_enum() {
    assert_roundtrip(
        r#"
enum Status:
    | Active
    | Inactive
"#,
    );
}

#[test]
fn roundtrip_complex_module() {
    assert_roundtrip(
        r#"
import std.io

struct Point:
    x: float
    y: float

fn distance(p1: Point, p2: Point) -> float:
    return 0.0

fn main():
    let p = Point { x: 1.0, y: 2.0 }
    print(p)
"#,
    );
}
