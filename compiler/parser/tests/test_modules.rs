//! Tests for module and import parsing

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
fn test_import_simple() {
    assert_parses("import std.io");
}

#[test]
fn test_import_with_alias() {
    assert_parses("import std.io as io");
}

#[test]
fn test_from_import() {
    assert_parses("from std.io import File");
}

#[test]
fn test_from_import_multiple() {
    assert_parses("from std.io import File, Reader, Writer");
}

#[test]
fn test_pub_import() {
    assert_parses("pub import std.io");
}

#[test]
fn test_empty_module() {
    assert_parses("");
}

#[test]
fn test_module_with_function() {
    assert_parses(
        r#"
fn main():
    print("Hello, World!")
"#,
    );
}

#[test]
fn test_module_with_imports() {
    assert_parses(
        r#"
import std.io
import std.collections

fn main():
    pass
"#,
    );
}

#[test]
fn test_module_complex() {
    assert_parses(
        r#"
import std.io

struct Point:
    x: float
    y: float

fn distance(p1: Point, p2: Point) -> float:
    return 0.0
"#,
    );
}

#[test]
fn test_import_missing_path() {
    assert_parse_fails("import");
}

#[test]
fn test_from_import_missing_items() {
    assert_parse_fails("from std.io import");
}
