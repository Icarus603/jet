//! Tests for definition parsing

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
fn test_function_simple() {
    assert_parses(
        r#"
fn foo():
    pass
"#,
    );
}

#[test]
fn test_function_with_params() {
    assert_parses(
        r#"
fn add(a: int, b: int) -> int:
    return a + b
"#,
    );
}

#[test]
fn test_function_public() {
    assert_parses(
        r#"
pub fn public_func():
    pass
"#,
    );
}

#[test]
fn test_function_generic() {
    assert_parses(
        r#"
fn identity[T](x: T) -> T:
    return x
"#,
    );
}

#[test]
fn test_function_async() {
    assert_parses(
        r#"
async fn fetch() -> Data:
    return await load()
"#,
    );
}

#[test]
fn test_struct_simple() {
    assert_parses(
        r#"
struct Point:
    x: float
    y: float
"#,
    );
}

#[test]
fn test_struct_public() {
    assert_parses(
        r#"
pub struct PublicStruct:
    field: int
"#,
    );
}

#[test]
fn test_struct_generic() {
    assert_parses(
        r#"
struct Box[T]:
    value: T
"#,
    );
}

#[test]
fn test_enum_simple() {
    assert_parses(
        r#"
enum Status:
    | Active
    | Inactive
"#,
    );
}

#[test]
fn test_enum_with_data() {
    assert_parses(
        r#"
enum Option[T]:
    | Some(T)
    | None
"#,
    );
}

#[test]
fn test_trait() {
    assert_parses(
        r#"
trait Show:
    fn show(self) -> string
"#,
    );
}

#[test]
fn test_impl() {
    assert_parses(
        r#"
impl Point:
    fn distance(self, other: Point) -> float:
        return 0.0
"#,
    );
}

#[test]
fn test_impl_trait() {
    assert_parses(
        r#"
impl Show for Point:
    fn show(self) -> string:
        return "Point"
"#,
    );
}

#[test]
fn test_type_alias() {
    assert_parses(
        r#"
type StringVec = Vec[string]
"#,
    );
}

#[test]
fn test_const() {
    assert_parses(
        r#"
const PI: float = 3.14159
"#,
    );
}

#[test]
fn test_function_missing_name() {
    assert_parse_fails(
        r#"
fn () -> int:
    return 0
"#,
    );
}

#[test]
fn test_function_missing_body() {
    assert_parse_fails("fn foo() -> int");
}

#[test]
fn test_struct_missing_name() {
    assert_parse_fails(
        r#"
struct:
    x: int
"#,
    );
}
