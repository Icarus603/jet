//! Integration tests for the lexer with complete Jet programs

use common::tokenize_kinds;
use jet_lexer::Token;

mod common;

#[test]
fn test_factorial_function() {
    let input = r#"### Calculate factorial
fn factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

fn main():
    print(factorial(5))
"#;

    let tokens = tokenize_kinds(input);

    // Verify key tokens are present
    assert!(tokens.contains(&Token::Fn));
    assert!(tokens.contains(&Token::If));
    assert!(tokens.contains(&Token::Return));
    assert!(tokens.contains(&Token::Indent));
    assert!(tokens.contains(&Token::Dedent));

    // Check for proper structure
    let fn_count = tokens.iter().filter(|t| matches!(t, Token::Fn)).count();
    assert_eq!(fn_count, 2, "Should have 2 function definitions");
}

#[test]
fn test_struct_definition() {
    let input = r#"struct Point:
    x: float
    y: float

fn new_point(x: float, y: float) -> Point:
    return Point { x: x, y: y }
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Struct));
    assert!(tokens.contains(&Token::Fn));
    assert!(tokens.contains(&Token::Return));
    assert!(tokens.contains(&Token::LBrace));
    assert!(tokens.contains(&Token::RBrace));
}

#[test]
fn test_enum_definition() {
    let input = r#"enum Option:
    Some(value)
    None

fn unwrap_or(opt: Option, default):
    match opt:
        Some(v) => v
        None => default
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Enum));
    assert!(tokens.contains(&Token::Match));
    assert!(tokens.contains(&Token::FatArrow));
}

#[test]
fn test_generic_function() {
    let input = r#"fn identity<T>(x: T) -> T:
    return x

fn main():
    let n = identity::<int>(42)
    let s = identity::<string>("hello")
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Fn));
    assert!(tokens.contains(&Token::Let));
    assert!(tokens.contains(&Token::Return));
}

#[test]
fn test_list_operations() {
    let input = r#"fn sum(list: [int]) -> int:
    let total = 0
    for item in list:
        total = total + item
    return total

fn main():
    let numbers = [1, 2, 3, 4, 5]
    print(sum(numbers))
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::For));
    assert!(tokens.contains(&Token::In));
    assert!(tokens.contains(&Token::LBracket));
    assert!(tokens.contains(&Token::RBracket));
}

#[test]
fn test_pattern_matching() {
    let input = r#"fn describe(x):
    match x:
        0 => "zero"
        1 => "one"
        n if n < 0 => "negative"
        _ => "positive"
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Match));
    assert!(tokens.contains(&Token::FatArrow));
    assert!(tokens.contains(&Token::Underscore));
}

#[test]
fn test_concurrent_code() {
    let input = r#"fn main():
    let ch = chan<int>()

    spawn:
        ch.send(42)

    let value = ch.recv()
    print(value)
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Chan));
    assert!(tokens.contains(&Token::Spawn));
}

#[test]
fn test_async_await() {
    let input = r#"async fn fetch_data() -> string:
    let response = await http_get("https://example.com")
    return response.body

fn main():
    let data = await fetch_data()
    print(data)
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Async));
    assert!(tokens.contains(&Token::Await));
}

#[test]
fn test_trait_definition() {
    let input = r#"trait Printable:
    fn print(self)

impl Printable for int:
    fn print(self):
        io.println(self.to_string())
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Trait));
    assert!(tokens.contains(&Token::Fn));
}

#[test]
fn test_module_import() {
    let input = r#"import std.io
import std.collections.{List, Map}

use std.math.{sin, cos, tan}

fn main():
    io.println("Hello, World!")
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Import));
    assert!(tokens.contains(&Token::Use));
    assert!(tokens.contains(&Token::LBrace));
    assert!(tokens.contains(&Token::RBrace));
}

#[test]
fn test_error_handling() {
    let input = r#"fn divide(a: float, b: float) -> float:
    if b == 0.0:
        raise DivisionByZeroError
    return a / b

fn main():
    try:
        let result = divide(10.0, 0.0)
    catch e:
        print("Error: " + e.message)
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Raise));
}

#[test]
fn test_complex_expression() {
    let input = r#"fn calculate():
    let a = 1 + 2 * 3 - 4 / 5
    let b = (1 + 2) * (3 - 4)
    let c = a ** 2 + b ** 2
    let d = a & b | c ^ 0xFF
    let e = a << 2 | b >> 1
    return a + b + c + d + e
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Plus));
    assert!(tokens.contains(&Token::Minus));
    assert!(tokens.contains(&Token::Star));
    assert!(tokens.contains(&Token::Slash));
    assert!(tokens.contains(&Token::Power));
    assert!(tokens.contains(&Token::AndBit));
    assert!(tokens.contains(&Token::OrBit));
    assert!(tokens.contains(&Token::Xor));
    assert!(tokens.contains(&Token::Shl));
    assert!(tokens.contains(&Token::Shr));
}

#[test]
fn test_closure() {
    let input = r#"fn main():
    let add = |x, y| x + y
    let result = add(1, 2)
    print(result)

    let nums = [1, 2, 3, 4, 5]
    let evens = nums.filter(|n| n % 2 == 0)
"#;

    let tokens = tokenize_kinds(input);

    // Check for closure parameters
    let or_count = tokens.iter().filter(|t| matches!(t, Token::OrBit)).count();
    assert!(or_count >= 2, "Should have | for closures");
}

#[test]
fn test_type_annotations() {
    let input = r#"fn process(data: [int], config: Config) -> Result<string, Error>:
    let count: int = data.len()
    let name: string = config.name
    return Ok(name)
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Colon));
    assert!(tokens.contains(&Token::Arrow));
}

#[test]
fn test_complex_indentation() {
    let input = r#"fn outer():
    let a = 1
    if true:
        let b = 2
        for i in range(10):
            if i % 2 == 0:
                print(i)
            else:
                continue
        let c = 3
    let d = 4

fn inner():
    pass
"#;

    let tokens = tokenize_kinds(input);

    // Count indentation tokens
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    assert_eq!(
        indent_count, dedent_count,
        "Indent and dedent should be balanced"
    );
}

#[test]
fn test_comments() {
    let input = r#"# This is a single-line comment
fn main():  # inline comment
    let x = 5  # another comment
    # Comment on its own line
    print(x)
"#;

    let tokens = tokenize_kinds(input);

    // Comments should be skipped, verify the structure is still correct
    assert!(tokens.contains(&Token::Fn));
    assert!(tokens.contains(&Token::Let));
    // print is an identifier, not a keyword
    assert!(tokens
        .iter()
        .any(|t| matches!(t, Token::Ident(s) if s == "print")));
}

#[test]
fn test_doc_comments() {
    let input = r#"### This is a doc comment
### It spans multiple lines
fn documented():
    pass
"#;

    let tokens = tokenize_kinds(input);

    // Doc comments are currently treated as errors (placeholder)
    // This test documents the current behavior
    assert!(tokens.contains(&Token::Fn));
}

#[test]
fn test_unicode_in_code() {
    let input = r#"fn greet():
    let message = "Hello, 世界! 🌍"
    print(message)
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Fn));
    assert!(tokens.contains(&Token::Let));
}

#[test]
fn test_operators_precedence() {
    let input = r#"fn test():
    let a = 1 + 2 + 3
    let b = 1 * 2 * 3
    let c = 1 + 2 * 3
    let d = (1 + 2) * 3
    let e = 1 < 2 and 3 > 4 or 5 == 6
"#;

    let tokens = tokenize_kinds(input);

    // Verify all operators are present
    assert!(tokens.contains(&Token::Plus));
    assert!(tokens.contains(&Token::Star));
    assert!(tokens.contains(&Token::Lt));
    assert!(tokens.contains(&Token::Gt));
    assert!(tokens.contains(&Token::Eq));
    assert!(tokens.contains(&Token::And));
    assert!(tokens.contains(&Token::Or));
}

#[test]
fn test_range_operators() {
    let input = r#"fn test():
    let r1 = 1..10      # exclusive range
    let r2 = 1...10     # inclusive range
    let slice = arr[0..5]
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::DotDot));
    assert!(tokens.contains(&Token::DotDotDot));
}

#[test]
fn test_path_expressions() {
    let input = r#"fn test():
    let x = std::io::stdin()
    let y = collections::vec::Vec::new()
    use std::fs::File
"#;

    let tokens = tokenize_kinds(input);

    let colon_colon_count = tokens
        .iter()
        .filter(|t| matches!(t, Token::ColonColon))
        .count();
    assert!(
        colon_colon_count >= 3,
        "Should have multiple path separators"
    );
}

#[test]
fn test_compound_assignment() {
    let input = r#"fn test():
    let x = 0
    x += 1
    x -= 1
    x *= 2
    x /= 2
    x %= 3
    x &= 0xFF
    x |= 0x10
    x ^= 0x01
    x <<= 1
    x >>= 1
"#;

    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::PlusAssign));
    assert!(tokens.contains(&Token::MinusAssign));
    assert!(tokens.contains(&Token::StarAssign));
    assert!(tokens.contains(&Token::SlashAssign));
    assert!(tokens.contains(&Token::PercentAssign));
    assert!(tokens.contains(&Token::AndAssign));
    assert!(tokens.contains(&Token::OrAssign));
    assert!(tokens.contains(&Token::XorAssign));
    assert!(tokens.contains(&Token::ShlAssign));
    assert!(tokens.contains(&Token::ShrAssign));
}

#[test]
fn test_empty_lines_in_indentation() {
    let input = r#"fn main():
    let a = 1

    let b = 2


    let c = 3
"#;

    let tokens = tokenize_kinds(input);

    // Should only have one indent/dedent pair
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    assert_eq!(indent_count, 1);
    assert_eq!(dedent_count, 1);
}
