//! Integration tests for the Jet lexer
//!
//! Tests all token types and edge cases.

use jet_integration_tests::harness::tokenize_source;
use jet_lexer::Token;

#[test]
fn test_all_keywords() {
    // Test keywords without indentation issues
    let source = "and async await break chan concurrent continue \
        elif else enum false fn for if impl import in \
        let loop match mod mut not or pub raise return \
        self spawn struct trait true type unit use \
        where while";

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .filter(|t| {
            !matches!(
                t,
                Token::Newline | Token::Eof | Token::Indent | Token::Dedent
            )
        })
        .collect();

    assert_eq!(
        tokens,
        vec![
            Token::And,
            Token::Async,
            Token::Await,
            Token::Break,
            Token::Chan,
            Token::Concurrent,
            Token::Continue,
            Token::Elif,
            Token::Else,
            Token::Enum,
            Token::False,
            Token::Fn,
            Token::For,
            Token::If,
            Token::Impl,
            Token::Import,
            Token::In,
            Token::Let,
            Token::Loop,
            Token::Match,
            Token::Mod,
            Token::Mut,
            Token::Not,
            Token::Or,
            Token::Pub,
            Token::Raise,
            Token::Return,
            Token::Self_,
            Token::Spawn,
            Token::Struct,
            Token::Trait,
            Token::True,
            Token::Type,
            Token::Unit,
            Token::Use,
            Token::Where,
            Token::While,
        ]
    );
}

#[test]
fn test_all_operators() {
    let source = r#"+ - * / % ** & | ^ ~ << >> == != < > <= >= = += -= *= /= %= &= |= ^= <<= >>="#;

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .filter(|t| !matches!(t, Token::Eof))
        .collect();

    assert_eq!(
        tokens,
        vec![
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Percent,
            Token::Power,
            Token::AndBit,
            Token::OrBit,
            Token::Xor,
            Token::NotBit,
            Token::Shl,
            Token::Shr,
            Token::Eq,
            Token::Ne,
            Token::Lt,
            Token::Gt,
            Token::Le,
            Token::Ge,
            Token::Assign,
            Token::PlusAssign,
            Token::MinusAssign,
            Token::StarAssign,
            Token::SlashAssign,
            Token::PercentAssign,
            Token::AndAssign,
            Token::OrAssign,
            Token::XorAssign,
            Token::ShlAssign,
            Token::ShrAssign,
        ]
    );
}

#[test]
fn test_all_delimiters() {
    let source = r#"( ) [ ] { } : , . .. ... -> => ! ? @ _ ; ::"#;

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .filter(|t| !matches!(t, Token::Eof))
        .collect();

    assert_eq!(
        tokens,
        vec![
            Token::LParen,
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::LBrace,
            Token::RBrace,
            Token::Colon,
            Token::Comma,
            Token::Dot,
            Token::DotDot,
            Token::DotDotDot,
            Token::Arrow,
            Token::FatArrow,
            Token::Bang,
            Token::Question,
            Token::At,
            Token::Underscore,
            Token::Semi,
            Token::ColonColon,
        ]
    );
}

#[test]
fn test_integer_literals() {
    let source = r#"0 42 1234567890 -5 +10"#;

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .filter(|t| !matches!(t, Token::Eof))
        .collect();

    assert!(matches!(tokens[0], Token::Integer(_)));
    assert!(matches!(tokens[1], Token::Integer(_)));
    assert!(matches!(tokens[2], Token::Integer(_)));
    assert!(matches!(tokens[3], Token::Minus));
    assert!(matches!(tokens[4], Token::Integer(_)));
}

#[test]
fn test_float_literals() {
    let source = r#"0.0 3.14 1.5e10 2.5e-5"#;

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .filter(|t| !matches!(t, Token::Eof))
        .collect();

    assert!(matches!(tokens[0], Token::Float(_)));
    assert!(matches!(tokens[1], Token::Float(_)));
    assert!(matches!(tokens[2], Token::Float(_)));
    assert!(matches!(tokens[3], Token::Float(_)));
}

#[test]
fn test_string_literals() {
    let source = r#""hello" "world" "escaped \"quote\"" "newline\n" "tab\t""#;

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .filter(|t| !matches!(t, Token::Eof))
        .collect();

    assert!(matches!(tokens[0], Token::String(_)));
    assert!(matches!(tokens[1], Token::String(_)));
    assert!(matches!(tokens[2], Token::String(_)));
}

#[test]
fn test_char_literals() {
    let source = r#"'a' 'b' '\n' '\t' '\\' '\''"#;

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .filter(|t| !matches!(t, Token::Eof))
        .collect();

    assert!(matches!(tokens[0], Token::Char(_)));
    assert!(matches!(tokens[1], Token::Char(_)));
    assert!(matches!(tokens[2], Token::Char(_)));
}

#[test]
fn test_identifiers() {
    let source = r#"foo bar_baz camelCase PascalCase snake_case _private __dunder"#;

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .filter(|t| !matches!(t, Token::Eof))
        .collect();

    assert!(matches!(tokens[0], Token::Ident(_)));
    assert!(matches!(tokens[1], Token::Ident(_)));
    assert!(matches!(tokens[2], Token::Ident(_)));
    assert!(matches!(tokens[3], Token::Ident(_)));
    assert!(matches!(tokens[4], Token::Ident(_)));
    assert!(matches!(tokens[5], Token::Ident(_)));
    assert!(matches!(tokens[6], Token::Ident(_)));
}

#[test]
fn test_indentation() {
    let source = r#"if true:
    print("hello")
    if false:
        print("world")
print("done")
"#;

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .collect();

    // Check for INDENT and DEDENT tokens
    assert!(tokens.iter().any(|t| matches!(t, Token::Indent)));
    assert!(tokens.iter().any(|t| matches!(t, Token::Dedent)));
}

#[test]
fn test_comments() {
    let source = r#"# This is a comment
print("hello") # inline comment
# Another comment
"#;

    let tokens: Vec<_> = tokenize_source(source)
        .into_iter()
        .map(|t| t.token)
        .filter(|t| {
            !matches!(
                t,
                Token::Newline | Token::Eof | Token::Indent | Token::Dedent
            )
        })
        .collect();

    // Comments should be skipped, only print and string should remain
    assert!(tokens.contains(&Token::Ident("print".to_string())));
    assert!(tokens.iter().any(|t| matches!(t, Token::String(_))));
}

#[test]
fn test_complete_program() {
    let source = r#"fn main():
    let x = 42
    let y = 3.14
    let s = "hello"
    print(s)
"#;

    let tokens = tokenize_source(source);

    // Should not have any error tokens
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));

    // Should have the expected structure
    let token_types: Vec<_> = tokens.iter().map(|t| t.token.clone()).collect();
    assert!(token_types.contains(&Token::Fn));
    assert!(token_types.contains(&Token::Let));
    assert!(token_types.contains(&Token::Ident("main".to_string())));
}

#[test]
fn test_type_annotations() {
    let source = r#"fn add(x: int, y: int) -> int:
    return x + y

let items: [string] = []
let pair: (int, string) = (1, "one")
"#;

    let tokens = tokenize_source(source);

    // Should not have any error tokens
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));
}

#[test]
fn test_generics() {
    let source = r#"fn identity[T](value: T) -> T:
    return value

let box: Box[int] = Box::new(42)
"#;

    let tokens = tokenize_source(source);

    // Should not have any error tokens
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));
}

#[test]
fn test_effects() {
    let source = r#"fn divide(a: int, b: int) -> int ! DivisionError:
    if b == 0:
        raise DivisionError
    return a / b

fn process() -> Result ! Error1 | Error2:
    return Ok(42)
"#;

    let tokens = tokenize_source(source);

    // Should not have any error tokens
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));
}

#[test]
fn test_pattern_matching() {
    let source = r#"match value:
    | Some(x) -> print(x)
    | None -> print("nothing")
    | _ -> print("default")
"#;

    let tokens = tokenize_source(source);

    // Should not have any error tokens
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));
}

#[test]
fn test_async_await() {
    let source = r#"async fn fetch() -> string:
    let result = await http.get("url")
    return result

let handle = spawn fetch()
"#;

    let tokens = tokenize_source(source);

    // Should not have any error tokens
    assert!(!tokens.iter().any(|t| matches!(t.token, Token::Error(_))));
}
