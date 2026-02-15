//! Common test utilities for the lexer

#![allow(dead_code)]

use jet_lexer::{tokenize, Lexer, SpannedToken, Token};

/// Tokenize input and return just the token kinds (without span info)
pub fn tokenize_kinds(input: &str) -> Vec<Token> {
    tokenize(input).into_iter().map(|t| t.token).collect()
}

/// Tokenize input and return the full spanned tokens
pub fn tokenize_spanned(input: &str) -> Vec<SpannedToken> {
    tokenize(input)
}

/// Create a new lexer for the given input
pub fn make_lexer(input: &str) -> Lexer<'_> {
    Lexer::new(input)
}

/// Assert that tokenizing the input produces the expected token kinds
pub fn assert_tokens(input: &str, expected: Vec<Token>) {
    let tokens = tokenize_kinds(input);
    assert_eq!(tokens, expected, "Token mismatch for input: {:?}", input);
}

/// Assert that the first token is of the expected kind
pub fn assert_first_token(input: &str, expected: Token) {
    let tokens = tokenize_kinds(input);
    assert!(
        !tokens.is_empty(),
        "Expected at least one token for input: {:?}",
        input
    );
    assert_eq!(
        tokens[0], expected,
        "First token mismatch for input: {:?}",
        input
    );
}

/// Get all tokens except EOF
pub fn tokens_without_eof(input: &str) -> Vec<Token> {
    tokenize_kinds(input)
        .into_iter()
        .filter(|t| !matches!(t, Token::Eof))
        .collect()
}

/// Check if the input produces any error tokens
pub fn has_error_tokens(input: &str) -> bool {
    tokenize_kinds(input)
        .iter()
        .any(|t| matches!(t, Token::Error(_)))
}

/// Get all error tokens from the input
pub fn get_errors(input: &str) -> Vec<String> {
    tokenize_kinds(input)
        .into_iter()
        .filter_map(|t| match t {
            Token::Error(msg) => Some(msg),
            _ => None,
        })
        .collect()
}

/// Helper to create an identifier token
pub fn ident(s: &str) -> Token {
    Token::Ident(s.to_string())
}

/// Helper to create an integer token
pub fn int(s: &str) -> Token {
    Token::Integer(s.to_string())
}

/// Helper to create a float token
pub fn float(s: &str) -> Token {
    Token::Float(s.to_string())
}

/// Helper to create a string token
pub fn string(s: &str) -> Token {
    Token::String(s.to_string())
}

/// Helper to create a character token
pub fn char(c: char) -> Token {
    Token::Char(c)
}
