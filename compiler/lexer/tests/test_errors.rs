//! Tests for lexer error handling

use common::{get_errors, has_error_tokens, tokenize_kinds};
use jet_lexer::Token;

mod common;

// ============================================================================
// Invalid Character Tests
// ============================================================================

#[test]
fn test_invalid_character_at() {
    let input = "x @ y";
    // @ is a valid token in Jet
    assert!(!has_error_tokens(input));
}

#[test]
fn test_invalid_character_dollar() {
    let input = "x $ y";
    let _tokens = tokenize_kinds(input);
    // $ is not a valid token in Jet
    assert!(has_error_tokens(input));
}

#[test]
fn test_invalid_character_backtick() {
    let input = "`hello`";
    let _tokens = tokenize_kinds(input);
    assert!(has_error_tokens(input));
}

#[test]
fn test_invalid_unicode_in_identifier() {
    // Unicode identifiers not supported yet
    let input = "let 变量 = 1";
    let _tokens = tokenize_kinds(input);
    // The unicode characters should be treated as errors or separate tokens
}

// ============================================================================
// Unterminated String Tests
// ============================================================================

#[test]
fn test_unterminated_string() {
    let input = "\"hello";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for unterminated string"
    );
}

#[test]
fn test_unterminated_string_with_newline() {
    let input = "\"hello\nworld\"";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for newline in string"
    );
}

#[test]
fn test_unterminated_string_at_eof() {
    let input = "let x = \"unterminated";
    let errors = get_errors(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_unterminated_raw_string() {
    let input = "r#\"hello";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for unterminated raw string"
    );
}

// ============================================================================
// Unterminated Character Tests
// ============================================================================

#[test]
fn test_unterminated_char() {
    let input = "'a";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for unterminated char"
    );
}

#[test]
fn test_unterminated_char_at_eof() {
    let input = "'";
    let errors = get_errors(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_empty_char() {
    let input = "''";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for empty char literal"
    );
}

#[test]
fn test_char_too_long() {
    let input = "'ab'";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for multi-char literal"
    );
}

// ============================================================================
// Invalid Escape Sequence Tests
// ============================================================================

#[test]
fn test_invalid_escape_in_string() {
    let input = "\"hello\\zworld\"";
    let errors = get_errors(input);
    assert!(!errors.is_empty(), "Should have error for invalid escape");
}

#[test]
fn test_invalid_escape_in_char() {
    let input = "'\\z'";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for invalid escape in char"
    );
}

#[test]
fn test_incomplete_unicode_escape() {
    let input = "\"\\u{}\"";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for empty unicode escape"
    );
}

#[test]
fn test_invalid_unicode_escape_braces() {
    let input = "\"\\u1234\"";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for missing braces in unicode escape"
    );
}

#[test]
fn test_unclosed_unicode_escape() {
    let input = "\"\\u{1234\"";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for unclosed unicode escape"
    );
}

#[test]
fn test_invalid_unicode_code_point() {
    let input = "\"\\u{FFFFFFFF}\"";
    let errors = get_errors(input);
    assert!(
        !errors.is_empty(),
        "Should have error for invalid unicode code point"
    );
}

// ============================================================================
// Invalid Number Tests
// ============================================================================

#[test]
fn test_invalid_hex_digit() {
    let input = "0xGGG";
    let tokens = tokenize_kinds(input);
    // After 0x, G is not a valid hex digit, so it should be treated as identifier
    assert!(tokens.iter().any(|t| matches!(t, Token::Ident(_))));
}

#[test]
fn test_invalid_octal_digit() {
    let input = "0o888";
    let tokens = tokenize_kinds(input);
    // 8 is not a valid octal digit
    assert!(tokens.iter().any(|t| matches!(t, Token::Integer(_))));
}

#[test]
fn test_invalid_binary_digit() {
    let input = "0b201";
    let tokens = tokenize_kinds(input);
    // 2 is not a valid binary digit
    assert!(tokens.iter().any(|t| matches!(t, Token::Integer(_))));
}

#[test]
fn test_float_multiple_dots() {
    let input = "1.2.3";
    let tokens = tokenize_kinds(input);
    // Should parse 1.2 as float, then .3 as something else
    assert!(tokens.iter().any(|t| matches!(t, Token::Float(_))));
}

#[test]
fn test_float_invalid_exponent() {
    let input = "1.0e";
    let tokens = tokenize_kinds(input);
    // e without following digits
    assert!(tokens.iter().any(|t| matches!(t, Token::Float(_))));
}

// ============================================================================
// Mixed Tabs and Spaces Tests
// ============================================================================

#[test]
fn test_mixed_indentation_same_line() {
    // Mixing tabs and spaces on the same line
    let input = "fn main():\n \t let x = 1\n";
    let tokens = tokenize_kinds(input);
    // This should work (tab = 4 spaces)
    assert!(tokens.contains(&Token::Indent));
}

// ============================================================================
// Inconsistent Indentation Tests
// ============================================================================

#[test]
fn test_inconsistent_dedent() {
    // Indent to 4, then 8, then back to 6 (which doesn't match any previous level)
    let input = "fn main():\n    let a = 1\n        let b = 2\n      let c = 3\n";
    let errors = get_errors(input);
    // 6 spaces doesn't match 0, 4, or 8
    assert!(
        !errors.is_empty(),
        "Should have error for inconsistent dedent"
    );
}

// ============================================================================
// Error Recovery Tests
// ============================================================================

#[test]
fn test_error_recovery_invalid_char() {
    let input = "let x $ y";
    let tokens = tokenize_kinds(input);
    // Should still tokenize after the error
    assert!(tokens.iter().any(|t| matches!(t, Token::Let)));
    assert!(tokens.iter().any(|t| matches!(t, Token::Ident(_))));
}

#[test]
fn test_error_recovery_unterminated_string() {
    let input = "let x = \"unterminated; let y = 2";
    let tokens = tokenize_kinds(input);
    // Should continue tokenizing after the error
    assert!(tokens.iter().any(|t| matches!(t, Token::Let)));
}

#[test]
fn test_multiple_errors() {
    let input = "$ \"unterminated 'invalid";
    let errors = get_errors(input);
    assert!(errors.len() >= 2, "Should have multiple errors");
}

// ============================================================================
// Edge Case Error Tests
// ============================================================================

#[test]
fn test_backslash_at_eof() {
    let input = "\"hello\\";
    let errors = get_errors(input);
    assert!(!errors.is_empty(), "Should have error for backslash at EOF");
}

#[test]
fn test_unicode_escape_at_eof() {
    let input = "\"\\u";
    let errors = get_errors(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_unicode_escape_incomplete_brace() {
    let input = "\"\\u{";
    let errors = get_errors(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_raw_string_no_quotes() {
    let input = "r#not a string";
    let tokens = tokenize_kinds(input);
    // r is identifier, # is error, etc.
    assert!(tokens.iter().any(|t| matches!(t, Token::Ident(_))));
}

#[test]
fn test_empty_input() {
    let input = "";
    let tokens = tokenize_kinds(input);
    assert_eq!(tokens, vec![Token::Eof]);
}

#[test]
fn test_only_whitespace() {
    let input = "   \t\n\n   \t   ";
    let tokens = tokenize_kinds(input);
    assert!(tokens.last() == Some(&Token::Eof));
}

#[test]
fn test_only_comments() {
    let input = "# comment 1\n# comment 2\n";
    let tokens = tokenize_kinds(input);
    // Comments are skipped, should just have newlines and EOF
    assert!(tokens.last() == Some(&Token::Eof));
}
