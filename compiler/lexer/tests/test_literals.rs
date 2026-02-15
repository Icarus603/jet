//! Tests for literal recognition

use common::{assert_tokens, char, float, int, string};
use jet_lexer::Token;

mod common;

// ============================================================================
// Integer Literal Tests
// ============================================================================

#[test]
fn test_integer_zero() {
    assert_tokens("0", vec![int("0"), Token::Eof]);
}

#[test]
fn test_integer_single_digit() {
    assert_tokens("5", vec![int("5"), Token::Eof]);
}

#[test]
fn test_integer_multi_digit() {
    assert_tokens("42", vec![int("42"), Token::Eof]);
}

#[test]
fn test_integer_large() {
    assert_tokens("1234567890", vec![int("1234567890"), Token::Eof]);
}

#[test]
fn test_integer_with_underscores() {
    assert_tokens("1_000_000", vec![int("1000000"), Token::Eof]);
}

#[test]
fn test_integer_hex() {
    assert_tokens("0xFF", vec![int("FF"), Token::Eof]);
    assert_tokens("0xff", vec![int("ff"), Token::Eof]);
    assert_tokens("0xDEADBEEF", vec![int("DEADBEEF"), Token::Eof]);
    assert_tokens("0x0", vec![int("0"), Token::Eof]);
    assert_tokens("0xABCDEF", vec![int("ABCDEF"), Token::Eof]);
}

#[test]
fn test_integer_octal() {
    assert_tokens("0o755", vec![int("755"), Token::Eof]);
    assert_tokens("0o777", vec![int("777"), Token::Eof]);
    assert_tokens("0o0", vec![int("0"), Token::Eof]);
    assert_tokens("0o123", vec![int("123"), Token::Eof]);
}

#[test]
fn test_integer_binary() {
    assert_tokens("0b1010", vec![int("1010"), Token::Eof]);
    assert_tokens("0b0", vec![int("0"), Token::Eof]);
    assert_tokens("0b11110000", vec![int("11110000"), Token::Eof]);
    assert_tokens("0B1010", vec![int("1010"), Token::Eof]);
}

// ============================================================================
// Float Literal Tests
// ============================================================================

#[test]
fn test_float_simple() {
    assert_tokens("3.14", vec![float("3.14"), Token::Eof]);
}

#[test]
fn test_float_zero_point() {
    assert_tokens("0.5", vec![float("0.5"), Token::Eof]);
}

#[test]
fn test_float_point_zero() {
    assert_tokens("1.0", vec![float("1.0"), Token::Eof]);
}

#[test]
fn test_float_many_digits() {
    assert_tokens("3.14159265359", vec![float("3.14159265359"), Token::Eof]);
}

#[test]
fn test_float_exponent() {
    assert_tokens("1.0e10", vec![float("1.0e10"), Token::Eof]);
    assert_tokens("1.0E10", vec![float("1.0e10"), Token::Eof]);
}

#[test]
fn test_float_negative_exponent() {
    assert_tokens("2.5e-3", vec![float("2.5e-3"), Token::Eof]);
}

#[test]
fn test_float_positive_exponent() {
    assert_tokens("2.5e+3", vec![float("2.5e+3"), Token::Eof]);
}

#[test]
fn test_float_no_decimal_exponent() {
    assert_tokens("1e10", vec![float("1e10"), Token::Eof]);
}

#[test]
fn test_float_with_underscores() {
    assert_tokens("1_000.000_001", vec![float("1000.000001"), Token::Eof]);
}

// ============================================================================
// String Literal Tests
// ============================================================================

#[test]
fn test_string_empty() {
    assert_tokens("\"\"", vec![string(""), Token::Eof]);
}

#[test]
fn test_string_simple() {
    assert_tokens("\"hello\"", vec![string("hello"), Token::Eof]);
}

#[test]
fn test_string_with_spaces() {
    assert_tokens("\"hello world\"", vec![string("hello world"), Token::Eof]);
}

#[test]
fn test_string_escape_newline() {
    assert_tokens(
        "\"hello\\nworld\"",
        vec![string("hello\nworld"), Token::Eof],
    );
}

#[test]
fn test_string_escape_tab() {
    assert_tokens(
        "\"hello\\tworld\"",
        vec![string("hello\tworld"), Token::Eof],
    );
}

#[test]
fn test_string_escape_carriage_return() {
    assert_tokens(
        "\"hello\\rworld\"",
        vec![string("hello\rworld"), Token::Eof],
    );
}

#[test]
fn test_string_escape_backslash() {
    assert_tokens(
        "\"hello\\\\world\"",
        vec![string("hello\\world"), Token::Eof],
    );
}

#[test]
fn test_string_escape_quote() {
    assert_tokens(
        "\"hello\\\"world\"",
        vec![string("hello\"world"), Token::Eof],
    );
}

#[test]
fn test_string_escape_null() {
    assert_tokens(
        "\"hello\\0world\"",
        vec![string("hello\0world"), Token::Eof],
    );
}

#[test]
fn test_string_escape_single_quote() {
    assert_tokens("\"hello\\'world\"", vec![string("hello'world"), Token::Eof]);
}

#[test]
fn test_string_unicode_escape() {
    assert_tokens("\"\\u{1F600}\"", vec![string("😀"), Token::Eof]);
    assert_tokens("\"\\u{0041}\"", vec![string("A"), Token::Eof]);
    assert_tokens("\"\\u{00E9}\"", vec![string("é"), Token::Eof]);
}

#[test]
fn test_string_multiple_escapes() {
    assert_tokens(
        "\"line1\\nline2\\nline3\"",
        vec![string("line1\nline2\nline3"), Token::Eof],
    );
}

#[test]
fn test_string_with_special_chars() {
    assert_tokens(
        "\"hello @#$%^&*() world\"",
        vec![string("hello @#$%^&*() world"), Token::Eof],
    );
}

// ============================================================================
// Raw String Literal Tests
// ============================================================================

#[test]
fn test_raw_string_simple() {
    assert_tokens("r\"hello\"", vec![string("hello"), Token::Eof]);
}

#[test]
fn test_raw_string_with_backslash() {
    assert_tokens(
        "r\"hello\\world\"",
        vec![string("hello\\world"), Token::Eof],
    );
}

#[test]
fn test_raw_string_with_quotes() {
    assert_tokens(
        "r#\"hello \"world\"#",
        vec![string("hello \"world"), Token::Eof],
    );
}

#[test]
fn test_raw_string_empty() {
    assert_tokens("r\"\"", vec![string(""), Token::Eof]);
}

#[test]
fn test_raw_string_multiple_hashes() {
    assert_tokens(
        "r##\"hello \"#world\"##",
        vec![string("hello \"#world"), Token::Eof],
    );
}

#[test]
fn test_raw_string_with_newline() {
    assert_tokens(
        "r\"hello\nworld\"",
        vec![string("hello\nworld"), Token::Eof],
    );
}

// ============================================================================
// Character Literal Tests
// ============================================================================

#[test]
fn test_char_simple() {
    assert_tokens("'a'", vec![char('a'), Token::Eof]);
    assert_tokens("'b'", vec![char('b'), Token::Eof]);
    assert_tokens("'Z'", vec![char('Z'), Token::Eof]);
}

#[test]
fn test_char_digit() {
    assert_tokens("'0'", vec![char('0'), Token::Eof]);
    assert_tokens("'9'", vec![char('9'), Token::Eof]);
}

#[test]
fn test_char_special() {
    assert_tokens("'@'", vec![char('@'), Token::Eof]);
    assert_tokens("'$'", vec![char('$'), Token::Eof]);
}

#[test]
fn test_char_escape_newline() {
    assert_tokens("'\\n'", vec![char('\n'), Token::Eof]);
}

#[test]
fn test_char_escape_tab() {
    assert_tokens("'\\t'", vec![char('\t'), Token::Eof]);
}

#[test]
fn test_char_escape_carriage_return() {
    assert_tokens("'\\r'", vec![char('\r'), Token::Eof]);
}

#[test]
fn test_char_escape_backslash() {
    assert_tokens("'\\\\'", vec![char('\\'), Token::Eof]);
}

#[test]
fn test_char_escape_single_quote() {
    assert_tokens("'\\''", vec![char('\''), Token::Eof]);
}

#[test]
fn test_char_escape_null() {
    assert_tokens("'\\0'", vec![char('\0'), Token::Eof]);
}

#[test]
fn test_char_unicode_escape() {
    assert_tokens("'\\u{1F600}'", vec![char('😀'), Token::Eof]);
    assert_tokens("'\\u{0041}'", vec![char('A'), Token::Eof]);
    assert_tokens("'\\u{00E9}'", vec![char('é'), Token::Eof]);
}

// ============================================================================
// Boolean Literal Tests
// ============================================================================

#[test]
fn test_bool_true() {
    assert_tokens("true", vec![Token::True, Token::Eof]);
}

#[test]
fn test_bool_false() {
    assert_tokens("false", vec![Token::False, Token::Eof]);
}

// ============================================================================
// Literal Edge Cases
// ============================================================================

#[test]
fn test_integer_followed_by_dot() {
    // "5." is parsed as integer 5 followed by dot (needs digit after dot for float)
    assert_tokens("5.", vec![int("5"), Token::Dot, Token::Eof]);
}

#[test]
fn test_integer_followed_by_identifier() {
    assert_tokens("5x", vec![int("5"), ident("x"), Token::Eof]);
}

#[test]
fn test_float_followed_by_identifier() {
    assert_tokens("3.14x", vec![float("3.14"), ident("x"), Token::Eof]);
}

#[test]
fn test_zero_prefix_not_octal() {
    // In Jet, 077 is just 77 (decimal), not octal
    assert_tokens("077", vec![int("077"), Token::Eof]);
}

#[test]
fn test_hex_case_insensitive() {
    assert_tokens("0xABC", vec![int("ABC"), Token::Eof]);
    assert_tokens("0xabc", vec![int("abc"), Token::Eof]);
    assert_tokens("0xAbc", vec![int("Abc"), Token::Eof]);
}

#[test]
fn test_string_with_unicode() {
    assert_tokens("\"hello 世界\"", vec![string("hello 世界"), Token::Eof]);
    assert_tokens("\"🎉 party 🎊\"", vec![string("🎉 party 🎊"), Token::Eof]);
}

// Helper function for identifier
fn ident(s: &str) -> Token {
    Token::Ident(s.to_string())
}
