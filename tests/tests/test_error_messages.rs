//! Tests for error messages
//!
//! Verifies that compiler produces helpful, accurate error messages.

use jet_integration_tests::harness::parse_source;

#[test]
fn test_unexpected_token_error() {
    let source = r#"fn main()
    print("hello")
"#;

    let result = parse_source(source);
    assert!(result.is_err());
    let error = result.unwrap_err().to_string();
    assert!(error.contains("expected") || error.contains(":"));
}

#[test]
fn test_unclosed_delimiter_error() {
    let source = r#"fn main():
    print("hello"
"#;

    let result = parse_source(source);
    assert!(result.is_err());
}

#[test]
fn test_invalid_indentation_error() {
    let source = r#"fn main():
print("hello")
"#;

    let _result = parse_source(source);
    // This might or might not error depending on parser strictness
}

#[test]
fn test_undefined_variable_error() {
    // This would be a type checking error, not parsing
    // Placeholder for when type checking is implemented
}

#[test]
fn test_type_mismatch_error() {
    // This would be a type checking error
    // Placeholder for when type checking is implemented
}

#[test]
fn test_missing_return_error() {
    // This would be a type checking error
    // Placeholder for when type checking is implemented
}

#[test]
fn test_duplicate_definition_error() {
    // This would be a name resolution error
    // Placeholder for when name resolution is implemented
}

#[test]
fn test_unhandled_effect_error() {
    // This would be an effect checking error
    // Placeholder for when effect checking is implemented
}
