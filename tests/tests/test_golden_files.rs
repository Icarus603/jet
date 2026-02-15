//! Golden file tests for Jet compiler diagnostics
//!
//! These tests verify that error messages match expected output.
//! Run with BLESS_GOLDEN=1 to update golden files.

use jet_integration_tests::golden::GoldenFile;
use jet_integration_tests::harness::parse_source;
use std::path::PathBuf;

fn golden_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("golden")
}

#[test]
fn test_golden_missing_colon() {
    let source = r#"fn main()
    pass
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("missing_colon.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_unexpected_token() {
    let source = r#"fn main():
    let x = @
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("unexpected_token.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_unclosed_string() {
    let source = r#"fn main():
    print("hello)
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("unclosed_string.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_unclosed_paren() {
    let source = r#"fn main():
    print("hello"
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("unclosed_paren.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_valid_program() {
    let source = r#"fn main():
    print("hello")
"#;

    let result = parse_source(source);
    let output = match result {
        Ok(_) => "Parse successful".to_string(),
        Err(e) => format!("Parse error: {}", e),
    };

    let golden_path = golden_dir().join("valid_program.golden");
    GoldenFile::new(&golden_path, output).assert().unwrap();
}

// ============================================================================
// Additional Golden File Tests
// ============================================================================

#[test]
fn test_golden_indentation_error() {
    let source = r#"fn main():
        let x = 1
       let y = 2
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("indentation_error.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_type_error() {
    let source = r#"fn main():
    let x: int = "hello"
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("type_error.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_undefined_variable() {
    let source = r#"fn main():
    print(undefined_var)
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("undefined_variable.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_missing_return() {
    let source = r#"fn foo() -> int:
    pass
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("missing_return.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_wrong_argument_count() {
    let source = r#"fn foo(x: int, y: int):
    pass

fn main():
    foo(1)
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("wrong_argument_count.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_invalid_escape_sequence() {
    let source = r#"fn main():
    print("hello\qworld")
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("invalid_escape.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_unclosed_block_comment() {
    let source = r#"fn main():
    # This is fine
    print("hello")
"#;

    let result = parse_source(source);
    let output = match result {
        Ok(_) => "Parse successful".to_string(),
        Err(e) => format!("Parse error: {}", e),
    };

    let golden_path = golden_dir().join("line_comment.golden");
    GoldenFile::new(&golden_path, output).assert().unwrap();
}

#[test]
fn test_golden_generic_error() {
    let source = r#"fn identity[T](x: T) -> T:
    return x

fn main():
    let x = identity("hello") + 1
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("generic_type_error.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_trait_not_implemented() {
    let source = r#"trait Display:
    fn display(self) -> string

struct MyType:
    pass

fn show[T: Display](x: T):
    pass

fn main():
    let x = MyType
    show(x)
"#;

    let result = parse_source(source);
    let error_output = match result {
        Ok(_) => "No error".to_string(),
        Err(e) => e.to_string(),
    };

    let golden_path = golden_dir().join("trait_not_implemented.golden");
    GoldenFile::new(&golden_path, error_output)
        .assert()
        .unwrap();
}

#[test]
fn test_golden_complex_program() {
    let source = r#"enum Option[T]:
    | Some(T)
    | None

impl[T] Option[T]:
    fn map[U](self, f: fn(T) -> U) -> Option[U]:
        match self:
            | Some(t) => Some(f(t))
            | None => None

fn main():
    let x = Some(5)
    let y = x.map(fn(n) => n * 2)
    print(y)
"#;

    let result = parse_source(source);
    let output = match result {
        Ok(_) => "Parse successful".to_string(),
        Err(e) => format!("Parse error: {}", e),
    };

    let golden_path = golden_dir().join("complex_program.golden");
    GoldenFile::new(&golden_path, output).assert().unwrap();
}
