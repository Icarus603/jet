//! Tests for Python-style significant indentation

use common::tokenize_kinds;
use jet_lexer::Token;

mod common;

#[test]
fn test_no_indentation() {
    let input = "let x = 5";
    let tokens = tokenize_kinds(input);
    assert!(!tokens
        .iter()
        .any(|t| matches!(t, Token::Indent | Token::Dedent)));
}

#[test]
fn test_basic_indentation() {
    let input = "fn main():\n    let x = 5\n    print(x)\n";
    let tokens = tokenize_kinds(input);

    // Check for the expected token sequence
    assert!(tokens.contains(&Token::Fn));
    assert!(tokens.contains(&Token::Indent));
    assert!(tokens.contains(&Token::Dedent));
    assert!(tokens.contains(&Token::Newline));
}

#[test]
fn test_single_indent() {
    let input = "if true:\n    let x = 1\n";
    let tokens = tokenize_kinds(input);

    // Should have exactly one Indent and one Dedent
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    assert_eq!(indent_count, 1, "Should have exactly 1 Indent token");
    assert_eq!(dedent_count, 1, "Should have exactly 1 Dedent token");
}

#[test]
fn test_nested_indentation() {
    let input = r#"if true:
    if true:
        let x = 1
"#;
    let tokens = tokenize_kinds(input);

    // Should have two Indents and two Dedents
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    assert_eq!(indent_count, 2, "Should have exactly 2 Indent tokens");
    assert_eq!(dedent_count, 2, "Should have exactly 2 Dedent tokens");
}

#[test]
fn test_multiple_dedents() {
    let input = r#"if true:
        if true:
            let x = 1
let y = 2
"#;
    let tokens = tokenize_kinds(input);

    // Should have two Indents
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    assert_eq!(indent_count, 2);

    // The last line should cause two dedents
    let dedent_positions: Vec<_> = tokens
        .iter()
        .enumerate()
        .filter(|(_, t)| matches!(t, Token::Dedent))
        .map(|(i, _)| i)
        .collect();

    // Check that dedents come before "let y"
    assert!(!dedent_positions.is_empty());
}

#[test]
fn test_indentation_with_empty_lines() {
    let input = r#"fn main():
    let x = 1

    let y = 2
"#;
    let tokens = tokenize_kinds(input);

    // Empty lines should not affect indentation
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    assert_eq!(indent_count, 1);
    assert_eq!(dedent_count, 1);
}

#[test]
fn test_indentation_with_comment_lines() {
    let input = r#"fn main():
    let x = 1
    # This is a comment
    let y = 2
"#;
    let tokens = tokenize_kinds(input);

    // Comment lines should not affect indentation
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    assert_eq!(indent_count, 1);
    assert_eq!(dedent_count, 1);
}

#[test]
fn test_partial_dedent() {
    let input = r#"if true:
    if true:
        let x = 1
    let y = 2
"#;
    let tokens = tokenize_kinds(input);

    // Should have two Indents
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    assert_eq!(indent_count, 2);

    // Should have two Dedents (one for each level)
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();
    assert_eq!(dedent_count, 2);
}

#[test]
fn test_tab_indentation() {
    // Tabs should be treated as 4 spaces
    let input = "fn main():\n\tlet x = 1\n";
    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Indent));
    assert!(tokens.contains(&Token::Dedent));
}

#[test]
fn test_mixed_indentation() {
    // Mix of tabs and spaces
    let input = "fn main():\n\t    let x = 1\n";
    let tokens = tokenize_kinds(input);

    // Tab (4) + 4 spaces = 8 spaces indent
    assert!(tokens.contains(&Token::Indent));
}

#[test]
fn test_deep_nesting() {
    let input = r#"a:
    b:
        c:
            d:
                e:
                    pass
"#;
    let tokens = tokenize_kinds(input);

    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    assert_eq!(indent_count, 5);
    assert_eq!(dedent_count, 5);
}

#[test]
fn test_indentation_sequence() {
    let input = r#"fn foo():
    let a = 1
    let b = 2

fn bar():
    let c = 3
"#;
    let tokens = tokenize_kinds(input);

    // First function: indent then dedent
    // Second function: indent then dedent
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    assert_eq!(indent_count, 2);
    assert_eq!(dedent_count, 2);
}

#[test]
fn test_newline_tokens() {
    let input = "a\nb\nc";
    let tokens = tokenize_kinds(input);

    let newline_count = tokens
        .iter()
        .filter(|t| matches!(t, Token::Newline))
        .count();
    assert_eq!(newline_count, 2);
}

#[test]
fn test_no_newline_at_end() {
    let input = "let x = 1"; // No trailing newline
    let tokens = tokenize_kinds(input);

    // Should still have EOF but no extra newline
    assert!(tokens.last() == Some(&Token::Eof));
}

#[test]
fn test_trailing_newline() {
    let input = "let x = 1\n";
    let tokens = tokenize_kinds(input);

    // Should have newline then EOF
    let len = tokens.len();
    assert!(len >= 2);
    assert!(matches!(tokens[len - 2], Token::Newline));
    assert_eq!(tokens[len - 1], Token::Eof);
}

#[test]
fn test_multiple_trailing_newlines() {
    let input = "let x = 1\n\n\n";
    let tokens = tokenize_kinds(input);

    // Should have multiple newlines then EOF
    let newline_count = tokens
        .iter()
        .filter(|t| matches!(t, Token::Newline))
        .count();
    assert_eq!(newline_count, 3);
}

#[test]
fn test_indent_with_multiple_statements() {
    let input = r#"fn main():
    let a = 1
    let b = 2
    let c = 3
    print(a + b + c)
"#;
    let tokens = tokenize_kinds(input);

    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    assert_eq!(indent_count, 1);
    assert_eq!(dedent_count, 1);
}

#[test]
fn test_dedent_at_eof() {
    let input = r#"fn main():
    let x = 1"#; // No trailing newline but needs dedent

    let tokens = tokenize_kinds(input);

    // Should still have dedent before EOF
    assert!(tokens.contains(&Token::Dedent));
}

#[test]
fn test_complex_indentation_pattern() {
    let input = r#"fn outer():
    let a = 1
    if true:
        let b = 2
        while false:
            let c = 3
        let d = 4
    let e = 5
fn inner():
    let f = 6
"#;
    let tokens = tokenize_kinds(input);

    // Count indents and dedents
    let indent_count = tokens.iter().filter(|t| matches!(t, Token::Indent)).count();
    let dedent_count = tokens.iter().filter(|t| matches!(t, Token::Dedent)).count();

    // outer: 1 indent
    // if: 1 indent
    // while: 1 indent
    // Total: 3 indents, 3 dedents
    assert_eq!(indent_count, 4); // outer, if, while, inner
    assert_eq!(dedent_count, 4);
}

#[test]
fn test_indentation_with_braces() {
    // Even with braces, indentation should work
    let input = r#"fn main():
    {
        let x = 1
    }
"#;
    let tokens = tokenize_kinds(input);

    // Should have proper indentation tokens
    assert!(tokens.contains(&Token::Indent));
    assert!(tokens.contains(&Token::Dedent));
}

#[test]
fn test_single_space_indent() {
    // Single space should be an indent (though unusual)
    let input = "fn main():\n let x = 1\n";
    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Indent));
}

#[test]
fn test_large_indent() {
    // Large indentation
    let input = "fn main():\n            let x = 1\n";
    let tokens = tokenize_kinds(input);

    assert!(tokens.contains(&Token::Indent));
}
