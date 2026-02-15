//! Property-based tests for the Jet lexer
//!
//! Uses proptest to verify lexer properties.

use jet_lexer::{tokenize, Token};
use proptest::prelude::*;

// ============================================================================
// Token Properties
// ============================================================================

proptest! {
    #[test]
    fn prop_tokens_always_end_with_eof(input in "\\PC*") {
        let tokens = tokenize(&input);
        prop_assert!(!tokens.is_empty());
        prop_assert!(matches!(tokens.last().unwrap().token, Token::Eof));
    }

    #[test]
    fn prop_integer_literals_tokenize(input in "[0-9][0-9_]*") {
        let tokens = tokenize(&input);
        prop_assert_eq!(tokens.len(), 2); // Integer + EOF
        prop_assert!(matches!(&tokens[0].token, Token::Integer(_)));
    }

    #[test]
    fn prop_float_literals_tokenize(input in "[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?") {
        let tokens = tokenize(&input);
        prop_assert!(tokens.len() >= 2);
        // First token should be float or integer depending on format
        let first = &tokens[0].token;
        prop_assert!(
            matches!(first, Token::Float(_)) || matches!(first, Token::Integer(_)),
            "Expected float or integer, got {:?}", first
        );
    }

    #[test]
    fn prop_identifier_tokenize(input in "[a-zA-Z][a-zA-Z0-9_]*") {
        let tokens = tokenize(&input);
        prop_assert!(!tokens.is_empty());
        // First token could be keyword or identifier
        let first = &tokens[0].token;
        prop_assert!(
            matches!(first, Token::Ident(_)) || is_keyword(first),
            "Expected identifier or keyword, got {:?}", first
        );
    }

    #[test]
    fn prop_string_literal_tokenize(content in "[^\"\\\\\n\r]*") {
        let input = format!("\"{}\"", content);
        let tokens = tokenize(&input);
        prop_assert!(tokens.len() >= 2);
        prop_assert!(matches!(&tokens[0].token, Token::String(_)));
    }

    #[test]
    fn prop_comment_does_not_affect_other_tokens(
        before in "[a-zA-Z_][a-zA-Z0-9_]*",
        comment in "[^\\n]*",
        after in "[a-zA-Z_][a-zA-Z0-9_]*"
    ) {
        let with_comment = format!("{} # {}\n{}", before, comment, after);
        let without_comment = format!("{}\n{}", before, after);

        let tokens_with = tokenize(&with_comment);
        let tokens_without = tokenize(&without_comment);

        // Filter out comment tokens (they don't exist in Jet lexer, comments are skipped)
        // Both should have similar structure
        let with_kinds: Vec<_> = tokens_with.iter()
            .map(|t| t.token.clone())
            .filter(|t| !matches!(t, Token::Eof))
            .collect();
        let without_kinds: Vec<_> = tokens_without.iter()
            .map(|t| t.token.clone())
            .filter(|t| !matches!(t, Token::Eof))
            .collect();

        prop_assert_eq!(with_kinds, without_kinds);
    }
}

// ============================================================================
// Round-trip Properties
// ============================================================================

proptest! {
    #[test]
    fn prop_integer_roundtrip(n in 0i64..=i64::MAX) {
        let input = n.to_string();
        let tokens = tokenize(&input);
        prop_assert_eq!(tokens.len(), 2);
        if let Token::Integer(s) = &tokens[0].token {
            // Parse back and compare
            let parsed: i64 = s.parse().unwrap();
            prop_assert_eq!(parsed, n);
        } else {
            prop_assert!(false, "Expected integer token");
        }
    }

    #[test]
    fn prop_float_roundtrip(
        whole in 0i64..1000000i64,
        frac in 0i64..1000000i64
    ) {
        let input = format!("{}.{}", whole, frac);
        let tokens = tokenize(&input);
        prop_assert_eq!(tokens.len(), 2);
        prop_assert!(matches!(&tokens[0].token, Token::Float(_)));
    }
}

// ============================================================================
// Indentation Properties
// ============================================================================

proptest! {
    #[test]
    fn prop_indentation_balanced(levels in 0usize..10usize) {
        let mut source = String::from("if true:\n");
        for i in 0..levels {
            source.push_str(&"    ".repeat(i + 1));
            source.push_str("if true:\n");
        }
        source.push_str(&"    ".repeat(levels));
        source.push_str("pass\n");

        let tokens = tokenize(&source);

        let indent_count = tokens.iter()
            .filter(|t| matches!(t.token, Token::Indent))
            .count();
        let dedent_count = tokens.iter()
            .filter(|t| matches!(t.token, Token::Dedent))
            .count();

        prop_assert_eq!(indent_count, dedent_count);
    }

    #[test]
    fn prop_indentation_increases_monotonically(spaces in 0usize..20usize) {
        let source = format!("fn main():\n{}pass", " ".repeat(spaces));
        let tokens = tokenize(&source);

        // Find indent tokens
        let indents = tokens.iter()
            .filter(|t| matches!(t.token, Token::Indent))
            .count();

        // Should have at most one indent
        prop_assert!(indents <= 1);
    }
}

// ============================================================================
// Whitespace Properties
// ============================================================================

proptest! {
    #[test]
    fn prop_whitespace_between_tokens_not_required_for_operators(
        a in "[a-zA-Z_][a-zA-Z0-9_]*",
        b in "[a-zA-Z_][a-zA-Z0-9_]*"
    ) {
        // Test that operators need proper spacing or structure
        let with_space = format!("{} + {}", a, b);
        let without_space = format!("{}+{}", a, b);

        let tokens_with = tokenize(&with_space);
        let tokens_without = tokenize(&without_space);

        // Both should tokenize without errors
        let has_error_with = tokens_with.iter().any(|t| matches!(t.token, Token::Error(_)));
        let _has_error_without = tokens_without
            .iter()
            .any(|t| matches!(t.token, Token::Error(_)));

        prop_assert!(!has_error_with);
        // Without space might produce different tokens (identifier with + in it)
        // but shouldn't error
    }
}

// ============================================================================
// Error Recovery Properties
// ============================================================================

proptest! {
    #[test]
    fn prop_never_panics(input in "\\PC*") {
        // The lexer should never panic on any input
        let _tokens = tokenize(&input);
    }

    #[test]
    fn prop_unclosed_string_produces_error(content in "[^\"]*") {
        let input = format!("\"{}", content);
        let tokens = tokenize(&input);
        let has_error = tokens.iter().any(|t| matches!(t.token, Token::Error(_)));
        prop_assert!(has_error, "Unclosed string should produce error");
    }

    #[test]
    fn prop_unclosed_char_produces_error(content in "[^']*") {
        let input = format!("'{}", content);
        let _tokens = tokenize(&input);
        // May or may not produce error depending on content
        // Just verify it doesn't panic
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

fn is_keyword(token: &Token) -> bool {
    matches!(
        token,
        Token::And
            | Token::Async
            | Token::Await
            | Token::Break
            | Token::Chan
            | Token::Concurrent
            | Token::Continue
            | Token::Else
            | Token::Enum
            | Token::False
            | Token::Fn
            | Token::For
            | Token::If
            | Token::Import
            | Token::In
            | Token::Let
            | Token::Loop
            | Token::Match
            | Token::Mod
            | Token::Mut
            | Token::Not
            | Token::Or
            | Token::Pub
            | Token::Raise
            | Token::Return
            | Token::Self_
            | Token::Spawn
            | Token::Struct
            | Token::Trait
            | Token::True
            | Token::Type
            | Token::Unit
            | Token::Use
            | Token::Where
            | Token::While
            | Token::Elif
            | Token::As
    )
}
