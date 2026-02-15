//! Tests for basic token recognition

use common::{assert_tokens, ident, int, tokenize_kinds};
use jet_lexer::Token;

mod common;

#[test]
fn test_eof() {
    let tokens = tokenize_kinds("");
    assert_eq!(tokens, vec![Token::Eof]);
}

#[test]
fn test_whitespace_only() {
    let tokens = tokenize_kinds("   \t\n  ");
    assert_eq!(tokens, vec![Token::Newline, Token::Eof]);
}

// ============================================================================
// Keyword Tests
// ============================================================================

#[test]
fn test_keyword_let() {
    assert_tokens("let", vec![Token::Let, Token::Eof]);
}

#[test]
fn test_keyword_fn() {
    assert_tokens("fn", vec![Token::Fn, Token::Eof]);
}

#[test]
fn test_keyword_if() {
    assert_tokens("if", vec![Token::If, Token::Eof]);
}

#[test]
fn test_keyword_else() {
    assert_tokens("else", vec![Token::Else, Token::Eof]);
}

#[test]
fn test_keyword_while() {
    assert_tokens("while", vec![Token::While, Token::Eof]);
}

#[test]
fn test_keyword_for() {
    assert_tokens("for", vec![Token::For, Token::Eof]);
}

#[test]
fn test_keyword_in() {
    assert_tokens("in", vec![Token::In, Token::Eof]);
}

#[test]
fn test_keyword_return() {
    assert_tokens("return", vec![Token::Return, Token::Eof]);
}

#[test]
fn test_keyword_break() {
    assert_tokens("break", vec![Token::Break, Token::Eof]);
}

#[test]
fn test_keyword_continue() {
    assert_tokens("continue", vec![Token::Continue, Token::Eof]);
}

#[test]
fn test_keyword_match() {
    assert_tokens("match", vec![Token::Match, Token::Eof]);
}

#[test]
fn test_keyword_struct() {
    assert_tokens("struct", vec![Token::Struct, Token::Eof]);
}

#[test]
fn test_keyword_enum() {
    assert_tokens("enum", vec![Token::Enum, Token::Eof]);
}

#[test]
fn test_keyword_impl() {
    assert_tokens("impl", vec![Token::Impl, Token::Eof]);
}

#[test]
fn test_keyword_trait() {
    assert_tokens("trait", vec![Token::Trait, Token::Eof]);
}

#[test]
fn test_keyword_type() {
    assert_tokens("type", vec![Token::Type, Token::Eof]);
}

#[test]
fn test_keyword_pub() {
    assert_tokens("pub", vec![Token::Pub, Token::Eof]);
}

#[test]
fn test_keyword_use() {
    assert_tokens("use", vec![Token::Use, Token::Eof]);
}

#[test]
fn test_keyword_mod() {
    assert_tokens("mod", vec![Token::Mod, Token::Eof]);
}

#[test]
fn test_keyword_as() {
    // 'as' is a keyword for type casting
    assert_tokens("as", vec![Token::As, Token::Eof]);
}

#[test]
fn test_keyword_mut() {
    assert_tokens("mut", vec![Token::Mut, Token::Eof]);
}

#[test]
fn test_keyword_async() {
    assert_tokens("async", vec![Token::Async, Token::Eof]);
}

#[test]
fn test_keyword_await() {
    assert_tokens("await", vec![Token::Await, Token::Eof]);
}

#[test]
fn test_keyword_import() {
    assert_tokens("import", vec![Token::Import, Token::Eof]);
}

#[test]
fn test_keyword_export() {
    // Note: export is not in the current keyword list, treated as identifier
    assert_tokens("export", vec![ident("export"), Token::Eof]);
}

#[test]
fn test_keyword_from() {
    // 'from' is a keyword for import statements
    assert_tokens("from", vec![Token::From, Token::Eof]);
}

#[test]
fn test_keyword_true() {
    assert_tokens("true", vec![Token::True, Token::Eof]);
}

#[test]
fn test_keyword_false() {
    assert_tokens("false", vec![Token::False, Token::Eof]);
}

#[test]
fn test_keyword_and() {
    assert_tokens("and", vec![Token::And, Token::Eof]);
}

#[test]
fn test_keyword_or() {
    assert_tokens("or", vec![Token::Or, Token::Eof]);
}

#[test]
fn test_keyword_not() {
    assert_tokens("not", vec![Token::Not, Token::Eof]);
}

#[test]
fn test_keyword_loop() {
    assert_tokens("loop", vec![Token::Loop, Token::Eof]);
}

#[test]
fn test_keyword_spawn() {
    assert_tokens("spawn", vec![Token::Spawn, Token::Eof]);
}

#[test]
fn test_keyword_chan() {
    assert_tokens("chan", vec![Token::Chan, Token::Eof]);
}

#[test]
fn test_keyword_concurrent() {
    assert_tokens("concurrent", vec![Token::Concurrent, Token::Eof]);
}

#[test]
fn test_keyword_raise() {
    assert_tokens("raise", vec![Token::Raise, Token::Eof]);
}

#[test]
fn test_keyword_self() {
    assert_tokens("self", vec![Token::Self_, Token::Eof]);
}

#[test]
fn test_keyword_unit() {
    assert_tokens("unit", vec![Token::Unit, Token::Eof]);
}

#[test]
fn test_keyword_where() {
    assert_tokens("where", vec![Token::Where, Token::Eof]);
}

#[test]
fn test_all_keywords() {
    // Test that keywords are recognized and not treated as identifiers
    let keywords = vec![
        ("and", Token::And),
        ("async", Token::Async),
        ("await", Token::Await),
        ("break", Token::Break),
        ("chan", Token::Chan),
        ("concurrent", Token::Concurrent),
        ("continue", Token::Continue),
        ("else", Token::Else),
        ("enum", Token::Enum),
        ("false", Token::False),
        ("fn", Token::Fn),
        ("for", Token::For),
        ("if", Token::If),
        ("import", Token::Import),
        ("in", Token::In),
        ("let", Token::Let),
        ("loop", Token::Loop),
        ("match", Token::Match),
        ("mod", Token::Mod),
        ("mut", Token::Mut),
        ("not", Token::Not),
        ("or", Token::Or),
        ("pub", Token::Pub),
        ("raise", Token::Raise),
        ("return", Token::Return),
        ("self", Token::Self_),
        ("spawn", Token::Spawn),
        ("struct", Token::Struct),
        ("trait", Token::Trait),
        ("true", Token::True),
        ("type", Token::Type),
        ("unit", Token::Unit),
        ("use", Token::Use),
        ("where", Token::Where),
        ("while", Token::While),
    ];

    for (kw, expected) in keywords {
        let tokens = tokenize_kinds(kw);
        assert_eq!(
            tokens[0], expected,
            "Keyword '{}' should produce {:?}",
            kw, expected
        );
    }
}

// ============================================================================
// Identifier Tests
// ============================================================================

#[test]
fn test_simple_identifier() {
    assert_tokens("x", vec![ident("x"), Token::Eof]);
}

#[test]
fn test_identifier_with_underscore() {
    assert_tokens("foo_bar", vec![ident("foo_bar"), Token::Eof]);
}

#[test]
fn test_identifier_starting_with_underscore() {
    assert_tokens("_temp", vec![ident("_temp"), Token::Eof]);
}

#[test]
fn test_identifier_with_numbers() {
    assert_tokens("x1", vec![ident("x1"), Token::Eof]);
    assert_tokens("x1y2", vec![ident("x1y2"), Token::Eof]);
    assert_tokens("_123", vec![ident("_123"), Token::Eof]);
}

#[test]
fn test_identifier_camel_case() {
    assert_tokens("FooBar", vec![ident("FooBar"), Token::Eof]);
}

#[test]
fn test_long_identifier() {
    assert_tokens(
        "very_long_identifier_name",
        vec![ident("very_long_identifier_name"), Token::Eof],
    );
}

#[test]
fn test_identifier_not_keyword() {
    // Identifiers that start with keywords
    assert_tokens("letx", vec![ident("letx"), Token::Eof]);
    assert_tokens("ifelse", vec![ident("ifelse"), Token::Eof]);
    assert_tokens("function", vec![ident("function"), Token::Eof]);
}

// ============================================================================
// Operator Tests
// ============================================================================

#[test]
fn test_operator_plus() {
    assert_tokens("+", vec![Token::Plus, Token::Eof]);
}

#[test]
fn test_operator_minus() {
    assert_tokens("-", vec![Token::Minus, Token::Eof]);
}

#[test]
fn test_operator_star() {
    assert_tokens("*", vec![Token::Star, Token::Eof]);
}

#[test]
fn test_operator_slash() {
    assert_tokens("/", vec![Token::Slash, Token::Eof]);
}

#[test]
fn test_operator_percent() {
    assert_tokens("%", vec![Token::Percent, Token::Eof]);
}

#[test]
fn test_operator_power() {
    assert_tokens("**", vec![Token::Power, Token::Eof]);
}

#[test]
fn test_operator_assign() {
    assert_tokens("=", vec![Token::Assign, Token::Eof]);
}

#[test]
fn test_operator_eq() {
    assert_tokens("==", vec![Token::Eq, Token::Eof]);
}

#[test]
fn test_operator_not_eq() {
    assert_tokens("!=", vec![Token::Ne, Token::Eof]);
}

#[test]
fn test_operator_lt() {
    assert_tokens("<", vec![Token::Lt, Token::Eof]);
}

#[test]
fn test_operator_gt() {
    assert_tokens(">", vec![Token::Gt, Token::Eof]);
}

#[test]
fn test_operator_le() {
    assert_tokens("<=", vec![Token::Le, Token::Eof]);
}

#[test]
fn test_operator_ge() {
    assert_tokens(">=", vec![Token::Ge, Token::Eof]);
}

#[test]
fn test_operator_and_bit() {
    assert_tokens("&", vec![Token::AndBit, Token::Eof]);
}

#[test]
fn test_operator_or_bit() {
    assert_tokens("|", vec![Token::OrBit, Token::Eof]);
}

#[test]
fn test_operator_xor() {
    assert_tokens("^", vec![Token::Xor, Token::Eof]);
}

#[test]
fn test_operator_not_bit() {
    assert_tokens("~", vec![Token::NotBit, Token::Eof]);
}

#[test]
fn test_operator_shl() {
    assert_tokens("<<", vec![Token::Shl, Token::Eof]);
}

#[test]
fn test_operator_shr() {
    assert_tokens(">>", vec![Token::Shr, Token::Eof]);
}

#[test]
fn test_operator_plus_assign() {
    assert_tokens("+=", vec![Token::PlusAssign, Token::Eof]);
}

#[test]
fn test_operator_minus_assign() {
    assert_tokens("-=", vec![Token::MinusAssign, Token::Eof]);
}

#[test]
fn test_operator_star_assign() {
    assert_tokens("*=", vec![Token::StarAssign, Token::Eof]);
}

#[test]
fn test_operator_slash_assign() {
    assert_tokens("/=", vec![Token::SlashAssign, Token::Eof]);
}

#[test]
fn test_operator_percent_assign() {
    assert_tokens("%=", vec![Token::PercentAssign, Token::Eof]);
}

#[test]
fn test_operator_and_assign() {
    assert_tokens("&=", vec![Token::AndAssign, Token::Eof]);
}

#[test]
fn test_operator_or_assign() {
    assert_tokens("|=", vec![Token::OrAssign, Token::Eof]);
}

#[test]
fn test_operator_xor_assign() {
    assert_tokens("^=", vec![Token::XorAssign, Token::Eof]);
}

#[test]
fn test_operator_shl_assign() {
    assert_tokens("<<=", vec![Token::ShlAssign, Token::Eof]);
}

#[test]
fn test_operator_shr_assign() {
    assert_tokens(">>=", vec![Token::ShrAssign, Token::Eof]);
}

#[test]
fn test_operator_arrow() {
    assert_tokens("->", vec![Token::Arrow, Token::Eof]);
}

#[test]
fn test_operator_fat_arrow() {
    assert_tokens("=>", vec![Token::FatArrow, Token::Eof]);
}

#[test]
fn test_operator_bang() {
    assert_tokens("!", vec![Token::Bang, Token::Eof]);
}

#[test]
fn test_operator_question() {
    assert_tokens("?", vec![Token::Question, Token::Eof]);
}

#[test]
fn test_operator_at() {
    assert_tokens("@", vec![Token::At, Token::Eof]);
}

#[test]
fn test_operator_underscore() {
    assert_tokens("_", vec![Token::Underscore, Token::Eof]);
}

#[test]
fn test_operator_dot() {
    assert_tokens(".", vec![Token::Dot, Token::Eof]);
}

#[test]
fn test_operator_dot_dot() {
    assert_tokens("..", vec![Token::DotDot, Token::Eof]);
}

#[test]
fn test_operator_dot_dot_dot() {
    assert_tokens("...", vec![Token::DotDotDot, Token::Eof]);
}

// ============================================================================
// Delimiter Tests
// ============================================================================

#[test]
fn test_delimiter_lparen() {
    assert_tokens("(", vec![Token::LParen, Token::Eof]);
}

#[test]
fn test_delimiter_rparen() {
    assert_tokens(")", vec![Token::RParen, Token::Eof]);
}

#[test]
fn test_delimiter_lbracket() {
    assert_tokens("[", vec![Token::LBracket, Token::Eof]);
}

#[test]
fn test_delimiter_rbracket() {
    assert_tokens("]", vec![Token::RBracket, Token::Eof]);
}

#[test]
fn test_delimiter_lbrace() {
    assert_tokens("{", vec![Token::LBrace, Token::Eof]);
}

#[test]
fn test_delimiter_rbrace() {
    assert_tokens("}", vec![Token::RBrace, Token::Eof]);
}

#[test]
fn test_delimiter_colon() {
    assert_tokens(":", vec![Token::Colon, Token::Eof]);
}

#[test]
fn test_delimiter_semi() {
    assert_tokens(";", vec![Token::Semi, Token::Eof]);
}

#[test]
fn test_delimiter_comma() {
    assert_tokens(",", vec![Token::Comma, Token::Eof]);
}

#[test]
fn test_delimiter_colon_colon() {
    assert_tokens("::", vec![Token::ColonColon, Token::Eof]);
}

// ============================================================================
// Complex Token Sequences
// ============================================================================

#[test]
fn test_variable_declaration() {
    assert_tokens(
        "let x = 5",
        vec![Token::Let, ident("x"), Token::Assign, int("5"), Token::Eof],
    );
}

#[test]
fn test_function_call() {
    assert_tokens(
        "print(x)",
        vec![
            ident("print"),
            Token::LParen,
            ident("x"),
            Token::RParen,
            Token::Eof,
        ],
    );
}

#[test]
fn test_binary_expression() {
    assert_tokens(
        "a + b * c",
        vec![
            ident("a"),
            Token::Plus,
            ident("b"),
            Token::Star,
            ident("c"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_comparison() {
    // Note: Jet uses `and`/`or` keywords for logical operators, not `&&`/`||`
    assert_tokens(
        "x <= y and z > w",
        vec![
            ident("x"),
            Token::Le,
            ident("y"),
            Token::And,
            ident("z"),
            Token::Gt,
            ident("w"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_nested_parens() {
    assert_tokens(
        "((x))",
        vec![
            Token::LParen,
            Token::LParen,
            ident("x"),
            Token::RParen,
            Token::RParen,
            Token::Eof,
        ],
    );
}

#[test]
fn test_multiple_statements() {
    assert_tokens(
        "let x = 1; let y = 2",
        vec![
            Token::Let,
            ident("x"),
            Token::Assign,
            int("1"),
            Token::Semi,
            Token::Let,
            ident("y"),
            Token::Assign,
            int("2"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_type_annotation() {
    assert_tokens(
        "x: int",
        vec![ident("x"), Token::Colon, ident("int"), Token::Eof],
    );
}

#[test]
fn test_function_signature() {
    assert_tokens(
        "fn add(x: int, y: int) -> int",
        vec![
            Token::Fn,
            ident("add"),
            Token::LParen,
            ident("x"),
            Token::Colon,
            ident("int"),
            Token::Comma,
            ident("y"),
            Token::Colon,
            ident("int"),
            Token::RParen,
            Token::Arrow,
            ident("int"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_list_literal() {
    assert_tokens(
        "[1, 2, 3]",
        vec![
            Token::LBracket,
            int("1"),
            Token::Comma,
            int("2"),
            Token::Comma,
            int("3"),
            Token::RBracket,
            Token::Eof,
        ],
    );
}

#[test]
fn test_struct_field_access() {
    assert_tokens(
        "obj.field",
        vec![ident("obj"), Token::Dot, ident("field"), Token::Eof],
    );
}

#[test]
fn test_match_arm() {
    assert_tokens(
        "Some(x) => x",
        vec![
            ident("Some"),
            Token::LParen,
            ident("x"),
            Token::RParen,
            Token::FatArrow,
            ident("x"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_generic_type() {
    assert_tokens(
        "Vec<T>",
        vec![ident("Vec"), Token::Lt, ident("T"), Token::Gt, Token::Eof],
    );
}

#[test]
fn test_nested_generics() {
    assert_tokens(
        "Map<K, V>",
        vec![
            ident("Map"),
            Token::Lt,
            ident("K"),
            Token::Comma,
            ident("V"),
            Token::Gt,
            Token::Eof,
        ],
    );
}

#[test]
fn test_bitwise_operations() {
    assert_tokens(
        "a & b | c ^ d",
        vec![
            ident("a"),
            Token::AndBit,
            ident("b"),
            Token::OrBit,
            ident("c"),
            Token::Xor,
            ident("d"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_shift_operations() {
    assert_tokens(
        "a << 2 >> 1",
        vec![
            ident("a"),
            Token::Shl,
            int("2"),
            Token::Shr,
            int("1"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_compound_assignment() {
    assert_tokens(
        "x += 1; y -= 2; z *= 3",
        vec![
            ident("x"),
            Token::PlusAssign,
            int("1"),
            Token::Semi,
            ident("y"),
            Token::MinusAssign,
            int("2"),
            Token::Semi,
            ident("z"),
            Token::StarAssign,
            int("3"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_range_operators() {
    assert_tokens(
        "a..b...c",
        vec![
            ident("a"),
            Token::DotDot,
            ident("b"),
            Token::DotDotDot,
            ident("c"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_path_separator() {
    assert_tokens(
        "std::io::println",
        vec![
            ident("std"),
            Token::ColonColon,
            ident("io"),
            Token::ColonColon,
            ident("println"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_tuple_literal() {
    assert_tokens(
        "(1, 2, 3)",
        vec![
            Token::LParen,
            int("1"),
            Token::Comma,
            int("2"),
            Token::Comma,
            int("3"),
            Token::RParen,
            Token::Eof,
        ],
    );
}

#[test]
fn test_closure_params() {
    assert_tokens(
        "|x, y| x + y",
        vec![
            Token::OrBit,
            ident("x"),
            Token::Comma,
            ident("y"),
            Token::OrBit,
            ident("x"),
            Token::Plus,
            ident("y"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_pattern_matching() {
    assert_tokens("@binding", vec![Token::At, ident("binding"), Token::Eof]);
}

#[test]
fn test_optional_type() {
    assert_tokens(
        "x?: int",
        vec![
            ident("x"),
            Token::Question,
            Token::Colon,
            ident("int"),
            Token::Eof,
        ],
    );
}

#[test]
fn test_label_token() {
    use jet_lexer::{tokenize, Token};

    // Test labeled while loop
    let tokens = tokenize("'outer: while true:");
    assert!(matches!(&tokens[0].token, Token::Label(name) if name == "outer"));
    assert!(matches!(tokens[1].token, Token::Colon));
    assert!(matches!(tokens[2].token, Token::While));

    // Test labeled for loop
    let tokens = tokenize("'inner: for i in items:");
    assert!(matches!(&tokens[0].token, Token::Label(name) if name == "inner"));
    assert!(matches!(tokens[1].token, Token::Colon));
    assert!(matches!(tokens[2].token, Token::For));

    // Test labeled loop
    let tokens = tokenize("'main: loop:");
    assert!(matches!(&tokens[0].token, Token::Label(name) if name == "main"));
    assert!(matches!(tokens[1].token, Token::Colon));
    assert!(matches!(tokens[2].token, Token::Loop));

    // Test break with label
    let tokens = tokenize("break 'outer");
    assert!(matches!(tokens[0].token, Token::Break));
    assert!(matches!(&tokens[1].token, Token::Label(name) if name == "outer"));

    // Test continue with label
    let tokens = tokenize("continue 'inner");
    assert!(matches!(tokens[0].token, Token::Continue));
    assert!(matches!(&tokens[1].token, Token::Label(name) if name == "inner"));
}

#[test]
fn test_char_literal_not_label() {
    use jet_lexer::{tokenize, Token};

    // Character literals should still work
    let tokens = tokenize("'a'");
    assert!(matches!(&tokens[0].token, Token::Char('a')));

    // Single char with identifier name - should be char literal
    let tokens = tokenize("'x'");
    assert!(matches!(&tokens[0].token, Token::Char('x')));

    // Escape sequences in char literals
    let tokens = tokenize("'\\n'");
    assert!(matches!(&tokens[0].token, Token::Char('\n')));
}
