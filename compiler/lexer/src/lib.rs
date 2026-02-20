//! Jet Language Lexer
//!
//! This module provides lexical analysis for the Jet programming language.

pub mod error;
pub mod lexer;

pub use error::{LexError, LexResult};
pub use lexer::Lexer;

use std::fmt;

/// A token in the Jet language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // Literals
    Integer(String),
    Float(String),
    String(String),
    Char(char),
    Bool(bool),

    // Identifiers
    Ident(String),
    Label(String), // 'label_name (for loop labels)

    // Keywords
    And,
    As,
    Async,
    Await,
    Break,
    Chan,
    Concurrent,
    Const,
    Continue,
    Pass,
    Elif,
    Else,
    Enum,
    False,
    Fn,
    For,
    From,
    If,
    Impl,
    Import,
    In,
    Let,
    Loop,
    Match,
    Mod,
    Mut,
    Not,
    Or,
    Pub,
    Effect,
    Raise,
    Handle,
    With,
    Resume,
    Return,
    Self_,
    Spawn,
    Struct,
    Trait,
    True,
    Type,
    Unit,
    Use,
    Where,
    While,

    // Operators
    Plus,          // +
    Minus,         // -
    Star,          // *
    Slash,         // /
    Percent,       // %
    Power,         // **
    AndBit,        // &
    OrBit,         // |
    Xor,           // ^
    NotBit,        // ~
    Shl,           // <<
    Shr,           // >>
    Eq,            // ==
    Ne,            // !=
    Lt,            // <
    Gt,            // >
    Le,            // <=
    Ge,            // >=
    Assign,        // =
    PlusAssign,    // +=
    MinusAssign,   // -=
    StarAssign,    // *=
    SlashAssign,   // /=
    PercentAssign, // %=
    AndAssign,     // &=
    OrAssign,      // |=
    XorAssign,     // ^=
    ShlAssign,     // <<=
    ShrAssign,     // >>=

    // Delimiters
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }
    Colon,      // :
    Comma,      // ,
    Dot,        // .
    DotDot,     // ..
    DotDotDot,  // ...
    Arrow,      // ->
    FatArrow,   // =>
    Bang,       // !
    Question,   // ?
    At,         // @
    Underscore, // _
    Semi,       // ;
    ColonColon, // ::

    // Indentation
    Newline,
    Indent,
    Dedent,

    // Special
    Eof,
    Error(String),
    DocComment(String),

    // AI Annotation keywords
    Confidence,
    GeneratedBy,
    Prompt,
    HumanEditCount,

    // Literate programming annotations
    AtSpec,    // @spec
    AtExample, // @example
    AtTestFor, // @test_for

    // Contract keywords
    Requires,  // requires
    Ensures,   // ensures
    Invariant, // invariant
    Ghost,     // ghost
    Result,    // result (for postconditions)
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Integer(s) => write!(f, "{}", s),
            Token::Float(s) => write!(f, "{}", s),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Char(c) => write!(f, "'{}'", c),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Label(s) => write!(f, "'{}", s),
            Token::And => write!(f, "and"),
            Token::As => write!(f, "as"),
            Token::Async => write!(f, "async"),
            Token::Await => write!(f, "await"),
            Token::Break => write!(f, "break"),
            Token::Chan => write!(f, "chan"),
            Token::Concurrent => write!(f, "concurrent"),
            Token::Const => write!(f, "const"),
            Token::Continue => write!(f, "continue"),
            Token::Pass => write!(f, "pass"),
            Token::Elif => write!(f, "elif"),
            Token::Else => write!(f, "else"),
            Token::Enum => write!(f, "enum"),
            Token::False => write!(f, "false"),
            Token::Fn => write!(f, "fn"),
            Token::For => write!(f, "for"),
            Token::From => write!(f, "from"),
            Token::If => write!(f, "if"),
            Token::Impl => write!(f, "impl"),
            Token::Import => write!(f, "import"),
            Token::In => write!(f, "in"),
            Token::Let => write!(f, "let"),
            Token::Loop => write!(f, "loop"),
            Token::Match => write!(f, "match"),
            Token::Mod => write!(f, "mod"),
            Token::Mut => write!(f, "mut"),
            Token::Not => write!(f, "not"),
            Token::Or => write!(f, "or"),
            Token::Pub => write!(f, "pub"),
            Token::Raise => write!(f, "raise"),
            Token::Handle => write!(f, "handle"),
            Token::With => write!(f, "with"),
            Token::Resume => write!(f, "resume"),
            Token::Effect => write!(f, "effect"),
            Token::Return => write!(f, "return"),
            Token::Self_ => write!(f, "self"),
            Token::Spawn => write!(f, "spawn"),
            Token::Struct => write!(f, "struct"),
            Token::Trait => write!(f, "trait"),
            Token::True => write!(f, "true"),
            Token::Type => write!(f, "type"),
            Token::Unit => write!(f, "unit"),
            Token::Use => write!(f, "use"),
            Token::Where => write!(f, "where"),
            Token::While => write!(f, "while"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Power => write!(f, "**"),
            Token::AndBit => write!(f, "&"),
            Token::OrBit => write!(f, "|"),
            Token::Xor => write!(f, "^"),
            Token::NotBit => write!(f, "~"),
            Token::Shl => write!(f, "<<"),
            Token::Shr => write!(f, ">>"),
            Token::Eq => write!(f, "=="),
            Token::Ne => write!(f, "!="),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Le => write!(f, "<="),
            Token::Ge => write!(f, ">="),
            Token::Assign => write!(f, "="),
            Token::PlusAssign => write!(f, "+="),
            Token::MinusAssign => write!(f, "-="),
            Token::StarAssign => write!(f, "*="),
            Token::SlashAssign => write!(f, "/="),
            Token::PercentAssign => write!(f, "%="),
            Token::AndAssign => write!(f, "&="),
            Token::OrAssign => write!(f, "|="),
            Token::XorAssign => write!(f, "^="),
            Token::ShlAssign => write!(f, "<<="),
            Token::ShrAssign => write!(f, ">>="),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::DotDot => write!(f, ".."),
            Token::DotDotDot => write!(f, "..."),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::Bang => write!(f, "!"),
            Token::Question => write!(f, "?"),
            Token::At => write!(f, "@"),
            Token::Underscore => write!(f, "_"),
            Token::Semi => write!(f, ";"),
            Token::ColonColon => write!(f, "::"),
            Token::Newline => write!(f, "\\n"),
            Token::Indent => write!(f, "INDENT"),
            Token::Dedent => write!(f, "DEDENT"),
            Token::Eof => write!(f, "EOF"),
            Token::Error(s) => write!(f, "ERROR({})", s),
            Token::DocComment(s) => write!(f, "///{}", s),
            Token::Confidence => write!(f, "confidence"),
            Token::GeneratedBy => write!(f, "generated_by"),
            Token::Prompt => write!(f, "prompt"),
            Token::HumanEditCount => write!(f, "human_edit_count"),
            Token::AtSpec => write!(f, "@spec"),
            Token::AtExample => write!(f, "@example"),
            Token::AtTestFor => write!(f, "@test_for"),
            Token::Requires => write!(f, "requires"),
            Token::Ensures => write!(f, "ensures"),
            Token::Invariant => write!(f, "invariant"),
            Token::Ghost => write!(f, "ghost"),
            Token::Result => write!(f, "result"),
        }
    }
}

/// A token with span information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

/// Source span (byte offsets)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(&self, other: &Span) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// Tokenize source code into tokens
pub fn tokenize(source: &str) -> Vec<SpannedToken> {
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
}

/// Check if a string is a keyword
pub fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "and"
            | "as"
            | "async"
            | "await"
            | "break"
            | "chan"
            | "concurrent"
            | "const"
            | "continue"
            | "elif"
            | "else"
            | "enum"
            | "false"
            | "fn"
            | "for"
            | "from"
            | "handle"
            | "if"
            | "impl"
            | "import"
            | "in"
            | "let"
            | "loop"
            | "match"
            | "mod"
            | "mut"
            | "not"
            | "or"
            | "pass"
            | "pub"
            | "raise"
            | "resume"
            | "return"
            | "self"
            | "spawn"
            | "struct"
            | "trait"
            | "true"
            | "type"
            | "unit"
            | "use"
            | "where"
            | "while"
            | "with"
            | "confidence"
            | "generated_by"
            | "prompt"
            | "human_edit_count"
            | "spec"
            | "example"
            | "test_for"
            | "requires"
            | "ensures"
            | "invariant"
            | "ghost"
            | "result"
    )
}

/// Convert a keyword string to a Token
pub fn keyword_to_token(s: &str) -> Option<Token> {
    match s {
        "and" => Some(Token::And),
        "as" => Some(Token::As),
        "async" => Some(Token::Async),
        "await" => Some(Token::Await),
        "break" => Some(Token::Break),
        "chan" => Some(Token::Chan),
        "concurrent" => Some(Token::Concurrent),
        "const" => Some(Token::Const),
        "continue" => Some(Token::Continue),
        "elif" => Some(Token::Elif),
        "else" => Some(Token::Else),
        "enum" => Some(Token::Enum),
        "false" => Some(Token::False),
        "fn" => Some(Token::Fn),
        "for" => Some(Token::For),
        "from" => Some(Token::From),
        "handle" => Some(Token::Handle),
        "if" => Some(Token::If),
        "impl" => Some(Token::Impl),
        "import" => Some(Token::Import),
        "in" => Some(Token::In),
        "let" => Some(Token::Let),
        "loop" => Some(Token::Loop),
        "match" => Some(Token::Match),
        "mod" => Some(Token::Mod),
        "mut" => Some(Token::Mut),
        "not" => Some(Token::Not),
        "or" => Some(Token::Or),
        "pass" => Some(Token::Pass),
        "pub" => Some(Token::Pub),
        "effect" => Some(Token::Effect),
        "raise" => Some(Token::Raise),
        "resume" => Some(Token::Resume),
        "return" => Some(Token::Return),
        "self" => Some(Token::Self_),
        "spawn" => Some(Token::Spawn),
        "struct" => Some(Token::Struct),
        "trait" => Some(Token::Trait),
        "true" => Some(Token::True),
        "type" => Some(Token::Type),
        "unit" => Some(Token::Unit),
        "use" => Some(Token::Use),
        "where" => Some(Token::Where),
        "while" => Some(Token::While),
        "with" => Some(Token::With),
        "confidence" => Some(Token::Confidence),
        "generated_by" => Some(Token::GeneratedBy),
        "prompt" => Some(Token::Prompt),
        "human_edit_count" => Some(Token::HumanEditCount),
        "spec" => Some(Token::AtSpec),
        "example" => Some(Token::AtExample),
        "test_for" => Some(Token::AtTestFor),
        "requires" => Some(Token::Requires),
        "ensures" => Some(Token::Ensures),
        "invariant" => Some(Token::Invariant),
        "ghost" => Some(Token::Ghost),
        "result" => Some(Token::Result),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_recognition() {
        assert!(is_keyword("let"));
        assert!(is_keyword("fn"));
        assert!(is_keyword("if"));
        assert!(!is_keyword("foo"));
        assert!(!is_keyword("bar"));
    }

    #[test]
    fn test_keyword_to_token() {
        assert_eq!(keyword_to_token("let"), Some(Token::Let));
        assert_eq!(keyword_to_token("fn"), Some(Token::Fn));
        assert_eq!(keyword_to_token("if"), Some(Token::If));
        assert_eq!(keyword_to_token("foo"), None);
    }
}

#[cfg(test)]
mod contract_tests {
    use super::*;

    #[test]
    fn test_contract_keywords() {
        assert!(is_keyword("requires"));
        assert!(is_keyword("ensures"));
        assert!(is_keyword("invariant"));
        assert!(is_keyword("ghost"));
        assert!(is_keyword("result"));

        assert_eq!(keyword_to_token("requires"), Some(Token::Requires));
        assert_eq!(keyword_to_token("ensures"), Some(Token::Ensures));
        assert_eq!(keyword_to_token("invariant"), Some(Token::Invariant));
        assert_eq!(keyword_to_token("ghost"), Some(Token::Ghost));
        assert_eq!(keyword_to_token("result"), Some(Token::Result));
    }
}
