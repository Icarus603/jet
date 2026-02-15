//! Semantic tokens for syntax highlighting in the Jet LSP server
//!
//! This module provides semantic token analysis for rich syntax highlighting.
//! It maps Jet AST nodes to LSP semantic token types and modifiers.

use jet_lexer::{SpannedToken, Token};
use jet_parser::ast::Module;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenModifier, SemanticTokenType};

/// Token type indices (must match the order in get_semantic_token_types)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
#[allow(dead_code)]
pub enum TokenType {
    Namespace = 0,
    Type = 1,
    Class = 2,
    Enum = 3,
    Interface = 4,
    Struct = 5,
    TypeParameter = 6,
    Parameter = 7,
    Variable = 8,
    Property = 9,
    EnumMember = 10,
    Function = 11,
    Method = 12,
    Keyword = 13,
    Modifier = 14,
    Comment = 15,
    String = 16,
    Number = 17,
    Operator = 18,
}

/// Token modifier indices (must match the order in get_semantic_token_modifiers)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
#[allow(dead_code)]
pub enum TokenModifier {
    Declaration = 0,
    Definition = 1,
    Readonly = 2,
    Static = 3,
    Abstract = 4,
    Deprecated = 5,
    Async = 6,
    Modification = 7,
}

impl TokenModifier {
    #[allow(dead_code)]
    fn to_bitset(self) -> u32 {
        1 << (self as u32)
    }
}

/// Line information for a source position
#[derive(Debug)]
struct LineInfo {
    line: u32,
    character: u32,
}

/// Compute line and character from a byte offset
fn offset_to_position(source: &str, offset: usize) -> LineInfo {
    let mut line = 0u32;
    let mut last_line_start = 0usize;

    for (i, ch) in source.char_indices() {
        if i >= offset {
            return LineInfo {
                line,
                character: (i - last_line_start) as u32,
            };
        }
        if ch == '\n' {
            line += 1;
            last_line_start = i + 1;
        }
    }

    LineInfo {
        line,
        character: (source.len().saturating_sub(last_line_start)) as u32,
    }
}

/// A builder for semantic tokens
pub struct SemanticTokensBuilder {
    tokens: Vec<SemanticToken>,
    prev_line: u32,
    prev_char: u32,
}

impl SemanticTokensBuilder {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            prev_line: 0,
            prev_char: 0,
        }
    }

    /// Add a token
    pub fn push(&mut self, line: u32, character: u32, length: u32, token_type: u32, modifier: u32) {
        let delta_line = line - self.prev_line;
        let delta_start = if delta_line == 0 {
            character - self.prev_char
        } else {
            character
        };

        self.tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: modifier,
        });

        self.prev_line = line;
        self.prev_char = character;
    }

    /// Build the final token list
    pub fn build(self) -> Vec<SemanticToken> {
        self.tokens
    }
}

impl Default for SemanticTokensBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Compute semantic tokens for a module
pub fn compute_semantic_tokens(
    source: &str,
    _module: &Module,
    tokens: &[SpannedToken],
) -> Vec<SemanticToken> {
    let mut builder = SemanticTokensBuilder::new();

    // First pass: token-based highlighting (keywords, literals, operators)
    for token in tokens {
        if let Some((token_type, modifier)) = classify_token(&token.token) {
            let start_pos = offset_to_position(source, token.span.start);
            let length = (token.span.end - token.span.start) as u32;

            builder.push(
                start_pos.line,
                start_pos.character,
                length,
                token_type,
                modifier,
            );
        }
    }

    // TODO: Second pass: AST-based highlighting (identifiers with semantic meaning)
    // For now, we rely on token-based highlighting for simplicity

    builder.build()
}

/// Compute semantic tokens for a specific range
pub fn compute_semantic_tokens_range(
    source: &str,
    _module: &Module,
    tokens: &[SpannedToken],
    start_line: u32,
    end_line: u32,
) -> Vec<SemanticToken> {
    let mut builder = SemanticTokensBuilder::new();

    for token in tokens {
        let start_pos = offset_to_position(source, token.span.start);

        // Skip tokens outside the requested range
        if start_pos.line < start_line || start_pos.line > end_line {
            continue;
        }

        if let Some((token_type, modifier)) = classify_token(&token.token) {
            let length = (token.span.end - token.span.start) as u32;

            builder.push(
                start_pos.line,
                start_pos.character,
                length,
                token_type,
                modifier,
            );
        }
    }

    builder.build()
}

/// Classify a lexer token
fn classify_token(token: &Token) -> Option<(u32, u32)> {
    match token {
        // Keywords
        Token::Fn
        | Token::Let
        | Token::If
        | Token::Else
        | Token::Elif
        | Token::Match
        | Token::Return
        | Token::Struct
        | Token::Enum
        | Token::Trait
        | Token::Impl
        | Token::Import
        | Token::From
        | Token::Pub
        | Token::Mut
        | Token::Async
        | Token::Await
        | Token::Spawn
        | Token::Concurrent
        | Token::For
        | Token::While
        | Token::Loop
        | Token::Break
        | Token::Continue
        | Token::In
        | Token::And
        | Token::Or
        | Token::Not
        | Token::True
        | Token::False
        | Token::Unit
        | Token::Type
        | Token::Mod
        | Token::Use
        | Token::Where
        | Token::Chan
        | Token::Raise
        | Token::Handle
        | Token::Resume
        | Token::Effect
        | Token::Self_
        | Token::Pass
        | Token::Const
        | Token::As
        | Token::With => Some((TokenType::Keyword as u32, 0)),

        // Literals
        Token::Integer(_) | Token::Float(_) => Some((TokenType::Number as u32, 0)),
        Token::String(_) => Some((TokenType::String as u32, 0)),
        Token::Char(_) => Some((TokenType::String as u32, 0)),
        Token::Bool(_) => Some((TokenType::Keyword as u32, 0)), // true/false as keywords

        // Operators - using correct Token variant names from jet_lexer
        Token::Plus
        | Token::Minus
        | Token::Star
        | Token::Slash
        | Token::Percent
        | Token::Power
        | Token::Eq
        | Token::Ne
        | Token::Lt
        | Token::Gt
        | Token::Le
        | Token::Ge
        | Token::AndBit
        | Token::OrBit
        | Token::Xor
        | Token::NotBit
        | Token::Shl
        | Token::Shr
        | Token::Arrow
        | Token::FatArrow
        | Token::Colon
        | Token::ColonColon
        | Token::Dot
        | Token::DotDot
        | Token::DotDotDot
        | Token::Comma
        | Token::Semi
        | Token::Question
        | Token::At
        | Token::Underscore
        | Token::Bang
        | Token::Assign
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::StarAssign
        | Token::SlashAssign
        | Token::PercentAssign
        | Token::AndAssign
        | Token::OrAssign
        | Token::XorAssign
        | Token::ShlAssign
        | Token::ShrAssign
        | Token::LParen
        | Token::RParen
        | Token::LBracket
        | Token::RBracket
        | Token::LBrace
        | Token::RBrace => Some((TokenType::Operator as u32, 0)),

        // Identifiers - we'll handle these via AST
        Token::Ident(_) => None,
        Token::Label(_) => None,

        // Other tokens - skip
        Token::Eof => None,
        Token::Error(_) => None,
        Token::Newline => None,
        Token::Indent => None,
        Token::Dedent => None,
    }
}

/// Get semantic token types for LSP server capabilities
pub fn get_semantic_token_types() -> Vec<SemanticTokenType> {
    vec![
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::TYPE,
        SemanticTokenType::CLASS,
        SemanticTokenType::ENUM,
        SemanticTokenType::INTERFACE,
        SemanticTokenType::STRUCT,
        SemanticTokenType::TYPE_PARAMETER,
        SemanticTokenType::PARAMETER,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::METHOD,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::MODIFIER,
        SemanticTokenType::COMMENT,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::OPERATOR,
    ]
}

/// Get semantic token modifiers for LSP server capabilities
pub fn get_semantic_token_modifiers() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DECLARATION,
        SemanticTokenModifier::DEFINITION,
        SemanticTokenModifier::READONLY,
        SemanticTokenModifier::STATIC,
        SemanticTokenModifier::ABSTRACT,
        SemanticTokenModifier::DEPRECATED,
        SemanticTokenModifier::ASYNC,
        SemanticTokenModifier::MODIFICATION,
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position() {
        let source = "line1\nline2\nline3";

        // Start of file
        let pos = offset_to_position(source, 0);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);

        // After "line1\n" (position 6)
        let pos = offset_to_position(source, 6);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);

        // Middle of line 2
        let pos = offset_to_position(source, 8);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 2);
    }

    #[test]
    fn test_semantic_tokens_builder() {
        let mut builder = SemanticTokensBuilder::new();

        // Add tokens at increasing positions
        builder.push(0, 0, 2, TokenType::Keyword as u32, 0); // "fn"
        builder.push(
            0,
            3,
            4,
            TokenType::Function as u32,
            TokenModifier::Declaration.to_bitset(),
        ); // "main"
        builder.push(1, 4, 5, TokenType::Keyword as u32, 0); // "let"

        let tokens = builder.build();
        assert_eq!(tokens.len(), 3);

        // Check first token
        assert_eq!(tokens[0].delta_line, 0);
        assert_eq!(tokens[0].delta_start, 0);
        assert_eq!(tokens[0].length, 2);

        // Check second token (same line)
        assert_eq!(tokens[1].delta_line, 0);
        assert_eq!(tokens[1].delta_start, 3);
        assert_eq!(tokens[1].length, 4);

        // Check third token (new line)
        assert_eq!(tokens[2].delta_line, 1);
        assert_eq!(tokens[2].delta_start, 4);
    }

    #[test]
    fn test_classify_token() {
        assert!(classify_token(&Token::Fn).is_some());
        assert!(classify_token(&Token::Let).is_some());
        assert!(classify_token(&Token::Integer("42".to_string())).is_some());
        assert!(classify_token(&Token::String("hello".to_string())).is_some());
        assert!(classify_token(&Token::Plus).is_some());
        assert!(classify_token(&Token::Ident("foo".to_string())).is_none());
    }
}
