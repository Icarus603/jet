use crate::{Span, SpannedToken, Token};

/// The lexer for the Jet programming language
pub struct Lexer<'a> {
    #[allow(dead_code)]
    input: &'a str,
    chars: std::str::Chars<'a>,
    current: Option<char>,
    position: usize,
    line: usize,
    column: usize,
    indent_stack: Vec<usize>,
    at_line_start: bool,
    pending_tokens: Vec<SpannedToken>,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given input
    pub fn new(input: &'a str) -> Self {
        let mut chars = input.chars();
        let current = chars.next();
        Self {
            input,
            chars,
            current,
            position: 0,
            line: 1,
            column: 1,
            indent_stack: vec![0],
            at_line_start: true,
            pending_tokens: Vec::new(),
        }
    }

    /// Tokenize the entire input and return all tokens
    pub fn tokenize(&mut self) -> Vec<SpannedToken> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            let is_eof = matches!(token.token, Token::Eof);
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    /// Get the next token
    pub fn next_token(&mut self) -> SpannedToken {
        // Return any pending tokens first (from indentation handling)
        if let Some(token) = self.pending_tokens.pop() {
            return token;
        }

        // Handle indentation at line start
        if self.at_line_start {
            if let Some(token) = self.handle_indentation() {
                return token;
            }
        }

        self.skip_whitespace();

        let start = self.position;
        let _start_line = self.line;
        let _start_col = self.column;

        let token = match self.current {
            None => {
                // Before returning EOF, emit any pending dedents
                if self.indent_stack.len() > 1 {
                    self.indent_stack.pop();
                    let span = Span::new(self.position, self.position);
                    return SpannedToken {
                        token: Token::Dedent,
                        span,
                    };
                }
                Token::Eof
            }
            Some(ch) => match ch {
                '\n' => {
                    self.advance();
                    self.at_line_start = true;
                    Token::Newline
                }
                '#' => self.lex_comment(),
                '"' => self.lex_string(),
                'r' if self.peek() == Some('"') || self.peek() == Some('#') => {
                    self.lex_raw_string()
                }
                '\'' => self.lex_char(),
                '(' => {
                    self.advance();
                    Token::LParen
                }
                ')' => {
                    self.advance();
                    Token::RParen
                }
                '[' => {
                    self.advance();
                    Token::LBracket
                }
                ']' => {
                    self.advance();
                    Token::RBracket
                }
                '{' => {
                    self.advance();
                    Token::LBrace
                }
                '}' => {
                    self.advance();
                    Token::RBrace
                }
                ':' => {
                    self.advance();
                    if self.current == Some(':') {
                        self.advance();
                        Token::ColonColon
                    } else {
                        Token::Colon
                    }
                }
                ';' => {
                    self.advance();
                    Token::Semi
                }
                ',' => {
                    self.advance();
                    Token::Comma
                }
                '.' => {
                    self.advance();
                    if self.current == Some('.') {
                        self.advance();
                        if self.current == Some('.') {
                            self.advance();
                            Token::DotDotDot
                        } else {
                            Token::DotDot
                        }
                    } else {
                        Token::Dot
                    }
                }
                '+' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::PlusAssign
                    } else {
                        Token::Plus
                    }
                }
                '-' => {
                    self.advance();
                    if self.current == Some('>') {
                        self.advance();
                        Token::Arrow
                    } else if self.current == Some('=') {
                        self.advance();
                        Token::MinusAssign
                    } else {
                        Token::Minus
                    }
                }
                '*' => {
                    self.advance();
                    if self.current == Some('*') {
                        self.advance();
                        if self.current == Some('=') {
                            self.advance();
                            Token::Error("**= not supported".to_string())
                        } else {
                            Token::Power
                        }
                    } else if self.current == Some('=') {
                        self.advance();
                        Token::StarAssign
                    } else {
                        Token::Star
                    }
                }
                '/' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::SlashAssign
                    } else {
                        Token::Slash
                    }
                }
                '%' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::PercentAssign
                    } else {
                        Token::Percent
                    }
                }
                '=' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::Eq
                    } else if self.current == Some('>') {
                        self.advance();
                        Token::FatArrow
                    } else {
                        Token::Assign
                    }
                }
                '!' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::Ne
                    } else {
                        Token::Bang
                    }
                }
                '<' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::Le
                    } else if self.current == Some('<') {
                        self.advance();
                        if self.current == Some('=') {
                            self.advance();
                            Token::ShlAssign
                        } else {
                            Token::Shl
                        }
                    } else {
                        Token::Lt
                    }
                }
                '>' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::Ge
                    } else if self.current == Some('>') {
                        self.advance();
                        if self.current == Some('=') {
                            self.advance();
                            Token::ShrAssign
                        } else {
                            Token::Shr
                        }
                    } else {
                        Token::Gt
                    }
                }
                '&' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::AndAssign
                    } else {
                        Token::AndBit
                    }
                }
                '|' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::OrAssign
                    } else {
                        Token::OrBit
                    }
                }
                '^' => {
                    self.advance();
                    if self.current == Some('=') {
                        self.advance();
                        Token::XorAssign
                    } else {
                        Token::Xor
                    }
                }
                '~' => {
                    self.advance();
                    Token::NotBit
                }
                '?' => {
                    self.advance();
                    Token::Question
                }
                '@' => {
                    // Check if followed by an identifier (annotation)
                    // We need to look ahead to see if it's a known annotation
                    if let Some(ch) = self.peek() {
                        if is_ident_start(ch) {
                            // Look ahead to read the full identifier without consuming
                            let mut lookahead = self.chars.clone();
                            let mut ident = String::new();
                            ident.push(ch);
                            lookahead.next(); // consume the first char we already peeked
                            for c in lookahead {
                                if is_ident_continue(c) {
                                    ident.push(c);
                                } else {
                                    break;
                                }
                            }

                            // Check if it's a known annotation keyword
                            let is_annotation = matches!(
                                ident.as_str(),
                                "confidence"
                                    | "generated_by"
                                    | "prompt"
                                    | "human_edit_count"
                                    | "spec"
                                    | "example"
                                    | "test_for"
                            );

                            if is_annotation {
                                // Consume the '@' and lex the annotation
                                self.advance();
                                self.lex_annotation()
                            } else {
                                // Not a known annotation, just return At
                                self.advance();
                                Token::At
                            }
                        } else {
                            self.advance();
                            Token::At
                        }
                    } else {
                        self.advance();
                        Token::At
                    }
                }
                '_' => {
                    // Check if this is a standalone _ (wildcard) or start of identifier
                    self.advance();
                    if is_ident_continue(self.current.unwrap_or('\0')) {
                        // It's an identifier starting with _
                        self.lex_identifier_rest("_".to_string())
                    } else {
                        Token::Underscore
                    }
                }
                ch if ch.is_ascii_digit() => self.lex_number(),
                ch if is_ident_start(ch) => self.lex_identifier(),
                ch => {
                    self.advance();
                    Token::Error(format!("Invalid character: {}", ch))
                }
            },
        };

        let span = Span::new(start, self.position);
        SpannedToken { token, span }
    }

    /// Handle Python-style indentation
    fn handle_indentation(&mut self) -> Option<SpannedToken> {
        self.at_line_start = false;

        // Count leading whitespace
        let mut spaces = 0;
        let start_line = self.line;
        let _start_col = self.column;

        while let Some(ch) = self.current {
            match ch {
                ' ' => {
                    spaces += 1;
                    self.advance();
                }
                '\t' => {
                    spaces += 4; // Treat tab as 4 spaces
                    self.advance();
                }
                _ => break,
            }
        }

        // Skip empty lines and comment-only lines
        if self.current == Some('\n') || self.current == Some('#') {
            return None;
        }

        let current_indent = *self.indent_stack.last().unwrap();

        // At EOF, we need to dedent to base level
        if self.current.is_none() && spaces < current_indent {
            // Dedent - may need multiple dedents
            let mut dedents = Vec::new();
            while spaces < *self.indent_stack.last().unwrap() {
                self.indent_stack.pop();
                let span = Span::new(self.position.saturating_sub(spaces), self.position);
                dedents.push(SpannedToken {
                    token: Token::Dedent,
                    span,
                });
            }

            // Check for inconsistent indentation
            if spaces != *self.indent_stack.last().unwrap() {
                let span = Span::new(self.position.saturating_sub(spaces), self.position);
                return Some(SpannedToken {
                    token: Token::Error(format!(
                        "Inconsistent indentation at line {}: unindent does not match any outer level",
                        start_line
                    )),
                    span,
                });
            }

            // Return dedents in reverse order (LIFO)
            if !dedents.is_empty() {
                let first = dedents.remove(0);
                self.pending_tokens = dedents;
                return Some(first);
            }
        }

        // At EOF with no dedent needed, skip
        self.current?;

        if spaces > current_indent {
            // Indent
            self.indent_stack.push(spaces);
            let span = Span::new(self.position.saturating_sub(spaces), self.position);
            return Some(SpannedToken {
                token: Token::Indent,
                span,
            });
        } else if spaces < current_indent {
            // Dedent - may need multiple dedents
            let mut dedents = Vec::new();
            while spaces < *self.indent_stack.last().unwrap() {
                self.indent_stack.pop();
                let span = Span::new(self.position.saturating_sub(spaces), self.position);
                dedents.push(SpannedToken {
                    token: Token::Dedent,
                    span,
                });
            }

            // Check for inconsistent indentation
            if spaces != *self.indent_stack.last().unwrap() {
                let span = Span::new(self.position.saturating_sub(spaces), self.position);
                return Some(SpannedToken {
                    token: Token::Error(format!(
                        "Inconsistent indentation at line {}: unindent does not match any outer level",
                        start_line
                    )),
                    span,
                });
            }

            // Return dedents in reverse order (LIFO)
            if !dedents.is_empty() {
                let first = dedents.remove(0);
                self.pending_tokens = dedents;
                return Some(first);
            }
        }

        None
    }

    /// Lex a comment (starts with #)
    fn lex_comment(&mut self) -> Token {
        let _start = self.position;
        self.advance(); // consume #

        // Check for doc comment (###)
        if self.current == Some('#') && self.peek() == Some('#') {
            self.advance();
            self.advance();
            let mut content = String::new();
            while let Some(ch) = self.current {
                if ch == '\n' {
                    break;
                }
                content.push(ch);
                self.advance();
            }
            // Return as doc comment token
            return Token::DocComment(content);
        }

        // Regular comment - skip it and get next token
        while let Some(ch) = self.current {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
        // Return the next token after the comment
        self.next_token().token
    }

    /// Lex a string literal
    fn lex_string(&mut self) -> Token {
        self.advance(); // consume opening "
        let mut content = String::new();

        while let Some(ch) = self.current {
            match ch {
                '"' => {
                    self.advance();
                    return Token::String(content);
                }
                '\\' => {
                    self.advance();
                    match self.current {
                        Some('n') => {
                            content.push('\n');
                            self.advance();
                        }
                        Some('t') => {
                            content.push('\t');
                            self.advance();
                        }
                        Some('r') => {
                            content.push('\r');
                            self.advance();
                        }
                        Some('\\') => {
                            content.push('\\');
                            self.advance();
                        }
                        Some('"') => {
                            content.push('"');
                            self.advance();
                        }
                        Some('0') => {
                            content.push('\0');
                            self.advance();
                        }
                        Some('\'') => {
                            content.push('\'');
                            self.advance();
                        }
                        Some('u') => {
                            self.advance();
                            if self.current == Some('{') {
                                self.advance();
                                let mut hex = String::new();
                                let mut found_close = false;
                                while let Some(ch) = self.current {
                                    if ch == '}' {
                                        self.advance();
                                        found_close = true;
                                        break;
                                    }
                                    if ch.is_ascii_hexdigit() {
                                        hex.push(ch);
                                        self.advance();
                                    } else {
                                        break;
                                    }
                                }
                                if !found_close {
                                    return Token::Error("Unclosed Unicode escape".to_string());
                                }
                                if let Ok(code) = u32::from_str_radix(&hex, 16) {
                                    if let Some(c) = char::from_u32(code) {
                                        content.push(c);
                                    } else {
                                        return Token::Error(format!(
                                            "Invalid Unicode code point: {}",
                                            hex
                                        ));
                                    }
                                } else {
                                    return Token::Error(format!(
                                        "Invalid Unicode escape: {}",
                                        hex
                                    ));
                                }
                            } else {
                                return Token::Error(
                                    "Invalid Unicode escape: expected {{".to_string(),
                                );
                            }
                        }
                        Some(c) => {
                            return Token::Error(format!("Invalid escape sequence: \\ {}", c));
                        }
                        None => {
                            return Token::Error("Unterminated escape sequence".to_string());
                        }
                    }
                }
                '\n' => {
                    return Token::Error("Unterminated string literal".to_string());
                }
                _ => {
                    content.push(ch);
                    self.advance();
                }
            }
        }

        Token::Error("Unterminated string literal".to_string())
    }

    /// Lex a raw string literal (r"..." or r#"..."#)
    fn lex_raw_string(&mut self) -> Token {
        self.advance(); // consume 'r'

        // Count # characters
        let mut hashes = 0;
        while self.current == Some('#') {
            hashes += 1;
            self.advance();
        }

        if self.current != Some('"') {
            return Token::Error("Expected \" after r".to_string());
        }
        self.advance(); // consume opening "

        let mut content = String::new();

        loop {
            match self.current {
                Some('"') => {
                    self.advance();
                    // Check for matching # characters
                    let mut close_hashes = 0;
                    while self.current == Some('#') && close_hashes < hashes {
                        close_hashes += 1;
                        self.advance();
                    }
                    if close_hashes == hashes {
                        return Token::String(content);
                    } else {
                        content.push('"');
                        for _ in 0..close_hashes {
                            content.push('#');
                        }
                    }
                }
                Some(ch) => {
                    content.push(ch);
                    self.advance();
                }
                None => {
                    return Token::Error("Unterminated raw string literal".to_string());
                }
            }
        }
    }

    /// Lex a character literal or label
    fn lex_char(&mut self) -> Token {
        self.advance(); // consume opening '

        // Check if this is a label: 'identifier followed by :
        // We need to look ahead: if we see an identifier and then a colon,
        // it's a label, not a char literal
        if let Some(ch) = self.current {
            if is_ident_start(ch) {
                // Look ahead to determine if this is a label or char literal
                // Labels: 'identifier (optionally followed by : for loop labels)
                // Char literals: 'x' (single char with closing quote)
                //
                // We need to decide without context whether 'abc is a label or an unterminated char.
                // Rule: Multi-char after ' is a label. Single-char without closing ' is an error.
                let saved_pos = self.position;
                let saved_line = self.line;
                let saved_col = self.column;
                let saved_current = self.current;
                let saved_chars = self.chars.clone();

                // Try to read an identifier
                let mut label = String::new();
                label.push(ch);
                self.advance();
                while let Some(c) = self.current {
                    if is_ident_continue(c) {
                        label.push(c);
                        self.advance();
                    } else {
                        break;
                    }
                }

                // Decision logic:
                // 1. If followed by ':', it's definitely a label ('label:)
                // 2. If identifier length > 1, it's a label ('outer)
                // 3. If identifier length == 1 and followed by '\'', it's a char literal ('x')
                // 4. Otherwise (single char not followed by '\''), it's an unterminated char error
                let followed_by_colon = self.current == Some(':');
                let followed_by_quote = self.current == Some('\'');

                if followed_by_colon || label.len() > 1 {
                    // Label (either has colon or is multi-character)
                    return Token::Label(label);
                } else if followed_by_quote {
                    // Single char followed by closing quote - char literal
                    // Restore and parse as char
                    self.position = saved_pos;
                    self.line = saved_line;
                    self.column = saved_col;
                    self.current = saved_current;
                    self.chars = saved_chars;
                } else {
                    // Single char without closing quote - error (unterminated char)
                    // Restore and let char literal code handle the error
                    self.position = saved_pos;
                    self.line = saved_line;
                    self.column = saved_col;
                    self.current = saved_current;
                    self.chars = saved_chars;
                }
            }
        }

        let ch = match self.current {
            Some('\\') => {
                self.advance();
                match self.current {
                    Some('n') => {
                        self.advance();
                        '\n'
                    }
                    Some('t') => {
                        self.advance();
                        '\t'
                    }
                    Some('r') => {
                        self.advance();
                        '\r'
                    }
                    Some('\\') => {
                        self.advance();
                        '\\'
                    }
                    Some('\'') => {
                        self.advance();
                        '\''
                    }
                    Some('0') => {
                        self.advance();
                        '\0'
                    }
                    Some('u') => {
                        self.advance();
                        if self.current == Some('{') {
                            self.advance();
                            let mut hex = String::new();
                            while let Some(ch) = self.current {
                                if ch == '}' {
                                    self.advance();
                                    break;
                                }
                                if ch.is_ascii_hexdigit() {
                                    hex.push(ch);
                                    self.advance();
                                } else {
                                    break;
                                }
                            }
                            if let Ok(code) = u32::from_str_radix(&hex, 16) {
                                if let Some(c) = char::from_u32(code) {
                                    c
                                } else {
                                    return Token::Error(format!(
                                        "Invalid Unicode code point: {}",
                                        hex
                                    ));
                                }
                            } else {
                                return Token::Error(format!("Invalid Unicode escape: {}", hex));
                            }
                        } else {
                            return Token::Error("Invalid Unicode escape: expected {{".to_string());
                        }
                    }
                    Some(c) => {
                        return Token::Error(format!("Invalid escape sequence: \\ {}", c));
                    }
                    None => {
                        return Token::Error("Unterminated char literal".to_string());
                    }
                }
            }
            Some(ch) => {
                self.advance();
                ch
            }
            None => {
                return Token::Error("Unterminated char literal".to_string());
            }
        };

        if self.current != Some('\'') {
            return Token::Error("Invalid char literal: expected closing '".to_string());
        }
        self.advance(); // consume closing '

        Token::Char(ch)
    }

    /// Lex a number (integer or float)
    fn lex_number(&mut self) -> Token {
        let _start = self.position;

        // Check for base prefix
        let base = if self.current == Some('0') {
            match self.peek() {
                Some('x') | Some('X') => {
                    self.advance();
                    self.advance();
                    16
                }
                Some('o') | Some('O') => {
                    self.advance();
                    self.advance();
                    8
                }
                Some('b') | Some('B') => {
                    self.advance();
                    self.advance();
                    2
                }
                _ => 10,
            }
        } else {
            10
        };

        let mut digits = String::new();
        while let Some(ch) = self.current {
            if ch == '_' {
                self.advance(); // underscore is ignored in numbers
                continue;
            }
            if ch.is_digit(base) {
                digits.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for decimal point (float)
        // Note: "5." is parsed as Integer(5) followed by Dot, not Float(5.)
        // A float requires at least one digit after the decimal point
        if base == 10
            && self.current == Some('.')
            && self.peek().is_some_and(|c| c.is_ascii_digit())
        {
            self.advance(); // consume .
            digits.push('.');
            while let Some(ch) = self.current {
                if ch == '_' {
                    self.advance();
                    continue;
                }
                if ch.is_ascii_digit() {
                    digits.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Check for exponent
        if base == 10 && (self.current == Some('e') || self.current == Some('E')) {
            self.advance();
            digits.push('e');
            if self.current == Some('+') || self.current == Some('-') {
                digits.push(self.current.unwrap());
                self.advance();
            }
            while let Some(ch) = self.current {
                if ch == '_' {
                    self.advance();
                    continue;
                }
                if ch.is_ascii_digit() {
                    digits.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if digits.contains('.') || digits.contains('e') {
            Token::Float(digits)
        } else {
            Token::Integer(digits)
        }
    }

    /// Lex an identifier or keyword
    fn lex_identifier(&mut self) -> Token {
        let mut ident = String::new();
        while let Some(ch) = self.current {
            if is_ident_continue(ch) {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for keywords
        match ident.as_str() {
            "and" => Token::And,
            "as" => Token::As,
            "async" => Token::Async,
            "await" => Token::Await,
            "break" => Token::Break,
            "chan" => Token::Chan,
            "concurrent" => Token::Concurrent,
            "const" => Token::Const,
            "continue" => Token::Continue,
            "elif" => Token::Elif,
            "else" => Token::Else,
            "enum" => Token::Enum,
            "false" => Token::False,
            "fn" => Token::Fn,
            "for" => Token::For,
            "from" => Token::From,
            "if" => Token::If,
            "impl" => Token::Impl,
            "import" => Token::Import,
            "in" => Token::In,
            "let" => Token::Let,
            "loop" => Token::Loop,
            "match" => Token::Match,
            "mod" => Token::Mod,
            "mut" => Token::Mut,
            "not" => Token::Not,
            "or" => Token::Or,
            "pass" => Token::Pass,
            "pub" => Token::Pub,
            "raise" => Token::Raise,
            "handle" => Token::Handle,
            "with" => Token::With,
            "resume" => Token::Resume,
            "effect" => Token::Effect,
            "return" => Token::Return,
            "self" => Token::Self_,
            "spawn" => Token::Spawn,
            "struct" => Token::Struct,
            "trait" => Token::Trait,
            "true" => Token::True,
            "type" => Token::Type,
            "unit" => Token::Unit,
            "use" => Token::Use,
            "where" => Token::Where,
            "while" => Token::While,
            "requires" => Token::Requires,
            "ensures" => Token::Ensures,
            "invariant" => Token::Invariant,
            "ghost" => Token::Ghost,
            "result" => Token::Result,
            _ => Token::Ident(ident),
        }
    }

    /// Lex the rest of an identifier given an initial string
    fn lex_identifier_rest(&mut self, mut ident: String) -> Token {
        while let Some(ch) = self.current {
            if is_ident_continue(ch) {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for keywords (identifiers starting with _ can't be keywords)
        Token::Ident(ident)
    }

    /// Lex an annotation token (after @)
    fn lex_annotation(&mut self) -> Token {
        let mut ident = String::new();
        while let Some(ch) = self.current {
            if is_ident_continue(ch) {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        // Check for AI annotation keywords and literate programming annotations
        match ident.as_str() {
            "confidence" => Token::Confidence,
            "generated_by" => Token::GeneratedBy,
            "prompt" => Token::Prompt,
            "human_edit_count" => Token::HumanEditCount,
            "spec" => Token::AtSpec,
            "example" => Token::AtExample,
            "test_for" => Token::AtTestFor,
            _ => Token::Ident(ident),
        }
    }

    /// Advance to the next character
    fn advance(&mut self) {
        if let Some(ch) = self.current {
            self.position += ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        self.current = self.chars.next();
    }

    /// Peek at the next character without consuming it
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// Skip whitespace (but not newlines)
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current {
            if ch.is_ascii_whitespace() && ch != '\n' {
                self.advance();
            } else {
                break;
            }
        }
    }
}

/// Check if a character can start an identifier
fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

/// Check if a character can continue an identifier
fn is_ident_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("let x = 5");
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 5); // let, x, =, 5, EOF
        assert!(matches!(tokens[0].token, Token::Let));
        assert!(matches!(tokens[1].token, Token::Ident(ref s) if s == "x"));
        assert!(matches!(tokens[2].token, Token::Assign));
        assert!(matches!(tokens[3].token, Token::Integer(ref s) if s == "5"));
    }
}
