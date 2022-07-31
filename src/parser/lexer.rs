pub mod lexing_rules;
#[cfg(test)]
mod tests;

use std::{fmt, path::PathBuf, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Whitespace,
    Eof,
    Symbol(String),
    Special(String),
    Ident(String),
    Number(String),
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Whitespace => write!(f, " "),
            TokenKind::Eof => write!(f, "<EOF>"),
            TokenKind::Symbol(s) => write!(f, "{}", s),
            TokenKind::Special(s) => write!(f, "{}", s),
            TokenKind::Ident(s) => write!(f, "{}", s),
            TokenKind::Number(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub source: Rc<PathBuf>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(source: Rc<PathBuf>, start: usize, end: usize) -> Self {
        Self { source, start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    fn width(&self) -> usize {
        self.span.end - self.span.start
    }
}
#[derive(Debug, Clone)]
pub struct Lexer {
    source: Rc<PathBuf>,
    input: String,
    position: usize,
}

impl Lexer {
    pub fn new(source: Rc<PathBuf>, input: String) -> Self {
        Self {
            source,
            input,
            position: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while self.peek_token().kind != TokenKind::Eof {
            self.skip_ws();
            let token = self.peek_token();
            self.expect_token(&token);
            tokens.push(token);
        }
        return tokens;
    }

    fn peek_token_at(&self, position: usize) -> Token {
        assert!(
            self.remaining().is_char_boundary(position),
            "invalid position: {}, remaining: '{}'",
            position,
            self.remaining()
        );

        let input = &self.remaining()[position..];
        tracing::trace!(self.position, input, "Lexer::next");

        if input.is_empty() {
            // EOF
            return Token::new(
                TokenKind::Eof,
                Span::new(self.source.clone(), self.position, self.position),
            );
        }

        match input.chars().next().unwrap() {
            '0'..='9' => {
                let value = input.split(|c| !('0'..='9').contains(&c)).next().unwrap();
                Token::new(
                    TokenKind::Number(value.to_string()),
                    Span::new(
                        self.source.clone(),
                        self.position + position,
                        self.position + position + value.len(),
                    ),
                )
            }
            c if lexing_rules::is_special(c) => {
                let value = c.to_string();
                Token::new(
                    TokenKind::Special(value.to_string()),
                    Span::new(
                        self.source.clone(),
                        self.position + position,
                        self.position + position + value.len(),
                    ),
                )
            }
            c if lexing_rules::is_symbol(c) => {
                let value = input.split(|c| !lexing_rules::is_symbol(c)).next().unwrap();
                Token::new(
                    TokenKind::Symbol(value.to_string()),
                    Span::new(
                        self.source.clone(),
                        self.position + position,
                        self.position + position + value.len(),
                    ),
                )
            }
            c if lexing_rules::is_ident_start(c) => {
                let value = input
                    .split(|c| !lexing_rules::is_ident_continue(c))
                    .next()
                    .unwrap();
                Token::new(
                    TokenKind::Ident(value.to_string()),
                    Span::new(
                        self.source.clone(),
                        self.position + position,
                        self.position + position + value.len(),
                    ),
                )
            }
            c if c.is_whitespace() => {
                let value = input.split(|c: char| !c.is_whitespace()).next().unwrap();
                Token::new(
                    TokenKind::Whitespace,
                    Span::new(
                        self.source.clone(),
                        self.position + position,
                        self.position + position + value.len(),
                    ),
                )
            }
            c => Token::new(
                TokenKind::Symbol(c.to_string()),
                Span::new(
                    self.source.clone(),
                    self.position + position,
                    self.position + position + c.len_utf8(),
                ),
            ),
        }
    }

    fn peek_token(&self) -> Token {
        self.peek_token_at(0)
    }

    /// Expects the given token at the current position.
    fn expect_token(&mut self, token: &Token) {
        let lookahead = self.peek_token();
        if lookahead == *token {
            self.consume_token();
        } else {
            panic!("expected {:?}, got {:?}", token, lookahead);
        }
    }

    /// Returns the remaining input after the current position.
    fn remaining(&self) -> &str {
        &self.input[self.position..]
    }

    /// Consumes the first n characters of the input.
    fn consume_chars(&mut self, n: usize) {
        self.position += n;
    }

    /// Consumes the next token.
    fn consume_token(&mut self) {
        let token = self.peek_token();
        self.consume_chars(token.width());
    }

    /// Skips whitespace characters.
    fn skip_ws(&mut self) {
        let token = self.peek_token();
        if token.kind == TokenKind::Whitespace {
            self.expect_token(&token);
        }
    }
}
