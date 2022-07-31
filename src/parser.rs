pub mod ast;
mod expr;
pub mod lexer;
#[cfg(test)]
mod tests;

use self::{
    ast::Expr,
    lexer::{Span, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        self.parse_expr()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn consume(&mut self) {
        self.current += 1;
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Span, String> {
        match self.peek() {
            Some(Token { span, kind }) if *kind == expected => {
                let span = span.clone();
                self.consume();
                Ok(span)
            }
            Some(token) => Err(format!("unexpected token: {}", token)),
            None => Err("unexpected EOF".to_string()),
        }
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        match self.peek() {
            Some(Token {
                span: _,
                kind: TokenKind::Ident(s),
            }) => {
                let result = Ok(s.to_owned());
                self.consume();
                result
            }
            Some(token) => Err(format!("unexpected token: {}", token)),
            None => Err("unexpected EOF".to_string()),
        }
    }
}

fn concat_span(span_1: &Span, span_2: &Span) -> Span {
    assert_eq!(*span_1.source, *span_2.source);
    Span {
        source: span_1.source.clone(),
        start: span_1.start,
        end: span_2.end,
    }
}
