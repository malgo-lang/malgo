pub mod lexer;
#[cfg(test)]
mod tests;

use self::lexer::{Span, Token, TokenKind};

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

    // expr ::= let | fun | if | equality
    // let ::= "let" ident "=" expr "in" expr
    // fun ::= "{" params "->" expr "}"
    // params ::= ident ("," ident)*
    // if ::= "if" expr "then" expr "else" expr
    // equality ::= add (("==" | "!=") add)*
    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_add()
    }

    /// add ::= mul (("+" | "-") mul)*
    fn parse_add(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_mul()?;
        loop {
            match self.peek() {
                Some(Token {
                    kind: TokenKind::Symbol(s),
                    span: _,
                }) if *s == "+" => {
                    self.current += 1;
                    let rhs = self.parse_mul()?;
                    lhs = Expr::new(
                        concat_span(&lhs.span, &rhs.span),
                        ExprKind::Binary {
                            left: Box::new(lhs),
                            operator: BinaryOperator::Plus,
                            right: Box::new(rhs),
                        },
                    );
                }
                Some(Token {
                    kind: TokenKind::Symbol(s),
                    span: _,
                }) if *s == "-" => {
                    self.current += 1;
                    let rhs = self.parse_mul()?;
                    lhs = Expr::new(
                        concat_span(&lhs.span, &rhs.span),
                        ExprKind::Binary {
                            left: Box::new(lhs),
                            operator: BinaryOperator::Minus,
                            right: Box::new(rhs),
                        },
                    );
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    /// mul ::= unary (("*" | "/") unary)*
    fn parse_mul(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_unary()?;
        loop {
            match self.peek() {
                Some(Token {
                    kind: TokenKind::Symbol(s),
                    span: _,
                }) if *s == "*" => {
                    self.current += 1;
                    let rhs = self.parse_unary()?;
                    lhs = Expr::new(
                        concat_span(&lhs.span, &rhs.span),
                        ExprKind::Binary {
                            left: Box::new(lhs),
                            operator: BinaryOperator::Asterisk,
                            right: Box::new(rhs),
                        },
                    );
                }
                Some(Token {
                    kind: TokenKind::Symbol(s),
                    span: _,
                }) if *s == "/" => {
                    self.current += 1;
                    let rhs = self.parse_unary()?;
                    lhs = Expr::new(
                        concat_span(&lhs.span, &rhs.span),
                        ExprKind::Binary {
                            left: Box::new(lhs),
                            operator: BinaryOperator::Slash,
                            right: Box::new(rhs),
                        },
                    );
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    /// unary ::= "-" unary | primary
    fn parse_unary(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token {
                kind: TokenKind::Symbol(s),
                span,
            }) if *s == "-" => {
                let span = span.clone();
                self.current += 1;
                let expr = self.parse_unary()?;
                Ok(Expr::new(
                    concat_span(&span, &expr.span),
                    ExprKind::Unary {
                        operator: UnaryOperator::Minus,
                        operand: Box::new(expr),
                    },
                ))
            }
            _ => self.parse_primary(),
        }
    }

    /// primary ::= ident | number | "(" expr ")"
    fn parse_primary(&mut self) -> Result<Expr, String> {
        if let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::Ident(_) => self.parse_variable(),
                TokenKind::Number(_) => self.parse_number(),
                TokenKind::Special(s) if *s == "(" => self.parse_paren(),
                _ => Err(format!("unexpected token: {}", token)),
            }
        } else {
            Err("unexpected EOF".to_string())
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn parse_variable(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token {
                span,
                kind: TokenKind::Ident(s),
            }) => {
                let result = Ok(Expr::new(span.to_owned(), ExprKind::Variable(s.to_owned())));
                self.current += 1;
                result
            }
            Some(token) => Err(format!("unexpected token: {}", token)),
            None => Err("unexpected EOF".to_owned()),
        }
    }

    fn parse_number(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token {
                span,
                kind: TokenKind::Number(s),
            }) => {
                let result = Ok(Expr::new(
                    span.to_owned(),
                    ExprKind::Number(s.parse().unwrap()),
                ));
                self.current += 1;
                result
            }
            Some(token) => Err(format!("unexpected token: {}", token)),
            None => Err("unexpected EOF".to_owned()),
        }
    }

    fn parse_paren(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token {
                span: _,
                kind: TokenKind::Special(s),
            }) if *s == "(" => {
                self.current += 1;
                let expr = self.parse_expr()?;
                match self.peek() {
                    Some(Token {
                        span: _,
                        kind: TokenKind::Special(s),
                    }) if *s == ")" => {
                        self.current += 1;
                        Ok(expr)
                    }
                    Some(token) => Err(format!("unexpected token: {}", token)),
                    None => Err("unexpected EOF".to_owned()),
                }
            }
            Some(token) => Err(format!("unexpected token: {}", token)),
            None => Err("unexpected EOF".to_owned()),
        }
    }
}

fn concat_span(span_1: &Span, span_2: &Span) -> Span {
    Span {
        source: span_1.source.clone(),
        start: span_1.start,
        end: span_2.end,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Variable(String),
    Number(u64),
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Let {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    Fun {
        parameters: Vec<String>,
        body: Box<Expr>,
    },
    Call {
        function: Box<Expr>,
        arguments: Vec<Box<Expr>>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
}
