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
    fn parse_expr(&mut self) -> Result<Expr, String> {
        // let ::= "let" ident "=" expr "in" expr
        if let Ok(span1) = self.expect(TokenKind::Ident("let".to_string())) {
            let ident = self.expect_ident()?;
            let _ = self.expect(TokenKind::Symbol("=".to_string()))?;
            let value = self.parse_expr()?;
            let _ = self.expect(TokenKind::Ident("in".to_string()))?;
            let body = self.parse_expr()?;
            return Ok(Expr::new(
                concat_span(&span1, &body.span),
                ExprKind::Let {
                    name: ident,
                    value: Box::new(value),
                    body: Box::new(body),
                },
            ));
        }

        // fun ::= "{" params "->" expr "}"
        if let Ok(span1) = self.expect(TokenKind::Special("{".to_string())) {
            let mut parameters = Vec::new();
            // params ::= (ident ("," ident)*)?
            if let Err(_) = self.expect(TokenKind::Symbol("->".to_string())) {
                loop {
                    let ident = self.expect_ident()?;
                    parameters.push(ident);
                    if let Ok(_) = self.expect(TokenKind::Special(",".to_string())) {
                        continue;
                    } else {
                        let _ = self.expect(TokenKind::Symbol("->".to_string()))?;
                        break;
                    }
                }
            }
            let body = self.parse_expr()?;
            let _ = self.expect(TokenKind::Special("}".to_string()))?;
            return Ok(Expr::new(
                concat_span(&span1, &body.span),
                ExprKind::Fun {
                    parameters,
                    body: Box::new(body),
                },
            ));
        }

        // if ::= "if" expr "then" expr "else" expr
        if let Ok(span1) = self.expect(TokenKind::Ident("if".to_string())) {
            let cond = self.parse_expr()?;
            let _ = self.expect(TokenKind::Ident("then".to_string()))?;
            let then_branch = self.parse_expr()?;
            let _ = self.expect(TokenKind::Ident("else".to_string()))?;
            let else_branch = self.parse_expr()?;
            return Ok(Expr::new(
                concat_span(&span1, &else_branch.span),
                ExprKind::If {
                    condition: Box::new(cond),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                },
            ));
        }
        self.parse_equality()
    }

    /// equality ::= add ("==" add)*
    fn parse_equality(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_add()?;
        loop {
            if let Ok(_) = self.expect(TokenKind::Symbol("==".to_string())) {
                let rhs = self.parse_add()?;
                lhs = Expr::new(
                    concat_span(&lhs.span, &rhs.span),
                    ExprKind::Binary {
                        left: Box::new(lhs),
                        operator: BinaryOperator::Equal,
                        right: Box::new(rhs),
                    },
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// add ::= mul (("+" | "-") mul)*
    fn parse_add(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_mul()?;
        loop {
            if let Ok(_) = self.expect(TokenKind::Symbol("+".to_string())) {
                let rhs = self.parse_mul()?;
                lhs = Expr::new(
                    concat_span(&lhs.span, &rhs.span),
                    ExprKind::Binary {
                        left: Box::new(lhs),
                        operator: BinaryOperator::Plus,
                        right: Box::new(rhs),
                    },
                );
            } else if let Ok(_) = self.expect(TokenKind::Symbol("-".to_string())) {
                let rhs = self.parse_mul()?;
                lhs = Expr::new(
                    concat_span(&lhs.span, &rhs.span),
                    ExprKind::Binary {
                        left: Box::new(lhs),
                        operator: BinaryOperator::Minus,
                        right: Box::new(rhs),
                    },
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// mul ::= unary (("*" | "/") unary)*
    fn parse_mul(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_unary()?;
        loop {
            if let Ok(_) = self.expect(TokenKind::Symbol("*".to_string())) {
                let rhs = self.parse_unary()?;
                lhs = Expr::new(
                    concat_span(&lhs.span, &rhs.span),
                    ExprKind::Binary {
                        left: Box::new(lhs),
                        operator: BinaryOperator::Asterisk,
                        right: Box::new(rhs),
                    },
                );
            } else if let Ok(_) = self.expect(TokenKind::Symbol("/".to_string())) {
                let rhs = self.parse_unary()?;
                lhs = Expr::new(
                    concat_span(&lhs.span, &rhs.span),
                    ExprKind::Binary {
                        left: Box::new(lhs),
                        operator: BinaryOperator::Slash,
                        right: Box::new(rhs),
                    },
                );
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    /// unary ::= "-" unary | call
    fn parse_unary(&mut self) -> Result<Expr, String> {
        if let Ok(span) = self.expect(TokenKind::Symbol("-".to_string())) {
            let operand = self.parse_unary()?;
            return Ok(Expr::new(
                concat_span(&span, &operand.span),
                ExprKind::Unary {
                    operator: UnaryOperator::Minus,
                    operand: Box::new(operand),
                },
            ));
        } else {
            return self.parse_call();
        }
    }

    /// call ::= primary ("(" (expr ("," expr)*)? ")")*
    fn parse_call(&mut self) -> Result<Expr, String> {
        let mut operand = self.parse_primary()?;
        loop {
            if let Ok(_) = self.expect(TokenKind::Special("(".to_string())) {
                let mut arguments = Vec::new();
                let span_end = if let Ok(span) = self.expect(TokenKind::Special(")".to_string())) {
                    span
                } else {
                    loop {
                        let arg = self.parse_expr()?;
                        arguments.push(arg);
                        if let Ok(_) = self.expect(TokenKind::Special(",".to_string())) {
                            continue;
                        } else {
                            break self.expect(TokenKind::Special(")".to_string()))?;
                        }
                    }
                };
                operand = Expr::new(
                    concat_span(&operand.span, &span_end),
                    ExprKind::Call {
                        callee: Box::new(operand),
                        arguments,
                    },
                );
            } else {
                break;
            }
        }
        Ok(operand)
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

    fn parse_variable(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Some(Token {
                span,
                kind: TokenKind::Ident(s),
            }) => {
                let result = Ok(Expr::new(span.to_owned(), ExprKind::Variable(s.to_owned())));
                self.consume();
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
                self.consume();
                result
            }
            Some(token) => Err(format!("unexpected token: {}", token)),
            None => Err("unexpected EOF".to_owned()),
        }
    }

    fn parse_paren(&mut self) -> Result<Expr, String> {
        if let Ok(span_1) = self.expect(TokenKind::Special("(".to_string())) {
            let expr = self.parse_expr()?;
            if let Ok(span_2) = self.expect(TokenKind::Special(")".to_string())) {
                return Ok(Expr::new(concat_span(&span_1, &span_2), expr.kind));
            } else {
                return Err("expected ')'".to_string());
            }
        }
        if let Some(token) = self.peek() {
            return Err(format!("unexpected token: {}", token));
        }
        Err("unexpected EOF".to_string())
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
        callee: Box<Expr>,
        arguments: Vec<Expr>,
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
