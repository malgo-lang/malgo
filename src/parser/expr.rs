use super::{
    ast::{BinaryOperator, Expr, ExprKind, UnaryOperator},
    concat_span,
    lexer::{Token, TokenKind},
    Parser,
};

impl Parser {
    // expr ::= let | fun | if | equality
    pub fn parse_expr(&mut self) -> Result<Expr, String> {
        // let ::= "let" ident "=" expr "in" expr
        if let Ok(span1) = self.expect(TokenKind::ident("let")) {
            let ident = self.expect_ident()?;
            let _ = self.expect(TokenKind::symbol("="))?;
            let value = self.parse_expr()?;
            let _ = self.expect(TokenKind::ident("in"))?;
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
        if let Ok(span1) = self.expect(TokenKind::special("{")) {
            let mut parameters = Vec::new();
            // params ::= (ident ("," ident)*)?
            if let Err(_) = self.expect(TokenKind::symbol("->")) {
                loop {
                    let ident = self.expect_ident()?;
                    parameters.push(ident);
                    if let Ok(_) = self.expect(TokenKind::special(",")) {
                        continue;
                    } else {
                        let _ = self.expect(TokenKind::symbol("->"))?;
                        break;
                    }
                }
            }
            let body = self.parse_expr()?;
            let span2 = self.expect(TokenKind::special("}"))?;
            return Ok(Expr::new(
                concat_span(&span1, &span2),
                ExprKind::Fun {
                    parameters,
                    body: Box::new(body),
                },
            ));
        }

        // if ::= "if" expr "then" expr "else" expr
        if let Ok(span1) = self.expect(TokenKind::ident("if")) {
            let cond = self.parse_expr()?;
            let _ = self.expect(TokenKind::ident("then"))?;
            let then_branch = self.parse_expr()?;
            let _ = self.expect(TokenKind::ident("else"))?;
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
            if let Ok(_) = self.expect(TokenKind::symbol("==")) {
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
                return Ok(lhs);
            }
        }
    }

    /// add ::= mul (("+" | "-") mul)*
    fn parse_add(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_mul()?;
        loop {
            if let Ok(_) = self.expect(TokenKind::symbol("+")) {
                let rhs = self.parse_mul()?;
                lhs = Expr::new(
                    concat_span(&lhs.span, &rhs.span),
                    ExprKind::Binary {
                        left: Box::new(lhs),
                        operator: BinaryOperator::Plus,
                        right: Box::new(rhs),
                    },
                );
            } else if let Ok(_) = self.expect(TokenKind::symbol("-")) {
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
                return Ok(lhs);
            }
        }
    }

    /// mul ::= unary (("*" | "/") unary)*
    fn parse_mul(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_unary()?;
        loop {
            if let Ok(_) = self.expect(TokenKind::symbol("*")) {
                let rhs = self.parse_unary()?;
                lhs = Expr::new(
                    concat_span(&lhs.span, &rhs.span),
                    ExprKind::Binary {
                        left: Box::new(lhs),
                        operator: BinaryOperator::Asterisk,
                        right: Box::new(rhs),
                    },
                );
            } else if let Ok(_) = self.expect(TokenKind::symbol("/")) {
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
                return Ok(lhs);
            }
        }
    }

    /// unary ::= "-" unary | call
    fn parse_unary(&mut self) -> Result<Expr, String> {
        if let Ok(span) = self.expect(TokenKind::symbol("-")) {
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
            if let Ok(_) = self.expect(TokenKind::special("(")) {
                let mut arguments = Vec::new();
                let span_end = if let Ok(span) = self.expect(TokenKind::special(")")) {
                    span
                } else {
                    loop {
                        let arg = self.parse_expr()?;
                        arguments.push(arg);
                        if let Ok(_) = self.expect(TokenKind::special(",")) {
                            continue;
                        } else {
                            break self.expect(TokenKind::special(")"))?;
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
                return Ok(operand);
            }
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
        if let Ok(span_1) = self.expect(TokenKind::special("(")) {
            let expr = self.parse_expr()?;
            if let Ok(span_2) = self.expect(TokenKind::special(")")) {
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
}
