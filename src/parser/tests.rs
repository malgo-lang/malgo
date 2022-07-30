use std::{path::PathBuf, rc::Rc};

use crate::parser::lexer::{Lexer, Span, TokenKind};

use super::*;

#[test]
fn test_lexer_tokenize() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "1 + 2 * 3";
    let mut lexer = Lexer::new(source.clone(), input.to_string());
    let tokens = lexer.tokenize();
    assert_eq!(
        tokens,
        vec![
            Token::new(
                TokenKind::Number("1".to_string()),
                Span::new(source.clone(), 0, 1),
            ),
            Token::new(
                TokenKind::Symbol("+".to_string()),
                Span::new(source.clone(), 2, 3),
            ),
            Token::new(
                TokenKind::Number("2".to_string()),
                Span::new(source.clone(), 4, 5),
            ),
            Token::new(
                TokenKind::Symbol("*".to_string()),
                Span::new(source.clone(), 6, 7),
            ),
            Token::new(
                TokenKind::Number("3".to_string()),
                Span::new(source.clone(), 8, 9),
            )
        ]
    );
}

#[test]
fn test_lexer_tokenize_complex_ascii() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "foo + bar()";
    let mut lexer = Lexer::new(source.clone(), input.to_string());
    let tokens = lexer
        .tokenize()
        .into_iter()
        .map(|t| t.kind)
        .collect::<Vec<_>>();
    assert_eq!(
        tokens,
        vec![
            TokenKind::Ident("foo".to_string()),
            TokenKind::Symbol("+".to_string()),
            TokenKind::Ident("bar".to_string()),
            TokenKind::Special("(".to_string()),
            TokenKind::Special(")".to_string()),
        ]
    );
}

#[test]
fn test_lexer_tokenize_complex_unicode() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "λ + 変数（）";
    let tokens = Lexer::new(source.clone(), input.to_string()).tokenize();
    assert_eq!(
        tokens.into_iter().map(|t| t.kind).collect::<Vec<_>>(),
        vec![
            TokenKind::Ident("λ".to_string()),
            TokenKind::Symbol("+".to_string()),
            TokenKind::Ident("変数".to_string()),
            TokenKind::Symbol("（）".to_string()),
        ]
    );
}

#[test]
fn test_parser_primary() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "1";
    let tokens = Lexer::new(source.clone(), input.to_string()).tokenize();
    let mut parser = Parser::new(tokens);
    assert_eq!(
        parser.parse(),
        Ok(Expr::new(
            Span::new(source.clone(), 0, 1),
            ExprKind::Number(1),
        ))
    );
}

#[test]
fn test_parser_unary() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "-1";
    let tokens = Lexer::new(source.clone(), input.to_string()).tokenize();
    let mut parser = Parser::new(tokens);
    assert_eq!(
        parser.parse(),
        Ok(Expr::new(
            Span::new(source.clone(), 0, 2),
            ExprKind::Unary {
                operator: UnaryOperator::Minus,
                operand: Box::new(Expr::new(
                    Span::new(source.clone(), 1, 2),
                    ExprKind::Number(1),
                ))
            },
        ))
    );
}

#[test]
fn test_parser_mul() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "1 * 2";
    let tokens = Lexer::new(source.clone(), input.to_string()).tokenize();
    let mut parser = Parser::new(tokens);
    assert_eq!(
        parser.parse(),
        Ok(Expr::new(
            Span::new(source.clone(), 0, 5),
            ExprKind::Binary {
                operator: BinaryOperator::Asterisk,
                left: Box::new(Expr::new(
                    Span::new(source.clone(), 0, 1),
                    ExprKind::Number(1),
                )),
                right: Box::new(Expr::new(
                    Span::new(source.clone(), 4, 5),
                    ExprKind::Number(2),
                )),
            },
        ))
    );
}

#[test]
fn test_parser_mul_assoc() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "1 * 2 * 3";
    let tokens = Lexer::new(source.clone(), input.to_string()).tokenize();
    let mut parser = Parser::new(tokens);
    assert_eq!(
        parser.parse(),
        Ok(Expr::new(
            Span::new(source.clone(), 0, 9),
            ExprKind::Binary {
                operator: BinaryOperator::Asterisk,
                left: Box::new(Expr::new(
                    Span::new(source.clone(), 0, 5),
                    ExprKind::Binary {
                        operator: BinaryOperator::Asterisk,
                        left: Box::new(Expr::new(
                            Span::new(source.clone(), 0, 1),
                            ExprKind::Number(1),
                        )),
                        right: Box::new(Expr::new(
                            Span::new(source.clone(), 4, 5),
                            ExprKind::Number(2),
                        )),
                    },
                )),
                right: Box::new(Expr::new(
                    Span::new(source.clone(), 8, 9),
                    ExprKind::Number(3),
                )),
            },
        ))
    );
}

#[test]
fn test_parser_add() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "1 + 2";
    let tokens = Lexer::new(source.clone(), input.to_string()).tokenize();
    let mut parser = Parser::new(tokens);
    assert_eq!(
        parser.parse(),
        Ok(Expr::new(
            Span::new(source.clone(), 0, 5),
            ExprKind::Binary {
                operator: BinaryOperator::Plus,
                left: Box::new(Expr::new(
                    Span::new(source.clone(), 0, 1),
                    ExprKind::Number(1),
                )),
                right: Box::new(Expr::new(
                    Span::new(source.clone(), 4, 5),
                    ExprKind::Number(2),
                )),
            },
        ))
    );
}

#[test]
fn test_parser_add_mul() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "1 + 2 * 3";
    let tokens = Lexer::new(source.clone(), input.to_string()).tokenize();
    let mut parser = Parser::new(tokens);
    assert_eq!(
        parser.parse(),
        Ok(Expr::new(
            Span::new(source.clone(), 0, 9),
            ExprKind::Binary {
                operator: BinaryOperator::Plus,
                left: Box::new(Expr::new(
                    Span::new(source.clone(), 0, 1),
                    ExprKind::Number(1),
                )),
                right: Box::new(Expr::new(
                    Span::new(source.clone(), 4, 9),
                    ExprKind::Binary {
                        operator: BinaryOperator::Asterisk,
                        left: Box::new(Expr::new(
                            Span::new(source.clone(), 4, 5),
                            ExprKind::Number(2),
                        )),
                        right: Box::new(Expr::new(
                            Span::new(source.clone(), 8, 9),
                            ExprKind::Number(3),
                        )),
                    },
                )),
            },
        ))
    );
}
