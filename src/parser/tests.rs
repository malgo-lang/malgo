use super::*;

#[test]
fn test_lexer_peek_token() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "1 + 2 * 3";
    let lexer = Lexer::new(source.clone(), input.to_string());
    let token = lexer.peek_token();
    assert_eq!(token.kind, TokenKind::Number("1".to_string()));
    assert_eq!(token.span, Span::new(source.clone(), 0, 1));
}

#[test]
fn test_lexer_peek_token_at() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "1 + 2 * 3";
    let lexer = Lexer::new(source.clone(), input.to_string());
    let token = lexer.peek_token_at(1);
    assert_eq!(token.kind, TokenKind::Whitespace);
    assert_eq!(token.span, Span::new(source.clone(), 1, 2));
}

#[test]
fn test_lexer_expect_token() {
    let source = Rc::new(PathBuf::from("test.mlg"));
    let input = "1 + 2 * 3";
    let mut lexer = Lexer::new(source.clone(), input.to_string());
    lexer.expect_token(&Token::new(
        TokenKind::Number("1".to_string()),
        Span::new(source.clone(), 0, 1),
    ));
    lexer.expect_token(&Token::new(
        TokenKind::Whitespace,
        Span::new(source.clone(), 1, 2),
    ));
}

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
            TokenKind::Symbol("（".to_string()),
            TokenKind::Symbol("）".to_string()),
        ]
    );
}
