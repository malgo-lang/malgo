use std::collections::HashMap;

use super::ast::*;
use super::language::*;
use super::*;

fn success_complete(language: Language, input: &str, expected: &str, evaluated: i64) {
    let _ = tracing_subscriber::fmt::try_init();

    let mut parser = Parser::new(input, language);
    parser.expr();
    assert!(
        parser.errors.is_empty(),
        "{}",
        parser.error_report().unwrap()
    );
    assert!(parser.rest().is_empty(), "{}", parser.rest());

    let root: rowan::SyntaxNode<MalgoLang> = rowan::SyntaxNode::new_root(parser.builder.finish());
    assert_eq!(root.text_range().len(), input.len().try_into().unwrap());
    let printer = PrintSyntaxNode(&root);
    assert_eq!(
        printer.to_string(),
        expected,
        "parse result does'nt match: \ninput = {}\n{:#?}",
        input,
        root,
    );

    let state = HashMap::new();
    let res = Root::cast(root).unwrap().expr().unwrap().eval(&state);
    if evaluated != 0xdeadbeaf {
        assert_eq!(res, Some(evaluated));
    }
}

#[test]
fn test_int() {
    success_complete(language(), "42", "(Root (Primitive 42))", 42);
}

#[test]
fn test_pre_minus() {
    success_complete(
        language(),
        "-42",
        "(Root (UnaryMinus - (Primitive 42)))",
        -42,
    );
}

#[test]
fn test_parens() {
    success_complete(
        language(),
        "-(-42)",
        "(Root (UnaryMinus - (Parens ( (UnaryMinus - (Primitive 42)) ))))",
        42,
    );
}

#[test]
fn test_function() {
    success_complete(
        language(),
        "{ x -> x }",
        "(Root (Fun { x -> (Primitive x) }))",
        0xdeadbeaf,
    );
}

#[test]
fn test_function_noparam() {
    success_complete(
        language(),
        "{ x }",
        "(Root (Block { (Primitive x) }))",
        0xdeadbeaf,
    );
}

#[test]
fn test_if_then_else() {
    success_complete(
        language(),
        "if x then y else z",
        "(Root (IfThenElse if (Primitive x) then (Primitive y) else (Primitive z)))",
        0xdeadbeaf,
    );
}

#[test]
fn test_if_then() {
    success_complete(
        language(),
        "if x then y",
        "(Root (IfThen if (Primitive x) then (Primitive y)))",
        0xdeadbeaf,
    );
}

#[test]
fn test_plus() {
    success_complete(
        language(),
        "1 + 2",
        "(Root (Plus (Primitive 1) + (Primitive 2)))",
        3,
    );
}

#[test]
fn test_plus_mul() {
    success_complete(
        language(),
        "1 + 2 * 3",
        "(Root (Plus (Primitive 1) + (Asterisk (Primitive 2) * (Primitive 3))))",
        7,
    );
}

#[test]
fn test_mul_plus() {
    success_complete(
        language(),
        "1 * 2 + 3",
        "(Root (Plus (Asterisk (Primitive 1) * (Primitive 2)) + (Primitive 3)))",
        5,
    );
}

#[test]
fn test_equal() {
    success_complete(
        language(),
        "x = y = z",
        "(Root (Equal (Primitive x) = (Equal (Primitive y) = (Primitive z))))",
        0xdeadbeaf,
    );
}

#[test]
fn test_fn_call() {
    success_complete(
        language(),
        "f(x)",
        "(Root (FunCall (Primitive f) ( (Primitive x) )))",
        0xdeadbeaf,
    );
}

#[test]
fn test_fn_call_plus() {
    success_complete(
        language(),
        "f(x) + 1",
        "(Root (Plus (FunCall (Primitive f) ( (Primitive x) )) + (Primitive 1)))",
        0xdeadbeaf,
    );
}

#[test]
fn test_let() {
    success_complete(
        language(),
        "let x = 1 in x",
        "(Root (Let let x = (Primitive 1) in (Primitive x)))",
        0xdeadbeaf,
    );
}
