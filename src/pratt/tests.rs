use super::eval;
use super::eval::Eval;
use super::parser::ast::*;
use super::parser::language::*;
use super::parser::Parser;
use super::parser::PrintSyntaxNode;
use std::collections::HashMap;

fn success_complete(
    language: Language,
    input: &str,
    expected: &str,
    evaluated: eval::Result<eval::Value>,
) {
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
    assert_eq!(res, evaluated);
}

#[test]
fn test_int() {
    success_complete(
        language(),
        "42",
        "(Root (Primitive 42))",
        Ok(eval::Value::Int(42)),
    );
}

#[test]
fn test_pre_minus() {
    success_complete(
        language(),
        "-42",
        "(Root (UnaryMinus - (Primitive 42)))",
        Ok(eval::Value::Int(-42)),
    );
}

#[test]
fn test_parens() {
    success_complete(
        language(),
        "-(-42)",
        "(Root (UnaryMinus - (Parens ( (UnaryMinus - (Primitive 42)) ))))",
        Ok(eval::Value::Int(42)),
    );
}

#[test]
fn test_function() {
    success_complete(
        language(),
        "{ x -> x }(1)",
        "(Root (FunCall (Fun { (Ident x) -> (Primitive x) }) ( (Primitive 1) )))",
        Ok(eval::Value::Int(1)),
    );
}

#[test]
fn test_function_noparam() {
    success_complete(
        language(),
        "{ x }",
        "(Root (Block { (Primitive x) }))",
        Err(eval::Error::Undefined),
    );
}

#[test]
fn test_if_then_else() {
    success_complete(
        language(),
        "if 1 = 1 then 42 else 54",
        "(Root (IfThenElse if (Equal (Primitive 1) = (Primitive 1)) then (Primitive 42) else (Primitive 54)))",
        Ok(eval::Value::Int(42)),
    );
}

#[test]
fn test_if_then() {
    success_complete(
        language(),
        "if 1 = 1 then 42",
        "(Root (IfThen if (Equal (Primitive 1) = (Primitive 1)) then (Primitive 42)))",
        Ok(eval::Value::Int(42)),
    );
}

#[test]
fn test_if_then2() {
    success_complete(
        language(),
        "if 0 = 1 then 42",
        "(Root (IfThen if (Equal (Primitive 0) = (Primitive 1)) then (Primitive 42)))",
        Err(eval::Error::Undefined),
    );
}

#[test]
fn test_plus() {
    success_complete(
        language(),
        "1 + 2",
        "(Root (Plus (Primitive 1) + (Primitive 2)))",
        Ok(eval::Value::Int(3)),
    );
}

#[test]
fn test_plus_mul() {
    success_complete(
        language(),
        "1 + 2 * 3",
        "(Root (Plus (Primitive 1) + (Asterisk (Primitive 2) * (Primitive 3))))",
        Ok(eval::Value::Int(7)),
    );
}

#[test]
fn test_mul_plus() {
    success_complete(
        language(),
        "1 * 2 + 3",
        "(Root (Plus (Asterisk (Primitive 1) * (Primitive 2)) + (Primitive 3)))",
        Ok(eval::Value::Int(5)),
    );
}

#[test]
fn test_equal() {
    success_complete(
        language(),
        "1 = 1",
        "(Root (Equal (Primitive 1) = (Primitive 1)))",
        Ok(eval::Value::Bool(true)),
    );
}

#[test]
fn test_fn_call() {
    success_complete(
        language(),
        "f(x)",
        "(Root (FunCall (Primitive f) ( (Primitive x) )))",
        Err(eval::Error::VariableNotDefined("f".to_string())),
    );
}

#[test]
fn test_fn_call_plus() {
    success_complete(
        language(),
        "f(x) + 1",
        "(Root (Plus (FunCall (Primitive f) ( (Primitive x) )) + (Primitive 1)))",
        Err(eval::Error::VariableNotDefined("f".to_string())),
    );
}

#[test]
fn test_let() {
    success_complete(
        language(),
        "let x = 1 in x",
        "(Root (Let let (Ident x) = (Primitive 1) in (Primitive x)))",
        Ok(eval::Value::Int(1)),
    );
}

#[test]
fn test_let2() {
    success_complete(
        language(),
        "(let x = 1 in x) + (let x = 2 in x)",
       "(Root (Plus (Parens ( (Let let (Ident x) = (Primitive 1) in (Primitive x)) )) + (Parens ( (Let let (Ident x) = (Primitive 2) in (Primitive x)) ))))",
        Ok(eval::Value::Int(3)),
    );
}
