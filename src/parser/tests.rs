use super::language::*;
use super::*;

fn success_complete(language: Language, input: &str, expected: &str) {
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
}

#[test]
fn test_int() {
    success_complete(language(), "42", "(ROOT (PRIM 42))");
}

#[test]
fn test_pre_minus() {
    success_complete(language(), "-42", "(ROOT (OP - (PRIM 42)))");
}

#[test]
fn test_parens() {
    success_complete(
        language(),
        "-(-42)",
        "(ROOT (OP - (OP ( (OP - (PRIM 42)) ))))",
    );
}

#[test]
fn test_function() {
    success_complete(language(), "{ x -> x }", "(ROOT (OP { x -> (PRIM x) }))");
}

#[test]
fn test_function_noparam() {
    success_complete(language(), "{ x }", "(ROOT (OP { (PRIM x) }))");
}

#[test]
fn test_if_then_else() {
    success_complete(
        language(),
        "if x then y else z",
        "(ROOT (OP if (PRIM x) then (PRIM y) else (PRIM z)))",
    );
}

#[test]
fn test_if_then() {
    success_complete(
        language(),
        "if x then y",
        "(ROOT (OP if (PRIM x) then (PRIM y)))",
    );
}

#[test]
fn test_plus() {
    success_complete(language(), "1 + 2", "(ROOT (OP (PRIM 1) + (PRIM 2)))");
}

#[test]
fn test_plus_mul() {
    success_complete(
        language(),
        "1 + 2 * 3",
        "(ROOT (OP (PRIM 1) + (OP (PRIM 2) * (PRIM 3))))",
    );
}

#[test]
fn test_mul_plus() {
    success_complete(
        language(),
        "1 * 2 + 3",
        "(ROOT (OP (OP (PRIM 1) * (PRIM 2)) + (PRIM 3)))",
    );
}

#[test]
fn test_equal() {
    success_complete(
        language(),
        "x = y = z",
        "(ROOT (OP (PRIM x) = (OP (PRIM y) = (PRIM z))))",
    );
}

#[test]
fn test_fn_call() {
    success_complete(language(), "f(x)", "(ROOT (OP (PRIM f) ( (PRIM x) )))");
}

#[test]
fn test_fn_call_plus() {
    success_complete(
        language(),
        "f(x) + 1",
        "(ROOT (OP (OP (PRIM f) ( (PRIM x) )) + (PRIM 1)))",
    );
}

#[test]
fn test_let() {
    success_complete(
        language(),
        "let x = 1; x",
        "(ROOT (OP (OP let x = (PRIM 1)) ; (PRIM x)))",
    );
}

#[test]
fn test_let2() {
    success_complete(
        language(),
        "let x = 1; x;",
        "(ROOT (OP (OP (OP let x = (PRIM 1)) ; (PRIM x)) ;))",
    );
}
