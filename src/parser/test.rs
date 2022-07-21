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
    assert!(parser.rest().is_empty());

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
