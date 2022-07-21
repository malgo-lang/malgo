#![feature(assert_matches, exclusive_range_pattern)]
use std::{assert_matches::assert_matches, fmt};

use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Whitespace,
    Eof,
    Error,

    Symbol,
    Ident,
    Int,

    Operator,
    Primitive,

    Root,
}

impl SyntaxKind {
    fn trivial(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, Whitespace | Eof)
    }

    fn non_trivial(&self) -> bool {
        !self.trivial()
    }
}
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SyntaxKind::*;

        match self {
            Whitespace => write!(f, "WS"),
            Eof => write!(f, "EOF"),
            Error => write!(f, "ERROR"),
            Symbol => write!(f, "SYM"),
            Ident => write!(f, "IDENT"),
            Int => write!(f, "INT"),
            Operator => write!(f, "OP"),
            Primitive => write!(f, "PRIM"),
            Root => write!(f, "ROOT"),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::Root as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    kind: SyntaxKind,
    value: &'a str,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            SyntaxKind::Eof => write!(f, "ERROR"),
            SyntaxKind::Error => write!(f, "EOF"),
            _ => write!(f, "{}", self.value),
        }
    }
}

impl<'a> Token<'a> {
    fn width(&self) -> usize {
        self.value.len()
    }
}

#[derive(Debug)]
pub struct PrintSyntaxNode<'a>(&'a rowan::SyntaxNode<Lang>);

impl fmt::Display for PrintSyntaxNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let node = self.0;
        write!(f, "({}", node.kind())?;
        for child in node.children_with_tokens() {
            match child {
                rowan::SyntaxElement::Node(node) => write!(f, " {}", PrintSyntaxNode(&node))?,
                rowan::SyntaxElement::Token(token) => {
                    if token.kind().non_trivial() {
                        write!(f, " {}", token)?
                    }
                }
            }
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LeadingOperator {
    Prefix { right_bp: u16 },
    Closed,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FollowingOperator {
    InfixL { bp: u16 },
    InfixR { bp: u16 },
    Postfix { left_bp: u16 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlaceholderKind {
    Ident,
    Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Part {
    Tag { name: String },
    Placeholder { kind: PlaceholderKind, name: String },
}

impl fmt::Display for Part {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Part::Tag { name } => write!(f, "{}", name),
            Part::Placeholder { kind, name } => write!(f, "{:?} {}", kind, name),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Operator<F> {
    fix: F,
    parts: Vec<Part>,
}

impl Operator<LeadingOperator> {
    pub fn prefix(right_bp: u16, parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: LeadingOperator::Prefix { right_bp },
            parts,
        }
    }

    pub fn closed(parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: LeadingOperator::Closed,
            parts,
        }
    }
}

impl Operator<FollowingOperator> {
    pub fn infixl(bp: u16, parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: FollowingOperator::InfixL { bp },
            parts,
        }
    }
    pub fn infixr(bp: u16, parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: FollowingOperator::InfixR { bp },
            parts,
        }
    }
    pub fn postfix(left_bp: u16, parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: FollowingOperator::Postfix { left_bp },
            parts,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Language {
    pub leading_operators: Vec<Operator<LeadingOperator>>,
    pub following_operators: Vec<Operator<FollowingOperator>>,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    position: usize,
    message: String,
}

#[derive(Debug)]
pub struct Parser<'a> {
    input: &'a str,
    language: Language,
    position: usize,
    errors: Vec<ParseError>,
    builder: rowan::GreenNodeBuilder<'static>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, language: Language) -> Self {
        Self {
            input,
            language,
            position: 0,
            errors: Vec::new(),
            builder: rowan::GreenNodeBuilder::new(),
        }
    }

    pub fn rest(&self) -> &'a str {
        &self.input[self.position..]
    }

    fn consume(&mut self, len: usize) {
        assert!(self.rest().len() >= len, "input is too short");
        tracing::debug!("consume: {}", &self.rest()[0..len]);
        self.position += len;
    }

    fn push_error(&mut self, message: String) {
        tracing::debug!("push error: {}", message);
        self.errors.push(ParseError {
            position: self.position,
            message,
        });
    }

    fn peek_token_at(&self, position: usize) -> Token<'a> {
        assert!(
            self.rest().is_char_boundary(position),
            "invalid position: {}, rest = {}",
            position,
            self.rest()
        );

        let input = &self.rest()[position..];
        tracing::trace!(position, input, "peek_token_at");

        if input.is_empty() {
            return Token {
                kind: SyntaxKind::Eof,
                value: "",
            };
        }

        fn is_ident_start(c: char) -> bool {
            let regex = Regex::new(r"\p{XID_Start}").unwrap();
            regex.is_match(&c.to_string())
        }
        fn is_ident_continue(c: char) -> bool {
            let regex = Regex::new(r"\p{XID_Continue}").unwrap();
            regex.is_match(&c.to_string())
        }
        fn is_special(c: char) -> bool {
            let special_chars = "(),;[]{}`";
            special_chars.chars().any(|s| s == c)
        }
        fn is_symbol(c: char) -> bool {
            let regex = Regex::new(r"\p{Punctuation}|\p{Symbol}").unwrap();
            regex.is_match(&c.to_string())
        }

        match input.chars().next().unwrap() {
            '0'..='9' => {
                let value = input.split(|c| c < '0' || c > '9').next().unwrap();

                Token {
                    kind: SyntaxKind::Int,
                    value,
                }
            }
            c if c.is_whitespace() => {
                let value = input.split(|c: char| !c.is_whitespace()).next().unwrap();
                Token {
                    kind: SyntaxKind::Whitespace,
                    value,
                }
            }
            c if is_ident_start(c) => {
                let value = input.split(|c: char| !is_ident_continue(c)).next().unwrap();
                Token {
                    kind: SyntaxKind::Ident,
                    value,
                }
            }
            c if is_special(c) => Token {
                kind: SyntaxKind::Symbol,
                value: &input[0..c.len_utf8()],
            },
            c if is_symbol(c) => {
                let value = input
                    .split(|c: char| is_special(c) || !is_symbol(c))
                    .next()
                    .unwrap();
                Token {
                    kind: SyntaxKind::Symbol,
                    value,
                }
            }
            c => Token {
                kind: SyntaxKind::Symbol,
                value: &input[0..c.len_utf8()],
            },
        }
    }

    fn peek_token(&self) -> Token<'a> {
        self.peek_token_at(0)
    }

    fn expect_token(&mut self, token: Token<'a>) {
        let lookahead = self.peek_token();
        if lookahead == token {
            self.consume_token();
        } else {
            self.push_error(format!("expected {:?}, got {:?}", token, lookahead));
        }
    }

    fn consume_token(&mut self) {
        let token = self.peek_token();
        self.consume(token.width());
        self.builder.token(token.kind.into(), token.value.into());
    }

    fn consume_ident(&mut self) {
        let token = self.peek_token();
        match token.kind {
            SyntaxKind::Ident => {
                self.consume(token.width());
                self.builder.token(token.kind.into(), token.value.into());
            }
            _ => {
                self.push_error(format!("expected identifier, got {:?}", token));
            }
        }
    }

    fn skip_ws(&mut self) {
        let token = self.peek_token();
        if token.kind == SyntaxKind::Whitespace {
            self.expect_token(token);
        }
    }

    fn expect_symbol(&mut self, tag: &str) {
        if self.rest().starts_with(tag) {
            self.builder
                .token(SyntaxKind::Symbol.into(), self.rest()[0..tag.len()].into());
            self.consume(tag.len());
        }
    }

    fn primary_expr(&mut self) {
        match self.peek_token() {
            token if token.kind == SyntaxKind::Int || token.kind == SyntaxKind::Ident => {
                self.expect_token(token);
            }
            token => {
                self.push_error(format!("expected int or ident, got {:?}", token));
            }
        }
    }

    fn parse_operator<F>(&mut self, op: &Operator<F>) {
        for i in 0..op.parts.len() {
            match &op.parts[i] {
                Part::Tag { name } => {
                    self.expect_symbol(&name);
                    self.skip_ws();
                }
                Part::Placeholder {
                    kind: PlaceholderKind::Expr,
                    name: _,
                } => {
                    let span = tracing::debug_span!("inside", part = %op.parts[i], i);
                    let _enter = span.enter();
                    self.expr_bp(0);
                    self.skip_ws();
                }
                Part::Placeholder {
                    kind: PlaceholderKind::Ident,
                    name: _,
                } => {
                    self.consume_ident();
                    self.skip_ws();
                }
            }
        }
    }

    fn expr_bp(&mut self, min_bp: u16) {
        let checkpoint = self.builder.checkpoint();
        if let Some(op) = self.peek_leading_operator() {
            self.builder.start_node(SyntaxKind::Operator.into());
            self.parse_operator(&op);

            match op.fix {
                LeadingOperator::Prefix { right_bp } => {
                    let span = tracing::debug_span!("prefix", part = %op.parts[0], i = 0);
                    let _enter = span.enter();
                    self.expr_bp(right_bp);
                }
                LeadingOperator::Closed => {}
            }

            self.builder.finish_node();
        } else {
            self.builder.start_node(SyntaxKind::Primitive.into());
            self.primary_expr();
            self.builder.finish_node();
        }

        loop {
            let token = match self.peek_token() {
                token if token.kind == SyntaxKind::Eof => break,
                token
                    if token.kind == SyntaxKind::Whitespace
                        && token.width() == self.rest().len() =>
                {
                    break
                }
                token => token,
            };
            let skip_width = if token.kind == SyntaxKind::Whitespace {
                token.width()
            } else {
                0
            };

            if let Some(op) = self.peek_following_operator(skip_width) {
                tracing::debug!(?op, "hit op");
                match op.fix {
                    FollowingOperator::InfixL { bp } => {
                        if bp <= min_bp {
                            break;
                        }
                    }
                    FollowingOperator::InfixR { bp } => {
                        if bp < min_bp {
                            break;
                        }
                    }
                    FollowingOperator::Postfix { left_bp } => {
                        if left_bp < min_bp {
                            break;
                        }
                    }
                }

                self.builder
                    .start_node_at(checkpoint, SyntaxKind::Operator.into());
                self.skip_ws();
                self.parse_operator(&op);

                match op.fix {
                    FollowingOperator::InfixL { bp } | FollowingOperator::InfixR { bp } => {
                        let span = tracing::debug_span!("infix", part = %op.parts[0], i = 0);
                        let _enter = span.enter();
                        self.skip_ws();
                        self.expr_bp(bp);
                    }
                    FollowingOperator::Postfix { .. } => {}
                }

                self.builder.finish_node();
                continue;
            }

            break;
        }
    }

    fn peek_leading_operator(&self) -> Option<Operator<LeadingOperator>> {
        self.peek_operator(&self.language.leading_operators, 0)
    }

    fn peek_following_operator(&self, skip_width: usize) -> Option<Operator<FollowingOperator>> {
        self.peek_operator(&self.language.following_operators, skip_width)
    }

    fn peek_operator<F>(&self, operators: &[Operator<F>], position: usize) -> Option<Operator<F>>
    where
        Operator<F>: Clone,
    {
        for op in operators.iter() {
            if self.peek(&op.parts[0], position) {
                return Some(op.clone());
            }
        }
        None
    }

    fn peek(&self, part: &Part, mut position: usize) -> bool {
        let mut symbols = match part {
            Part::Tag { name } => name.as_str(),
            Part::Placeholder { kind: _, name } => name.as_str(),
        };
        loop {
            if symbols.is_empty() {
                return true;
            }

            match self.peek_token_at(position) {
                token if token.kind == SyntaxKind::Eof || token.kind == SyntaxKind::Error => {
                    return false
                }
                token if symbols.starts_with(token.value) => {
                    symbols = &symbols[token.value.len()..];
                    position += token.value.len();
                }
                _ => return false,
            }
        }
    }

    pub fn expr(&mut self) {
        self.builder.start_node(SyntaxKind::Root.into());
        self.skip_ws();
        self.expr_bp(0);
        self.skip_ws();
        self.builder.finish_node();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn language() -> Language {
        Language {
            leading_operators: vec![
                Operator::prefix(60, vec![Part::Tag { name: "-".into() }]),
                Operator::closed(vec![
                    Part::Tag { name: "(".into() },
                    Part::Placeholder {
                        kind: PlaceholderKind::Expr,
                        name: "expr".into(),
                    },
                    Part::Tag { name: ")".into() },
                ]),
                Operator::closed(vec![
                    Part::Tag { name: "{".into() },
                    Part::Placeholder {
                        kind: PlaceholderKind::Ident,
                        name: "param".into(),
                    },
                    Part::Tag { name: "->".into() },
                    Part::Placeholder {
                        kind: PlaceholderKind::Expr,
                        name: "body".into(),
                    },
                    Part::Tag { name: "}".into() },
                ]),
            ],
            following_operators: vec![],
        }
    }

    fn success_complete(language: Language, input: &str, expected: &str) {
        let _ = tracing_subscriber::fmt::try_init();

        let mut parser = Parser::new(input, language);
        parser.expr();
        assert!(parser.errors.is_empty());
        assert!(
            parser.rest().is_empty(),
            "input must be consumed, remaing: {}",
            parser.rest()
        );

        let root: rowan::SyntaxNode<Lang> = rowan::SyntaxNode::new_root(parser.builder.finish());
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
}
fn main() {}
