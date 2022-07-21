pub mod language;
pub mod syntax_kind;
#[cfg(test)]
mod test;
mod tree_sink;

use std::fmt;

use regex::Regex;

use crate::parser::language::FollowingOperator;

use self::{
    language::{Language, LeadingOperator, MalgoLang, Operator, Part, PlaceholderKind},
    syntax_kind::SyntaxKind,
    tree_sink::TreeSink,
};

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
pub struct PrintSyntaxNode<'a>(pub &'a rowan::SyntaxNode<MalgoLang>);

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

#[derive(Debug, Clone)]
pub struct ParseError {
    position: usize,
    message: String,
}

pub struct ErrorReport {
    errors: Vec<ParseError>,
}

impl fmt::Display for ErrorReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for error in self.errors.iter() {
            write!(f, "error in {}\n\t{}", error.position, error.message)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    input: &'a str,
    language: Language,
    position: usize,
    errors: Vec<ParseError>,
    builder: TreeSink<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, language: Language) -> Self {
        Self {
            input,
            language,
            position: 0,
            errors: Vec::new(),
            builder: TreeSink::new(),
        }
    }

    pub fn parse<F>(mut self, root_parser: F) -> Result<rowan::SyntaxNode<MalgoLang>, ErrorReport>
    where
        F: FnOnce(&mut Parser<'a>),
    {
        root_parser(&mut self);
        if let Some(report) = self.error_report() {
            return Err(report);
        }
        let root = self.builder.finish();
        Ok(rowan::SyntaxNode::new_root(root))
    }

    pub fn expr(&mut self) {
        self.builder.start_node(SyntaxKind::Root);
        self.skip_ws();
        self.expr_bp(0);
        self.skip_ws();
        self.builder.finish_node();
    }

    fn error_report(&self) -> Option<ErrorReport> {
        if self.errors.is_empty() {
            return None;
        }
        Some(ErrorReport {
            errors: self.errors.clone(),
        })
    }

    fn rest(&self) -> &'a str {
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
                let value = input.split(|c| !('0'..='9').contains(&c)).next().unwrap();

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
        self.builder.token(token.kind, token.value);
    }

    fn consume_ident(&mut self) {
        let token = self.peek_token();
        match token.kind {
            SyntaxKind::Ident => {
                self.consume(token.width());
                self.builder.token(token.kind, token.value);
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
                .token(SyntaxKind::Symbol, self.rest()[0..tag.len()].into());
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
                    self.expect_symbol(name);
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
        let ops = self.peek_leading_operator();

        // save states for backtracking
        let events = self.builder.save();
        let position = self.position;
        let errors = self.errors.clone();
        for op in ops.iter() {
            // load states
            self.builder.load(events.clone());
            self.position = position;
            self.errors = errors.clone();

            self.builder.start_node(SyntaxKind::Operator);
            self.parse_operator(op);

            if let LeadingOperator::Prefix { right_bp } = op.fix {
                let span = tracing::debug_span!("prefix", part = %op.parts[0], i = 0);
                let _enter = span.enter();
                self.expr_bp(right_bp);
            }

            self.builder.finish_node();
            if op == ops.last().unwrap() {
                break;
            }
            if self.errors.len() > errors.len() {
                // backtrack
                continue;
            } else {
                break;
            }
        }
        if ops.is_empty() {
            self.builder.start_node(SyntaxKind::Primitive);
            self.primary_expr();
            self.builder.finish_node();
        }

        'following: loop {
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

            let ops = self.peek_following_operator(skip_width);

            // save states for backtracking
            let events = self.builder.save();
            let position = self.position;
            let errors = self.errors.clone();
            for op in ops.iter() {
                // load states
                self.builder.load(events.clone());
                self.position = position;
                self.errors = errors.clone();

                tracing::debug!(?op, "hit op");
                match op.fix {
                    FollowingOperator::InfixL { bp } => {
                        if bp <= min_bp {
                            break 'following;
                        }
                    }
                    FollowingOperator::InfixR { bp } => {
                        if bp < min_bp {
                            break 'following;
                        }
                    }
                    FollowingOperator::Postfix { left_bp } => {
                        if left_bp < min_bp {
                            break 'following;
                        }
                    }
                }

                self.builder.start_node_at(checkpoint, SyntaxKind::Operator);
                self.skip_ws();
                self.parse_operator(op);

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

                if op == ops.last().unwrap() {
                    continue 'following;
                }
                if self.errors.len() > errors.len() {
                    // backtrack
                    continue;
                }
                continue 'following;
            }

            break;
        }
    }

    fn peek_leading_operator(&self) -> Vec<Operator<LeadingOperator>> {
        self.peek_operator(&self.language.leading_operators, 0)
    }

    fn peek_following_operator(&self, skip_width: usize) -> Vec<Operator<FollowingOperator>> {
        self.peek_operator(&self.language.following_operators, skip_width)
    }

    fn peek_operator<F>(&self, operators: &[Operator<F>], position: usize) -> Vec<Operator<F>>
    where
        Operator<F>: Clone,
    {
        let mut ops = vec![];
        for op in operators.iter() {
            if self.peek(&op.parts[0], position) {
                ops.push(op.clone());
            }
        }
        ops
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
}
