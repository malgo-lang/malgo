use std::{assert_matches::assert_matches, fmt};

use super::syntax_kind::SyntaxKind;

// TODO:
// 1. Add Part::Sequence(Part, token Vec of a delimiter]).
// 1. Add Language.statement and Part::Stmt.

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MalgoLang {}
impl rowan::Language for MalgoLang {
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

impl Part {
    fn tag(name: &str) -> Part {
        Part::Tag { name: name.into() }
    }
    fn placeholder(kind: PlaceholderKind, name: &str) -> Part {
        Part::Placeholder {
            kind,
            name: name.into(),
        }
    }
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
    pub fix: F,
    pub kind: SyntaxKind,
    pub parts: Vec<Part>,
}

impl Operator<LeadingOperator> {
    pub fn prefix(right_bp: u16, kind: SyntaxKind, parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: LeadingOperator::Prefix { right_bp },
            kind,
            parts,
        }
    }

    pub fn closed(kind: SyntaxKind, parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: LeadingOperator::Closed,
            kind,
            parts,
        }
    }
}

impl Operator<FollowingOperator> {
    pub fn infixl(bp: u16, kind: SyntaxKind, parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: FollowingOperator::InfixL { bp },
            kind,
            parts,
        }
    }
    pub fn infixr(bp: u16, kind: SyntaxKind, parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: FollowingOperator::InfixR { bp },
            kind,
            parts,
        }
    }
    pub fn postfix(left_bp: u16, kind: SyntaxKind, parts: Vec<Part>) -> Self {
        assert_matches!(parts.get(0), Some(Part::Tag { name: _ }));
        assert_matches!(parts.last(), Some(Part::Tag { name: _ }));
        Self {
            fix: FollowingOperator::Postfix { left_bp },
            kind,
            parts,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Language {
    pub expression: ExprOperators,
}

#[derive(Debug, Clone, Default)]
pub struct ExprOperators {
    pub leading_operators: Vec<Operator<LeadingOperator>>,
    pub following_operators: Vec<Operator<FollowingOperator>>,
}

pub fn language() -> Language {
    use PlaceholderKind as P;
    use SyntaxKind::*;
    Language {
        expression: ExprOperators {
            leading_operators: vec![
                Operator::prefix(60, UnaryMinus, vec![Part::tag("-")]),
                Operator::prefix(
                    40,
                    IfThenElse,
                    vec![
                        Part::tag("if"),
                        Part::placeholder(P::Expr, "cond"),
                        Part::tag("then"),
                        Part::placeholder(P::Expr, "then-body"),
                        Part::tag("else"),
                    ],
                ),
                Operator::prefix(
                    40,
                    IfThen,
                    vec![
                        Part::tag("if"),
                        Part::placeholder(P::Expr, "cond"),
                        Part::tag("then"),
                    ],
                ),
                Operator::closed(
                    Parens,
                    vec![
                        Part::tag("("),
                        Part::placeholder(P::Expr, "expr"),
                        Part::tag(")"),
                    ],
                ),
                Operator::prefix(
                    20,
                    Let,
                    vec![
                        Part::tag("let"),
                        Part::placeholder(P::Ident, "var"),
                        Part::tag("="),
                        Part::placeholder(P::Expr, "body"),
                        Part::tag("in"),
                    ],
                ),
                Operator::closed(
                    Fun,
                    vec![
                        Part::tag("{"),
                        Part::placeholder(P::Ident, "param"),
                        Part::tag("->"),
                        Part::placeholder(P::Expr, "body"),
                        Part::tag("}"),
                    ],
                ),
                Operator::closed(
                    Block,
                    vec![
                        Part::tag("{"),
                        Part::placeholder(P::Expr, "body"),
                        Part::tag("}"),
                    ],
                ),
            ],
            following_operators: vec![
                Operator::infixl(50, Plus, vec![Part::tag("+")]),
                Operator::infixl(50, Minus, vec![Part::tag("-")]),
                Operator::infixl(60, Asterisk, vec![Part::tag("*")]),
                Operator::infixl(60, Slash, vec![Part::tag("/")]),
                Operator::infixr(30, Equal, vec![Part::tag("=")]),
                Operator::postfix(
                    100,
                    FunCall,
                    vec![
                        Part::tag("("),
                        Part::placeholder(P::Expr, "arg"),
                        Part::tag(")"),
                    ],
                ),
            ],
        },
    }
}
