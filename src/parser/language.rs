use std::{assert_matches::assert_matches, fmt};

use super::syntax_kind::SyntaxKind;

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
    pub parts: Vec<Part>,
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
    pub expression: ExprOperators,
}

#[derive(Debug, Clone, Default)]
pub struct ExprOperators {
    pub leading_operators: Vec<Operator<LeadingOperator>>,
    pub following_operators: Vec<Operator<FollowingOperator>>,
}

pub fn language() -> Language {
    use PlaceholderKind::*;
    Language {
        expression: ExprOperators {
            leading_operators: vec![
                Operator::prefix(60, vec![Part::tag("-")]),
                Operator::prefix(
                    40,
                    vec![
                        Part::tag("if"),
                        Part::placeholder(Expr, "cond"),
                        Part::tag("then"),
                        Part::placeholder(Expr, "then-body"),
                        Part::tag("else"),
                    ],
                ),
                Operator::prefix(
                    40,
                    vec![
                        Part::tag("if"),
                        Part::placeholder(Expr, "cond"),
                        Part::tag("then"),
                    ],
                ),
                Operator::closed(vec![
                    Part::tag("("),
                    Part::placeholder(Expr, "expr"),
                    Part::tag(")"),
                ]),
                Operator::closed(vec![
                    Part::tag("{"),
                    Part::placeholder(Ident, "param"),
                    Part::tag("->"),
                    Part::placeholder(Expr, "body"),
                    Part::tag("}"),
                ]),
                Operator::closed(vec![
                    Part::tag("{"),
                    Part::placeholder(Expr, "body"),
                    Part::tag("}"),
                ]),
            ],
            following_operators: vec![
                Operator::infixl(50, vec![Part::tag("+")]),
                Operator::infixl(50, vec![Part::tag("-")]),
                Operator::infixl(60, vec![Part::tag("*")]),
                Operator::infixl(60, vec![Part::tag("/")]),
                Operator::infixr(30, vec![Part::tag("=")]),
                Operator::postfix(
                    100,
                    vec![
                        Part::tag("("),
                        Part::placeholder(Expr, "arg"),
                        Part::tag(")"),
                    ],
                ),
            ],
        },
    }
}
