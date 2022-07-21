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
    pub leading_operators: Vec<Operator<LeadingOperator>>,
    pub following_operators: Vec<Operator<FollowingOperator>>,
}

pub fn language() -> Language {
    Language {
        leading_operators: vec![
            Operator::prefix(60, vec![Part::Tag { name: "-".into() }]),
            Operator::prefix(
                40,
                vec![
                    Part::Tag { name: "if".into() },
                    Part::Placeholder {
                        kind: PlaceholderKind::Expr,
                        name: "cond".into(),
                    },
                    Part::Tag {
                        name: "then".into(),
                    },
                    Part::Placeholder {
                        kind: PlaceholderKind::Expr,
                        name: "then-body".into(),
                    },
                    Part::Tag {
                        name: "else".into(),
                    },
                ],
            ),
            Operator::prefix(
                40,
                vec![
                    Part::Tag { name: "if".into() },
                    Part::Placeholder {
                        kind: PlaceholderKind::Expr,
                        name: "cond".into(),
                    },
                    Part::Tag {
                        name: "then".into(),
                    },
                ],
            ),
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
            Operator::closed(vec![
                Part::Tag { name: "{".into() },
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
