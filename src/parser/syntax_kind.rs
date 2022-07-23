use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    Whitespace,
    Eof,
    Error,

    Symbol,
    Ident,
    Int,

    UnaryMinus,
    IfThenElse,
    IfThen,
    Parens,
    Let,
    Fun,
    Block,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
    FunCall,

    Primitive,

    Root,
}

impl SyntaxKind {
    pub fn trivial(&self) -> bool {
        use SyntaxKind::*;
        matches!(self, Whitespace | Eof)
    }

    pub fn non_trivial(&self) -> bool {
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
        write!(f, "{:?}", self)
    }
}
