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

    Operator,
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
