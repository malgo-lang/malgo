use super::lexer::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Variable(String),
    Number(u64),
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Let {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    Fun {
        parameters: Vec<String>,
        body: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: BinaryOperator,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Minus,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
}
