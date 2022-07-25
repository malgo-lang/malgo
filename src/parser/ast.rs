use super::syntax_kind::SyntaxKind;
use super::SyntaxNode;

macro_rules! ast_node {
    ($ast:ident) => {
        #[derive(PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast(pub SyntaxNode);
        impl $ast {
            #[allow(unused)]
            pub fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == SyntaxKind::$ast {
                    Some(Self(node))
                } else {
                    None
                }
            }
            #[allow(unused)]
            pub fn nth_with_cast<F, T>(&self, n: usize, cast: F) -> Option<T>
            where
                F: Fn(SyntaxNode) -> Option<T>,
            {
                self.0.children().nth(n).map(cast)?
            }
        }
    };
}

ast_node!(Symbol);
ast_node!(Ident);
ast_node!(Int);
ast_node!(UnaryMinus);
ast_node!(IfThenElse);
ast_node!(IfThen);
ast_node!(Parens);
ast_node!(Let);
ast_node!(Fun);
ast_node!(Block);
ast_node!(Plus);
ast_node!(Minus);
ast_node!(Asterisk);
ast_node!(Slash);
ast_node!(Equal);
ast_node!(FunCall);
ast_node!(Primitive);
ast_node!(Root);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Identifier(SyntaxNode);

impl Identifier {
    #[allow(unused)]
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if Ident::cast(node.clone()).is_some() {
            Some(Identifier(node))
        } else {
            None
        }
    }
    #[allow(unused)]
    pub fn text(&self) -> String {
        format!("{}", self.0.first_token().unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Expr(SyntaxNode);

pub enum ExprKind {
    UnaryMinus(UnaryMinus),
    IfThenElse(IfThenElse),
    IfThen(IfThen),
    Parens(Parens),
    Let(Let),
    Fun(Fun),
    Block(Block),
    Plus(Plus),
    Minus(Minus),
    Asterisk(Asterisk),
    Slash(Slash),
    Equal(Equal),
    FunCall(FunCall),
    Primitive(Primitive),
}

impl Expr {
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if UnaryMinus::cast(node.clone()).is_some()
            || IfThenElse::cast(node.clone()).is_some()
            || IfThen::cast(node.clone()).is_some()
            || Parens::cast(node.clone()).is_some()
            || Let::cast(node.clone()).is_some()
            || Fun::cast(node.clone()).is_some()
            || Block::cast(node.clone()).is_some()
            || Plus::cast(node.clone()).is_some()
            || Minus::cast(node.clone()).is_some()
            || Asterisk::cast(node.clone()).is_some()
            || Slash::cast(node.clone()).is_some()
            || Equal::cast(node.clone()).is_some()
            || FunCall::cast(node.clone()).is_some()
            || Primitive::cast(node.clone()).is_some()
        {
            Some(Expr(node))
        } else {
            None
        }
    }

    pub fn kind(&self) -> ExprKind {
        UnaryMinus::cast(self.0.clone())
            .map(ExprKind::UnaryMinus)
            .or_else(|| IfThenElse::cast(self.0.clone()).map(ExprKind::IfThenElse))
            .or_else(|| IfThen::cast(self.0.clone()).map(ExprKind::IfThen))
            .or_else(|| Parens::cast(self.0.clone()).map(ExprKind::Parens))
            .or_else(|| Let::cast(self.0.clone()).map(ExprKind::Let))
            .or_else(|| Fun::cast(self.0.clone()).map(ExprKind::Fun))
            .or_else(|| Block::cast(self.0.clone()).map(ExprKind::Block))
            .or_else(|| Plus::cast(self.0.clone()).map(ExprKind::Plus))
            .or_else(|| Minus::cast(self.0.clone()).map(ExprKind::Minus))
            .or_else(|| Asterisk::cast(self.0.clone()).map(ExprKind::Asterisk))
            .or_else(|| Slash::cast(self.0.clone()).map(ExprKind::Slash))
            .or_else(|| Equal::cast(self.0.clone()).map(ExprKind::Equal))
            .or_else(|| FunCall::cast(self.0.clone()).map(ExprKind::FunCall))
            .or_else(|| Primitive::cast(self.0.clone()).map(ExprKind::Primitive))
            .unwrap()
    }
}

impl Root {
    #[allow(unused)]
    pub fn expr(&self) -> Option<Expr> {
        self.nth_with_cast(0, Expr::cast)
    }
}
