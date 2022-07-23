use std::collections::HashMap;

use super::syntax_kind::SyntaxKind;
use super::SyntaxNode;

macro_rules! ast_node {
    ($ast:ident) => {
        #[derive(PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast(SyntaxNode);
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

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Identifier(SyntaxNode);

impl Identifier {
    #[allow(unused)]
    fn cast(node: SyntaxNode) -> Option<Self> {
        if Ident::cast(node.clone()).is_some() {
            Some(Identifier(node))
        } else {
            None
        }
    }
    #[allow(unused)]
    fn text(&self) -> String {
        format!("{}", self.0.first_token().unwrap())
    }
}

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Expr(SyntaxNode);

enum ExprKind {
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

    fn kind(&self) -> ExprKind {
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

pub trait Eval<T> {
    type State;
    fn eval(&self, state: &Self::State) -> Option<T>;
}

impl Eval<i64> for Expr {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        use ExprKind::*;
        match self.kind() {
            UnaryMinus(x) => x.eval(state),
            IfThenElse(x) => x.eval(state),
            IfThen(x) => x.eval(state),
            Parens(x) => x.eval(state),
            Let(x) => x.eval(state),
            Fun(x) => x.eval(state),
            Block(x) => x.eval(state),
            Plus(x) => x.eval(state),
            Minus(x) => x.eval(state),
            Asterisk(x) => x.eval(state),
            Slash(x) => x.eval(state),
            Equal(x) => x.eval(state),
            FunCall(x) => x.eval(state),
            Primitive(x) => x.eval(state),
        }
    }
}

impl Eval<i64> for UnaryMinus {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        let arg = self.nth_with_cast(0, Expr::cast)?;
        let val = arg.eval(state)?;
        Some(-val)
    }
}

impl Eval<i64> for IfThenElse {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        let cond = self.nth_with_cast(0, Expr::cast)?;
        let val = cond.eval(state)?;
        if val != 0 {
            let e1 = self.nth_with_cast(1, Expr::cast)?;
            let val = e1.eval(state)?;
            Some(val)
        } else {
            let e2 = self.nth_with_cast(2, Expr::cast)?;
            let val = e2.eval(state)?;
            Some(val)
        }
    }
}

impl Eval<i64> for IfThen {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        let cond = self.nth_with_cast(0, Expr::cast)?;
        let val = cond.eval(state)?;
        if val != 0 {
            let e1 = self.nth_with_cast(1, Expr::cast)?;
            let val = e1.eval(state)?;
            Some(val)
        } else {
            None
        }
    }
}

impl Eval<i64> for Parens {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        self.nth_with_cast(0, Expr::cast)?.eval(state)
    }
}

impl Eval<i64> for Let {
    type State = HashMap<String, i64>;
    fn eval(&self, _state: &Self::State) -> Option<i64> {
        None
    }
}

impl Eval<i64> for Fun {
    type State = HashMap<String, i64>;
    fn eval(&self, _state: &Self::State) -> Option<i64> {
        None
    }
}

impl Eval<i64> for Block {
    type State = HashMap<String, i64>;
    fn eval(&self, _state: &Self::State) -> Option<i64> {
        None
    }
}

impl Eval<i64> for Plus {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        let left = self.nth_with_cast(0, Expr::cast)?.eval(state)?;
        let right = self.nth_with_cast(1, Expr::cast)?.eval(state)?;
        Some(left + right)
    }
}

impl Eval<i64> for Minus {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        let left = self.nth_with_cast(0, Expr::cast)?.eval(state)?;
        let right = self.nth_with_cast(1, Expr::cast)?.eval(state)?;
        Some(left - right)
    }
}

impl Eval<i64> for Asterisk {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        let left = self.nth_with_cast(0, Expr::cast)?.eval(state)?;
        let right = self.nth_with_cast(1, Expr::cast)?.eval(state)?;
        Some(left * right)
    }
}

impl Eval<i64> for Slash {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        let left = self.nth_with_cast(0, Expr::cast)?.eval(state)?;
        let right = self.nth_with_cast(1, Expr::cast)?.eval(state)?;
        Some(left / right)
    }
}

impl Eval<i64> for Equal {
    type State = HashMap<String, i64>;
    fn eval(&self, state: &Self::State) -> Option<i64> {
        let left = self.nth_with_cast(0, Expr::cast)?.eval(state)?;
        let right = self.nth_with_cast(1, Expr::cast)?.eval(state)?;
        Some(if left == right { 1 } else { 0 })
    }
}

impl Eval<i64> for FunCall {
    type State = HashMap<String, i64>;
    fn eval(&self, _state: &Self::State) -> Option<i64> {
        None
    }
}

impl Eval<i64> for Primitive {
    type State = HashMap<String, i64>;
    fn eval(&self, _state: &Self::State) -> Option<i64> {
        fn text(node: &Primitive) -> String {
            match node.0.green().children().next() {
                Some(rowan::NodeOrToken::Token(token)) => token.text().to_string(),
                _ => unreachable!(),
            }
        }
        text(self).parse().ok()
    }
}
