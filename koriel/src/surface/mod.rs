use anyhow::anyhow;
use anyhow::Result;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Literal {
    Int32(i32),
    Int64(i64),
    Float(f32),
    Double(f64),
    Char(char),
    String(String),
}

impl HasType for Literal {
    fn get_type(&self, _context: &Context) -> Result<Type> {
        Ok(match self {
            Literal::Int32(_) => Type::primitive(Primitive::Int32),
            Literal::Int64(_) => Type::primitive(Primitive::Int64),
            Literal::Float(_) => Type::primitive(Primitive::Float),
            Literal::Double(_) => Type::primitive(Primitive::Double),
            Literal::Char(_) => Type::primitive(Primitive::Char),
            Literal::String(_) => Type::primitive(Primitive::String),
        })
    }
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

impl HasType for Expr {
    fn get_type(&self, context: &Context) -> Result<Type> {
        self.kind.get_type(context)
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Var(Name),
    Literal(Literal),
}

impl HasType for ExprKind {
    fn get_type(&self, context: &Context) -> Result<Type> {
        match self {
            ExprKind::Var(name) => name.get_type(context),
            ExprKind::Literal(literal) => literal.get_type(context),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name {
    pub name: String,
}

impl HasType for Name {
    fn get_type(&self, context: &Context) -> Result<Type> {
        context.get_type(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Type {
    pub kind: TypeKind,
}

impl Type {
    pub fn primitive(primitive: Primitive) -> Self {
        Self {
            kind: TypeKind::Primitive(primitive),
        }
    }
}

trait HasType {
    fn get_type(&self, context: &Context) -> Result<Type>;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeKind {
    Primitive(Primitive),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Primitive {
    Int32,
    Int64,
    Float,
    Double,
    Char,
    String,
}

#[derive(Debug)]
pub struct Context {
    pub types: HashMap<Name, Type>,
}

impl Context {
    pub fn get_type(&self, name: &Name) -> Result<Type> {
        self.types
            .get(name)
            .map(|t| t.clone())
            .ok_or(anyhow!("Type not found"))
    }
}
