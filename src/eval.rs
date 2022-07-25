use std::{collections::HashMap, fmt::Display};

use super::parser::ast::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Int(i64),
    Bool(bool),
}

impl Value {
    pub fn int(&self) -> Result<i64> {
        match self {
            &Value::Int(n) => Ok(n),
            _ => Err(Error::TypeMismatch {
                expected: Type::Int,
                actual: self.type_of(),
            }),
        }
    }

    pub fn bool(&self) -> Result<bool> {
        match self {
            &Value::Bool(b) => Ok(b),
            _ => Err(Error::TypeMismatch {
                expected: Type::Bool,
                actual: self.type_of(),
            }),
        }
    }

    pub fn type_of(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Bool(_) => Type::Bool,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Int,
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    TypeMismatch { expected: Type, actual: Type },
    InvalidNodeAccess(usize),
    Undefined,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::TypeMismatch { expected, actual } => {
                write!(
                    f,
                    "type mismatch: expected {}, but actual {}",
                    expected, actual
                )
            }
            Error::InvalidNodeAccess(n) => {
                write!(f, "invalid node access: {}", n)
            }
            Error::Undefined => {
                write!(f, "undefined")
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Eval {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value>;
}

impl Eval for Expr {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
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

impl Eval for UnaryMinus {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let arg = self
            .nth_with_cast(0, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(0))?;
        let val = arg.eval(state)?.int()?;
        Ok(Value::Int(-val))
    }
}

impl Eval for IfThenElse {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let cond = self
            .nth_with_cast(0, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(0))?;
        let val = cond.eval(state)?.bool()?;
        if val {
            let e1 = self
                .nth_with_cast(1, Expr::cast)
                .ok_or(Error::InvalidNodeAccess(1))?;
            let val = e1.eval(state)?;
            Ok(val)
        } else {
            let e2 = self
                .nth_with_cast(2, Expr::cast)
                .ok_or(Error::InvalidNodeAccess(2))?;
            let val = e2.eval(state)?;
            Ok(val)
        }
    }
}

impl Eval for IfThen {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let cond = self
            .nth_with_cast(0, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(0))?;
        let val = cond.eval(state)?.bool()?;
        if val {
            let e1 = self
                .nth_with_cast(1, Expr::cast)
                .ok_or(Error::InvalidNodeAccess(1))?;
            let val = e1.eval(state)?;
            Ok(val)
        } else {
            Err(Error::Undefined)
        }
    }
}

impl Eval for Parens {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        self.nth_with_cast(0, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(0))?
            .eval(state)
    }
}

impl Eval for Let {
    fn eval(&self, _state: &HashMap<String, Value>) -> Result<Value> {
        Err(Error::Undefined)
    }
}

impl Eval for Fun {
    fn eval(&self, _state: &HashMap<String, Value>) -> Result<Value> {
        Err(Error::Undefined)
    }
}

impl Eval for Block {
    fn eval(&self, _state: &HashMap<String, Value>) -> Result<Value> {
        Err(Error::Undefined)
    }
}

impl Eval for Plus {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self
            .nth_with_cast(0, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(0))?
            .eval(state)?
            .int()?;
        let right = self
            .nth_with_cast(1, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(1))?
            .eval(state)?
            .int()?;
        Ok(Value::Int(left + right))
    }
}

impl Eval for Minus {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self
            .nth_with_cast(0, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(0))?
            .eval(state)?
            .int()?;
        let right = self
            .nth_with_cast(1, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(1))?
            .eval(state)?
            .int()?;
        Ok(Value::Int(left - right))
    }
}

impl Eval for Asterisk {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self
            .nth_with_cast(0, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(0))?
            .eval(state)?
            .int()?;
        let right = self
            .nth_with_cast(1, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(1))?
            .eval(state)?
            .int()?;
        Ok(Value::Int(left * right))
    }
}

impl Eval for Slash {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self
            .nth_with_cast(0, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(0))?
            .eval(state)?
            .int()?;
        let right = self
            .nth_with_cast(1, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(1))?
            .eval(state)?
            .int()?;
        Ok(Value::Int(left / right))
    }
}

impl Eval for Equal {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self
            .nth_with_cast(0, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(0))?
            .eval(state)?;
        let right = self
            .nth_with_cast(1, Expr::cast)
            .ok_or(Error::InvalidNodeAccess(1))?
            .eval(state)?;
        Ok(Value::Bool(left == right))
    }
}

impl Eval for FunCall {
    fn eval(&self, _state: &HashMap<String, Value>) -> Result<Value> {
        Err(Error::Undefined)
    }
}

impl Eval for Primitive {
    fn eval(&self, _state: &HashMap<String, Value>) -> Result<Value> {
        fn text(node: &Primitive) -> String {
            match node.0.green().children().next() {
                Some(rowan::NodeOrToken::Token(token)) => token.text().to_string(),
                _ => unreachable!(),
            }
        }
        Ok(Value::Int(text(self).parse().unwrap()))
    }
}
