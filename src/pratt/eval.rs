use std::{collections::HashMap, fmt::Display};

use super::parser::ast::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Fun(FunValue),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunValue {
    param: String,
    body: Expr,
    state: HashMap<String, Value>,
}

impl Display for FunValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<function>")
    }
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

    #[allow(dead_code)]
    pub fn fun(&self) -> Result<FunValue> {
        match self {
            Value::Fun(fun) => Ok(fun.clone()),
            _ => Err(Error::TypeMismatch {
                expected: Type::Fun,
                actual: self.type_of(),
            }),
        }
    }

    pub fn type_of(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Bool(_) => Type::Bool,
            Value::Fun(_) => Type::Fun,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Fun(fun) => write!(f, "{}", fun),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Int,
    Bool,
    Fun,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Fun => write!(f, "Fun"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error {
    TypeMismatch { expected: Type, actual: Type },
    Undefined,
    VariableNotDefined(String),
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
            Error::VariableNotDefined(name) => {
                write!(f, "variable {} is not defined", name)
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
        let val = self.arg().eval(state)?.int()?;
        Ok(Value::Int(-val))
    }
}

impl Eval for IfThenElse {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        if self.cond().eval(state)?.bool()? {
            self.then_body().eval(state)
        } else {
            self.else_body().eval(state)
        }
    }
}

impl Eval for IfThen {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        if self.cond().eval(state)?.bool()? {
            self.then_body().eval(state)
        } else {
            Err(Error::Undefined)
        }
    }
}

impl Eval for Parens {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        self.expr().eval(state)
    }
}

impl Eval for Let {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let val = self.value().eval(state)?;

        let mut newstate = state.clone();
        newstate.insert(self.var().text(), val);

        self.body().eval(&newstate)
    }
}

impl Eval for Fun {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        Ok(Value::Fun(FunValue {
            param: self.param().text(),
            body: self.body(),
            state: state.clone(),
        }))
    }
}

impl Eval for Block {
    fn eval(&self, _state: &HashMap<String, Value>) -> Result<Value> {
        Err(Error::Undefined)
    }
}

impl Eval for Plus {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self.left().eval(state)?.int()?;
        let right = self.right().eval(state)?.int()?;
        Ok(Value::Int(left + right))
    }
}

impl Eval for Minus {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self.left().eval(state)?.int()?;
        let right = self.right().eval(state)?.int()?;
        Ok(Value::Int(left - right))
    }
}

impl Eval for Asterisk {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self.left().eval(state)?.int()?;
        let right = self.right().eval(state)?.int()?;
        Ok(Value::Int(left * right))
    }
}

impl Eval for Slash {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self.left().eval(state)?.int()?;
        let right = self.right().eval(state)?.int()?;
        Ok(Value::Int(left / right))
    }
}

impl Eval for Equal {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let left = self.left().eval(state)?.int()?;
        let right = self.right().eval(state)?.int()?;
        Ok(Value::Bool(left == right))
    }
}

impl Eval for FunCall {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        let fun = self.fun().eval(state)?.fun()?;
        let arg = self.arg().eval(state)?;
        let mut fun_state = fun.state;
        fun_state.extend_one((fun.param, arg));
        fun.body.eval(&fun_state)
    }
}

impl Eval for Primitive {
    fn eval(&self, state: &HashMap<String, Value>) -> Result<Value> {
        fn text(node: &Primitive) -> String {
            match node.0.green().children().next() {
                Some(rowan::NodeOrToken::Token(token)) => token.text().to_string(),
                _ => unreachable!(),
            }
        }

        if let Ok(int) = text(self).parse() {
            Ok(Value::Int(int))
        } else {
            let name = text(self);
            state
                .get(&name)
                .ok_or(Error::VariableNotDefined(name))
                .map(|v| v.clone())
        }
    }
}
