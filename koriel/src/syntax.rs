use crate::name;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

type Name = name::Name<Type>;

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    pub variables: Vec<VariableDef>,
    pub functions: Vec<FunctionDef>,
    pub externals: Vec<ExternalDef>,
}

pub trait HasType {
    fn get_type(&self) -> Type;
}

#[derive(Serialize, Deserialize, Debug)]
pub struct VariableDef {
    pub name: Name,
    #[serde(rename = "type")]
    pub typ: Type,
    pub value: Expr,
}

impl HasType for VariableDef {
    fn get_type(&self) -> Type {
        self.typ.clone()
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct FunctionDef {
    pub name: Name,
    pub parameters: Vec<Name>,
    #[serde(rename = "type")]
    pub typ: Type,
    pub body: Expr,
}

impl HasType for FunctionDef {
    fn get_type(&self) -> Type {
        self.typ.clone()
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ExternalDef {
    pub name: String,
    #[serde(rename = "type")]
    pub typ: Type,
}

impl HasType for ExternalDef {
    fn get_type(&self) -> Type {
        self.typ.clone()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[serde(tag = "tag")]
pub enum Type {
    FuncT {
        parameters: Vec<Type>,
        returns: Box<Type>,
    },
    Int32T,
    Int64T,
    FloatT,
    DoubleT,
    CharT,
    StringT,
    BoolT,
    SumT {
        constructors: Vec<Con>,
    },
    PtrT {
        inner: Box<Type>,
    },
    RecordT {
        map: BTreeMap<String, Type>,
    },
    AnyT,
    VoidT,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Con {
    pub tag: Tag,
    pub parameters: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[serde(tag = "tag")]
pub enum Tag {
    Data { name: String },
    Tuple,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag")]
pub enum Expr {
    Atom {
        atom: Atom,
    },
    Call {
        callee: Atom,
        arguments: Vec<Atom>,
    },
    RawCall {
        name: String,
        #[serde(rename = "type")]
        typ: Type,
        arguments: Vec<Atom>,
    },
    Cast {
        #[serde(rename = "type")]
        typ: Type,
        value: Atom,
    },
    Let {
        bindings: Vec<LocalDef>,
        body: Box<Expr>,
    },
    Match {
        scrutinee: Atom,
        clauses: Vec<Case>,
    },
    Assign {
        variable: Name,
        expression: Box<Expr>,
        body: Box<Expr>,
    },
    Error {
        #[serde(rename = "type")]
        typ: Type,
    },
}

impl HasType for Expr {
    fn get_type(&self) -> Type {
        match self {
            Expr::Atom { atom } => atom.get_type(),
            Expr::Call { callee, arguments } => {
                let fun_type = callee.get_type();
                assert!(matches!(fun_type, Type::FuncT { .. }));
                match &fun_type {
                    Type::FuncT {
                        parameters,
                        returns,
                    } => {
                        assert_eq!(parameters.len(), arguments.len());
                        for (param, arg) in parameters.iter().zip(arguments.iter()) {
                            assert_eq!(param, &arg.get_type());
                        }
                        *returns.clone()
                    }
                    _ => unreachable!(),
                }
            }
            Expr::RawCall { typ, arguments, .. } => {
                assert!(matches!(typ, Type::FuncT { .. }));
                match typ {
                    Type::FuncT {
                        parameters,
                        returns,
                    } => {
                        assert_eq!(parameters.len(), arguments.len());
                        for (param, arg) in parameters.iter().zip(arguments.iter()) {
                            assert_eq!(param, &arg.get_type());
                        }
                        *returns.clone()
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Cast { typ, .. } => typ.clone(),
            Expr::Let { body, .. } => body.get_type(),
            Expr::Match { clauses, .. } => {
                assert!(clauses.len() > 0);
                clauses[0].get_type()
            }
            Expr::Assign { body, .. } => body.get_type(),
            Expr::Error { typ } => typ.clone(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag")]
pub enum Atom {
    Var { variable: Name },
    Unboxed { literal: Unboxed },
}

impl HasType for Atom {
    fn get_type(&self) -> Type {
        match self {
            Atom::Var { variable } => variable.meta.clone(),
            Atom::Unboxed { literal } => match literal {
                Unboxed::Int32(_) => Type::Int32T,
                Unboxed::Int64(_) => Type::Int64T,
                Unboxed::Float(_) => Type::FloatT,
                Unboxed::Double(_) => Type::DoubleT,
                Unboxed::Char(_) => Type::CharT,
                Unboxed::String(_) => Type::StringT,
                Unboxed::Bool(_) => Type::BoolT,
            },
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
pub enum Unboxed {
    Int32(i32),
    Int64(i64),
    Float(f32),
    Double(f64),
    Char(char),
    String(String),
    Bool(bool),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct LocalDef {
    pub variable: Name,
    #[serde(rename = "type")]
    pub typ: Type,
    pub object: Obj,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag")]
pub enum Case {
    Unpack {
        constructor: Con,
        variables: Vec<Name>,
        body: Expr,
    },
    OpenRecord {
        fields: BTreeMap<String, Name>,
        body: Expr,
    },
    Exact {
        literal: Unboxed,
        body: Expr,
    },
    Bind {
        variable: Name,
        #[serde(rename = "type")]
        typ: Type,
        body: Expr,
    },
}

impl HasType for Case {
    fn get_type(&self) -> Type {
        match self {
            Case::Unpack { body, .. } => body.get_type(),
            Case::OpenRecord { body, .. } => body.get_type(),
            Case::Exact { body, .. } => body.get_type(),
            Case::Bind { body, .. } => body.get_type(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag")]
pub enum Obj {
    Fun {
        parameters: Vec<Name>,
        body: Expr,
    },
    Pack {
        #[serde(rename = "type")]
        typ: Type,
        constructor: Con,
        arguments: Vec<Atom>,
    },
    Record {
        fields: BTreeMap<String, Atom>,
    },
}
