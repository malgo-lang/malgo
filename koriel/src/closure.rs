use crate::name::Name;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    pub variables: Vec<(Name, Type, Expr)>,
    pub functions: Vec<(Name, Vec<Name>, Type, Expr)>,
    pub externals: Vec<(String, Type)>,
}

#[derive(Serialize, Deserialize, Debug)]
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
    /// The type of the environment in a closure.
    EnvT,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Con {
    pub tag: Tag,
    pub parameters: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug)]
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
    CallClosure {
        callee: Atom,
        arguments: Vec<Atom>,
    },
    CallDirect {
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
        scrutinee: Box<Expr>,
        clauses: Vec<Case>,
    },
    Assign {
        variable: Name,
        expression: Box<Expr>,
        body: Box<Expr>,
    },
    /// Select a variable from an environment.
    /// `environment` must be a map from a variable name to an atom and includes the `variable` as a key.
    SelectEnv {
        variable: Name,
        environment: Atom,
        body: Box<Expr>,
    },
    Error {
        #[serde(rename = "type")]
        typ: Type,
    },
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag")]
pub enum Atom {
    Var { variable: Name },
    Unboxed { literal: Unboxed },
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

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag")]
pub enum Obj {
    /// A closure object.
    /// The closure object is a pair of a function and an environment.
    /// The function is a name of a function.
    /// The environment is a map from a variable name to an atom.
    Closure {
        function: Name,
        environment: BTreeMap<Name, Atom>,
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
