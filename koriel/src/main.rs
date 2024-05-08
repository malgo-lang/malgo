use std::{
    collections::BTreeMap,
    io::{self, Read},
};

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
struct Program {
    top_vars: Vec<(String, Type, Expr)>,
    top_funs: Vec<(String, Vec<String>, Type, Expr)>,
    ext_funs: Vec<(String, Type)>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Type {
    FuncT(Vec<Type>, Box<Type>),
    Int32T,
    Int64T,
    FloatT,
    DoubleT,
    CharT,
    StringT,
    BoolT,
    SumT(Vec<Con>),
    PtrT(Box<Type>),
    RecordT(BTreeMap<String, Type>),
    AnyT,
    VoidT,
}

#[derive(Serialize, Deserialize, Debug)]
struct Con(Tag, Vec<Type>);

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Tag {
    Data(String),
    Tuple,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Expr {
    Atom(Atom),
    Call(Atom, Vec<Atom>),
    CallDirect(String, Vec<Atom>),
    RawCall(String, Type, Vec<Atom>),
    Cast(Type, Atom),
    Let(Vec<LocalDef>, Box<Expr>),
    Match(Box<Expr>, Vec<Case>),
    Switch(Atom, Vec<(Tag, Expr)>, Box<Expr>),
    SwitchUnboxed(Atom, Vec<(Unboxed, Expr)>, Box<Expr>),
    Destruct(Atom, Con, Vec<String>, Box<Expr>),
    DestructRecord(Atom, BTreeMap<String, String>, Box<Expr>),
    Assign(String, Box<Expr>, Box<Expr>),
    Error(Type),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Atom {
    Var(String),
    Unboxed(Unboxed),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Unboxed {
    Int32(i32),
    Int64(i64),
    Float(f32),
    Double(f64),
    Char(char),
    String(String),
    Bool(bool),
}

#[derive(Serialize, Deserialize, Debug)]
struct LocalDef {
    variable: String,
    typ: Type,
    object: Obj,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Case {
    Unpack(Con, Vec<String>, Expr),
    OpenRecord(BTreeMap<String, String>, Expr),
    Exact(Unboxed, Expr),
    Bind(String, Type, Expr),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Obj {
    Fun(Vec<String>, Expr),
    Pack(Type, Con, Vec<Atom>),
    Record(BTreeMap<String, Atom>),
}

fn main() -> io::Result<()> {
    // Read the stdin until EOF is reached
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    // Parse the input as JSON using serde_json
    let json: Program = serde_json::from_str(&input)?;

    // Print the JSON to stdout
    println!("{:#?}", json);

    Ok(())
}
