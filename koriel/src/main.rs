use std::{
    collections::BTreeMap,
    io::{self, Read},
};

use serde::{Deserialize, Serialize};
use unicode_xid::UnicodeXID;

#[derive(Debug)]
struct Name(Vec<String>);

impl<'de> Deserialize<'de> for Name {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: String = Deserialize::deserialize(deserializer)?;
        let prefix_removed = s.trim_start_matches("_M");
        let messages = split_by_run_length(prefix_removed);
        let replaced = messages.into_iter().map(hex_to_char).collect();

        Ok(Name(replaced))
    }
}

/// Convert a hex in a string to a char
/// Hex is represented as:
/// - "_xXX"
/// - "_uXXXX"
/// - "_UXXXXXXXX"
fn hex_to_char(s: String) -> String {
    let mut result = String::new();
    let mut iter = s.chars();

    while let Some(c) = iter.next() {
        if c == '_' {
            if let Some(next) = iter.next() {
                match next {
                    'x' => {
                        let hex = iter.by_ref().take(2).collect::<String>();
                        let char = char::from_u32(u32::from_str_radix(&hex, 16).unwrap()).unwrap();
                        result.push(char);
                    }
                    'u' => {
                        let hex = iter.by_ref().take(4).collect::<String>();
                        let char = char::from_u32(u32::from_str_radix(&hex, 16).unwrap()).unwrap();
                        result.push(char);
                    }
                    'U' => {
                        let hex = iter.by_ref().take(8).collect::<String>();
                        let char = char::from_u32(u32::from_str_radix(&hex, 16).unwrap()).unwrap();
                        result.push(char);
                    }
                    _ => {
                        result.push(next);
                    }
                }
            }
        } else {
            result.push(c);
        }
    }

    result
}

fn split_by_run_length(s: &str) -> Vec<String> {
    dbg!(s);
    let mut messages = Vec::new();
    let mut current = String::new();
    let mut count = 0;

    let mut iter = s.chars();

    while let Some(c) = iter.next() {
        if c.is_digit(10) {
            count = count * 10 + c.to_digit(10).unwrap();
        } else {
            current.push(c);
            for _ in 1..count {
                if let Some(next) = iter.next() {
                    current.push(next);
                } else {
                    panic!("Invalid input: {} => {}", s, current);
                }
            }
            count = 0;
            messages.push(current);
            current = String::new();
        }
    }

    messages
}

impl Serialize for Name {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let underscore_replaced = self
            .0
            .iter()
            .map(|c| {
                let mut result = String::new();
                for ch in c.chars() {
                    match ch {
                        '_' => result.push_str("_x5F"),
                        _ => result.push(ch),
                    }
                }
                result
            })
            .collect::<Vec<String>>();
        let other_replaced = underscore_replaced
            .iter()
            .map(char_to_hex)
            .collect::<Vec<String>>();
        let joined = join_by_run_length(other_replaced);

        let prefix_added = format!("_M{}", joined);
        prefix_added.serialize(serializer)
    }
}

fn join_by_run_length(msgs: Vec<String>) -> String {
    let mut result = String::new();

    for msg in msgs {
        let len_msg = msg.len().to_string();
        result.push_str(&len_msg);
        result.push_str(&msg);
    }

    result
}

/// Convert a no-ID_Continue char to hex in a string.
fn char_to_hex(s: &String) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            _ if c.is_xid_continue() => result.push(c),
            _ if c as u32 <= 0xFF => {
                result.push('_');
                result.push('x');
                result.push_str(&format!("{:02X}", c as u32));
            }
            _ if c as u32 <= 0xFFFF => {
                result.push('_');
                result.push('u');
                result.push_str(&format!("{:04X}", c as u32));
            }
            _ => {
                result.push('_');
                result.push('U');
                result.push_str(&format!("{:08X}", c as u32));
            }
        }
    }

    result
}

#[derive(Serialize, Deserialize, Debug)]
struct Program {
    top_vars: Vec<(Name, Type, Expr)>,
    top_funs: Vec<(Name, Vec<Name>, Type, Expr)>,
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
    CallDirect(Name, Vec<Atom>),
    RawCall(String, Type, Vec<Atom>),
    Cast(Type, Atom),
    Let(Vec<LocalDef>, Box<Expr>),
    Match(Box<Expr>, Vec<Case>),
    Switch(Atom, Vec<(Tag, Expr)>, Box<Expr>),
    SwitchUnboxed(Atom, Vec<(Unboxed, Expr)>, Box<Expr>),
    Destruct(Atom, Con, Vec<Name>, Box<Expr>),
    DestructRecord(Atom, BTreeMap<String, Name>, Box<Expr>),
    Assign(Name, Box<Expr>, Box<Expr>),
    Error(Type),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Atom {
    Var(Name),
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
    variable: Name,
    typ: Type,
    object: Obj,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Case {
    Unpack(Con, Vec<Name>, Expr),
    OpenRecord(BTreeMap<String, Name>, Expr),
    Exact(Unboxed, Expr),
    Bind(Name, Type, Expr),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag", content = "contents")]
enum Obj {
    Fun(Vec<Name>, Expr),
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
