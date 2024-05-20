use std::{collections::BTreeMap, rc::Rc};

use crate::closure::{self, *};

#[derive(Debug, Clone)]
pub enum Value {
    Int32(i32),
    Int64(i64),
    Float(f32),
    Double(f64),
    Char(char),
    String(String),
    Bool(bool),
    Closure(Closure),
    Record(BTreeMap<String, Value>),
    Variant(Tag, Vec<Value>),
}

type Env = BTreeMap<Name, Value>;

#[derive(Debug, Clone)]
pub struct Closure {
    pub env: Env,
    pub parameters: Vec<Name>,
    pub body: Rc<Expr>,
}

pub type Result<T> = std::result::Result<T, String>;

pub fn eval_expr(env: &Env, expr: &Expr) -> Result<Value> {
    match expr {
        Expr::Atom { atom } => eval_atom(env, atom),
        Expr::CallClosure { callee, arguments } => {
            let closure = eval_atom(env, callee)?;
            let arguments = arguments
                .iter()
                .map(|arg| eval_atom(env, arg))
                .collect::<Result<Vec<_>>>()?;
            match closure {
                Value::Closure(closure) => {
                    let mut new_env = closure.env.clone();
                    for (name, value) in closure.parameters.iter().zip(arguments) {
                        new_env.insert(name.clone(), value);
                    }
                    eval_expr(&new_env, &closure.body)
                }
                _ => Err("Not a closure".to_string()),
            }
        }
        Expr::RawCall {
            name,
            typ,
            arguments,
        } => {
            let arguments = arguments
                .iter()
                .map(|arg| eval_atom(env, arg))
                .collect::<Result<Vec<_>>>()?;
            eval_primitive(name, typ, arguments)
        }
        Expr::Cast { value, .. } => eval_atom(env, value),
        Expr::Let { bindings, body } => {
            todo!()
        }
        Expr::Match { scrutinee, clauses } => todo!(),
        Expr::Assign {
            variable,
            expression,
            body,
        } => todo!(),
        Expr::SelectEnv {
            variable,
            environment,
            body,
        } => todo!(),
        Expr::Error { typ } => todo!(),
    }
}

fn eval_primitive(name: &str, typ: &Type, arguments: Vec<Value>) -> Result<Value> {
    match name {
        "malgo_print_string" => {
            let value = arguments.get(0).ok_or("No argument")?;
            match value {
                Value::String(s) => {
                    println!("{}", s);
                    Ok(Value::Variant(Tag::Tuple, vec![]))
                }
                _ => Err("Not a string".to_string()),
            }
        }
        "malgo_newline" => {
            println!();
            Ok(Value::Variant(Tag::Tuple, vec![]))
        }
        _ => Err(format!("Unknown primitive {}", name)),
    }
}

pub fn eval_atom(env: &Env, atom: &Atom) -> Result<Value> {
    match atom {
        Atom::Var { variable } => env
            .get(variable)
            .cloned()
            .ok_or_else(|| format!("Variable {} not found", variable.id)),
        Atom::Unboxed { literal } => {
            let value = match literal {
                Unboxed::Int32(x) => Value::Int32(*x),
                Unboxed::Int64(x) => Value::Int64(*x),
                Unboxed::Float(x) => Value::Float(*x),
                Unboxed::Double(x) => Value::Double(*x),
                Unboxed::Char(x) => Value::Char(*x),
                Unboxed::String(x) => Value::String(x.clone()),
                Unboxed::Bool(x) => Value::Bool(*x),
            };
            Ok(value)
        }
    }
}
