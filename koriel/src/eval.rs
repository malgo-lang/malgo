use std::{
    cell::OnceCell,
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use crate::closure::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueKind {
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

pub type Value = Rc<OnceCell<ValueKind>>;

pub type FunctionMap = BTreeMap<Name, (Vec<Name>, Rc<Expr>)>;

// To support recursive closures, we need to use OnceCell<Value> instead of Value.
#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    variables: BTreeMap<Name, Value>,
    functions: Rc<FunctionMap>,
}

impl Env {
    fn new(functions: Rc<FunctionMap>) -> Self {
        Self {
            variables: BTreeMap::new(),
            functions,
        }
    }

    /// Allocate a new variable in the environment.
    /// The variable is uninitialized.
    fn alloc(&mut self, name: Name) {
        self.variables.insert(name, Rc::new(OnceCell::new()));
    }

    /// Initialize a variable with a value.
    /// The variable must be uninitialized.
    fn initialize(&mut self, name: &Name, value_kind: ValueKind) -> Result<()> {
        self.variables
            .get(name)
            .ok_or(format!("Variable {} not found", name.id))?
            .set(value_kind)
            .map_err(|e| format!("Already initialized: {:?}", e))
    }

    /// Allocate a new variable in the environment and initialize it with a value.
    fn set(&mut self, name: Name, value: Value) {
        self.variables.insert(name, value);
    }

    /// Get the value of a variable.
    fn get(&self, name: &Name) -> Result<Value> {
        self.variables
            .get(name)
            .ok_or(format!("Variable {} not found", name.id))
            .map(|cell| cell.to_owned())
    }

    /// Get the function.
    fn get_function(&self, name: &Name) -> Result<(Vec<Name>, Rc<Expr>)> {
        self.functions
            .get(name)
            .ok_or(format!("Function {} not found", name.id))
            .map(|function| function.to_owned())
    }
}

/// Create a new value from a value kind.
pub fn wrap_value(value: ValueKind) -> Value {
    let cell = OnceCell::new();
    cell.set(value).unwrap();
    Rc::new(cell)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub env: Env,
    pub parameters: Vec<Name>,
    pub body: Rc<Expr>,
}

pub type Result<T> = std::result::Result<T, String>;

pub fn eval_program(program: Program) -> Result<Value> {
    let mut functions = BTreeMap::new();
    // Setup the function definitions
    for function in program.functions.into_iter() {
        let name = function.name;
        let parameters = function.parameters;
        let body = Rc::new(function.body);
        functions.insert(name, (parameters, body));
    }

    let mut env: Env = Env::new(Rc::new(functions));

    // Setup the global variables
    for variable in program.variables.iter() {
        let name = variable.name.clone();
        env.alloc(name);
    }

    // Setup the closures
    for closure in program.closures.iter() {
        let name = closure.name.clone();
        env.alloc(name);
    }

    for closure in program.closures.into_iter() {
        let name = closure.name;
        let function = env.get_function(&closure.function)?;
        env.initialize(
            &name,
            ValueKind::Closure(Closure {
                env: env.clone(),
                parameters: function.0.clone(),
                body: function.1.clone(),
            }),
        )?;
    }

    for variable in program.variables.into_iter() {
        let value_kind = eval_expr(&env, &variable.value)?
            .get()
            .ok_or("Uninitialized value")?
            .clone();
        env.initialize(&variable.name, value_kind)?;
    }

    // Search and evaluate the main function
    for (name, value) in env.variables.iter() {
        if name.id.name == "main" {
            match value.get() {
                Some(ValueKind::Closure(closure)) => {
                    let mut new_env = closure.env.clone();
                    assert_eq!(closure.parameters.len(), 2);
                    // Skip the first parameter, which is the environment
                    new_env.set(
                        closure.parameters[1].clone(),
                        wrap_value(ValueKind::Variant(Tag::Tuple, vec![])),
                    );
                    return eval_expr(&new_env, &closure.body);
                }
                _ => return Err("Not a closure".to_string()),
            }
        }
    }

    Err("Main function not found".to_string())
}

fn eval_expr(env: &Env, expr: &Expr) -> Result<Value> {
    match expr {
        Expr::Atom { atom } => eval_atom(env, atom),
        Expr::CallClosure { callee, arguments } => {
            let closure: Value = eval_atom(env, callee)?;
            let arguments: Vec<Value> = arguments
                .iter()
                .map(|arg| eval_atom(env, arg))
                .collect::<Result<Vec<_>>>()?;
            match closure.get().ok_or("Uninitialized value")? {
                ValueKind::Closure(closure) => {
                    let mut new_env = closure.env.clone();
                    // Skip the first parameter, which is the environment
                    for (name, value) in closure.parameters.iter().skip(1).zip(arguments) {
                        new_env.set(name.clone(), value);
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
            let mut new_env: Env = env.clone();
            for binding in bindings {
                let name = binding.variable.clone();
                new_env.alloc(name);
            }
            for binding in bindings {
                let name = &binding.variable;
                let value_kind = eval_obj(&new_env, &binding.object)?;
                new_env.initialize(name, value_kind)?;
            }
            eval_expr(&new_env, body)
        }
        Expr::Match { scrutinee, clauses } => {
            let scrutinee = eval_atom(env, scrutinee)?;
            eval_clauses(env, scrutinee, clauses)
        }
        Expr::Assign {
            variable,
            expression,
            body,
        } => {
            let value = eval_expr(env, expression)?;
            let mut new_env = env.clone();
            new_env.set(variable.clone(), value);
            eval_expr(&new_env, body)
        }
        Expr::SelectEnv { body, .. } => eval_expr(env, body),
        Expr::Error { typ } => Err(format!("Error: {:?}", typ)),
    }
}

fn eval_clauses(env: &Env, scrutinee: Value, clauses: &Vec<Case>) -> Result<Value> {
    for clause in clauses {
        let scrutinee = scrutinee.get().ok_or("Uninitialized value")?;
        let result = eval_clause(env, scrutinee, clause);
        if result.is_ok() {
            return result;
        }
    }
    Err("No matching clause".to_string())
}

fn eval_clause(env: &Env, scrutinee: &ValueKind, clause: &Case) -> Result<Value> {
    match clause {
        Case::Unpack {
            constructor,
            variables,
            body,
        } => {
            let expected_tag = &constructor.tag;
            match scrutinee {
                ValueKind::Variant(actual_tag, values) => {
                    if actual_tag != expected_tag {
                        return Err("Tag mismatch".to_string());
                    }
                    let mut new_env = env.clone();
                    for (variable, value) in variables.iter().zip(values.iter()) {
                        new_env.set(variable.clone(), value.to_owned());
                    }
                    eval_expr(&new_env, body)
                }
                _ => Err("Not a variant".to_string()),
            }
        }
        Case::OpenRecord { fields, body } => {
            let actual_fields = match scrutinee {
                ValueKind::Record(fields) => fields,
                _ => return Err("Not a record".to_string()),
            };
            let expected_keys: BTreeSet<&String> = fields.keys().collect();
            let actual_keys: BTreeSet<&String> = actual_fields.keys().collect();
            if !expected_keys.is_subset(&actual_keys) {
                return Err("Missing fields".to_string());
            }
            let mut new_env = env.clone();
            for (key, variable) in fields {
                let value = actual_fields.get(key).unwrap();
                new_env.set(variable.clone(), value.to_owned());
            }
            eval_expr(&new_env, body)
        }
        Case::Exact { literal, body } => {
            let expected_literal = eval_literal(literal);
            if scrutinee != &expected_literal {
                return Err("Literal mismatch".to_string());
            }
            eval_expr(env, body)
        }
        Case::Bind { variable, body, .. } => {
            let mut new_env = env.clone();
            new_env.set(variable.clone(), wrap_value(scrutinee.clone()));
            eval_expr(&new_env, body)
        }
    }
}

fn eval_obj(new_env: &Env, object: &Obj) -> Result<ValueKind> {
    match object {
        Obj::Closure {
            function,
            environment,
        } => {
            let function = new_env.get_function(function)?;

            let mut env = Env::new(new_env.functions.clone());

            // Keep only the variables that are used in the closure (in environment)
            for variable in environment {
                let value = new_env.get(variable)?;
                env.set(variable.clone(), value);
            }

            Ok(ValueKind::Closure(Closure {
                env,
                parameters: function.0.clone(),
                body: function.1.clone(),
            }))
        }
        Obj::Pack {
            constructor,
            arguments,
            ..
        } => {
            let arguments = arguments
                .iter()
                .map(|arg| eval_atom(new_env, arg))
                .collect::<Result<Vec<_>>>()?;
            Ok(ValueKind::Variant(constructor.tag.clone(), arguments))
        }
        Obj::Record { fields } => {
            let fields = fields
                .iter()
                .map(|(key, value)| {
                    let value = eval_atom(new_env, value)?;
                    Ok((key.clone(), value))
                })
                .collect::<Result<BTreeMap<String, Value>>>()?;
            Ok(ValueKind::Record(fields))
        }
    }
}

fn eval_primitive(name: &str, _typ: &Type, arguments: Vec<Value>) -> Result<Value> {
    match name {
        "malgo_print_string" => {
            let value = arguments.first().ok_or("No argument")?;
            let value = value.get().ok_or("Uninitialized value")?;
            match value {
                ValueKind::String(s) => {
                    println!("{}", s);
                    Ok(wrap_value(ValueKind::Variant(Tag::Tuple, vec![])))
                }
                _ => Err("Not a string".to_string()),
            }
        }
        "malgo_newline" => {
            println!();
            Ok(wrap_value(ValueKind::Variant(Tag::Tuple, vec![])))
        }
        _ => Err(format!("Unknown primitive {}", name)),
    }
}

fn eval_atom(env: &Env, atom: &Atom) -> Result<Value> {
    match atom {
        Atom::Var { variable } => env.get(variable),
        Atom::Unboxed { literal } => {
            let value_kind = eval_literal(literal);
            Ok(wrap_value(value_kind))
        }
    }
}

fn eval_literal(literal: &Unboxed) -> ValueKind {
    match literal {
        Unboxed::Int32(x) => ValueKind::Int32(*x),
        Unboxed::Int64(x) => ValueKind::Int64(*x),
        Unboxed::Float(x) => ValueKind::Float(*x),
        Unboxed::Double(x) => ValueKind::Double(*x),
        Unboxed::Char(x) => ValueKind::Char(*x),
        Unboxed::String(x) => ValueKind::String(x.clone()),
        Unboxed::Bool(x) => ValueKind::Bool(*x),
    }
}
