use crate::{name::Name, syntax};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

/// Context for closure conversion.
struct ClosureContext {
    program: Program,
    unique_supply: u64,
    source_path: String,
}

impl ClosureContext {
    pub fn new(source_path: String) -> Self {
        ClosureContext {
            program: Program {
                variables: Vec::new(),
                functions: Vec::new(),
                closures: Vec::new(),
                externals: Vec::new(),
            },
            unique_supply: 0,
            source_path,
        }
    }

    pub fn add_variable(&mut self, variable: VariableDef) {
        self.program.variables.push(variable);
    }

    pub fn add_function(&mut self, function: FunctionDef) {
        self.program.functions.push(function);
    }

    pub fn add_closure(&mut self, closure: ClosureDef) {
        self.program.closures.push(closure);
    }

    pub fn add_external(&mut self, external: ExternalDef) {
        self.program.externals.push(external);
    }

    pub fn fresh_name(&mut self, hint: &str) -> Name {
        use crate::name::Sort::Temporal;
        let name = Name {
            name: format!("{}${}", hint, self.unique_supply),
            path: self.source_path.clone(),
            sort: Temporal,
        };
        self.unique_supply += 1;
        name
    }
}

type Result<T> = std::result::Result<T, CCError>;

#[derive(Debug)]
enum CCError {
    Unimplemented,
    NotValidSuffix(Name, &'static str),
    InvalidType(Name, syntax::Type),
}

impl std::fmt::Display for CCError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CCError::Unimplemented => write!(f, "Unimplemented"),
            CCError::NotValidSuffix(name, suffix) => {
                write!(f, "Not valid suffix: {} (expected: {})", name, suffix)
            }
            CCError::InvalidType(name, typ) => {
                write!(f, "Invalid type: `{}` as {:?}", name, typ)
            }
        }
    }
}

impl std::error::Error for CCError {}

pub fn closure_conversion(program: syntax::Program, source_path: String) -> Result<Program> {
    let mut context = ClosureContext::new(source_path);
    for variable_def in program.variables {
        cc_variable_def(&mut context, variable_def)?;
    }
    for function_def in program.functions {
        cc_function_def(&mut context, function_def)?;
    }
    for external_def in program.externals {
        cc_external_def(&mut context, external_def)?;
    }
    Ok(context.program)
}

fn cc_variable_def(context: &mut ClosureContext, variable_def: syntax::VariableDef) -> Result<()> {
    let typ = cc_type(context, variable_def.typ)?;
    let value = cc_expr(context, variable_def.value)?;

    context.add_variable(VariableDef {
        name: variable_def.name,
        typ,
        value,
    });

    Ok(())
}

fn cc_function_def(context: &mut ClosureContext, function_def: syntax::FunctionDef) -> Result<()> {
    let lifted_name: Name = to_lifted_name(&function_def.name);
    let env_name: Name = to_env_name(&function_def.name);

    let mut parameters = Vec::new();
    parameters.push(env_name.clone());
    for parameter in function_def.parameters {
        parameters.push(parameter);
    }

    let typ = match function_def.typ {
        syntax::Type::FuncT {
            parameters,
            returns,
        } => {
            let mut cc_parameters = Vec::new();
            cc_parameters.push(Type::EnvT);
            for parameter in parameters {
                cc_parameters.push(cc_type(context, parameter)?);
            }
            Type::FuncT {
                parameters: cc_parameters,
                returns: Box::new(cc_type(context, *returns)?),
            }
        }
        _ => return Err(CCError::InvalidType(function_def.name, function_def.typ)),
    };

    let mut body = cc_expr(context, function_def.body)?;

    // Note: free_variables includes `function_def.name` as a free variable.
    let mut free_variables = body.free_variables();
    free_variables.remove(&lifted_name);
    for parameter in &parameters {
        free_variables.remove(parameter);
    }
    free_variables.remove(&env_name);

    for free_variable in &free_variables {
        body = Expr::SelectEnv {
            variable: free_variable.clone(),
            environment: Atom::Var {
                variable: env_name.clone(),
            },
            body: Box::new(body),
        };
    }

    context.add_function(FunctionDef {
        name: lifted_name.clone(),
        parameters,
        typ,
        body,
    });

    context.add_closure(ClosureDef {
        name: function_def.name.clone(),
        function: lifted_name.clone(),
        environment: free_variables.into_iter().collect(),
    });

    Ok(())
}

/// Add the suffix "$lifted" to the name.
/// lifted name is used for the function that takes the environment as an argument.
fn to_lifted_name(name: &Name) -> Name {
    Name {
        name: format!("{}$closure", name.name),
        path: name.path.clone(),
        sort: name.sort.clone(),
    }
}

/// Remove the suffix "$lifted" from the name.
/// If the name does not have the suffix "$lifted", return an error (NotValidSuffix).
fn from_lifted_name(name: &Name) -> Result<Name> {
    if name.name.ends_with("$lifted") {
        let func = name.name.trim_end_matches("$lifted");
        Ok(Name {
            name: func.to_string(),
            path: name.path.clone(),
            sort: name.sort,
        })
    } else {
        Err(CCError::NotValidSuffix(name.clone(), "$lifted"))
    }
}

/// Make a name for a capture environment.
/// Add the suffix "$env" to the name.
fn to_env_name(name: &Name) -> Name {
    Name {
        name: format!("{}$env", name.name),
        path: name.path.clone(),
        sort: name.sort,
    }
}

/// Extract the original function name from an environment name.
/// Remove the suffix "$env" from the name.
/// If the name does not have the suffix "$env", return an error (NotValidSuffix).
fn from_env_name(name: &Name) -> Result<Name> {
    if name.name.ends_with("$env") {
        let func = name.name.trim_end_matches("$env");
        Ok(Name {
            name: func.to_string(),
            path: name.path.clone(),
            sort: name.sort,
        })
    } else {
        Err(CCError::NotValidSuffix(name.clone(), "$env"))
    }
}

fn cc_external_def(context: &mut ClosureContext, external_def: syntax::ExternalDef) -> Result<()> {
    todo!()
}

fn cc_type(context: &mut ClosureContext, typ: syntax::Type) -> Result<Type> {
    todo!()
}

fn cc_expr(context: &mut ClosureContext, expr: syntax::Expr) -> Result<Expr> {
    todo!()
}

trait ClosureTerm {
    fn free_variables(&self) -> BTreeSet<Name>;
}

/// A Program is a collection of definitions.
/// All definitions are global and mutually recursive.
#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    pub variables: Vec<VariableDef>,
    pub functions: Vec<FunctionDef>,
    pub closures: Vec<ClosureDef>,
    pub externals: Vec<ExternalDef>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct VariableDef {
    pub name: Name,
    #[serde(rename = "type")]
    pub typ: Type,
    pub value: Expr,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct FunctionDef {
    pub name: Name,
    pub parameters: Vec<Name>,
    #[serde(rename = "type")]
    pub typ: Type,
    pub body: Expr,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ClosureDef {
    pub name: Name,
    pub function: Name,
    pub environment: Vec<Name>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ExternalDef {
    pub name: String,
    #[serde(rename = "type")]
    pub typ: Type,
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
    /// Let expression.
    /// Bindings are mutually recursive.
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
    /// `environment` is a map from a variable name to an atom and includes the `variable` as a key.
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

impl ClosureTerm for Expr {
    fn free_variables(&self) -> BTreeSet<Name> {
        match self {
            Expr::Atom { atom } => atom.free_variables(),
            Expr::CallClosure { callee, arguments } => {
                let mut set = callee.free_variables();
                for argument in arguments {
                    set.extend(argument.free_variables());
                }
                set
            }
            Expr::RawCall { arguments, .. } => {
                let mut set = BTreeSet::new();
                for argument in arguments {
                    set.extend(argument.free_variables());
                }
                set
            }
            Expr::Cast { value, .. } => value.free_variables(),
            Expr::Let { bindings, body } => {
                let mut set = body.free_variables();
                for binding in bindings {
                    set.extend(binding.free_variables());
                }
                for binding in bindings {
                    set.remove(&binding.variable);
                }
                set
            }
            Expr::Match { scrutinee, clauses } => {
                let mut set = scrutinee.free_variables();
                for clause in clauses {
                    set.extend(clause.free_variables());
                }
                set
            }
            Expr::Assign {
                variable,
                expression,
                body,
            } => {
                let mut set = body.free_variables();
                set.remove(variable); // Remove the assigned variable from the free variables of the body.
                set.extend(expression.free_variables());
                set
            }
            Expr::SelectEnv {
                variable,
                environment,
                body,
            } => {
                let mut set = body.free_variables();
                set.remove(variable); // Remove the assigned variable from the free variables of the body.
                set.extend(environment.free_variables());
                set
            }
            Expr::Error { .. } => BTreeSet::new(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(tag = "tag")]
pub enum Atom {
    Var { variable: Name },
    Unboxed { literal: Unboxed },
}

impl ClosureTerm for Atom {
    fn free_variables(&self) -> BTreeSet<Name> {
        match self {
            Atom::Var { variable } => {
                let mut set = BTreeSet::new();
                set.insert(variable.clone());
                set
            }
            Atom::Unboxed { literal: _ } => BTreeSet::new(),
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

impl ClosureTerm for LocalDef {
    fn free_variables(&self) -> BTreeSet<Name> {
        let mut set = self.object.free_variables();
        set.remove(&self.variable);
        set
    }
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

impl ClosureTerm for Case {
    fn free_variables(&self) -> BTreeSet<Name> {
        match self {
            Case::Unpack {
                constructor: _,
                variables,
                body,
            } => {
                let mut set = body.free_variables();
                for variable in variables {
                    set.remove(variable);
                }
                set
            }
            Case::OpenRecord { fields, body } => {
                let mut set = body.free_variables();
                for variable in fields.values() {
                    set.remove(variable);
                }
                set
            }
            Case::Exact { literal: _, body } => body.free_variables(),
            Case::Bind {
                variable,
                typ: _,
                body,
            } => {
                let mut set = body.free_variables();
                set.remove(variable);
                set
            }
        }
    }
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
        environment: Vec<Name>,
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

impl ClosureTerm for Obj {
    fn free_variables(&self) -> BTreeSet<Name> {
        match self {
            Obj::Closure {
                function: _,
                environment,
            } => {
                let mut set = BTreeSet::new();
                for atom in environment.values() {
                    set.extend(atom.free_variables());
                }
                set
            }
            Obj::Pack {
                typ: _,
                constructor: _,
                arguments,
            } => {
                let mut set = BTreeSet::new();
                for argument in arguments {
                    set.extend(argument.free_variables());
                }
                set
            }
            Obj::Record { fields } => {
                let mut set = BTreeSet::new();
                for atom in fields.values() {
                    set.extend(atom.free_variables());
                }
                set
            }
        }
    }
}
