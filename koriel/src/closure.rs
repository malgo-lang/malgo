use crate::{
    name::{self, Id},
    syntax,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

type SName = name::Name<syntax::Type>;
pub type Name = name::Name<Type>;

/// Context for closure conversion.
struct ClosureContext {
    program: Program,
}

impl ClosureContext {
    pub fn new() -> Self {
        ClosureContext {
            program: Program {
                variables: Vec::new(),
                functions: Vec::new(),
                closures: Vec::new(),
                externals: Vec::new(),
            },
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
}

type Result<T> = std::result::Result<T, CCError>;

#[derive(Debug)]
pub enum CCError {
    InvalidType(SName, syntax::Type),
    InvalidType2(Name, Type),
}

impl std::fmt::Display for CCError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CCError::InvalidType(name, typ) => {
                write!(f, "Invalid type: `{}` as {:?}", name.id, typ)
            }
            CCError::InvalidType2(name, typ) => {
                write!(f, "Invalid type: `{}` as {:?}", name.id, typ)
            }
        }
    }
}

impl std::error::Error for CCError {}

pub fn closure_conversion(program: syntax::Program) -> Result<Program> {
    let mut context = ClosureContext::new();
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

fn cc_name(context: &mut ClosureContext, name: SName) -> Result<Name> {
    let meta = cc_type(context, name.meta)?;

    Ok(Name { meta, id: name.id })
}

fn cc_variable_def(context: &mut ClosureContext, variable_def: syntax::VariableDef) -> Result<()> {
    let name = cc_name(context, variable_def.name)?;
    let value = cc_expr(context, variable_def.value)?;

    context.add_variable(VariableDef { name, value });

    Ok(())
}

fn cc_function_def(context: &mut ClosureContext, function_def: syntax::FunctionDef) -> Result<()> {
    let env_name: SName = to_env_name(&function_def.name);
    let env_name: Name = Name {
        meta: Type::EnvT,
        id: env_name.id,
    };

    let mut parameters = Vec::new();
    parameters.push(env_name.clone());
    for parameter in function_def.parameters {
        let parameter = cc_name(context, parameter)?;
        parameters.push(parameter);
    }

    let lifted_type = match function_def.typ {
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

    let lifted_name: SName = to_lifted_name(&function_def.name);
    let lifted_name = Name {
        meta: lifted_type.clone(),
        id: lifted_name.id,
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
        body,
    });

    let function_name = cc_name(context, function_def.name)?;
    context.add_closure(ClosureDef {
        name: function_name,
        function: lifted_name.clone(),
        environment: free_variables.into_iter().collect(),
    });

    Ok(())
}

/// Add the suffix "$lifted" to the name.
/// lifted name is used for the function that takes the environment as an argument.
fn to_lifted_name<T: Clone>(name: &name::Name<T>) -> name::Name<T> {
    name::Name {
        meta: name.meta.clone(),
        id: Id {
            name: format!("{}$lifted", name.id.name),
            module_name: name.id.module_name.clone(),
            sort: name.id.sort,
        },
    }
}

/// Make a name for a capture environment.
/// Add the suffix "$env" to the name.
fn to_env_name<T: Clone>(name: &name::Name<T>) -> name::Name<T> {
    name::Name {
        meta: name.meta.clone(),
        id: Id {
            name: format!("{}$env", name.id.name),
            module_name: name.id.module_name.clone(),
            sort: name.id.sort,
        },
    }
}

fn cc_external_def(context: &mut ClosureContext, external_def: syntax::ExternalDef) -> Result<()> {
    let typ = cc_type(context, external_def.typ)?;
    context.add_external(ExternalDef {
        name: external_def.name,
        typ,
    });

    Ok(())
}

fn cc_type(context: &mut ClosureContext, typ: syntax::Type) -> Result<Type> {
    match typ {
        syntax::Type::FuncT {
            parameters,
            returns,
        } => {
            let parameters = parameters
                .into_iter()
                .map(|parameter| cc_type(context, parameter))
                .collect::<Result<Vec<Type>>>()?;
            let returns = Box::new(cc_type(context, *returns)?);
            Ok(Type::FuncT {
                parameters,
                returns,
            })
        }
        syntax::Type::Int32T => Ok(Type::Int32T),
        syntax::Type::Int64T => Ok(Type::Int64T),
        syntax::Type::FloatT => Ok(Type::FloatT),
        syntax::Type::DoubleT => Ok(Type::DoubleT),
        syntax::Type::CharT => Ok(Type::CharT),
        syntax::Type::StringT => Ok(Type::StringT),
        syntax::Type::BoolT => Ok(Type::BoolT),
        syntax::Type::SumT { constructors } => {
            let constructors = constructors
                .into_iter()
                .map(|con| cc_con(context, con))
                .collect::<Result<Vec<Con>>>()?;
            Ok(Type::SumT { constructors })
        }
        syntax::Type::PtrT { inner } => {
            let inner = Box::new(cc_type(context, *inner)?);
            Ok(Type::PtrT { inner })
        }
        syntax::Type::RecordT { map } => {
            let map = map
                .into_iter()
                .map(|(key, value)| {
                    let cc_value = cc_type(context, value)?;
                    Ok((key, cc_value))
                })
                .collect::<Result<BTreeMap<String, Type>>>()?;
            Ok(Type::RecordT { map })
        }
        syntax::Type::AnyT => Ok(Type::AnyT),
        syntax::Type::VoidT => Ok(Type::VoidT),
    }
}

fn cc_con(context: &mut ClosureContext, con: syntax::Con) -> Result<Con> {
    let tag = cc_tag(con.tag)?;
    let parameters = con
        .parameters
        .into_iter()
        .map(|parameter| cc_type(context, parameter))
        .collect::<Result<Vec<Type>>>()?;
    Ok(Con { tag, parameters })
}

fn cc_tag(tag: syntax::Tag) -> Result<Tag> {
    match tag {
        syntax::Tag::Data { name } => Ok(Tag::Data { name }),
        syntax::Tag::Tuple => Ok(Tag::Tuple),
    }
}

fn cc_expr(context: &mut ClosureContext, expr: syntax::Expr) -> Result<Expr> {
    match expr {
        syntax::Expr::Atom { atom } => {
            let atom = cc_atom(context, atom)?;
            Ok(Expr::Atom { atom })
        }
        syntax::Expr::Call { callee, arguments } => {
            let callee = cc_atom(context, callee)?;
            let arguments = arguments
                .into_iter()
                .map(|argument| cc_atom(context, argument))
                .collect::<Result<Vec<Atom>>>()?;
            Ok(Expr::CallClosure { callee, arguments })
        }
        syntax::Expr::RawCall {
            name,
            typ,
            arguments,
        } => {
            let typ = cc_type(context, typ)?;
            let arguments = arguments
                .into_iter()
                .map(|argument| cc_atom(context, argument))
                .collect::<Result<Vec<Atom>>>()?;
            Ok(Expr::RawCall {
                name,
                typ,
                arguments,
            })
        }
        syntax::Expr::Cast { typ, value } => {
            let typ = cc_type(context, typ)?;
            let value = cc_atom(context, value)?;
            Ok(Expr::Cast { typ, value })
        }
        syntax::Expr::Let { bindings, body } => {
            let bindings = bindings
                .into_iter()
                .map(|binding| cc_local_def(context, binding))
                .collect::<Result<Vec<LocalDef>>>()?;
            let body = Box::new(cc_expr(context, *body)?);
            Ok(Expr::Let { bindings, body })
        }
        syntax::Expr::Match { scrutinee, clauses } => {
            let scrutinee = cc_atom(context, scrutinee)?;
            let clauses = clauses
                .into_iter()
                .map(|clause| cc_case(context, clause))
                .collect::<Result<Vec<Case>>>()?;
            Ok(Expr::Match { scrutinee, clauses })
        }
        syntax::Expr::Assign {
            variable,
            expression,
            body,
        } => {
            let variable = cc_name(context, variable)?;
            let expression = Box::new(cc_expr(context, *expression)?);
            let body = Box::new(cc_expr(context, *body)?);
            Ok(Expr::Assign {
                variable,
                expression,
                body,
            })
        }
        syntax::Expr::Error { typ } => Ok(Expr::Error {
            typ: cc_type(context, typ)?,
        }),
    }
}

fn cc_case(context: &mut ClosureContext, case: syntax::Case) -> Result<Case> {
    match case {
        syntax::Case::Unpack {
            constructor,
            variables,
            body,
        } => {
            let constructor = cc_con(context, constructor)?;
            let variables = variables
                .into_iter()
                .map(|variable| cc_name(context, variable))
                .collect::<Result<Vec<Name>>>()?;
            let body = cc_expr(context, body)?;
            Ok(Case::Unpack {
                constructor,
                variables,
                body,
            })
        }
        syntax::Case::OpenRecord { fields, body } => {
            let fields = fields
                .into_iter()
                .map(|(key, value)| {
                    let value = cc_name(context, value)?;
                    Ok((key, value))
                })
                .collect::<Result<BTreeMap<String, Name>>>()?;
            let body = cc_expr(context, body)?;
            Ok(Case::OpenRecord { fields, body })
        }
        syntax::Case::Exact { literal, body } => {
            let literal = cc_unboxed(context, literal);
            let body = cc_expr(context, body)?;
            Ok(Case::Exact { literal, body })
        }
        syntax::Case::Bind {
            variable,
            typ,
            body,
        } => {
            let variable = cc_name(context, variable)?;
            let typ = cc_type(context, typ)?;
            let body = cc_expr(context, body)?;
            Ok(Case::Bind {
                variable,
                typ,
                body,
            })
        }
    }
}

fn cc_local_def(context: &mut ClosureContext, local_def: syntax::LocalDef) -> Result<LocalDef> {
    let variable = cc_name(context, local_def.variable)?;
    let typ = cc_type(context, local_def.typ)?;
    let object = cc_obj(context, &variable, &typ, local_def.object)?;
    Ok(LocalDef {
        variable,
        typ,
        object,
    })
}

fn cc_obj(context: &mut ClosureContext, name: &Name, typ: &Type, obj: syntax::Obj) -> Result<Obj> {
    match obj {
        syntax::Obj::Fun { parameters, body } => {
            let env_name = to_env_name(name);
            let env_name = Name {
                meta: Type::EnvT,
                id: env_name.id,
            };

            let mut cc_parameters = Vec::new();
            cc_parameters.push(env_name.clone());
            for parameter in parameters {
                let parameter = cc_name(context, parameter)?;
                cc_parameters.push(parameter);
            }

            let lifted_type = match typ {
                Type::FuncT {
                    parameters,
                    returns,
                } => {
                    let mut cc_parameters = Vec::new();
                    cc_parameters.push(Type::EnvT);
                    for parameter in parameters {
                        cc_parameters.push(parameter.clone());
                    }
                    Type::FuncT {
                        parameters: cc_parameters,
                        returns: returns.clone(),
                    }
                }
                _ => return Err(CCError::InvalidType2(name.clone(), typ.clone())),
            };

            let mut body = cc_expr(context, body)?;

            let lifted_name = to_lifted_name(name);
            let lifted_name = Name {
                meta: lifted_type.clone(),
                id: lifted_name.id,
            };
            // Note: free_variables includes `name` and other bindings as free variables.
            let mut free_variables = body.free_variables();
            free_variables.remove(&lifted_name);
            for parameter in &cc_parameters {
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
                parameters: cc_parameters,
                body,
            });

            Ok(Obj::Closure {
                function: lifted_name,
                environment: free_variables.into_iter().collect(),
            })
        }
        syntax::Obj::Pack {
            typ,
            constructor,
            arguments,
        } => {
            let typ = cc_type(context, typ)?;
            let constructor = cc_con(context, constructor)?;
            let arguments = arguments
                .into_iter()
                .map(|argument| cc_atom(context, argument))
                .collect::<Result<Vec<Atom>>>()?;
            Ok(Obj::Pack {
                typ,
                constructor,
                arguments,
            })
        }
        syntax::Obj::Record { fields } => {
            let fields = fields
                .into_iter()
                .map(|(key, value)| {
                    let value = cc_atom(context, value)?;
                    Ok((key, value))
                })
                .collect::<Result<BTreeMap<String, Atom>>>()?;
            Ok(Obj::Record { fields })
        }
    }
}

fn cc_atom(context: &mut ClosureContext, atom: syntax::Atom) -> Result<Atom> {
    match atom {
        syntax::Atom::Var { variable } => {
            let variable = cc_name(context, variable)?;
            Ok(Atom::Var { variable })
        }
        syntax::Atom::Unboxed { literal } => {
            let literal = cc_unboxed(context, literal);
            Ok(Atom::Unboxed { literal })
        }
    }
}

fn cc_unboxed(_context: &mut ClosureContext, literal: syntax::Unboxed) -> Unboxed {
    match literal {
        syntax::Unboxed::Int32(i) => Unboxed::Int32(i),
        syntax::Unboxed::Int64(i) => Unboxed::Int64(i),
        syntax::Unboxed::Float(f) => Unboxed::Float(f),
        syntax::Unboxed::Double(f) => Unboxed::Double(f),
        syntax::Unboxed::Char(c) => Unboxed::Char(c),
        syntax::Unboxed::String(s) => Unboxed::String(s),
        syntax::Unboxed::Bool(b) => Unboxed::Bool(b),
    }
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
    pub value: Expr,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct FunctionDef {
    pub name: Name,
    pub parameters: Vec<Name>,
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
    /// The type of the environment in a closure.
    EnvT,
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
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
        scrutinee: Atom,
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, PartialEq)]
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
                for variable in environment {
                    set.insert(variable.clone());
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
