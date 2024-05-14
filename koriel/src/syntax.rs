use crate::name::{native_name, Name};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    pub variables: Vec<VariableDef>,
    pub functions: Vec<FunctionDef>,
    pub externals: Vec<ExternalDef>,
}

#[derive(Clone)]
pub struct TypeContext {
    context: BTreeMap<Name, Type>,
    scrutinee: Option<Type>,
}

impl TypeContext {
    pub fn new() -> TypeContext {
        TypeContext {
            context: BTreeMap::new(),
            scrutinee: None,
        }
    }

    pub fn insert(&mut self, name: Name, typ: Type) {
        self.context.insert(name, typ);
    }

    pub fn get(&self, name: &Name) -> Option<Type> {
        self.context.get(name).map(|t| t.clone())
    }

    pub fn type_of<T: HasType>(&self, term: &T) -> Type {
        term.get_type(self)
    }

    pub fn set_scrutinee(&mut self, typ: Type) {
        self.scrutinee = Some(typ);
    }

    pub fn reset_scrutinee(&mut self) {
        self.scrutinee = None;
    }
}

pub trait HasType {
    fn get_type(&self, ctx: &TypeContext) -> Type;
}

pub trait Definable {
    fn add_to_context(&self, ctx: &mut TypeContext);
}

#[derive(Serialize, Deserialize, Debug)]
pub struct VariableDef {
    pub name: Name,
    #[serde(rename = "type")]
    pub typ: Type,
    pub value: Expr,
}

impl HasType for VariableDef {
    fn get_type(&self, _ctx: &TypeContext) -> Type {
        self.typ.clone()
    }
}

impl Definable for VariableDef {
    fn add_to_context(&self, ctx: &mut TypeContext) {
        ctx.insert(self.name.clone(), self.typ.clone())
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
    fn get_type(&self, ctx: &TypeContext) -> Type {
        assert!(matches!(self.typ, Type::FuncT { .. }));
        match &self.typ {
            Type::FuncT {
                returns,
                parameters,
            } => {
                let mut new_ctx: TypeContext = ctx.clone();
                for (param, expected) in self.parameters.iter().zip(parameters.iter()) {
                    new_ctx.insert(param.clone(), expected.clone())
                }

                assert_eq!(**returns, self.body.get_type(&new_ctx))
            }
            _ => unreachable!(),
        }
        self.typ.clone()
    }
}

impl Definable for FunctionDef {
    fn add_to_context(&self, ctx: &mut TypeContext) {
        let fun_type = self.typ.clone();
        assert!(matches!(fun_type, Type::FuncT { .. }));

        ctx.insert(self.name.clone(), fun_type)
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct ExternalDef {
    pub name: String,
    #[serde(rename = "type")]
    pub typ: Type,
}

impl HasType for ExternalDef {
    fn get_type(&self, _ctx: &TypeContext) -> Type {
        self.typ.clone()
    }
}

impl Definable for ExternalDef {
    fn add_to_context(&self, ctx: &mut TypeContext) {
        ctx.insert(native_name(&self.name), self.typ.clone())
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Con {
    pub tag: Tag,
    pub parameters: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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
        scrutinee: Box<Expr>,
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
    fn get_type(&self, ctx: &TypeContext) -> Type {
        match self {
            Expr::Atom { atom } => ctx.type_of(atom),
            Expr::Call { callee, arguments } => {
                let fun_type = ctx.type_of(callee);
                assert!(matches!(fun_type, Type::FuncT { .. }));
                match &fun_type {
                    Type::FuncT {
                        parameters,
                        returns,
                    } => {
                        assert_eq!(parameters.len(), arguments.len());
                        for (param, arg) in parameters.iter().zip(arguments.iter()) {
                            assert_eq!(param, &arg.get_type(ctx));
                        }
                        *returns.clone()
                    }
                    _ => unreachable!(),
                }
            }
            Expr::RawCall {
                name: _,
                typ,
                arguments,
            } => {
                assert!(matches!(typ, Type::FuncT { .. }));
                match typ {
                    Type::FuncT {
                        parameters,
                        returns,
                    } => {
                        assert_eq!(parameters.len(), arguments.len());
                        for (param, arg) in parameters.iter().zip(arguments.iter()) {
                            assert_eq!(param, &arg.get_type(ctx));
                        }
                        *returns.clone()
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Cast { typ, .. } => typ.clone(),
            Expr::Let { bindings, body } => {
                let mut new_ctx: TypeContext = ctx.clone();
                for binding in bindings {
                    binding.add_to_context(&mut new_ctx);
                }
                new_ctx.type_of(&**body)
            }
            Expr::Match { scrutinee, clauses } => {
                let scrutinee_type = scrutinee.get_type(ctx);
                let mut new_ctx: TypeContext = ctx.clone();
                new_ctx.set_scrutinee(scrutinee_type);
                assert!(clauses.len() > 0);
                new_ctx.type_of(&clauses[0])
            }
            Expr::Assign {
                variable,
                expression,
                body,
            } => {
                let expr_type = expression.get_type(ctx);
                let mut new_ctx: TypeContext = ctx.clone();
                new_ctx.insert(variable.clone(), expr_type);
                new_ctx.type_of(&**body)
            }
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
    fn get_type(&self, ctx: &TypeContext) -> Type {
        match self {
            Atom::Var { variable } => ctx.get(variable).expect(&format!("{} not found", variable)),
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

impl Definable for LocalDef {
    fn add_to_context(&self, ctx: &mut TypeContext) {
        ctx.insert(self.variable.clone(), self.typ.clone())
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

impl HasType for Case {
    fn get_type(&self, ctx: &TypeContext) -> Type {
        let scrutinee = ctx.scrutinee.as_ref().unwrap();
        let mut new_ctx: TypeContext = ctx.clone();
        new_ctx.reset_scrutinee();

        match self {
            Case::Unpack {
                constructor,
                variables,
                body,
            } => {
                assert_eq!(variables.len(), constructor.parameters.len());
                for (variable, typ) in variables.iter().zip(&constructor.parameters) {
                    new_ctx.insert(variable.clone(), typ.clone())
                }
                new_ctx.type_of(body)
            }
            Case::OpenRecord { fields, body } => {
                assert!(matches!(scrutinee, Type::RecordT { .. }));
                let record_type = match scrutinee {
                    Type::RecordT { map } => map,
                    _ => unreachable!(),
                };
                for (field, variable) in fields {
                    let typ = record_type.get(field).unwrap();
                    new_ctx.insert(variable.clone(), typ.clone())
                }
                new_ctx.type_of(body)
            }
            Case::Exact { body, .. } => ctx.type_of(body),
            Case::Bind {
                variable,
                typ,
                body,
            } => {
                new_ctx.insert(variable.clone(), typ.clone());
                new_ctx.type_of(body)
            }
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
