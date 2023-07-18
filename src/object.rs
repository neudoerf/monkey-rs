use std::{collections::HashMap, fmt};

use crate::{ast::Program, token::Identifier};

#[derive(Debug)]
pub(crate) struct EvalError {
    details: String,
}

impl EvalError {
    pub(crate) fn new(msg: &str) -> EvalError {
        EvalError {
            details: msg.to_owned(),
        }
    }
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl std::error::Error for EvalError {
    fn description(&self) -> &str {
        &self.details
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Function(Function),
    Null,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Function {
    pub(crate) params: Vec<Identifier>,
    pub(crate) body: Program,
    pub(crate) env: Environment,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::ReturnValue(obj) => write!(f, "{}", *obj),
            Object::Function(func) => {
                write!(f, "fn(")?;
                write!(f, "{}", func.params.join(", "))?;
                write!(f, ") {{\n{}\n}}", func.body)
            }
            Object::Null => write!(f, "null"),
        }
    }
}

impl Object {
    pub(crate) fn type_str(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_owned(),
            Object::Boolean(_) => "BOOLEAN".to_owned(),
            Object::ReturnValue(obj) => obj.type_str(),
            Object::Function(_) => "FUNCTION".to_owned(),
            Object::Null => "NULL".to_owned(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub(crate) fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub(crate) fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
    }

    pub(crate) fn set(&mut self, name: &str, val: Object) -> Object {
        self.store.insert(name.to_owned(), val.clone());
        val
    }
}
