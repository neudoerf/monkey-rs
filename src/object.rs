use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

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
    pub(crate) env: Env,
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

pub(crate) type Env = Rc<RefCell<Environment>>;

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Environment {
    store: HashMap<String, Rc<Object>>,
    outer: Option<Env>,
}

impl Environment {
    pub(crate) fn new() -> Env {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub(crate) fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Env {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: Some(Rc::clone(&outer)),
        }))
    }

    pub(crate) fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.get(name) {
            Some(obj) => Some(Rc::clone(obj)),
            None => {
                if let Some(outer) = &self.outer {
                    outer.borrow().get(name)
                } else {
                    None
                }
            }
        }
    }

    pub(crate) fn set(&mut self, name: &str, val: Rc<Object>) {
        self.store.insert(name.to_owned(), val);
    }
}
