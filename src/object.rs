use std::fmt;

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Error(String),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::ReturnValue(obj) => write!(f, "{}", *obj),
            Object::Error(e) => write!(f, "ERROR: {}", e),
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
            Object::Error(_) => "ERROR".to_owned(),
            Object::Null => "NULL".to_owned(),
        }
    }
}
