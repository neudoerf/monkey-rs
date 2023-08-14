use super::object::{EvalError, Object};

pub(crate) fn lookup_builtin(ident: &str) -> Option<fn(Vec<Object>) -> Result<Object, EvalError>> {
    match ident {
        "len" => Some(eval_len),
        "first" => Some(eval_first),
        "rest" => Some(eval_rest),
        "push" => Some(eval_push),
        _ => None,
    }
}

fn eval_len(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match &args[0] {
        Object::String(s) => Ok(Object::Integer(s.len().try_into().unwrap())),
        Object::Array(a) => Ok(Object::Integer(a.len().try_into().unwrap())),
        _ => Err(EvalError::new(&format!(
            "argument to `len` not supported, got {}",
            args[0].type_str()
        ))),
    }
}

fn eval_first(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match &args[0] {
        Object::Array(a) => match a.len() {
            0 => Ok(Object::Null),
            1 => Ok(Object::Array(a.clone())),
            _ => Ok(Object::Array(vec![a[0].clone()])),
        },
        _ => Err(EvalError::new(&format!(
            "argument to `first` not supported, got {}",
            args[0].type_str()
        ))),
    }
}

fn eval_rest(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::new(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match &args[0] {
        Object::Array(a) => match a.len() {
            0 => Ok(Object::Null),
            1 => Ok(Object::Array(vec![])),
            _ => Ok(Object::Array(a[1..].to_vec())),
        },
        _ => Err(EvalError::new(&format!(
            "argument to `rest` not supported, got {}",
            args[0].type_str()
        ))),
    }
}

fn eval_push(args: Vec<Object>) -> Result<Object, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::new(&format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        )));
    }
    match &args[0] {
        Object::Array(a) => {
            let mut array = a.clone();
            array.push(args[1].clone());
            Ok(Object::Array(array))
        }
        _ => Err(EvalError::new(&format!(
            "argument to `rest` not supported, got {}",
            args[0].type_str()
        ))),
    }
}
