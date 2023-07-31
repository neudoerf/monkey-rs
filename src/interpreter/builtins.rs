use super::object::{EvalError, Object};

pub(crate) fn lookup_builtin(ident: &str) -> Option<fn(Vec<Object>) -> Result<Object, EvalError>> {
    match ident {
        "len" => Some(eval_len),
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
    if let Object::String(s) = &args[0] {
        Ok(Object::Integer(s.len().try_into().unwrap()))
    } else {
        Err(EvalError::new(&format!(
            "argument to `len` not supported, got {}",
            args[0].type_str()
        )))
    }
}
