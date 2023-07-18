use crate::{
    ast::{Expression, IfExpression, Program, Statement},
    object::{Environment, EvalError, Function, Object},
    token::Token,
};

pub(crate) fn eval(prog: Program, env: &mut Environment) -> Result<Object, EvalError> {
    let evaluated = eval_program(prog, env);
    if let Ok(Object::ReturnValue(ret)) = evaluated {
        Ok(*ret)
    } else {
        evaluated
    }
}

pub(crate) fn eval_program(prog: Program, env: &mut Environment) -> Result<Object, EvalError> {
    let mut evaluated = Object::Null;
    for stmt in prog {
        evaluated = eval_statement(stmt, env)?;
        if let Object::ReturnValue(_) = evaluated {
            return Ok(evaluated);
        }
    }
    Ok(evaluated)
}

fn eval_statement(stmt: Statement, env: &mut Environment) -> Result<Object, EvalError> {
    match stmt {
        Statement::ExpressionStatement(es) => eval_expression(es.expression, env),
        Statement::ReturnStatement(rs) => Ok(Object::ReturnValue(Box::new(eval_expression(
            rs.return_value,
            env,
        )?))),
        Statement::LetStatement(ls) => {
            let val = eval_expression(ls.value, env)?;
            Ok(env.set(&ls.ident, val))
        }
    }
}

fn eval_expression(expr: Expression, env: &mut Environment) -> Result<Object, EvalError> {
    match expr {
        Expression::Integer(i) => Ok(Object::Integer(i)),
        Expression::Boolean(b) => Ok(Object::Boolean(b)),
        Expression::PrefixExpression(pe) => {
            let right = eval_expression(*pe.right, env)?;
            eval_prefix_expression(pe.op, right)
        }
        Expression::InfixExpression(ie) => {
            let left = eval_expression(*ie.left, env)?;
            let right = eval_expression(*ie.right, env)?;
            eval_infix_expression(ie.op, left, right)
        }
        Expression::IfExpression(ie) => eval_if_expression(ie, env),
        Expression::Identifier(i) => {
            let v = env.get(&i);
            if let Some(obj) = v {
                Ok((*obj).clone())
            } else {
                Err(EvalError::new(&format!("identifier not found: {}", i)))
            }
        }
        Expression::Function(func) => Ok(Object::Function(Function {
            params: func.parameters,
            body: func.body,
            env: (*env).clone(),
        })),
        _ => todo!(),
    }
}

fn eval_prefix_expression(operator: Token, right: Object) -> Result<Object, EvalError> {
    match operator {
        Token::Bang => Ok(eval_bang_operator_expression(right)),
        Token::Minus => eval_minus_operator_expression(right),
        _ => todo!(),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(b) => Object::Boolean(!b),
        Object::Null => Object::Boolean(false),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_operator_expression(right: Object) -> Result<Object, EvalError> {
    if let Object::Integer(i) = right {
        Ok(Object::Integer(-i))
    } else {
        Err(EvalError::new(&format!(
            "unknown operator: -{}",
            right.type_str()
        )))
    }
}

fn eval_infix_expression(
    operator: Token,
    left: Object,
    right: Object,
) -> Result<Object, EvalError> {
    if let (Object::Integer(left), Object::Integer(right)) = (left.clone(), right.clone()) {
        return eval_integer_infix_expression(operator, left, right);
    }
    if let (Object::Boolean(left), Object::Boolean(right)) = (left.clone(), right.clone()) {
        return eval_boolean_infix_expression(operator, left, right);
    }
    if std::mem::discriminant(&left) != std::mem::discriminant(&right) {
        Err(EvalError::new(&format!(
            "type mismatch: {} {} {}",
            left.type_str(),
            operator,
            right.type_str()
        )))
    } else {
        Err(EvalError::new(&format!(
            "unknown operator: {} {} {}",
            left.type_str(),
            operator,
            right.type_str()
        )))
    }
}

fn eval_integer_infix_expression(
    operator: Token,
    left: i64,
    right: i64,
) -> Result<Object, EvalError> {
    match operator {
        Token::Plus => Ok(Object::Integer(left + right)),
        Token::Minus => Ok(Object::Integer(left - right)),
        Token::Asterisk => Ok(Object::Integer(left * right)),
        Token::Slash => Ok(Object::Integer(left / right)),
        Token::Lt => Ok(Object::Boolean(left < right)),
        Token::Gt => Ok(Object::Boolean(left > right)),
        Token::Eq => Ok(Object::Boolean(left == right)),
        Token::NotEq => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError::new(&format!(
            "unknown operator: INTEGER {} INTEGER",
            operator
        ))),
    }
}

fn eval_boolean_infix_expression(
    operator: Token,
    left: bool,
    right: bool,
) -> Result<Object, EvalError> {
    match operator {
        Token::Lt => Ok(Object::Boolean(left < right)),
        Token::Gt => Ok(Object::Boolean(left > right)),
        Token::Eq => Ok(Object::Boolean(left == right)),
        Token::NotEq => Ok(Object::Boolean(left != right)),
        _ => Err(EvalError::new(&format!(
            "unknown operator: BOOLEAN {} BOOLEAN",
            operator
        ))),
    }
}

fn eval_if_expression(ie: IfExpression, env: &mut Environment) -> Result<Object, EvalError> {
    let cond = eval_expression(*ie.condition, env)?;
    if is_truthy(cond) {
        eval_program(ie.consequence, env)
    } else {
        if let Some(alt) = ie.alternative {
            eval_program(alt, env)
        } else {
            Ok(Object::Null)
        }
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(b) => b,
        _ => true,
    }
}
#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (t, exp) in tests {
            match test_eval(t) {
                Ok(evaluated) => assert!(test_integer_object(evaluated, exp)),
                Err(e) => panic!("ERROR: {}", e),
            }
        }
    }

    #[test]
    fn eval_bool_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (t, exp) in tests {
            match test_eval(t) {
                Ok(evaluated) => assert!(test_boolean_object(evaluated, exp)),
                Err(e) => panic!("ERROR: {}", e),
            }
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (t, exp) in tests {
            match test_eval(t) {
                Ok(evaluated) => assert!(test_boolean_object(evaluated, exp)),
                Err(e) => panic!("ERROR: {}", e),
            }
        }
    }

    #[test]
    fn test_if_else_expression() {
        let tests = vec![
            ("if (true) {10}", Object::Integer(10)),
            ("if (false) {10}", Object::Null),
            ("if (1) {10}", Object::Integer(10)),
            ("if (1 < 2) {10}", Object::Integer(10)),
            ("if (1 > 2) {10}", Object::Null),
            ("if (1 > 2) {10} else { 20 }", Object::Integer(20)),
            ("if (1 < 2) {10} else { 20 }", Object::Integer(10)),
        ];

        for (test, exp) in tests {
            match test_eval(test) {
                Ok(evaluated) => assert_eq!(evaluated, exp),
                Err(e) => panic!("ERROR: {}", e),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 5 * 2; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                "
    if (10 > 1) {
        if 10 > 1 {
            return 10;
        }
        
        return 1;
    }",
                10,
            ),
        ];

        for (test, exp) in tests {
            match test_eval(test) {
                Ok(evaluated) => assert!(test_integer_object(evaluated, exp)),
                Err(e) => panic!("ERROR: {}", e),
            }
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5;", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar;", "identifier not found: foobar"),
        ];

        for (test, exp) in tests {
            match test_eval(test) {
                Ok(obj) => panic!("{} is not error object", obj),
                Err(e) => assert_eq!(e.to_string(), exp),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (test, exp) in tests {
            match test_eval(test) {
                Ok(evaluated) => assert!(test_integer_object(evaluated, exp)),
                Err(e) => panic!("ERROR: {}", e),
            }
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) {x + 2;};";
        match test_eval(input) {
            Ok(evaluated) => {
                if let Object::Function(func) = evaluated {
                    assert_eq!(func.params.len(), 1);
                    assert_eq!(func.params[0], "x");
                    assert_eq!(format!("{}", func.body), "(x + 2)\n");
                } else {
                    panic!("object is not function. got {}", evaluated);
                }
            }
            Err(e) => panic!("ERROR: {}", e),
        }
    }

    fn test_eval(prog: &str) -> Result<Object, EvalError> {
        let l = Lexer::new(prog);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut env = Environment::new();

        eval(program, &mut env)
    }

    fn test_integer_object(obj: Object, expected: i64) -> bool {
        match obj {
            Object::Integer(i) => i == expected,
            _ => panic!("{} is not integer", obj),
        }
    }

    fn test_boolean_object(obj: Object, expected: bool) -> bool {
        match obj {
            Object::Boolean(b) => b == expected,
            _ => panic!("{} is not boolean", obj),
        }
    }
}
