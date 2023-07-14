use crate::{
    ast::{Expression, IfExpression, Program, Statement},
    object::{Environment, Function, Object},
    token::Token,
};

pub(crate) fn eval(prog: Program, env: &mut Environment) -> Object {
    let evaluated = eval_program(prog, env);
    if let Object::ReturnValue(ret) = evaluated {
        *ret
    } else {
        evaluated
    }
}

pub(crate) fn eval_program(prog: Program, env: &mut Environment) -> Object {
    let mut evaluated = Object::Null;
    for stmt in prog {
        evaluated = eval_statement(stmt, env);
        match evaluated {
            Object::ReturnValue(_) => return evaluated,
            Object::Error(_) => return evaluated,
            _ => (),
        }
    }
    evaluated
}

fn eval_statement(stmt: Statement, env: &mut Environment) -> Object {
    match stmt {
        Statement::ExpressionStatement(es) => eval_expression(es.expression, env),
        Statement::ReturnStatement(rs) => {
            Object::ReturnValue(Box::new(eval_expression(rs.return_value, env)))
        }
        Statement::LetStatement(ls) => {
            let val = eval_expression(ls.value, env);
            env.set(&ls.ident, val)
        }
    }
}

fn eval_expression(expr: Expression, env: &mut Environment) -> Object {
    match expr {
        Expression::Integer(i) => Object::Integer(i),
        Expression::Boolean(b) => Object::Boolean(b),
        Expression::PrefixExpression(pe) => {
            let right = eval_expression(*pe.right, env);
            if let Object::Error(_) = right {
                return right;
            }
            eval_prefix_expression(pe.op, right)
        }
        Expression::InfixExpression(ie) => {
            let left = eval_expression(*ie.left, env);
            if let Object::Error(_) = left {
                return left;
            }
            let right = eval_expression(*ie.right, env);
            if let Object::Error(_) = right {
                return right;
            }
            eval_infix_expression(ie.op, left, right)
        }
        Expression::IfExpression(ie) => eval_if_expression(ie, env),
        Expression::Identifier(i) => {
            let v = env.get(&i);
            if let Some(obj) = v {
                (*obj).clone()
            } else {
                Object::Error(format!("identifier not found: {}", i))
            }
        }
        Expression::Function(func) => Object::Function(Function {
            params: func.parameters,
            body: func.body,
            env: (*env).clone(),
        }),
        _ => todo!(),
    }
}

fn eval_prefix_expression(operator: Token, right: Object) -> Object {
    match operator {
        Token::Bang => eval_bang_operator_expression(right),
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

fn eval_minus_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(i) => Object::Integer(-i),
        _ => Object::Error(format!("unknown operator: -{}", right.type_str())),
    }
}

fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Object {
    if let (Object::Integer(left), Object::Integer(right)) = (left.clone(), right.clone()) {
        return eval_integer_infix_expression(operator, left, right);
    }
    if let (Object::Boolean(left), Object::Boolean(right)) = (left.clone(), right.clone()) {
        return eval_boolean_infix_expression(operator, left, right);
    }
    if std::mem::discriminant(&left) != std::mem::discriminant(&right) {
        Object::Error(format!(
            "type mismatch: {} {} {}",
            left.type_str(),
            operator,
            right.type_str()
        ))
    } else {
        Object::Error(format!(
            "unknown operator: {} {} {}",
            left.type_str(),
            operator,
            right.type_str()
        ))
    }
}

fn eval_integer_infix_expression(operator: Token, left: i64, right: i64) -> Object {
    match operator {
        Token::Plus => Object::Integer(left + right),
        Token::Minus => Object::Integer(left - right),
        Token::Asterisk => Object::Integer(left * right),
        Token::Slash => Object::Integer(left / right),
        Token::Lt => Object::Boolean(left < right),
        Token::Gt => Object::Boolean(left > right),
        Token::Eq => Object::Boolean(left == right),
        Token::NotEq => Object::Boolean(left != right),
        _ => Object::Error(format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn eval_boolean_infix_expression(operator: Token, left: bool, right: bool) -> Object {
    match operator {
        Token::Lt => Object::Boolean(left < right),
        Token::Gt => Object::Boolean(left > right),
        Token::Eq => Object::Boolean(left == right),
        Token::NotEq => Object::Boolean(left != right),
        _ => Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", operator)),
    }
}

fn eval_if_expression(ie: IfExpression, env: &mut Environment) -> Object {
    let cond = eval_expression(*ie.condition, env);
    if let Object::Error(_) = cond {
        return cond;
    }
    if is_truthy(cond) {
        eval_program(ie.consequence, env)
    } else {
        if let Some(alt) = ie.alternative {
            eval_program(alt, env)
        } else {
            Object::Null
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
            let evaluated = test_eval(t);
            assert!(test_integer_object(evaluated, exp));
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
            let evaluated = test_eval(t);
            assert!(test_boolean_object(evaluated, exp));
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
            let evaluated = test_eval(t);
            test_boolean_object(evaluated, exp);
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
            let evaluated = test_eval(test);
            assert_eq!(evaluated, exp);
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
            let evaluated = test_eval(test);
            assert!(test_integer_object(evaluated, exp));
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
            let evaluated = test_eval(test);
            assert!(test_error_object(evaluated, exp));
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
            let evaluated = test_eval(test);
            assert!(test_integer_object(evaluated, exp));
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) {x + 2;};";
        let evaluated = test_eval(input);
        if let Object::Function(func) = evaluated {
            assert_eq!(func.params.len(), 1);
            assert_eq!(func.params[0], "x");
            assert_eq!(format!("{}", func.body), "(x + 2)\n");
        } else {
            panic!("object is not function. got {}", evaluated);
        }
    }

    fn test_eval(prog: &str) -> Object {
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

    fn test_error_object(obj: Object, expected: &str) -> bool {
        if let Object::Error(e) = obj {
            e == expected
        } else {
            panic!("{} is not error object", obj);
        }
    }
}
