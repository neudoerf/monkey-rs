use crate::{
    ast::{Expression, Program, Statement},
    object::Object,
    token::Token,
};

pub(crate) fn eval_program(mut prog: Program) -> Object {
    match prog.len() {
        0 => Object::Null,
        1 => eval_statement(prog.remove(0)),
        _ => {
            let s = prog.remove(0);
            let _ = eval_statement(s);
            eval_program(prog)
        }
    }
}

fn eval_statement(stmt: Statement) -> Object {
    match stmt {
        Statement::ExpressionStatement(es) => eval_expression(es.expression),
        _ => todo!(),
    }
}

fn eval_expression(expr: Expression) -> Object {
    match expr {
        Expression::Integer(i) => Object::Integer(i),
        Expression::Boolean(b) => Object::Boolean(b),
        Expression::PrefixExpression(pe) => {
            eval_prefix_expression(pe.op, eval_expression(*pe.right))
        }
        Expression::InfixExpression(ie) => {
            eval_infix_expression(ie.op, eval_expression(*ie.left), eval_expression(*ie.right))
        }
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
        _ => Object::Null,
    }
}

fn eval_infix_expression(operator: Token, left: Object, right: Object) -> Object {
    if let (Object::Integer(left), Object::Integer(right)) = (left, right) {
        return eval_integer_infix_expression(operator, left, right);
    }
    if let (Object::Boolean(left), Object::Boolean(right)) = (left, right) {
        return eval_boolean_infix_expression(operator, left, right);
    }
    Object::Null
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
        _ => Object::Null,
    }
}

fn eval_boolean_infix_expression(operator: Token, left: bool, right: bool) -> Object {
    match operator {
        Token::Lt => Object::Boolean(left < right),
        Token::Gt => Object::Boolean(left > right),
        Token::Eq => Object::Boolean(left == right),
        Token::NotEq => Object::Boolean(left != right),
        _ => Object::Null,
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
    }

    fn test_eval(prog: &str) -> Object {
        let l = Lexer::new(prog);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        eval_program(program)
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
