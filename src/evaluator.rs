use crate::{
    ast::{Expression, Program, Statement},
    object::Object,
};

fn eval_program(mut prog: Program) -> Object {
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
        _ => todo!(),
    }
}

mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];

        for (t, exp) in tests {
            let evaluated = test_eval(t);
            test_integer_object(evaluated, exp);
        }
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
}