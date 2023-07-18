use crate::{evaluator::eval, lexer::Lexer, object::Environment, parser::Parser};
use std::io::{self, Write};

const PROMPT: &str = ">> ";

pub(crate) fn start() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut env = Environment::new();

    loop {
        let mut buffer = String::new();
        print!("{}", PROMPT);
        stdout.flush().unwrap();
        stdin.read_line(&mut buffer).unwrap();
        let l = Lexer::new(&buffer);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if p.errors.len() != 0 {
            p.errors.iter().for_each(|e| println!("{}", e));
        } else {
            match eval(program, &mut env) {
                Ok(obj) => println!("{}", obj),
                Err(e) => println!("ERROR: {}", e),
            }
        }
    }
}
