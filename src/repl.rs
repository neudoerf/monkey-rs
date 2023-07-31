use crate::{interpreter::eval, interpreter::object::Environment, lexer::Lexer, parser::Parser};
use std::{
    io::{self, Write},
    rc::Rc,
};

const PROMPT: &str = ">> ";

pub(crate) fn start() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let env = Environment::new();

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
            match eval(program, Rc::clone(&env)) {
                Ok(obj) => println!("{}", obj),
                Err(e) => println!("ERROR: {}", e),
            }
        }
    }
}
