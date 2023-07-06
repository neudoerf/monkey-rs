use crate::{evaluator::eval_program, lexer::Lexer, parser::Parser};
use std::io::{self, Write};

const PROMPT: &str = ">> ";

pub(crate) fn start() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

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
            let eval = eval_program(program);
            println!("{}", eval);
        }
    }
}
