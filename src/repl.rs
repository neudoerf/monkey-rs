use crate::{lexer::Lexer, token::Token};
use std::io::{self, Write};

const PROMPT: &str = ">> ";

pub(crate) fn start() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buffer = String::new();

    loop {
        print!("{}", PROMPT);
        stdout.flush().unwrap();
        stdin.read_line(&mut buffer).unwrap();
        let mut lexer = Lexer::new(&buffer);
        loop {
            let token = lexer.next_token();
            println!("{:?}", token);
            if token == Token::EOF {
                break;
            }
        }
        buffer.clear();
    }
}
