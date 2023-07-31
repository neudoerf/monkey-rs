mod ast;
mod interpreter;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() {
    repl::start();
}
