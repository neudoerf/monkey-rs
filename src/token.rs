use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Token {
    Illegal,
    EOF,

    // Identifiers + literals
    Ident(Identifier),
    Int(i64),
    String(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Eq,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", tok_to_str(self))
    }
}

pub(crate) type Identifier = String;

pub(crate) fn lookup_ident(ident: &str) -> Token {
    match ident {
        "fn" => Token::Function,
        "let" => Token::Let,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "true" => Token::True,
        "false" => Token::False,
        _ => Token::Ident(ident.to_owned()),
    }
}

fn tok_to_str(t: &Token) -> String {
    match t {
        Token::Illegal => "illegal".to_owned(),
        Token::EOF => "eof".to_owned(),
        Token::Ident(id) => id.clone(),
        Token::Int(i) => format!("{}", i),
        Token::String(s) => format!("{}", s),
        Token::Assign => "=".to_owned(),
        Token::Plus => "+".to_owned(),
        Token::Minus => "-".to_owned(),
        Token::Bang => "!".to_owned(),
        Token::Asterisk => "*".to_owned(),
        Token::Slash => "/".to_owned(),
        Token::Lt => "<".to_owned(),
        Token::Gt => ">".to_owned(),
        Token::Eq => "==".to_owned(),
        Token::NotEq => "!=".to_owned(),
        Token::Comma => ",".to_owned(),
        Token::Semicolon => ";".to_owned(),
        Token::LParen => "(".to_owned(),
        Token::RParen => ")".to_owned(),
        Token::LBrace => "{".to_owned(),
        Token::RBrace => "}".to_owned(),
        Token::Function => "fn".to_owned(),
        Token::Let => "let".to_owned(),
        Token::True => "true".to_owned(),
        Token::False => "false".to_owned(),
        Token::If => "if".to_owned(),
        Token::Else => "else".to_owned(),
        Token::Return => "return".to_owned(),
    }
}
