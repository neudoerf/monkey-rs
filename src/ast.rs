use std::fmt;

use crate::token::{Identifier, Token};

pub(crate) type Program = Vec<Statement>;

pub(crate) enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

#[derive(PartialEq, Clone, Debug)]
pub(crate) enum Expression {
    Empty,
    Identifier(Identifier),
    IntegerLiteral(i64),
}

pub(crate) struct LetStatement {
    pub(crate) token: Token,
    pub(crate) value: Expression,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {:?} = {:?};", self.token, self.value)
    }
}

pub(crate) struct ReturnStatement {
    pub(crate) return_value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return {:?};", self.return_value)
    }
}

pub(crate) struct ExpressionStatement {
    pub(crate) expression: Expression,
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.expression)
    }
}
