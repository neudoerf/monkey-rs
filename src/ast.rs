use std::fmt;

use crate::token::{Identifier, Token};

pub(crate) type Program = Vec<Statement>;

pub(crate) enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::LetStatement(ls) => write!(f, "let {} = {}", ls.token, ls.value),
            Statement::ReturnStatement(rs) => write!(f, "return {}", rs.return_value),
            Statement::ExpressionStatement(es) => write!(f, "{}", es.expression),
        }
    }
}

pub(crate) struct LetStatement {
    pub(crate) token: Token,
    pub(crate) value: Expression,
}

pub(crate) struct ReturnStatement {
    pub(crate) return_value: Expression,
}

pub(crate) struct ExpressionStatement {
    pub(crate) expression: Expression,
}

#[derive(PartialEq, Clone, Debug)]
pub(crate) enum Expression {
    Empty,
    Identifier(Identifier),
    IntegerLiteral(i64),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
}

#[derive(PartialEq, Clone, Debug)]
pub(crate) struct PrefixExpression {
    pub(crate) op: Token,
    pub(crate) right: Box<Expression>,
}

#[derive(PartialEq, Clone, Debug)]
pub(crate) struct InfixExpression {
    pub(crate) left: Box<Expression>,
    pub(crate) op: Token,
    pub(crate) right: Box<Expression>,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Empty => write!(f, "EmptyExpression"),
            Expression::Identifier(i) => write!(f, "{}", i),
            Expression::IntegerLiteral(i) => write!(f, "{}", i),
            Expression::PrefixExpression(pe) => write!(f, "({}{})", pe.op, pe.right),
            Expression::InfixExpression(ie) => write!(f, "({} {} {})", ie.left, ie.op, ie.right),
        }
    }
}
