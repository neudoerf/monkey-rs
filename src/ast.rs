use std::{fmt, ops};

use crate::token::{Identifier, Token};

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Program(pub Vec<Statement>);

impl ops::Deref for Program {
    type Target = Vec<Statement>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ops::DerefMut for Program {
    fn deref_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.0
    }
}

impl IntoIterator for Program {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.iter().fold(Ok(()), |result, i| {
            result.and_then(|_| writeln!(f, "{}", i))
        })
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::LetStatement(ls) => write!(f, "let {} = {}", ls.ident, ls.value),
            Statement::ReturnStatement(rs) => write!(f, "return {}", rs.return_value),
            Statement::ExpressionStatement(es) => write!(f, "{}", es.expression),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct LetStatement {
    pub(crate) ident: Identifier,
    pub(crate) value: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct ReturnStatement {
    pub(crate) return_value: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct ExpressionStatement {
    pub(crate) expression: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Expression {
    Identifier(Identifier),
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Expression>),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    Function(Function),
    Call(Call),
    Index(Index),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct PrefixExpression {
    pub(crate) op: Token,
    pub(crate) right: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct InfixExpression {
    pub(crate) left: Box<Expression>,
    pub(crate) op: Token,
    pub(crate) right: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct IfExpression {
    pub(crate) condition: Box<Expression>,
    pub(crate) consequence: Program,
    pub(crate) alternative: Option<Program>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Function {
    pub(crate) parameters: Vec<Identifier>,
    pub(crate) body: Program,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Call {
    pub(crate) function: Box<Expression>,
    pub(crate) args: Vec<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Index {
    pub(crate) left: Box<Expression>,
    pub(crate) index: Box<Expression>,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(i) => write!(f, "{}", i),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::String(s) => write!(f, "{}", s),
            Expression::Array(a) => write!(
                f,
                "[{}]",
                a.iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Expression::PrefixExpression(pe) => write!(f, "({}{})", pe.op, pe.right),
            Expression::InfixExpression(ie) => write!(f, "({} {} {})", ie.left, ie.op, ie.right),
            Expression::IfExpression(ie) => {
                if let Some(alt) = &ie.alternative {
                    write!(f, "if {} {} else {}", ie.condition, ie.consequence, alt)
                } else {
                    write!(f, "if {} {}", ie.condition, ie.consequence)
                }
            }
            Expression::Function(func) => {
                write!(
                    f,
                    "fn({}) {}",
                    func.parameters
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<_>>()
                        .join(", "),
                    func.body
                )
            }
            Expression::Call(call) => {
                write!(
                    f,
                    "{}({})",
                    call.function,
                    call.args
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Expression::Index(i) => {
                write!(f, "({}[{}])", i.left, i.index)
            }
        }
    }
}
