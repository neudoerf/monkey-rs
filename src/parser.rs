use crate::ast::{
    Expression, ExpressionStatement, LetStatement, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::Token;

pub(crate) struct Parser {
    lexer: Lexer,

    cur_token: Token,
    peek_token: Token,

    errors: Vec<String>,
}

type ParserError = String;

enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut p = Parser {
            lexer,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            errors: Vec::new(),
        };

        p.next_token();
        p.next_token();

        return p;
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program: Program = Vec::new();

        while self.cur_token != Token::EOF {
            let stmt = self.parse_statement();
            match stmt {
                Ok(stmt) => program.push(stmt),
                Err(e) => self.errors.push(e),
            }
            self.next_token();
        }

        return program;
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::ExpressionStatement(ExpressionStatement {
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserError> {
        match self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_integer_literal(),
            _ => Ok(Expression::Empty),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        match &self.cur_token {
            Token::Ident(ident) => Ok(Expression::Identifier(ident.clone())),
            _ => Err(format!(
                "expected current token to be Ident, got {:?} instead",
                self.cur_token
            )),
        }
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token {
            Token::Int(int) => Ok(Expression::IntegerLiteral(int)),
            _ => Err(format!(
                "expected current token to be Int, got {:?} instead",
                self.cur_token
            )),
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        if self.cur_token != Token::Return {
            return Err(format!(
                "expected current token to be Return, got {:?} instead",
                self.cur_token
            ));
        }

        self.next_token();

        let stmt = Statement::ReturnStatement(ReturnStatement {
            return_value: Expression::Empty,
        });

        while !self.cur_token_is(Token::Semicolon) && !self.cur_token_is(Token::EOF) {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        if self.cur_token != Token::Let {
            return Err(format!(
                "expected current token to be Let, got {:?} instead",
                self.cur_token
            ));
        }

        self.next_token();

        let stmt = Statement::LetStatement(LetStatement {
            token: self.cur_token.clone(),
            value: Expression::Empty,
        });

        match self.cur_token {
            Token::Ident(_) => (),
            _ => {
                return Err(format!(
                    "expected current token to be Ident, got {:?} instead",
                    self.cur_token
                ))
            }
        }

        if !self.expect_peek(Token::Assign) {
            return Err(format!(
                "expected next token to be Assign, got {:?} instead",
                self.cur_token
            ));
        }

        while !self.cur_token_is(Token::Semicolon) && !self.cur_token_is(Token::EOF) {
            self.next_token();
        }

        Ok(stmt)
    }

    fn cur_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }

    fn peek_token_is(&self, t: Token) -> bool {
        self.peek_token == t
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            return true;
        } else {
            return false;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expression, LetStatement, ReturnStatement, Statement},
        lexer::Lexer,
        parser::Parser,
        token::Token,
    };

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();

        check_errors(p);
        assert_eq!(program.len(), 3);

        let tests = vec!["x", "y", "foobar"];

        for (i, tt) in tests.iter().enumerate() {
            let stmt = &program[i];
            test_let_statement(stmt, tt);
        }
    }

    fn test_let_statement(s: &Statement, name: &str) {
        let (t, v) = match s {
            Statement::LetStatement(LetStatement { token, value }) => (token, value),
            _ => panic!("s is not LetStatement"),
        };

        match t {
            Token::Ident(ident) => assert_eq!(ident, name),
            _ => panic!("t is not Let"),
        };

        assert_eq!(*v, Expression::Empty);
    }

    fn check_errors(p: Parser) {
        p.errors.iter().for_each(|e| println!("{}", e));
        assert_eq!(p.errors.len(), 0);
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();

        check_errors(p);
        assert_eq!(program.len(), 3);

        for stmt in program {
            match stmt {
                Statement::ReturnStatement(ReturnStatement { return_value }) => {
                    assert_eq!(return_value, Expression::Empty)
                }
                _ => panic!("stmt is not ReturnStatement"),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();

        check_errors(p);
        assert_eq!(program.len(), 1);

        for stmt in program {
            match stmt {
                Statement::ExpressionStatement(ex) => match ex.expression {
                    Expression::Identifier(ident) => assert_eq!(ident, "foobar"),
                    _ => panic!("expression is not Identifier"),
                },
                _ => panic!("stmt is not ExpressionStatement"),
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();

        check_errors(p);
        assert_eq!(program.len(), 1);

        for stmt in program {
            match stmt {
                Statement::ExpressionStatement(ex) => match ex.expression {
                    Expression::IntegerLiteral(int) => assert_eq!(int, 5),
                    _ => panic!("expression is not Identifier"),
                },
                _ => panic!("stmt is not ExpressionStatement"),
            }
        }
    }
}
