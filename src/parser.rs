use crate::ast::{
    Expression, ExpressionStatement, InfixExpression, LetStatement, PrefixExpression, Program,
    ReturnStatement, Statement,
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
        let mut left_expr = self.prefix_dispatch()?;

        while !self.peek_token_is(Token::Semicolon)
            && prec(&precedence) < prec(&self.peek_precedence())
        {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt => {
                    self.next_token();
                    left_expr = self.parse_infix_expression(left_expr)?
                }
                _ => return Ok(left_expr),
            }
        }

        Ok(left_expr)
    }

    fn prefix_dispatch(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_integer_literal(),
            Token::True | Token::False => self.parse_boolean(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            _ => Err(format!(
                "no prefix parse function for {:?} found",
                self.cur_token
            )),
        }
    }

    fn parse_boolean(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::Boolean(self.cur_token_is(Token::True)))
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

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let op = self.cur_token.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        Ok(Expression::InfixExpression(InfixExpression {
            left: Box::new(left),
            op,
            right: Box::new(self.parse_expression(precedence)?),
        }))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        match self.cur_token {
            Token::Bang | Token::Minus => {
                let op = self.cur_token.clone();
                self.next_token();
                let right = self.parse_expression(Precedence::Prefix)?;
                Ok(Expression::PrefixExpression(PrefixExpression {
                    op,
                    right: Box::new(right),
                }))
            }
            _ => Err(format!(
                "expected current token to be Bang or Minus, got {:?} instead",
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

    fn peek_precedence(&self) -> Precedence {
        match lookup_precedence(&self.peek_token) {
            Some(p) => p,
            None => Precedence::Lowest,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match lookup_precedence(&self.cur_token) {
            Some(p) => p,
            None => Precedence::Lowest,
        }
    }
}

fn lookup_precedence(t: &Token) -> Option<Precedence> {
    match t {
        Token::Eq | Token::NotEq => Some(Precedence::Equals),
        Token::Lt | Token::Gt => Some(Precedence::LessGreater),
        Token::Plus | Token::Minus => Some(Precedence::Sum),
        Token::Slash | Token::Asterisk => Some(Precedence::Product),
        _ => return None,
    }
}

fn prec(p: &Precedence) -> usize {
    match p {
        Precedence::Lowest => 0,
        Precedence::Equals => 1,
        Precedence::LessGreater => 2,
        Precedence::Sum => 3,
        Precedence::Product => 4,
        Precedence::Prefix => 5,
        Precedence::Call => 6,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            Expression, InfixExpression, LetStatement, PrefixExpression, ReturnStatement, Statement,
        },
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

    #[test]
    fn test_prefix_expressions() {
        let tests = vec![("!5", Token::Bang, 5), ("-15", Token::Minus, 15)];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();

            check_errors(p);
            assert_eq!(program.len(), 1);

            for stmt in program {
                match stmt {
                    Statement::ExpressionStatement(ex) => match ex.expression {
                        Expression::PrefixExpression(PrefixExpression { op, right }) => {
                            assert_eq!(op, tt.1);
                            assert_eq!(right, Box::new(Expression::IntegerLiteral(tt.2)));
                        }
                        _ => panic!("expression is not PrefixExpression"),
                    },
                    _ => panic!("stmt is not ExpressionStatement"),
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let tests = vec![
            ("5 + 5;", 5, Token::Plus, 5),
            ("5 - 5;", 5, Token::Minus, 5),
            ("5 * 5;", 5, Token::Asterisk, 5),
            ("5 / 5;", 5, Token::Slash, 5),
            ("5 > 5;", 5, Token::Gt, 5),
            ("5 < 5;", 5, Token::Lt, 5),
            ("5 == 5;", 5, Token::Eq, 5),
            ("5 != 5;", 5, Token::NotEq, 5),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();

            check_errors(p);
            assert_eq!(program.len(), 1);

            for stmt in program {
                match stmt {
                    Statement::ExpressionStatement(ex_stmt) => match ex_stmt.expression {
                        Expression::InfixExpression(InfixExpression { left, op, right }) => {
                            assert_eq!(left, Box::new(Expression::IntegerLiteral(tt.1)));
                            assert_eq!(op, tt.2);
                            assert_eq!(right, Box::new(Expression::IntegerLiteral(tt.3)));
                        }
                        _ => panic!("expression is not InfixExpression"),
                    },
                    _ => panic!("stmt is not ExpressionStatement"),
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();

            check_errors(p);
            // assert_eq!(program.len(), 1);
            let p: String = program.iter().map(|i| format!("{}", i)).collect();
            assert_eq!(p, tt.1);
        }
    }
}
