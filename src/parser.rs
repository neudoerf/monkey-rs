use crate::ast::{
    Call, Expression, ExpressionStatement, Function, IfExpression, InfixExpression, LetStatement,
    PrefixExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{Identifier, Token};

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
        let mut program: Program = Program(Vec::new());

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
                Token::LParen => {
                    self.next_token();
                    left_expr = self.parse_call_expression(left_expr)?
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
            Token::LParen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            _ => Err(format!(
                "no prefix parse function for {:?} found",
                self.cur_token
            )),
        }
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        if !self.expect_peek(Token::LParen) {
            return Err(format!(
                "function missing LParen, found {} instead",
                self.peek_token
            ));
        }

        let params = self.parse_function_params()?;

        if !self.expect_peek(Token::LBrace) {
            return Err(format!(
                "function missing LBrace, found {} instead",
                self.peek_token
            ));
        }

        let body = self.parse_block_statement();

        Ok(Expression::Function(Function {
            parameters: params,
            body,
        }))
    }

    fn parse_function_params(&mut self) -> Result<Vec<Identifier>, ParserError> {
        let mut identifiers: Vec<Identifier> = vec![];

        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        if let Token::Ident(i) = &self.cur_token {
            identifiers.push(i.clone());
        } else {
            return Err(format!(
                "non-identifier found in function params: {}",
                self.cur_token
            ));
        }

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            if let Token::Ident(i) = &self.cur_token {
                identifiers.push(i.clone());
            } else {
                return Err(format!(
                    "non-identifier found in function params: {}",
                    self.cur_token
                ));
            }
        }

        if !self.expect_peek(Token::RParen) {
            return Err(format!(
                "missing RParen at end of function parameters, found {} instead",
                self.peek_token
            ));
        }

        Ok(identifiers)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        if !self.expect_peek(Token::LParen) {
            return Err("if missing LParen".to_owned());
        }

        self.next_token();

        let cond = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Token::RParen) {
            return Err("if missing RParen".to_owned());
        }

        if !self.expect_peek(Token::LBrace) {
            return Err("if missing LBrace".to_owned());
        }

        let cons = self.parse_block_statement();

        if self.peek_token_is(Token::Else) {
            self.next_token();

            if !self.expect_peek(Token::LBrace) {
                return Err("else missing LBrace".to_owned());
            }

            return Ok(Expression::IfExpression(IfExpression {
                condition: Box::new(cond),
                consequence: cons,
                alternative: Some(self.parse_block_statement()),
            }));
        } else {
            return Ok(Expression::IfExpression(IfExpression {
                condition: Box::new(cond),
                consequence: cons,
                alternative: None,
            }));
        }
    }

    fn parse_block_statement(&mut self) -> Program {
        let mut p = Program(vec![]);

        self.next_token();

        while !self.cur_token_is(Token::RBrace) && !self.cur_token_is(Token::EOF) {
            if let Ok(stmt) = self.parse_statement() {
                p.push(stmt);
            }
            self.next_token();
        }
        p
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token();

        let e = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(Token::RParen) {
            Err("Not RParen".to_owned())
        } else {
            e
        }
    }

    fn parse_boolean(&mut self) -> Result<Expression, ParserError> {
        Ok(Expression::Boolean(self.cur_token_is(Token::True)))
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        if let Token::Ident(ident) = &self.cur_token {
            Ok(Expression::Identifier(ident.clone()))
        } else {
            Err(format!(
                "expected current token to be Ident, got {:?} instead",
                self.cur_token
            ))
        }
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParserError> {
        if let Token::Int(int) = &self.cur_token {
            Ok(Expression::Integer(*int))
        } else {
            Err(format!(
                "expected current token to be Int, got {:?} instead",
                self.cur_token
            ))
        }
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParserError> {
        Ok(Expression::Call(Call {
            function: Box::new(function),
            args: self.parse_call_args()?,
        }))
    }

    fn parse_call_args(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut args = vec![];

        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(Token::RParen) {
            Err(format!(
                "Expected RParen and end of call args, got: {}",
                self.peek_token
            ))
        } else {
            Ok(args)
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

        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::ReturnStatement(ReturnStatement {
            return_value: expr,
        }))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        if self.cur_token != Token::Let {
            return Err(format!(
                "expected current token to be Let, got {:?} instead",
                self.cur_token
            ));
        }

        self.next_token();

        let ident: Identifier = match &self.cur_token {
            Token::Ident(i) => Ok(i.clone()),
            _ => Err(format!(
                "expected current token to be Ident, got {:?} instead",
                self.cur_token
            )),
        }?;

        if !self.expect_peek(Token::Assign) {
            return Err(format!(
                "expected next token to be Assign, got {:?} instead",
                self.peek_token
            ));
        }

        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Ok(Statement::LetStatement(LetStatement { ident, value: expr }))
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
        Token::LParen => Some(Precedence::Call),
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
        let tests = vec![
            ("let x = 5;", "x", Expression::Integer(5)),
            ("let y = 10;", "y", Expression::Integer(10)),
            (
                "let foobar = 838383;",
                "foobar",
                Expression::Integer(838383),
            ),
        ];

        for (test, ident, val) in tests {
            let l = Lexer::new(test);
            let mut p = Parser::new(l);

            let program = p.parse_program();

            check_errors(p);
            assert_eq!(program.len(), 1);

            test_let_statement(&program[0], ident, val);
        }
    }

    fn test_let_statement(s: &Statement, name: &str, expr: Expression) {
        let (i, v) = match s {
            Statement::LetStatement(LetStatement { ident, value }) => (ident, value),
            _ => panic!("s is not LetStatement"),
        };

        assert_eq!(i, name);

        assert_eq!(*v, expr);
    }

    fn check_errors(p: Parser) {
        p.errors.iter().for_each(|e| println!("{}", e));
        assert_eq!(p.errors.len(), 0);
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 5;", Expression::Integer(5)),
            ("return 10;", Expression::Integer(10)),
            ("return 993322;", Expression::Integer(993322)),
        ];

        for (test, expr) in tests {
            let l = Lexer::new(test);
            let mut p = Parser::new(l);

            let program = p.parse_program();

            check_errors(p);
            assert_eq!(program.len(), 1);

            match &program[0] {
                Statement::ReturnStatement(ReturnStatement { return_value }) => {
                    assert_eq!(*return_value, expr)
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
                    Expression::Integer(int) => assert_eq!(int, 5),
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
                            assert_eq!(right, Box::new(Expression::Integer(tt.2)));
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
                            assert_eq!(left, Box::new(Expression::Integer(tt.1)));
                            assert_eq!(op, tt.2);
                            assert_eq!(right, Box::new(Expression::Integer(tt.3)));
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
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
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

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_errors(p);

        assert_eq!(program.len(), 1);
        match &program[0] {
            Statement::ExpressionStatement(es) => match &es.expression {
                Expression::IfExpression(ie) => {
                    assert_eq!(format!("{}", ie.condition), "(x < y)");
                    assert_eq!(ie.consequence.len(), 1);
                }
                _ => panic!("expression is not if expression"),
            },
            _ => panic!("statement is not epxression statement"),
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_errors(p);

        assert_eq!(program.len(), 1);
        match &program[0] {
            Statement::ExpressionStatement(es) => match &es.expression {
                Expression::IfExpression(ie) => {
                    assert_eq!(format!("{}", ie.condition), "(x < y)");
                    assert_eq!(ie.consequence.len(), 1);
                    if let Some(alt) = &ie.alternative {
                        assert_eq!(alt.len(), 1);
                    } else {
                        panic!("if does not have alternative branch");
                    }
                }
                _ => panic!("expression is not if expression"),
            },
            _ => panic!("statement is not epxression statement"),
        }
    }

    #[test]
    fn test_function_parsing() {
        let input = "fn(x, y) {x + y}";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        check_errors(p);

        assert_eq!(program.len(), 1);
        match &program[0] {
            Statement::ExpressionStatement(es) => match &es.expression {
                Expression::Function(func) => {
                    assert_eq!(func.parameters.len(), 2);
                    assert_eq!(func.parameters[0], "x".to_owned());
                    assert_eq!(func.parameters[1], "y".to_owned());

                    assert_eq!(func.body.len(), 1);
                    match &func.body[0] {
                        Statement::ExpressionStatement(body_stmt) => {
                            assert_eq!(format!("{}", body_stmt.expression), "(x + y)")
                        }
                        _ => panic!("function body is not expression"),
                    }
                }
                _ => panic!("expression is not function"),
            },
            _ => panic!("statement is not expression statement"),
        }
    }

    #[test]
    fn test_function_params() {
        let tests = vec![
            ("fn() {}", vec![]),
            ("fn(x) {}", vec!["x"]),
            ("fn(x, y, z) {}", vec!["x", "y", "z"]),
        ];

        for (input, expected_params) in tests {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_errors(p);

            assert_eq!(program.len(), 1);
            match &program[0] {
                Statement::ExpressionStatement(es) => match &es.expression {
                    Expression::Function(func) => {
                        assert_eq!(func.parameters.len(), expected_params.len());

                        for (act, ex) in func.parameters.iter().zip(expected_params) {
                            assert_eq!(act, ex);
                        }
                    }
                    _ => panic!("expression is not function"),
                },
                _ => panic!("statement is not expression"),
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_errors(p);

        assert_eq!(program.len(), 1);
        match &program[0] {
            Statement::ExpressionStatement(es) => match &es.expression {
                Expression::Call(call) => {
                    match &*call.function {
                        Expression::Identifier(i) => assert_eq!(i, "add"),
                        _ => panic!("function expression is not identifier"),
                    };
                    assert_eq!(call.args.len(), 3);
                    assert_eq!(format!("{}", call.args[0]), "1");
                    assert_eq!(format!("{}", call.args[1]), "(2 * 3)");
                    assert_eq!(format!("{}", call.args[2]), "(4 + 5)");
                }
                _ => panic!("expression is not call"),
            },
            _ => panic!("statement is not expression"),
        }
    }
}
