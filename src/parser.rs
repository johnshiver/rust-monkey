use crate::ast::Statement::{ExpressionStatement, Let, Return};
use crate::ast::{
    BlockStatement, CallExpression, Expression, FunctionExpression, IdentExpression, IfExpression,
    InfixExpression, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::Token::{Assign, Eof, If, Rparen};
use std::collections::HashMap;

use crate::ast::Expression::{Ident, IfStatement};
use lazy_static::lazy_static;

// TODO: might be nice to add metadata to parse error, like line number
type ParseError = String;
type ParseErrors = Vec<ParseError>;
pub type ParseResult<T> = Result<T, ParseError>;
type PrefixFn = fn(parser: &mut Parser<'_>) -> ParseResult<Expression>;
type InfixFn = fn(parser: &mut Parser<'_>, left: Expression) -> ParseResult<Expression>;

// define operator precedence
type Precedence = i8;
const LOWEST: Precedence = 1;
const EQUALS: Precedence = 2; // ==
const LESS_GREATER: Precedence = 3; // < or >
const SUM: Precedence = 4; // +
const PRODUCT: Precedence = 5; // *
const PREFIX: Precedence = 6; // -X or !X
const CALL: Precedence = 7; // myFunction(X)

lazy_static! {
    static ref PRECEDENCE_TABLE: HashMap<Token, Precedence> = [
        (Token::EQ, EQUALS),
        (Token::NEQ, EQUALS),
        (Token::LT, LESS_GREATER),
        (Token::GT, LESS_GREATER),
        (Token::Plus, SUM),
        (Token::Minus, SUM),
        (Token::Slash, PRODUCT),
        (Token::Asterisk, PRODUCT),
        (Token::Lparen, CALL),
    ]
    .iter()
    .cloned()
    .collect();
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    peek_token: Token,
    pub errors: Vec<ParseError>, // TODO: might make sense to move this to program
}

impl<'a> Parser<'a> {
    pub fn new(mut l: Lexer<'a>) -> Self {
        let curr_token = l.next_token();
        let peek_token = l.next_token();
        Parser {
            lexer: l,
            curr_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    fn advance_tokens(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn peek_token_is(&self, tok: &Token) -> bool {
        // TODO: dont think this needs to be a reference
        match (&tok, &self.peek_token) {
            (Token::Ident(_), Token::Ident(_)) => true,
            (Token::Int(_), Token::Int(_)) => true,
            _ => tok == &self.peek_token,
        }
    }

    fn expect_peek(&mut self, tok: &Token) -> Result<(), ParseError> {
        match self.peek_token_is(tok) {
            true => {
                self.advance_tokens();
                Ok(())
            }
            false => Err(format!(
                "expected next token to be {} got {} instead",
                tok, self.peek_token
            )),
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match PRECEDENCE_TABLE.get(&self.peek_token) {
            Some(p) => *p,
            None => LOWEST,
        }
    }

    fn curr_precedence(&self) -> Precedence {
        match PRECEDENCE_TABLE.get(&self.curr_token) {
            Some(p) => *p,
            None => LOWEST,
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.curr_token {
            Token::Let => match self.parse_let() {
                Ok(stmt) => Ok(stmt),
                Err(e) => Err(e),
            },
            Token::Return => self.parse_return(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let stmt = self.parse_expression(LOWEST);
        if self.peek_token == Token::Semicolon {
            self.advance_tokens();
        }
        match stmt {
            Ok(expr) => Ok(ExpressionStatement(expr)),
            Err(e) => Err(e),
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        // evaluate prefix and assign to left
        let left = match self.curr_token.clone() {
            Token::Ident(literal) => Some(self.parse_ident(literal)),
            Token::Int(literal) => Some(self.parse_int_literal(literal)),
            Token::True => Some(self.parse_bool_literal(true)),
            Token::False => Some(self.parse_bool_literal(false)),
            Token::Lparen => Some(self.parse_grouped_expression()),
            Token::Bang | Token::Minus => Some(self.parse_prefix_expression()),
            Token::If => Some(self.parse_if_expression().unwrap()),
            Token::Function => Some(self.parse_function_literal().unwrap()),
            _ => None,
        };

        // if there was no prefix, return an error
        if left.is_none() {
            return Err(format!(
                "while parsing expression: {:?} expression token not supported",
                self.curr_token
            ));
        }

        let mut l = left.unwrap();
        while !(self.peek_token == Token::Semicolon) && precedence < self.peek_precedence() {
            match &self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::EQ
                | Token::NEQ
                | Token::GT
                | Token::LT => {
                    self.advance_tokens();
                    l = self.parse_infix_expression(l);
                }
                Token::Lparen => {
                    self.advance_tokens();
                    l = self.parse_call_expression(l).unwrap();
                }
                _ => break,
            }
        }
        Ok(l)
    }

    fn parse_let(&mut self) -> Result<Statement, ParseError> {
        let ident_name = match self.expect_ident() {
            Ok(name) => name,
            Err(e) => return Err(e),
        };

        match self.expect_peek(&Token::Assign) {
            Ok(()) => {}
            Err(e) => return Err(e),
        };

        self.advance_tokens();

        let let_val = match self.parse_expression(LOWEST) {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.advance_tokens();
        }

        let let_stmt = LetStatement::new(ident_name, Some(let_val));
        Ok(Let(Box::new(let_stmt)))
    }

    fn parse_return(&mut self) -> Result<Statement, ParseError> {
        self.advance_tokens();
        let stmt = match self.parse_expression(LOWEST) {
            Ok(exp) => exp,
            Err(e) => return Err(e),
        };
        let return_stmt = ReturnStatement::new(stmt);
        Ok(Return(Box::new(return_stmt)))
    }

    fn parse_ident(&mut self, literal: String) -> Expression {
        let ie = IdentExpression::new(literal);
        Expression::Ident(Box::new(ie))
    }

    fn parse_int_literal(&mut self, literal: i64) -> Expression {
        Expression::Integer(literal)
    }

    fn parse_bool_literal(&mut self, val: bool) -> Expression {
        Expression::Bool(val)
    }

    fn parse_grouped_expression(&mut self) -> Expression {
        self.advance_tokens();
        let exp = self.parse_expression(LOWEST).unwrap();
        self.expect_peek(&Token::Rparen).unwrap();

        exp
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        match self.expect_peek(&Token::Lparen) {
            Ok(()) => {}
            Err(e) => return Err(e),
        }
        self.advance_tokens();
        let condition = self.parse_expression(LOWEST).unwrap();
        match self.expect_peek(&Token::Rparen) {
            Ok(()) => {}
            Err(e) => {
                return Err(e);
            }
        }
        match self.expect_peek(&Token::Lbrace) {
            Ok(()) => {}
            Err(e) => {
                return Err(e);
            }
        }

        let consequence = match self.parse_block_statement() {
            Ok(block_statement) => block_statement,
            Err(e) => return Err(e),
        };

        let mut alternative = None;
        if self.peek_token_is(&Token::Else) {
            self.advance_tokens();
            match self.expect_peek(&Token::Lbrace) {
                Ok(()) => {}
                Err(e) => return Err(e),
            };
            alternative = match self.parse_block_statement() {
                Ok(stmt) => Some(stmt),
                Err(e) => return Err(e),
            };
        }

        let if_exp = IfExpression::new(Token::If, condition, consequence, alternative);
        Ok(IfStatement(Box::new(if_exp)))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let block_token = self.curr_token.clone();
        self.advance_tokens();
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while !(self.curr_token == Token::Rbrace) && !(self.curr_token == Token::Eof) {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => errors.push(err),
            }
            self.advance_tokens();
        }
        if !errors.is_empty() {
            let err_string = errors.join(" | ");
            return Err(err_string);
        }
        Ok(BlockStatement::new(block_token, statements))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParseError> {
        // should be a function token
        let tok = self.curr_token.clone();
        match self.expect_peek(&Token::Lparen) {
            Ok(()) => {}
            Err(e) => return Err(e),
        };

        let fn_params = match self.parse_function_parameters() {
            Ok(params) => params,
            Err(e) => return Err(e),
        };

        match self.expect_peek(&Token::Lbrace) {
            Ok(()) => {}
            Err(e) => return Err(e),
        };

        let body = match self.parse_block_statement() {
            Ok(blk) => blk,
            Err(e) => return Err(e),
        };

        let fn_lit = FunctionExpression::new(tok, fn_params, body);
        Ok(Expression::Function(Box::new(fn_lit)))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<IdentExpression>, ParseError> {
        let mut params = Vec::new();

        if self.peek_token_is(&Token::Rparen) {
            self.advance_tokens();
            return Ok(params);
        }
        self.advance_tokens();

        let param = match self.parse_ident_into_ident_expression() {
            Ok(ident_exp) => ident_exp,
            Err(e) => return Err(e),
        };
        params.push(param);

        while self.peek_token_is(&Token::Comma) {
            self.advance_tokens();
            self.advance_tokens();
            let param = match self.parse_ident_into_ident_expression() {
                Ok(ident_exp) => ident_exp,
                Err(e) => return Err(e),
            };
            params.push(param);
        }

        match self.expect_peek(&Token::Rparen) {
            Ok(()) => {}
            Err(e) => return Err(e),
        }
        return Ok(params);
    }

    fn parse_ident_into_ident_expression(&mut self) -> Result<IdentExpression, ParseError> {
        if let Token::Ident(name) = &self.curr_token {
            return Ok(IdentExpression::new(name.to_string()));
        }
        Err(format!(
            "expected current token to be an identifier, received {}",
            &self.curr_token
        ))
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let tok = self.curr_token.clone();
        self.advance_tokens();
        // TODO: fix this
        let exp = self.parse_expression(PREFIX).unwrap(); // todo: check this precedence
        let prefix_exp = PrefixExpression::new(tok, exp);
        Expression::Prefix(Box::new(prefix_exp))
    }

    fn parse_infix_expression(&mut self, exp: Expression) -> Expression {
        let operator = self.curr_token.clone();
        let p = self.curr_precedence();
        self.advance_tokens();
        let right = self.parse_expression(p).unwrap(); // TODO: better error handling
        let infix = InfixExpression::new(operator, exp, right);
        Expression::Infix(Box::new(infix))
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParseError> {
        let tok = self.curr_token.clone();
        let args = match self.parse_call_arguments() {
            Ok(a) => a,
            Err(e) => return Err(e),
        };
        let call_exp = CallExpression::new(tok, function, args);
        Ok(Expression::Call(Box::new(call_exp)))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut args = Vec::new();

        // no arguments
        if self.peek_token_is(&Token::Rparen) {
            self.advance_tokens();
            return Ok(args);
        }

        self.advance_tokens();
        match self.parse_expression(LOWEST) {
            Ok(exp) => args.push(exp),
            Err(err) => return Err(err),
        };

        while self.peek_token_is(&Token::Comma) {
            self.advance_tokens();
            self.advance_tokens();
            match self.parse_expression(LOWEST) {
                Ok(exp) => args.push(exp),
                Err(err) => return Err(err),
            };
        }
        match self.expect_peek(&Token::Rparen) {
            Ok(()) => {}
            Err(e) => return Err(e),
        };

        Ok(args)
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match self.peek_token.clone() {
            Token::Ident(name) => {
                self.advance_tokens();
                Ok(name)
            }
            _ => Err(format!(
                "expected an identifier token, received {:?}",
                self.peek_token
            )),
        }
    }

    // TODO: return errors if you have them
    pub fn parse_program(&mut self) -> Node {
        let mut p = Program::new();
        while self.curr_token != Eof {
            let stmt = self.parse_statement();
            match stmt {
                Ok(stmt) => p.statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.advance_tokens();
        }
        Node::Program(Box::new(p))
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement::ExpressionStatement;
    use crate::ast::{Expression, IdentExpression, Node, Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;
    use crate::token::Token::Return;
    use std::ops::Index;

    #[test]
    fn let_statements() {
        struct Test<'a> {
            input: &'a str,
            expected_identifier: Token,
            expected_value: Token,
        }

        let tests = vec![
            Test {
                input: "let x = 5;",
                expected_identifier: Token::Ident("x".to_string()),
                expected_value: Token::Int(5),
            },
            Test {
                input: "let y = true;",
                expected_identifier: Token::Ident("y".to_string()),
                expected_value: Token::True,
            },
            Test {
                input: "let foobar = y;",
                expected_identifier: Token::Ident("foobar".to_string()),
                expected_value: Token::Ident("y".to_string()),
            },
        ];

        for t in tests {
            let program = test_setup(t.input);
            assert_eq!(program.statements.len(), 1);
            let let_stmt = program.statements.index(0);
            match let_stmt {
                Statement::Let(l_s) => {
                    assert_eq!(t.expected_identifier, l_s.name);
                    if l_s.value.is_none() {
                        panic!("expected let statement value not to be none!")
                    }
                    let l_val = l_s.value.as_ref().unwrap();
                    test_expression_token_value(t.expected_value, l_val);
                }
                _ => {
                    panic!("expected a let statement");
                }
            }
        }
    }

    #[test]
    fn invalid_let_statement() {
        let input = "
        let x 5;
        let = 10;
        let 83838383;
        ";
        let p = test_setup(input);
        // assert_eq!(p.errors.len(), 4);
    }

    #[test]
    fn return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let program = test_setup(input);
        assert_eq!(program.statements.len(), 3);
        let mut statements = program.statements.iter();
        match statements.next().unwrap() {
            Statement::Return(ref r) => {
                assert_eq!(Return, r.name)
            }
            _ => {
                panic!("didnt receive a statement expression!")
            }
        }
    }

    #[test]
    fn parse_ident() {
        let input = "foobar;";
        let program = test_setup(input);
        assert_eq!(program.statements.len(), 1);
        // assert_eq!(program.errors.len(), 0);
        let mut statements = program.statements.iter();
        match statements.next().unwrap() {
            Statement::ExpressionStatement(stmt) => match stmt {
                Expression::Ident(ident) => {
                    assert_eq!(Token::Ident("foobar".to_string()), ident.token);
                }
                _ => {
                    panic!("didnt receive a ident expression!")
                }
            },
            _ => {
                panic!("didnt receive a statement expression!")
            }
        }
    }

    #[test]
    fn parse_int_literal() {
        let input = "5;";
        let program = test_setup(input);
        assert_eq!(program.statements.len(), 1);
        // assert_eq!(program.errors.len(), 0);
        let mut statements = program.statements.iter();
        match statements.next().unwrap() {
            Statement::ExpressionStatement(stmt) => match stmt {
                Expression::Integer(int) => {
                    assert_eq!(5, *int);
                }
                _ => {
                    panic!("didnt receive a integer literal expression!")
                }
            },
            _ => {
                panic!("didnt receive a statement expression!")
            }
        }
    }

    #[test]
    fn parse_bool_literal() {
        let input = "true;";
        let program = test_setup(input);
        assert_eq!(program.statements.len(), 1);
        // assert_eq!(program.errors.len(), 0);
        let mut statements = program.statements.iter();
        match statements.next().unwrap() {
            Statement::ExpressionStatement(stmt) => match stmt {
                Expression::Bool(bool_lit) => {
                    assert_eq!(true, *bool_lit);
                }
                _ => {
                    panic!("didnt receive a bool literal expression!")
                }
            },
            _ => {
                panic!("didnt receive a statement expression!")
            }
        }
    }

    #[test]
    fn parse_prefix_expression() {
        struct Test<'a> {
            input: &'a str,
            expected_operator: Token,
            expected_val: Token,
        }

        let tests = vec![
            Test {
                input: "!5;",
                expected_operator: Token::Bang,
                expected_val: Token::Int(5),
            },
            Test {
                input: "-15;",
                expected_operator: Token::Minus,
                expected_val: Token::Int(15),
            },
            Test {
                input: "!true;",
                expected_operator: Token::Bang,
                expected_val: Token::True,
            },
            Test {
                input: "!false;",
                expected_operator: Token::Bang,
                expected_val: Token::False,
            },
        ];

        for t in tests {
            let program = test_setup(t.input);

            assert_eq!(1, program.statements.len());
            // assert_eq!(0, program.errors.len());

            let stmt = program.statements.index(0);
            match stmt {
                Statement::ExpressionStatement(stmt) => match stmt {
                    Expression::Prefix(prefix) => {
                        // check prefix operator
                        assert_eq!(t.expected_operator, prefix.prefix_operator);
                        // check prefix val
                        test_expression_token_value(t.expected_val, &prefix.right);
                    }
                    _ => {
                        panic!("expected a prefix expression!")
                    }
                },
                _ => {
                    panic!("expected expression statement!")
                }
            }
        }
    }

    #[test]
    fn parse_infix_expression() {
        struct Test<'a> {
            input: &'a str,
            expected_left: Token,
            expected_operator: Token,
            expected_right: Token,
        }

        let tests = vec![
            Test {
                input: "5 + 5;",
                expected_left: Token::Int(5),
                expected_operator: Token::Plus,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 - 5;",
                expected_left: Token::Int(5),
                expected_operator: Token::Minus,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 * 5;",
                expected_left: Token::Int(5),
                expected_operator: Token::Asterisk,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 / 5;",
                expected_left: Token::Int(5),
                expected_operator: Token::Slash,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 > 5;",
                expected_left: Token::Int(5),
                expected_operator: Token::GT,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 < 5;",
                expected_left: Token::Int(5),
                expected_operator: Token::LT,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 == 5;",
                expected_left: Token::Int(5),
                expected_operator: Token::EQ,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 != 5;",
                expected_left: Token::Int(5),
                expected_operator: Token::NEQ,
                expected_right: Token::Int(5),
            },
            Test {
                input: "true == true;",
                expected_left: Token::True,
                expected_operator: Token::EQ,
                expected_right: Token::True,
            },
            Test {
                input: "true != false;",
                expected_left: Token::True,
                expected_operator: Token::NEQ,
                expected_right: Token::False,
            },
        ];

        for t in tests {
            let program = test_setup(t.input);
            assert_eq!(1, program.statements.len());
            // assert_eq!(0, parser.errors.len());

            let stmt = program.statements.index(0);
            match stmt {
                Statement::ExpressionStatement(exp) => {
                    test_infix(exp, t.expected_left, t.expected_operator, t.expected_right);
                }
                _ => {
                    panic!("expected expression statement!")
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let tests = vec![
            Test {
                input: "-a * b",
                expected: "((-a) * b)",
            },
            Test {
                input: "!-a",
                expected: "(!(-a))",
            },
            Test {
                input: "a + b + c",
                expected: "((a + b) + c)",
            },
            Test {
                input: "a + b - c",
                expected: "((a + b) - c)",
            },
            Test {
                input: "a * b * c",
                expected: "((a * b) * c)",
            },
            Test {
                input: "a * b / c",
                expected: "((a * b) / c)",
            },
            Test {
                input: "a + b / c",
                expected: "(a + (b / c))",
            },
            Test {
                input: "a + b * c + d / e - f",
                expected: "(((a + (b * c)) + (d / e)) - f)",
            },
            Test {
                input: "3 + 4; -5 * 5",
                expected: "(3 + 4)((-5) * 5)",
            },
            Test {
                input: "5 > 4 == 3 < 4",
                expected: "((5 > 4) == (3 < 4))",
            },
            Test {
                input: "5 < 4 != 3 > 4",
                expected: "((5 < 4) != (3 > 4))",
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            },
            Test {
                input: "true",
                expected: "true",
            },
            Test {
                input: "false",
                expected: "false",
            },
            Test {
                input: "3 > 5 == false",
                expected: "((3 > 5) == false)",
            },
            Test {
                input: "3 < 5 == true",
                expected: "((3 < 5) == true)",
            },
            Test {
                input: "1 + (2 + 3) + 4",
                expected: "((1 + (2 + 3)) + 4)",
            },
            Test {
                input: "(5 + 5) * 2",
                expected: "((5 + 5) * 2)",
            },
            Test {
                input: "2 / (5 + 5)",
                expected: "(2 / (5 + 5))",
            },
            Test {
                input: "-(5 + 5)",
                expected: "(-(5 + 5))",
            },
            Test {
                input: "!(true == true)",
                expected: "(!(true == true))",
            },
        ];

        for t in tests {
            let program = test_setup(t.input);
            assert_eq!(t.expected, program.to_string());
        }
    }

    #[test]
    fn parse_if_expression() {
        let input = "if (x < y) { x }";
        let program = test_setup(input);
        assert_eq!(program.statements.len(), 1);
        // assert_eq!(p.errors.len(), 0);
        let if_expression = program.statements.index(0);
        match if_expression {
            Statement::ExpressionStatement(stmt) => match stmt {
                Expression::IfStatement(if_stmt) => {
                    assert_eq!(Token::If, if_stmt.token);
                    test_infix(
                        &if_stmt.condition,
                        Token::Ident("x".to_string()),
                        Token::LT,
                        Token::Ident("y".to_string()),
                    );
                    assert_eq!(if_stmt.consequence.statements.len(), 1);
                    if if_stmt.alternative.is_some() {
                        panic!("expected if statement to be none");
                    }
                    let consq = if_stmt.consequence.statements.index(0);
                    match consq {
                        Statement::ExpressionStatement(c_stmt) => {
                            test_expression_token_value(Token::Ident("x".to_string()), c_stmt);
                        }
                        _ => {
                            panic!("consequence was unexpected statement type");
                        }
                    }
                }
                _ => {
                    panic!("didnt receive an if statement!")
                }
            },
            _ => {
                panic!("didnt receive a statement expression!")
            }
        }

        // TODO: clean up
        let input = "if (x < y) { x } else { y }";
        let program = test_setup(input);
        assert_eq!(program.statements.len(), 1);
        // assert_eq!(p.errors.len(), 0);
        let if_expression = program.statements.index(0);
        match if_expression {
            Statement::ExpressionStatement(stmt) => match stmt {
                Expression::IfStatement(if_stmt) => {
                    assert_eq!(Token::If, if_stmt.token);
                    test_infix(
                        &if_stmt.condition,
                        Token::Ident("x".to_string()),
                        Token::LT,
                        Token::Ident("y".to_string()),
                    );

                    // check alternative block statement
                    match &if_stmt.alternative {
                        Some(s) => {
                            assert_eq!(s.statements.len(), 1);
                            let alt = s.statements.index(0);
                            match alt {
                                Statement::ExpressionStatement(c_stmt) => {
                                    test_expression_token_value(
                                        Token::Ident("y".to_string()),
                                        c_stmt,
                                    );
                                }
                                _ => {
                                    panic!("consequence was unexpected statement type");
                                }
                            }
                        }
                        None => {
                            panic!("expected alternative to have some")
                        }
                    }

                    // check consequence statement
                    assert_eq!(if_stmt.consequence.statements.len(), 1);
                    let consq = if_stmt.consequence.statements.index(0);
                    match consq {
                        Statement::ExpressionStatement(c_stmt) => {
                            test_expression_token_value(Token::Ident("x".to_string()), c_stmt);
                        }
                        _ => {
                            panic!("consequence was unexpected statement type");
                        }
                    }
                }
                _ => {
                    panic!("didnt receive an if statement!")
                }
            },
            _ => {
                panic!("didnt receive a statement expression!")
            }
        }
    }

    #[test]
    fn parse_function_literal() {
        let input = "fn(x, y) { x + y; }";
        let program = test_setup(input);
        assert_eq!(program.statements.len(), 1);
        // assert_eq!(p.errors.len(), 0);
        let function_statement = program.statements.index(0);
        match function_statement {
            Statement::ExpressionStatement(expression) => match expression {
                Expression::Function(fn_lit) => {
                    assert_eq!(fn_lit.parameters.len(), 2);
                    test_ident(Token::Ident("x".to_string()), fn_lit.parameters.index(0));
                    test_ident(Token::Ident("y".to_string()), fn_lit.parameters.index(1));

                    assert_eq!(fn_lit.body.statements.len(), 1);
                    let body_stmt = fn_lit.body.statements.index(0);
                    match body_stmt {
                        Statement::ExpressionStatement(exp) => {
                            test_infix(
                                &exp,
                                Token::Ident("x".to_string()),
                                Token::Plus,
                                Token::Ident("y".to_string()),
                            );
                        }
                        _ => {
                            panic!("expected body statement to be expression");
                        }
                    }
                }
                _ => {
                    panic!("didnt receive a function literal!")
                }
            },
            _ => {
                panic!("didnt receive a statement expression!")
            }
        }
    }

    #[test]
    fn test_function_parameters() {
        struct Test<'a> {
            input: &'a str,
            expected_params: Vec<&'a str>, // TODO this is probably broken
        }

        let tests = vec![
            Test {
                input: "fn() {};",
                expected_params: vec![],
            },
            Test {
                input: "fn(x) {};",
                expected_params: vec!["x"],
            },
            Test {
                input: "fn(x, y, z) {};",
                expected_params: vec!["x", "y", "z"],
            },
        ];
        for t in tests {
            let program = test_setup(t.input);
            assert_eq!(program.statements.len(), 1);
            let statement = program.statements.index(0);
            match statement {
                ExpressionStatement(exp) => match exp {
                    Expression::Function(func_lit) => {
                        let str_params: Vec<String> = func_lit
                            .parameters
                            .iter()
                            .map(|x| x.token.to_string())
                            .collect();
                        assert_eq!(t.expected_params, str_params)
                    }
                    _ => {
                        panic!("expected function literal expression")
                    }
                },
                _ => {
                    panic!("expected expression statemnet")
                }
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let program = test_setup(input);
        assert_eq!(program.statements.len(), 1);
        let statement = program.statements.index(0);
        match statement {
            ExpressionStatement(exp) => match exp {
                Expression::Call(call) => {
                    test_expression_token_value(Token::Ident("add".to_string()), &call.function);
                }
                _ => {
                    panic!("expected call expression")
                }
            },
            _ => {
                panic!("expected expression statement")
            }
        }

        match statement {
            ExpressionStatement(exp) => match exp {
                Expression::Call(call) => {
                    assert_eq!(call.arguments.len(), 3);
                    test_expression_token_value(Token::Int(1), call.arguments.index(0));
                    test_infix(
                        call.arguments.index(1),
                        Token::Int(2),
                        Token::Asterisk,
                        Token::Int(3),
                    );
                    test_infix(
                        call.arguments.index(2),
                        Token::Int(4),
                        Token::Plus,
                        Token::Int(5),
                    )
                }
                _ => {
                    panic!("expected call expression")
                }
            },
            _ => {
                panic!("expected expression statement")
            }
        }
    }

    // Helpers ------------------------------------------------------------------------------
    // TODO: maybe move these to separate place

    fn test_infix(exp: &Expression, left: Token, op: Token, right: Token) {
        match exp {
            Expression::Infix(infix) => {
                // check operator
                assert_eq!(op, infix.operator);
                // check right side
                test_expression_token_value(right, &infix.right);
                test_expression_token_value(left, &infix.left);
            }
            _ => {
                panic!("expected an infix expression!")
            }
        }
    }

    fn test_expression_token_value(expected_token: Token, expression: &Expression) {
        match expression {
            Expression::Integer(int_lit) => {
                assert_eq!(expected_token, Token::Int(*int_lit));
            }
            Expression::Bool(bool_lit) => match bool_lit {
                true => {
                    assert_eq!(expected_token, Token::True)
                }
                false => {
                    assert_eq!(expected_token, Token::False)
                }
            },
            Expression::Ident(id) => {
                assert_eq!(expected_token, id.token)
            }
            _ => {
                panic!("unsupported expression {}", expression)
            }
        }
    }

    fn test_ident(expected_token: Token, id_exp: &IdentExpression) {
        assert_eq!(expected_token, id_exp.token)
    }

    fn test_setup(input: &str) -> Box<Program> {
        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let program_node = parser.parse_program();

        let program = match program_node {
            Node::Program(pgrm) => pgrm,
            _ => panic!("expected program node"),
        };
        program
    }
}
