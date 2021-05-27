use crate::ast::Statement::{ExpressionStatement, Let, Return};
use crate::ast::{
    BooleanLiteralExpression, Expression, IdentExpression, InfixExpression,
    IntegerLiteralExpression, LetStatement, PrefixExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::Token::{Assign, Eof};
use std::collections::HashMap;

use lazy_static::lazy_static;

// TODO: might be nice to add metadata to parse error, like line number
type ParseError = String;

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
    ]
    .iter()
    .cloned()
    .collect();
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
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
            Token::Let => {
                let stmt = self.parse_let();
                Ok(stmt)
            }
            Token::Return => {
                let stmt = self.parse_return();
                Ok(stmt)
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let(&mut self) -> Statement {
        // next token should be an identifier
        let mut name = "".to_string();
        match self.expect_ident() {
            Ok(n) => name = n,
            Err(e) => self.errors.push(e),
        };

        let stmt = LetStatement::new(name, None);

        if self.peek_token != Assign {
            self.errors.push(format!(
                "expected assign token, received {:?}",
                self.peek_token
            ));
        }

        // eventually will have to parse the Expression
        while self.curr_token != Token::Semicolon {
            self.advance_tokens();
        }
        Let(Box::new(stmt))
    }

    fn parse_return(&mut self) -> Statement {
        // current token is return
        let stmt = ReturnStatement::new(None);

        self.advance_tokens();
        // eventually will have to parse the Expression
        while self.curr_token != Token::Semicolon {
            self.advance_tokens();
        }
        Return(Box::new(stmt))
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
        let mut left: Option<Expression> = None;
        // check prefix
        match self.curr_token.clone() {
            Token::Ident(literal) => left = Some(self.parse_ident(literal)),
            Token::Int(literal) => left = Some(self.parse_int_literal(literal)),
            Token::True | Token::False => {
                left = Some(self.parse_bool_literal(self.curr_token.clone()))
            }
            Token::Bang | Token::Minus => left = Some(self.parse_prefix_expression()),
            _ => left = None,
        }

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
                _ => break,
            }
        }
        Ok(l)
    }

    fn parse_ident(&mut self, literal: String) -> Expression {
        let ie = IdentExpression::new(literal);
        return Expression::Ident(Box::new(ie));
    }

    fn parse_int_literal(&mut self, literal: i64) -> Expression {
        let ie = IntegerLiteralExpression::new(literal);
        return Expression::IntegerLiteral(Box::new(ie));
    }

    fn parse_bool_literal(&mut self, val: Token) -> Expression {
        let ie = BooleanLiteralExpression::new(val);
        return Expression::BoolLiteral(Box::new(ie));
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let tok = self.curr_token.clone();
        self.advance_tokens();
        // TODO: fix this
        let exp = self.parse_expression(PREFIX).unwrap(); // todo: check this precedence
        let prefix_exp = PrefixExpression::new(tok, exp);
        return Expression::Prefix(Box::new(prefix_exp));
    }

    fn parse_infix_expression(&mut self, exp: Expression) -> Expression {
        let operator = self.curr_token.clone();
        let p = self.curr_precedence();
        self.advance_tokens();
        let right = self.parse_expression(p).unwrap(); // TODO: better error handling
        let infix = InfixExpression::new(operator, exp, right);
        Expression::Infix(Box::new(infix))
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

    pub fn parse_program(&mut self) -> Program {
        let mut p = Program::new();
        while self.curr_token != Eof {
            let stmt = self.parse_statement();
            match stmt {
                Ok(stmt) => p.statements.push(stmt),
                Err(err) => self.errors.push(err),
            }
            self.advance_tokens();
        }
        p
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;
    use crate::token::Token::Return;

    #[test]
    fn let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.statements.len(), 3);
        let expected_identifiers = vec![
            Token::Ident("x".to_string()),
            Token::Ident("y".to_string()),
            Token::Ident("foobar".to_string()),
        ];
        let mut statements = program.statements.iter();
        for id in expected_identifiers {
            match statements.next().unwrap() {
                Statement::Let(ref l) => {
                    assert_eq!(l.name, id)
                }
                _ => panic!("expected let statement"),
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
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program();
        assert_eq!(p.errors.len(), 4);
    }

    #[test]
    fn return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
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
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(p.errors.len(), 0);
        let mut statements = program.statements.iter();
        match statements.next().unwrap() {
            Statement::ExpressionStatement(stmt) => match stmt {
                Expression::Ident(ident) => {
                    assert_eq!(Token::Ident("foobar".to_string()), ident.value);
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
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(p.errors.len(), 0);
        let mut statements = program.statements.iter();
        match statements.next().unwrap() {
            Statement::ExpressionStatement(stmt) => match stmt {
                Expression::IntegerLiteral(ident) => {
                    assert_eq!(Token::Int(5), ident.value);
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
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        assert_eq!(program.statements.len(), 1);
        assert_eq!(p.errors.len(), 0);
        let mut statements = program.statements.iter();
        match statements.next().unwrap() {
            Statement::ExpressionStatement(stmt) => match stmt {
                Expression::BoolLiteral(ident) => {
                    assert_eq!(Token::True, ident.value);
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
        struct Test {
            input: String,
            expected_operator: Token,
            expected_integer: Token,
        }

        let tests = vec![
            Test {
                input: "!5;".to_string(),
                expected_operator: Token::Bang,
                expected_integer: Token::Int(5),
            },
            Test {
                input: "-15;".to_string(),
                expected_operator: Token::Minus,
                expected_integer: Token::Int(15),
            },
        ];

        for t in tests {
            let l = Lexer::new(t.input.as_str());
            let mut parser = Parser::new(l);
            let program = parser.parse_program();

            assert_eq!(1, program.statements.len());
            assert_eq!(0, parser.errors.len());

            let mut statements = program.statements.iter();
            match statements.next().unwrap() {
                Statement::ExpressionStatement(stmt) => match stmt {
                    Expression::Prefix(prefix) => {
                        assert_eq!(t.expected_operator, prefix.prefix_operator);
                        match &prefix.right {
                            Expression::IntegerLiteral(int_lit) => {
                                assert_eq!(t.expected_integer, int_lit.value);
                            }
                            _ => {
                                panic!("expected int literal!")
                            }
                        }
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
        struct Test {
            input: String,
            expected_left: Token,
            expected_operator: Token,
            expected_right: Token,
        }

        let tests = vec![
            Test {
                input: "5 + 5;".to_string(),
                expected_left: Token::Int(5),
                expected_operator: Token::Plus,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 - 5;".to_string(),
                expected_left: Token::Int(5),
                expected_operator: Token::Minus,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 * 5;".to_string(),
                expected_left: Token::Int(5),
                expected_operator: Token::Asterisk,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 / 5;".to_string(),
                expected_left: Token::Int(5),
                expected_operator: Token::Slash,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 > 5;".to_string(),
                expected_left: Token::Int(5),
                expected_operator: Token::GT,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 < 5;".to_string(),
                expected_left: Token::Int(5),
                expected_operator: Token::LT,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 == 5;".to_string(),
                expected_left: Token::Int(5),
                expected_operator: Token::EQ,
                expected_right: Token::Int(5),
            },
            Test {
                input: "5 != 5;".to_string(),
                expected_left: Token::Int(5),
                expected_operator: Token::NEQ,
                expected_right: Token::Int(5),
            },
        ];

        for t in tests {
            let l = Lexer::new(t.input.as_str());
            let mut parser = Parser::new(l);
            let program = parser.parse_program();

            assert_eq!(1, program.statements.len());
            assert_eq!(0, parser.errors.len());

            let mut statements = program.statements.iter();
            match statements.next().unwrap() {
                Statement::ExpressionStatement(stmt) => match stmt {
                    Expression::Infix(infix) => {
                        assert_eq!(t.expected_operator, infix.operator);
                        match &infix.right {
                            Expression::IntegerLiteral(int_lit) => {
                                assert_eq!(t.expected_right, int_lit.value);
                            }
                            _ => {
                                panic!("expected int literal!")
                            }
                        }
                        match &infix.left {
                            Expression::IntegerLiteral(int_lit) => {
                                assert_eq!(t.expected_left, int_lit.value);
                            }
                            _ => {
                                panic!("expected int literal!")
                            }
                        }
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
    fn test_program_display() {
        struct Test {
            input: String,
            expected: String,
        }

        let tests = vec![
            Test {
                input: "-a * b".to_string(),
                expected: "((-a) * b)".to_string(),
            },
            Test {
                input: "!-a".to_string(),
                expected: "(!(-a))".to_string(),
            },
            Test {
                input: "a + b + c".to_string(),
                expected: "((a + b) + c)".to_string(),
            },
            Test {
                input: "a + b - c".to_string(),
                expected: "((a + b) - c)".to_string(),
            },
            Test {
                input: "a * b * c".to_string(),
                expected: "((a * b) * c)".to_string(),
            },
            Test {
                input: "a * b / c".to_string(),
                expected: "((a * b) / c)".to_string(),
            },
            Test {
                input: "a + b / c".to_string(),
                expected: "(a + (b / c))".to_string(),
            },
            Test {
                input: "a + b * c + d / e - f".to_string(),
                expected: "(((a + (b * c)) + (d / e)) - f)".to_string(),
            },
            Test {
                input: "3 + 4; -5 * 5".to_string(),
                expected: "(3 + 4)((-5) * 5)".to_string(),
            },
            Test {
                input: "5 > 4 == 3 < 4".to_string(),
                expected: "((5 > 4) == (3 < 4))".to_string(),
            },
            Test {
                input: "5 < 4 != 3 > 4".to_string(),
                expected: "((5 < 4) != (3 > 4))".to_string(),
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            },
        ];

        for t in tests {
            let l = Lexer::new(t.input.as_str());
            let mut parser = Parser::new(l);
            let program = parser.parse_program();
            assert_eq!(t.expected, program.to_string());
        }
    }
}

// TODO: create some test helpers
