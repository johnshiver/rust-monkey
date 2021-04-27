use crate::ast::Statement::Let;
use crate::ast::{Expression, LetStatement, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::Token::Eof;
use std::borrow::BorrowMut;
use std::error::Error;

type ParseError = String;

struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut l: Lexer<'a>) -> Self {
        let curr_token = l.next_token();
        let peek_token = l.next_token();
        Parser {
            lexer: l,
            curr_token,
            peek_token,
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.curr_token {
            Token::Let => match self.parse_let() {
                Ok(stmt) => Ok(stmt),
                Err(err) => Err(err),
            },
            _ => Err(format!("not supported")),
        }
    }

    fn parse_let(&mut self) -> Result<Statement, ParseError> {
        // next token should be an identifier
        let name;
        match self.expect_ident() {
            Ok(n) => name = n,
            Err(e) => return Err(e),
        };

        self.next_token();

        let mut stmt = LetStatement {
            name: Token::Ident(name),
            value: None,
        };

        // eventually will have to parse the Expression
        while self.curr_token != Token::Semicolon {
            self.next_token();
        }
        Ok(Let(Box::new(stmt)))
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match self.peek_token.clone() {
            Token::Ident(name) => Ok(name),
            _ => Err(format!("expected an identifier token")),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut p = Program {
            statements: Vec::new(),
        };
        while self.curr_token != Eof {
            let stmt = self.parse_statement();
            match stmt {
                Ok(stmt) => p.statements.push(stmt),
                Err(err) => continue,
            }
            self.next_token();
        }
        p
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;

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
            }
        }
    }
}
