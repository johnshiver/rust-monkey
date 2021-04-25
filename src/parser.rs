use crate::ast::Program;
use crate::lexer::Lexer;
use crate::token::Token;

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

    pub fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        Program { statements: vec![] }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;

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
    }
}
