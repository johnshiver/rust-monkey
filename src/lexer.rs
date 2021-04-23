use crate::token::Token::Int;
use crate::token::{lookup_ident, Token};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        match self.read_char() {
            Some('+') => Token::Plus,
            Some('(') => Token::Lparen,
            Some(')') => Token::Rparen,
            Some('{') => Token::Lbrace,
            Some('}') => Token::Rbrace,
            Some(',') => Token::Comma,
            Some(';') => Token::Semicolon,
            Some('=') => Token::Assign,
            Some('-') => Token::Minus,
            Some('!') => Token::Bang,
            Some('*') => Token::Asterisk,
            Some('/') => Token::Slash,
            Some('<') => Token::LT,
            Some('>') => Token::GT,
            Some(ch) => {
                if ch.is_alphabetic() {
                    let ident = self.read_identifier(ch);
                    lookup_ident(ident)
                } else if ch.is_numeric() {
                    Token::Int(self.read_int(ch))
                } else {
                    Token::Illegal
                }
            }
            None => Token::Eof,
            _ => Token::Illegal,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.input.peek() {
            if ch.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn read_identifier(&mut self, ch: char) -> String {
        let mut ident = String::new();
        ident.push(ch);
        while let Some(&ch) = self.input.peek() {
            if ch.is_alphabetic() {
                ident.push(self.read_char().expect("scanned beyond end of input"));
            } else {
                break;
            }
        }
        ident
    }
    fn read_int(&mut self, ch: char) -> i64 {
        let mut ident = String::new();
        ident.push(ch);
        while let Some(&ch) = self.input.peek() {
            if ch.is_numeric() {
                ident.push(self.read_char().expect("scanned beyond end of input"));
            } else {
                break;
            }
        }
        ident
            .parse::<i64>()
            .expect("failed to parse int from input")
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token_single_val_tokens() {
        let input = "=+(){},;";
        let expected_tokens = vec![
            Token::Assign,
            Token::Plus,
            Token::Lparen,
            Token::Rparen,
            Token::Lbrace,
            Token::Rbrace,
            Token::Comma,
            Token::Semicolon,
        ];
        let mut lexer = Lexer::new(input);
        for token in expected_tokens {
            let next_token = lexer.next_token();
            assert_eq!(token, next_token);
        }
    }

    #[test]
    fn next_token_multi_line() {
        let input = "let five = 5;
    let ten = 10;
    
    let add = fn(x, y) {
      x + y;
    };
    
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else { 
        return false;
   }
    ";
        let expected_tokens = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::Lparen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LT,
            Token::Int(10),
            Token::GT,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int(5),
            Token::LT,
            Token::Int(10),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Eof,
        ];
        let mut lexer = Lexer::new(input);
        for token in expected_tokens {
            let next_token = lexer.next_token();
            assert_eq!(token, next_token);
        }
    }
}
