use crate::token::Token;
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
        self.eat_whitespace();
        match self.read_char() {
            Some('+') => Token::Plus,
            Some('(') => Token::Lparen,
            Some(')') => Token::Rparen,
            Some('{') => Token::Lbrace,
            Some('}') => Token::Rbrace,
            Some(',') => Token::Comma,
            Some(';') => Token::Semicolon,
            Some('=') => Token::Assign,
            None => Token::Eof,
            _ => Token::Illegal,
        }
    }

    fn eat_whitespace(&mut self) {
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

    //     #[test]
    //     fn next_token_multi_line() {
    //         let input = "let five = 5;
    // let ten = 10;
    //
    // let add = fn(x, y) {
    //   x + y;
    // };
    //
    // let result = add(five, ten);
    // ";
    //     let expected_tokens = vec![
    //         Token::Let,
    //         Token {
    //             literal: "five".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: "=".to_string(),
    //             kind: TokenType::Assign,
    //         },
    //         Token {
    //             literal: ";".to_string(),
    //             kind: TokenType::Semicolon,
    //         },
    //         Token {
    //             literal: "let".to_string(),
    //             kind: TokenType::Let,
    //         },
    //         Token {
    //             literal: "ten".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: "=".to_string(),
    //             kind: TokenType::Assign,
    //         },
    //         Token {
    //             literal: "10".to_string(),
    //             kind: TokenType::Int,
    //         },
    //         Token {
    //             literal: ";".to_string(),
    //             kind: TokenType::Semicolon,
    //         },
    //         Token {
    //             literal: "let".to_string(),
    //             kind: TokenType::Let,
    //         },
    //         Token {
    //             literal: "add".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: "=".to_string(),
    //             kind: TokenType::Assign,
    //         },
    //         Token {
    //             literal: "fn".to_string(),
    //             kind: TokenType::Function,
    //         },
    //         Token {
    //             literal: "(".to_string(),
    //             kind: TokenType::Lparen,
    //         },
    //         Token {
    //             literal: "x".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: ",".to_string(),
    //             kind: TokenType::Comma,
    //         },
    //         Token {
    //             literal: "y".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: ")".to_string(),
    //             kind: TokenType::Rparen,
    //         },
    //         Token {
    //             literal: "{".to_string(),
    //             kind: TokenType::Rbrace,
    //         },
    //         Token {
    //             literal: "x".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: "+".to_string(),
    //             kind: TokenType::Plus,
    //         },
    //         Token {
    //             literal: "y".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: ";".to_string(),
    //             kind: TokenType::Semicolon,
    //         },
    //         Token {
    //             literal: "}".to_string(),
    //             kind: TokenType::Rbrace,
    //         },
    //         Token {
    //             literal: ";".to_string(),
    //             kind: TokenType::Semicolon,
    //         },
    //         Token {
    //             literal: "let".to_string(),
    //             kind: TokenType::Let,
    //         },
    //         Token {
    //             literal: "result".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: "=".to_string(),
    //             kind: TokenType::Assign,
    //         },
    //         Token {
    //             literal: "add".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: "(".to_string(),
    //             kind: TokenType::Lparen,
    //         },
    //         Token {
    //             literal: "five".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: ",".to_string(),
    //             kind: TokenType::Comma,
    //         },
    //         Token {
    //             literal: "ten".to_string(),
    //             kind: TokenType::Ident,
    //         },
    //         Token {
    //             literal: ")".to_string(),
    //             kind: TokenType::Rparen,
    //         },
    //         Token {
    //             literal: ";".to_string(),
    //             kind: TokenType::Semicolon,
    //         },
    //         Token {
    //             literal: "\0".to_string(),
    //             kind: TokenType::Eof,
    //         },
    //     ];
    //     let lexer_input = input.to_string();
    //     let mut lexer = Lexer::new(lexer_input);
    //     for token in expected_tokens {
    //         let next_token = lexer.next_token();
    //         assert_eq!(token, next_token);
    //     }
    // }
}
