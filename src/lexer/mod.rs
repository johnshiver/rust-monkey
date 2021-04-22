use crate::token::TokenType::{Eof, Ident, Illegal};
use crate::token::{input_is_single_char_token, token_type_from_str, Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    read_position: usize,
    position: usize,
    curr_char: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            input,
            read_position: 0,
            position: 0,
            curr_char: '\0',
        }
    }

    fn read_char(&mut self) {
        if self.end_of_input() {}
    }
    pub fn next_token(&mut self) -> Token {
        if self.end_of_input() {
            return Token {
                literal: "\0".to_string(),
                kind: TokenType::Eof,
            };
        }

        let curr = self
            .input
            .chars()
            .nth(self.read_position)
            .expect("read position read past input!");

        let literal;
        let kind;

        if input_is_single_char_token(curr.to_string().as_str()) {
            kind = token_type_from_str(curr.to_string().as_str());
            literal = curr.to_string();
            self.read_position += 1;
        } else if curr.is_alphabetic() {
            kind = Ident;
            literal = self.read_identifier();
        } else {
            kind = Illegal;
            literal = curr.to_string();
        }

        Token { literal, kind }
    }

    pub fn parse_input(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while !self.end_of_input() {
            let new_token = self.next_token();
            tokens.push(new_token);
        }
        // capture EOF
        tokens.push(self.next_token());
        tokens
    }

    fn end_of_input(&self) -> bool {
        self.read_position >= self.input.len()
    }

    fn read_identifier(&mut self) -> String {
        let starting_pos = self.read_position;
        String::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;
    use crate::token::TokenType;

    #[test]
    fn next_token_single_val_tokens() {
        let input = "=+(){},;";
        let expected_tokens = vec![
            Token {
                literal: "=".to_string(),
                kind: TokenType::Assign,
            },
            Token {
                literal: "+".to_string(),
                kind: TokenType::Plus,
            },
            Token {
                literal: "(".to_string(),
                kind: TokenType::Lparen,
            },
            Token {
                literal: ")".to_string(),
                kind: TokenType::Rparen,
            },
            Token {
                literal: "{".to_string(),
                kind: TokenType::Lbrace,
            },
            Token {
                literal: "}".to_string(),
                kind: TokenType::Rbrace,
            },
            Token {
                literal: ",".to_string(),
                kind: TokenType::Comma,
            },
            Token {
                literal: ";".to_string(),
                kind: TokenType::Semicolon,
            },
            Token {
                literal: "\0".to_string(),
                kind: TokenType::Eof,
            },
        ];
        let lexer_input = input.to_string();
        let mut lexer = Lexer::new(lexer_input);
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
";
        let expected_tokens = vec![
            Token {
                literal: "let".to_string(),
                kind: TokenType::Let,
            },
            Token {
                literal: "five".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: "=".to_string(),
                kind: TokenType::Assign,
            },
            Token {
                literal: ";".to_string(),
                kind: TokenType::Semicolon,
            },
            Token {
                literal: "let".to_string(),
                kind: TokenType::Let,
            },
            Token {
                literal: "ten".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: "=".to_string(),
                kind: TokenType::Assign,
            },
            Token {
                literal: "10".to_string(),
                kind: TokenType::Int,
            },
            Token {
                literal: ";".to_string(),
                kind: TokenType::Semicolon,
            },
            Token {
                literal: "let".to_string(),
                kind: TokenType::Let,
            },
            Token {
                literal: "add".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: "=".to_string(),
                kind: TokenType::Assign,
            },
            Token {
                literal: "fn".to_string(),
                kind: TokenType::Function,
            },
            Token {
                literal: "(".to_string(),
                kind: TokenType::Lparen,
            },
            Token {
                literal: "x".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: ",".to_string(),
                kind: TokenType::Comma,
            },
            Token {
                literal: "y".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: ")".to_string(),
                kind: TokenType::Rparen,
            },
            Token {
                literal: "{".to_string(),
                kind: TokenType::Rbrace,
            },
            Token {
                literal: "x".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: "+".to_string(),
                kind: TokenType::Plus,
            },
            Token {
                literal: "y".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: ";".to_string(),
                kind: TokenType::Semicolon,
            },
            Token {
                literal: "}".to_string(),
                kind: TokenType::Rbrace,
            },
            Token {
                literal: ";".to_string(),
                kind: TokenType::Semicolon,
            },
            Token {
                literal: "let".to_string(),
                kind: TokenType::Let,
            },
            Token {
                literal: "result".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: "=".to_string(),
                kind: TokenType::Assign,
            },
            Token {
                literal: "add".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: "(".to_string(),
                kind: TokenType::Lparen,
            },
            Token {
                literal: "five".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: ",".to_string(),
                kind: TokenType::Comma,
            },
            Token {
                literal: "ten".to_string(),
                kind: TokenType::Ident,
            },
            Token {
                literal: ")".to_string(),
                kind: TokenType::Rparen,
            },
            Token {
                literal: ";".to_string(),
                kind: TokenType::Semicolon,
            },
            Token {
                literal: "\0".to_string(),
                kind: TokenType::Eof,
            },
        ];
        let lexer_input = input.to_string();
        let mut lexer = Lexer::new(lexer_input);
        for token in expected_tokens {
            let next_token = lexer.next_token();
            assert_eq!(token, next_token);
        }
    }
}
