use crate::token::TokenType::Eof;
use crate::token::{token_type_from_str, Token};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    read_position: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            input,
            read_position: 0,
        }
    }
    pub fn next_token(&mut self) -> Token {
        let token_type;
        let curr;
        if self.end_of_input() {
            curr = '\0';
            token_type = Eof;
        } else {
            curr = self
                .input
                .chars()
                .nth(self.read_position)
                .expect("read position read past input!");
            token_type = token_type_from_str(curr.to_string().as_str());
            self.advance_read_position();
        }
        Token {
            literal: curr.to_string(),
            kind: token_type,
        }
    }

    fn advance_read_position(&mut self) {
        if self.end_of_input() {
            return;
        }
        self.read_position += 1;
    }

    fn end_of_input(&self) -> bool {
        self.read_position >= self.input.len()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token;
    use crate::token::TokenType;

    #[test]
    fn next_token_single_vals() {
        let input = "=+(){},;";
        let expected_tokens = vec![
            token::Token {
                literal: "=".to_string(),
                kind: TokenType::Assign,
            },
            token::Token {
                literal: "+".to_string(),
                kind: TokenType::Plus,
            },
            token::Token {
                literal: "(".to_string(),
                kind: TokenType::Lparen,
            },
            token::Token {
                literal: ")".to_string(),
                kind: TokenType::Rparen,
            },
            token::Token {
                literal: "{".to_string(),
                kind: TokenType::Lbrace,
            },
            token::Token {
                literal: "}".to_string(),
                kind: TokenType::Rbrace,
            },
            token::Token {
                literal: ",".to_string(),
                kind: TokenType::Comma,
            },
            token::Token {
                literal: ";".to_string(),
                kind: TokenType::Semicolon,
            },
            token::Token {
                literal: "\0".to_string(),
                kind: TokenType::Eof,
            },
        ];
        let lexer_input = input.to_string();
        let mut lexer = Lexer {
            input: lexer_input,
            read_position: 0,
        };
        for t in expected_tokens {
            let next_token = lexer.next_token();
            assert_eq!(t, next_token);
        }
    }

    #[test]
    fn next_token_multi_line() {
        let input = "\
        ";
    }
}
