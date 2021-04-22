use crate::token::{token_type_from_str, Token};

pub struct Lexer<'a> {
    input: &'a str,
    position: i32,
    read_position: i32,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input,
            position: 0,
            read_position: 0,
        }
    }
    // TODO: need to figure out if we're at the end
    pub fn next_token(&mut self) -> Token {
        let curr = self.input[self.read_position];
        let token_type = token_type_from_str(curr);
        self.advance_read_position();
        Token {
            literal: curr,
            kind: token_type,
        }
    }

    fn advance_read_position(&mut self) {
        if self.read_position >= self.input.len() as i32 {
            return;
        }
        self.read_position += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token;
    use crate::token::TokenType;

    #[test]
    fn next_token() {
        let input = "=_(){},;";
        let expected_tokens = vec![
            token::Token {
                literal: "=",
                kind: TokenType::Assign,
            },
            token::Token {
                literal: "+",
                kind: TokenType::Plus,
            },
            token::Token {
                literal: "(",
                kind: TokenType::Lparen,
            },
            token::Token {
                literal: ")",
                kind: TokenType::Rparen,
            },
            token::Token {
                literal: "{",
                kind: TokenType::Rbrace,
            },
            token::Token {
                literal: "}",
                kind: TokenType::Lbrace,
            },
            token::Token {
                literal: ",",
                kind: TokenType::Comma,
            },
            token::Token {
                literal: ";",
                kind: TokenType::Semicolon,
            },
            token::Token {
                literal: "",
                kind: TokenType::Eof,
            },
        ];
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
        };
        for t in expected_tokens {
            let mut next_token = lexer.next_token();
            assert_eq!(t, next_token);
        }
    }
}
