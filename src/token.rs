#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Token {
    Illegal,
    Eof,

    // identifiers + literals
    Ident(String),
    Int(i64),
    String(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,

    // Two char operators
    EQ,
    NEQ,

    // Delimiters
    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub fn lookup_ident(ident: String) -> Token {
    match ident.as_str() {
        "let" => Token::Let,
        "fn" => Token::Function,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Ident(ident),
    }
}
