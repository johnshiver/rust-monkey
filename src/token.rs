#[derive(Debug, Eq, PartialEq)]
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
}

//
// // Constants --------------------------------------------
//
// const ILLEGAL: &str = "ILLEGAL";
// const EOF: &str = "EOF";
//
// // Identifiers and literals
// const IDENT: &str = "IDENT"; // add, foobar, x, y, ...
// const INT: &str = "INT"; // 134356
//
// // Operators
// const ASSIGN: &str = "=";
// const PLUS: &str = "+";
//
// // Delimiters
// const COMMA: &str = ",";
// const SEMICOLON: &str = ";";
// const LPAREN: &str = "(";
// const RPAREN: &str = ")";
// const LBRACE: &str = "{";
// const RBRACE: &str = "}";
//
// // Keywords
// const FUNCTION: &str = "FUNCTION";
// const LET: &str = "LET";
