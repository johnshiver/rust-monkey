// TODO: create token type enum

pub struct Token {
    pub literal: str,
}

// Constants --------------------------------------------

const ILLEGAL: &str = "ILLEGAL";
const EOF: &str = "EOF";

// Identifiers and literals
const IDENT: &str = "IDENT"; // add, foobar, x, y, ...
const INT: &str = "INT"; // 134356

// Operators
const ASSIGN: &str = "ASSIGN";
const PLUS: &str = "PLUS";

// Delimiters
const COMMA: &str = ",";
const SEMICOLON: &str = ";";
