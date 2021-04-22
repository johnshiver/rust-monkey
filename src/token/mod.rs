use crate::token::TokenType::{
    Assign, Comma, Eof, Function, Ident, Illegal, Int, Lbrace, Let, Lparen, Plus, Rbrace, Rparen,
    Semicolon,
};

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub literal: String,
    pub kind: TokenType,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,
    Ident,
    Int,
    Assign,
    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Function,
    Let,
}

pub fn input_is_single_char_token(input: &str) -> bool {
    match input {
        ASSIGN | SEMICOLON | LPAREN | RPAREN | COMMA | PLUS | LBRACE | RBRACE => true,
        _ => false,
    }
}

pub fn token_type_from_str(input: &str) -> TokenType {
    match input {
        EOF => Eof,
        IDENT => Ident,
        INT => Int,
        ASSIGN => Assign,
        PLUS => Plus,
        COMMA => Comma,
        SEMICOLON => Semicolon,
        LPAREN => Lparen,
        RPAREN => Rparen,
        LBRACE => Lbrace,
        RBRACE => Rbrace,
        FUNCTION => Function,
        LET => Let,
        _ => Illegal,
    }
}

// Constants --------------------------------------------

const ILLEGAL: &str = "ILLEGAL";
const EOF: &str = "EOF";

// Identifiers and literals
const IDENT: &str = "IDENT"; // add, foobar, x, y, ...
const INT: &str = "INT"; // 134356

// Operators
const ASSIGN: &str = "=";
const PLUS: &str = "+";

// Delimiters
const COMMA: &str = ",";
const SEMICOLON: &str = ";";
const LPAREN: &str = "(";
const RPAREN: &str = ")";
const LBRACE: &str = "{";
const RBRACE: &str = "}";

// Keywords
const FUNCTION: &str = "FUNCTION";
const LET: &str = "LET";
