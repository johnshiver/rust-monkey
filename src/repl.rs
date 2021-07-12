use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::Token::Eof;
use std::io::{BufRead, Write};

pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    loop {
        writer.write(b">> ");
        writer.flush();
        let mut line = String::new();
        reader.read_line(&mut line);
        // TODO: iterate over reader with .lines()
        let lexer = Lexer::new(line.as_str());
        let mut p = Parser::new(lexer);
        let program = p.parse_program();
        if p.errors.len() > 0 {
            for e in p.errors {
                println!("parse error: {}", e);
            }
            continue;
        }
        println!("{}", program);
    }
}

fn print_parser_errors() {}
