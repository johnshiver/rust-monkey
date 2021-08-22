use crate::ast::Node;
use crate::eval::eval;
use crate::lexer::Lexer;
use crate::object::Environment;
use crate::parser::Parser;
use crate::token::Token::Eof;
use std::cell::RefCell;
use std::io::{BufRead, Write};
use std::rc::Rc;

pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    let env = Rc::new(RefCell::new(Environment::new()));
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
        // TODO: do i need to clear env memory?
        match eval(program, Rc::clone(&env)) {
            Ok(obj) => println!("{}", obj),
            Err(e) => println!("evaluation error: {}", e.message),
        }
    }
}

fn print_parser_errors() {}
