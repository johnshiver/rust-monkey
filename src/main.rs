use crate::repl::start;
use std::io::{stdin, stdout};

mod ast;
mod eval;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    let input = stdin();
    let output = stdout();
    start(&mut input.lock(), &mut output.lock());
}
