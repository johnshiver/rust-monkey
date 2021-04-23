use crate::repl::start;
use std::io::{stdin, stdout};

mod lexer;
mod repl;
mod token;

fn main() {
    let input = stdin();
    let output = stdout();
    start(&mut input.lock(), &mut output.lock());
}
