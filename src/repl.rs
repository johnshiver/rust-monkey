use crate::lexer::Lexer;
use crate::token::Token::Eof;
use std::io::{BufRead, Read, Write};

const PROMPT: &str = ">> ";
const BUF_SIZE: usize = 8 * 1024;

// Params should be Reader, Writer
pub fn start<R: BufRead, W: Write>(reader: &mut R, writer: &mut W) {
    // read from reader
    // create lexer from the content
    // spit out tokens

    loop {
        writer.write(b">> ");
        writer.flush();
        let mut line = String::new();
        reader.read_line(&mut line);
        let mut lexer = Lexer::new(line.as_str());
        let mut keep_going = true;
        while keep_going {
            let next_token = lexer.next_token();
            if next_token == Eof {
                keep_going = false;
                break;
            }
            println!("{:?}", next_token);
        }
    }
}
