// AST consists solely of nodes, some statement some expression

use crate::token::Token;

pub enum Statement {
    Let,
}

pub enum Expression {}

// root node
pub struct Program {
    pub(crate) statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

pub struct Let {
    pub name: Token, // perhaps this should be new type, Identifier
    pub value: Expression,
}
