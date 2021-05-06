// AST consists solely of nodes, some statement some expression

use crate::token::Token;

pub enum Statement {
    Let(Box<LetStatement>),
    Return(Box<ReturnStatement>),
    Expression(Box<Expression>)
}

pub enum Expression {
    Ident(Box<IdentExpression>)

}

// root node
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

pub struct LetStatement {
    pub name: Token,
    pub value: Option<Expression>,
}

impl LetStatement {
    pub fn new(name: Token, value: Expression) -> Self {
        LetStatement {
            name,
            value: Some(value),
        }
    }
}

pub struct ReturnStatement {
    pub name: Token, // return token
    pub value: Option<Expression>,
}

impl ReturnStatement {
    pub fn new(name: Token, value: Expression) -> Self {
        ReturnStatement {
            name,
            value: Some(value),
        }
    }
}

pub struct IdentExpression {
    pub value: Token, // return token
}

impl IdentExpression {
    pub fn new(value: Token) -> Self {
        IdentExpression{
            value,
        }
    }
}
