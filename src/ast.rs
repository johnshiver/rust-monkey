// AST consists solely of nodes, some statement some expression

use crate::token::Token;

pub enum Statement {
    Let(Box<LetStatement>),
    Return(Box<ReturnStatement>),
    ExpressionStatement(Expression),
}

pub enum Expression {
    Ident(Box<IdentExpression>),
    IntegerLiteral(Box<IntegerLiteralExpression>),
    Prefix(Box<PrefixExpression>),
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
    pub fn new(name: String, value: Option<Expression>) -> Self {
        let tok = Token::Ident(name);
        LetStatement { name: tok, value }
    }
}

pub struct ReturnStatement {
    pub name: Token, // return token
    pub value: Option<Expression>,
}

impl ReturnStatement {
    pub fn new(value: Option<Expression>) -> Self {
        ReturnStatement {
            name: Token::Return,
            value,
        }
    }
}

pub struct IdentExpression {
    pub value: Token, // return token
}

impl IdentExpression {
    pub fn new(value: String) -> Self {
        let tok = Token::Ident(value);
        IdentExpression { value: tok }
    }
}

pub struct IntegerLiteralExpression {
    pub value: Token, // return token
}

impl IntegerLiteralExpression {
    pub fn new(value: i64) -> Self {
        let tok = Token::Int(value);
        IntegerLiteralExpression { value: tok }
    }
}

pub struct PrefixExpression {
    pub value: Token, // prefix token
    pub right: Expression,
}

impl PrefixExpression {
    pub fn new(value: Token, exp: Expression) -> Self {
        // TODO: might change this input
        PrefixExpression { value, right: exp }
    }
}
