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
    Infix(Box<InfixExpression>),
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
    pub prefix: Token, // prefix token, ! or -
    pub right: Expression,
}

impl PrefixExpression {
    pub fn new(prefix: Token, exp: Expression) -> Self {
        // TODO: could validate token to ensure its prefix token
        PrefixExpression { prefix, right: exp }
    }
}

pub struct InfixExpression {
    pub right: Expression,
    pub operator: Token, // various operator tokens
    pub left: Expression,
}

impl InfixExpression {
    pub fn new(operator: Token, left: Expression, right: Expression) -> Self {
        // TODO: validate token
        InfixExpression {
            right,
            operator,
            left,
        }
    }
}
