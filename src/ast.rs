// AST consists solely of nodes, some statement some expression

use crate::token::Token;
use std::fmt;
use std::fmt::Formatter;

pub enum Statement {
    Let(Box<LetStatement>),
    Return(Box<ReturnStatement>),
    ExpressionStatement(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<T, E> {
        match self {
            Statement::Let(l) => write!(f, "{}", l),
            Statement::Return(r) => write!(f, "{}", r),
            Statement::ExpressionStatement(es) => write!("{}", es),
        }
    }
}

pub enum Expression {
    Ident(Box<IdentExpression>),
    IntegerLiteral(Box<IntegerLiteralExpression>),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<T, E> {
        match self {
            Expression::Ident(idt) => write!(f, "{}", idt),
            Expression::IntegerLiteral(il) => write!(f, "{}", il),
            Expression::Prefix(pfx) => write!(f, "{}", pfx),
            Expression::Infix(ifx) => write!(f, "{}", ifx),
        }
    }
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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<T, E> {
        for statement in self.statements {
            write!(f, "{}", statement);
        }
        write!(f, "\n")
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

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<T, E> {
        write!(f, "let {} =", self.name);
        match self.value.as_ref() {
            Some(e) => write!(f, " {};", e),
            None => write!(f, ";"),
        }
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

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<T, E> {
        write!(f, "{}{};", self.prefix, self.right)
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

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<T, E> {
        write!(f, "(");
        write!(f, "{}", self.left);
        write!(f, " {} ", self.op);
        write!(f, "{}", self.right);
        write!(f, ")")
    }
}
