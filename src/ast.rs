// AST consists solely of nodes, some statement some expression

use crate::token::Token;
use std::fmt;
use std::fmt::Formatter;

pub enum Node {
    Program(Box<Program>),
    Statement(Box<Statement>), // do we need both?
    Expression(Box<Expression>),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Statement {
    Let(Box<LetStatement>),
    Return(Box<ReturnStatement>),
    ExpressionStatement(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Statement::Let(l) => write!(f, "{}", l),
            Statement::Return(r) => write!(f, "{}", r),
            Statement::ExpressionStatement(es) => write!(f, "{}", es),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Expression {
    Ident(Box<IdentExpression>),
    Integer(i64),
    Bool(bool),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    IfStatement(Box<IfExpression>),
    BlockStatement(Box<BlockStatement>),
    Function(Box<FunctionExpression>),
    Call(Box<CallExpression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Bool(b) => write!(f, "{}", b),
            Expression::Prefix(pfx) => write!(f, "{}", pfx),
            Expression::Infix(ifx) => write!(f, "{}", ifx),
            Expression::IfStatement(if_stmt) => write!(f, "{}", if_stmt),
            Expression::BlockStatement(blk) => write!(f, "{}", blk),
            Expression::Function(fl) => write!(f, "{}", fl),
            Expression::Call(call) => write!(f, "{}", call),
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
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement);
        }
        write!(f, "")
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct LetStatement {
    pub name: Token,
    pub value: Expression,
}

impl LetStatement {
    pub fn new(name: String, value: Expression) -> Self {
        let tok = Token::Ident(name);
        LetStatement { name: tok, value }
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ReturnStatement {
    pub name: Token, // return token
    pub value: Expression,
}

impl ReturnStatement {
    pub fn new(value: Expression) -> Self {
        ReturnStatement {
            name: Token::Return,
            value,
        }
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct IdentExpression {
    pub token: Token, // return token
}

impl IdentExpression {
    pub fn new(value: String) -> Self {
        let tok = Token::Ident(value);
        IdentExpression { token: tok }
    }

    pub fn new_from_token(token: Token) -> Self {
        IdentExpression { token }
    }
}

impl fmt::Display for IdentExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.token)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct BooleanLiteralExpression {
    pub value: Token,
}

impl BooleanLiteralExpression {
    pub fn new(value: Token) -> Self {
        return BooleanLiteralExpression { value };
    }
}

impl fmt::Display for BooleanLiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct PrefixExpression {
    pub operator: Token, // prefix token, ! or -
    pub right: Expression,
}

impl PrefixExpression {
    pub fn new(prefix: Token, exp: Expression) -> Self {
        // TODO: could validate token to ensure its prefix token
        //       would require refactor for better error handling
        PrefixExpression {
            operator: prefix,
            right: exp,
        }
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "(");
        write!(f, "{}", self.left);
        write!(f, " {} ", self.operator);
        write!(f, "{}", self.right);
        write!(f, ")")
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn new(
        token: Token,
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    ) -> Self {
        IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }
    }
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "if");
        write!(f, "{}", self.condition.to_string());
        write!(f, " ");
        write!(f, "{}", self.consequence.to_string());
        match &self.alternative {
            Some(stmt) => {
                write!(f, "{}", stmt.to_string());
            }
            _ => {}
        }
        write!(f, "")
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> Self {
        BlockStatement { token, statements }
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        for s in &self.statements {
            write!(f, "{}", s); // TODO: maybe writeln makes more sense
        }
        write!(f, "")
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct FunctionExpression {
    pub token: Token,
    pub parameters: Vec<IdentExpression>,
    pub body: BlockStatement,
}

impl FunctionExpression {
    pub fn new(token: Token, parameters: Vec<IdentExpression>, body: BlockStatement) -> Self {
        FunctionExpression {
            token,
            parameters,
            body,
        }
    }
}

impl fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut param_strs = Vec::new();
        for p in &self.parameters {
            let param_str = format!("{}", p);
            param_strs.push(param_str);
        }

        write!(f, "{}", self.token);
        write!(f, "(");
        write!(f, "{}", param_strs.join(", "));
        write!(f, ") ");
        write!(f, "{}", self.body)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct CallExpression {
    pub token: Token,
    pub function: Expression, //Identifier or function literal
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn new(token: Token, function: Expression, arguments: Vec<Expression>) -> Self {
        CallExpression {
            token,
            function,
            arguments,
        }
    }
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut arg_strs = Vec::new();
        for a in &self.arguments {
            let arg_str = format!("{}", a);
            arg_strs.push(arg_str);
        }

        write!(f, "{}", self.function);
        write!(f, "(");
        write!(f, "{}", arg_strs.join(", "));
        write!(f, ") ")
    }
}
