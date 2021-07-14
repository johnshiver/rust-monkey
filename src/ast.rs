// AST consists solely of nodes, some statement some expression

use crate::token::Token;
use std::fmt;
use std::fmt::Formatter;

pub enum Node {
    Program(Box<Program>),
    Statement(Box<Statement>), // do we need both?
    Expression(Box<Expression>),
}

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
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "let {} =", self.name);
        match self.value.as_ref() {
            Some(e) => write!(f, " {};", e),
            None => write!(f, ";"),
        }
    }
}

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
        write!(f, "return ");
        write!(f, " {};", self.value)
    }
}

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

pub struct PrefixExpression {
    pub prefix_operator: Token, // prefix token, ! or -
    pub right: Expression,
}

impl PrefixExpression {
    pub fn new(prefix: Token, exp: Expression) -> Self {
        // TODO: could validate token to ensure its prefix token
        //       would require refactor for better error handling
        PrefixExpression {
            prefix_operator: prefix,
            right: exp,
        }
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "({}{})", self.prefix_operator, self.right)
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
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "(");
        write!(f, "{}", self.left);
        write!(f, " {} ", self.operator);
        write!(f, "{}", self.right);
        write!(f, ")")
    }
}

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
