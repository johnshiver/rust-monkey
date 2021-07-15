use crate::ast::{Expression, Node, Program, Statement};
use crate::object::Object;

pub type EvalResult = Result<Object, EvalError>;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct EvalError {
    pub message: String,
}

pub fn Eval(node: Node) -> EvalResult {
    match node {
        Node::Program(prg) => Ok(eval_program(&prg)),
        Node::Statement(stm) => Ok(eval_statement(&stm)),
        Node::Expression(exp) => Ok(eval_expression(&exp)),
    }
}

fn eval_program(program: &Program) -> Object {
    let mut result = Object::Null;
    for stmt in &program.statements {
        result = eval_statement(&stmt);
    }
    result
}

fn eval_statement(statement: &Statement) -> Object {
    match statement {
        Statement::ExpressionStatement(exp) => eval_expression(exp),
        _ => panic!("not implemented"),
    }
}

fn eval_expression(exp: &Expression) -> Object {
    match exp {
        Expression::Integer(int) => return Object::Integer(*int),
        Expression::Bool(b) => match *b {
            true => TRUE,
            false => FALSE,
        },
        _ => panic!("expression not supported yet"),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::eval::{Eval, EvalResult};
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    fn test_eval(input: &str) -> EvalResult {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        Eval(program)
    }

    fn test_int_object(obj: Object, expected: i64) {
        match obj {
            Object::Integer(i) => {
                assert_eq!(expected, i);
            }
            _ => {
                panic!("expected object integer")
            }
        }
    }

    fn test_bool_object(obj: Object, expected: bool) {
        match obj {
            Object::Boolean(b) => {
                assert_eq!(expected, b);
            }
            _ => {
                panic!("expected object bool")
            }
        }
    }

    #[test]
    fn eval_integer_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: "5",
                expected: 5,
            },
            Test {
                input: "10",
                expected: 10,
            },
        ];

        for t in tests {
            match test_eval(t.input) {
                Ok(obj) => test_int_object(obj, t.expected),
                Err(e) => panic!("received unexpected eval error {}", e.message),
            }
        }
    }

    #[test]
    fn eval_bool_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }

        let tests = vec![
            Test {
                input: "true",
                expected: true,
            },
            Test {
                input: "false",
                expected: false,
            },
        ];

        for t in tests {
            match test_eval(t.input) {
                Ok(obj) => test_bool_object(obj, t.expected),
                Err(e) => panic!("received unexpected eval error {}", e.message),
            }
        }
    }
}
