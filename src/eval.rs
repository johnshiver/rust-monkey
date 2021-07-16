use crate::ast::{Expression, Node, Program, Statement};
use crate::object::Object;
use crate::object::Object::Null;
use crate::token::Token;

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
        Expression::Prefix(pfx) => {
            let right = eval_expression(&pfx.right);
            eval_prefix_expression(&pfx.operator, right)
        }
        Expression::Infix(ifx) => {
            let right = eval_expression(&ifx.right);
            let left = eval_expression(&ifx.left);
            eval_infix_expression(&ifx.operator, left, right)
        }
        _ => panic!("expression not supported yet"),
    }
}

fn eval_prefix_expression(operator: &Token, right: Object) -> Object {
    match operator {
        Token::Bang => eval_bang_prefix_operator_expression(right),
        Token::Minus => eval_minus_prefix_operator_expression(right),
        _ => Null,
    }
}

fn eval_infix_expression(operator: &Token, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(operator, l, r),
        (_, _) => return NULL,
    }
}

fn eval_integer_infix_expression(operator: &Token, left: i64, right: i64) -> Object {
    match operator {
        Token::Plus => Object::Integer(left + right),
        Token::Minus => Object::Integer(left - right),
        Token::Asterisk => Object::Integer(left * right),
        Token::Slash => Object::Integer(left / right),
        _ => NULL,
    }
}

fn eval_bang_prefix_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => NULL,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(i) => Object::Integer(-i),
        _ => NULL,
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
            Test {
                input: "-5",
                expected: -5,
            },
            Test {
                input: "-10",
                expected: -10,
            },
            Test {
                input: "2 * 2 * 2 * 2 * 2",
                expected: 32,
            },
            Test {
                input: "-50 + 100 + -50",
                expected: 0,
            },
            Test {
                input: "5 * 2 + 10",
                expected: 20,
            },
            Test {
                input: "5 + 2 * 10",
                expected: 25,
            },
            Test {
                input: "20 + 2 * -10",
                expected: 0,
            },
            Test {
                input: "50 / 2 * 2 + 10",
                expected: 60,
            },
            Test {
                input: "2 * (5 + 10)",
                expected: 30,
            },
            Test {
                input: "3 * 3 * 3 + 10",
                expected: 37,
            },
            Test {
                input: "3 * (3 * 3) + 10",
                expected: 37,
            },
            Test {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected: 50,
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

    #[test]
    fn test_bang_operator() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }

        let tests = vec![
            Test {
                input: "!true",
                expected: false,
            },
            Test {
                input: "!false",
                expected: true,
            },
            Test {
                input: "!5",
                expected: false,
            },
            Test {
                input: "!!true",
                expected: true,
            },
            Test {
                input: "!!false",
                expected: false,
            },
            Test {
                input: "!!5",
                expected: true,
            },
        ];
        for test in tests {
            match test_eval(test.input) {
                Ok(obj) => test_bool_object(obj, test.expected),
                Err(e) => panic!("received unexpected eval error {}", e.message),
            }
        }
    }
}
