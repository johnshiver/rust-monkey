use crate::ast::{BlockStatement, Expression, IfExpression, Node, Program, Statement};
use crate::object;
use crate::object::Object;
use crate::object::Object::Null;
use crate::token::Token;
use std::rc::Rc;

pub type EvalResult = Result<Rc<Object>, EvalError>;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct EvalError {
    pub message: String,
}

pub fn eval(node: Node) -> EvalResult {
    match node {
        Node::Program(prg) => eval_program(&prg),
        Node::Statement(stm) => eval_statement(&stm),
        Node::Expression(exp) => eval_expression(&exp),
    }
}

fn eval_program(prog: &Program) -> EvalResult {
    let mut result = Rc::new(Null);
    for stmt in &prog.statements {
        let res = match eval_statement(stmt) {
            Ok(r) => r,
            Err(e) => return Err(e),
        };

        match &*res {
            Object::Return(r) => return Ok(Rc::clone(&r.value)),
            _ => result = res,
        }
    }

    Ok(result)
}

fn eval_statement(statement: &Statement) -> EvalResult {
    match statement {
        Statement::ExpressionStatement(exp) => eval_expression(exp),
        Statement::Return(ret) => {
            let value = eval_expression(&ret.value)?;
            return Ok(Rc::new(Object::Return(Rc::new(object::Return { value }))));
        }
        _ => panic!("not implemented"),
    }
}

fn eval_expression(exp: &Expression) -> EvalResult {
    match exp {
        Expression::Integer(int) => return Ok(Rc::new(Object::Integer(*int))),
        Expression::Bool(b) => match *b {
            true => Ok(Rc::new(TRUE)),
            false => Ok(Rc::new(FALSE)),
        },
        Expression::Prefix(pfx) => {
            let right = match eval_expression(&pfx.right) {
                Ok(o) => o,
                Err(e) => return Err(e),
            };
            Ok(Rc::new(eval_prefix_expression(
                &pfx.operator,
                right.as_ref(),
            )))
        }
        Expression::Infix(ifx) => {
            let right = match eval_expression(&ifx.right) {
                Ok(o) => o,
                Err(e) => return Err(e),
            };
            let left = match eval_expression(&ifx.left) {
                Ok(o) => o,
                Err(e) => return Err(e),
            };
            Ok(Rc::new(eval_infix_expression(&ifx.operator, &left, &right)))
        }
        Expression::BlockStatement(blk) => eval_block(blk),
        Expression::IfStatement(if_stmt) => {
            // TODO: perhaps consequence should just be an expression
            let evaluated = match eval_expression(&if_stmt.condition) {
                Ok(eval) => eval,
                Err(e) => return Err(e),
            };
            if is_truthy(evaluated) {
                return eval_block(&if_stmt.consequence);
            }

            match &if_stmt.alternative {
                Some(alt) => eval_block(&alt),
                None => Ok(Rc::new(NULL)),
            }
        }
        _ => panic!("expression not supported yet"),
    }
}

fn eval_prefix_expression(operator: &Token, right: &Object) -> Object {
    match operator {
        Token::Bang => eval_bang_prefix_operator_expression(right),
        Token::Minus => eval_minus_prefix_operator_expression(right),
        _ => Null,
    }
}

fn eval_infix_expression(operator: &Token, left: &Object, right: &Object) -> Object {
    match (operator, left, right) {
        (_, Object::Integer(l), Object::Integer(r)) => {
            eval_integer_infix_expression(operator, *l, *r)
        }
        // these work because we represent TRUE and FALSE as sentinel values
        (Token::EQ, _, _) => Object::Boolean(left == right),
        (Token::NEQ, _, _) => Object::Boolean(left != right),
        (_, _, _) => return NULL,
    }
}

fn eval_integer_infix_expression(operator: &Token, left: i64, right: i64) -> Object {
    match operator {
        Token::Plus => Object::Integer(left + right),
        Token::Minus => Object::Integer(left - right),
        Token::Asterisk => Object::Integer(left * right),
        Token::Slash => Object::Integer(left / right),
        Token::LT => Object::Boolean(left < right),
        Token::GT => Object::Boolean(left > right),
        Token::EQ => Object::Boolean(left == right),
        Token::NEQ => Object::Boolean(left != right),
        _ => NULL,
    }
}

fn eval_bang_prefix_operator_expression(right: &Object) -> Object {
    match *right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => NULL,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    match right {
        Object::Integer(i) => {
            let ni = *i;
            Object::Integer(-ni)
        }
        _ => NULL,
    }
}

fn is_truthy(obj: Rc<Object>) -> bool {
    match *obj {
        NULL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

fn eval_block(block: &BlockStatement) -> EvalResult {
    let mut result = Rc::new(Null);
    for stmt in &block.statements {
        let res = match eval_statement(stmt) {
            Ok(r) => r,
            Err(e) => return Err(e),
        };

        match *res {
            Object::Return(_) => return Ok(res),
            _ => result = res,
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::eval::{eval, EvalResult};
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;
    use std::ops::Deref;

    fn test_eval(input: &str) -> EvalResult {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        eval(program)
    }

    fn test_int_object(obj: &Object, expected: Option<i64>) {
        match obj {
            Object::Integer(i) => {
                assert_eq!(expected.unwrap(), *i);
            }
            Object::Null => {
                assert_eq!(expected.is_none(), true);
            }
            _ => {
                panic!("expected object integer")
            }
        }
    }

    fn test_bool_object(obj: &Object, expected: bool) {
        match obj {
            Object::Boolean(b) => {
                assert_eq!(expected, *b);
            }
            _ => {
                panic!("expected object bool")
            }
        }
    }

    fn test_err_object(obj: &Object, expected: Option<i64>) {
        match obj {
            Object::Integer(i) => {
                assert_eq!(expected.unwrap(), *i);
            }
            Object::Null => {
                assert_eq!(expected.is_none(), true);
            }
            _ => {
                panic!("expected object integer")
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
                Ok(obj) => test_int_object(&obj, Some(t.expected)),
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
            Test {
                input: "1 < 2",
                expected: true,
            },
            Test {
                input: "1 > 2",
                expected: false,
            },
            Test {
                input: "1 < 1",
                expected: false,
            },
            Test {
                input: "1 > 1",
                expected: false,
            },
            Test {
                input: "1 == 1",
                expected: true,
            },
            Test {
                input: "1 != 1",
                expected: false,
            },
            Test {
                input: "1 == 2",
                expected: false,
            },
            Test {
                input: "1 != 2",
                expected: true,
            },
            Test {
                input: "true == true",
                expected: true,
            },
            Test {
                input: "false == false",
                expected: true,
            },
            Test {
                input: "true == false",
                expected: false,
            },
            Test {
                input: "true != false",
                expected: true,
            },
            Test {
                input: "false != true",
                expected: true,
            },
            Test {
                input: "(1 < 2) == true",
                expected: true,
            },
            Test {
                input: "(1 < 2) == false",
                expected: false,
            },
            Test {
                input: "(1 > 2) == true",
                expected: false,
            },
            Test {
                input: "(1 > 2) == false",
                expected: true,
            },
        ];

        for t in tests {
            match test_eval(t.input) {
                Ok(obj) => test_bool_object(&obj, t.expected),
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
                Ok(obj) => test_bool_object(&obj, test.expected),
                Err(e) => panic!("received unexpected eval error {}", e.message),
            }
        }
    }

    #[test]
    fn test_if_else_expressions() {
        struct Test<'a> {
            input: &'a str,
            expected: Option<i64>,
        }

        let tests = vec![
            Test {
                input: "if (true) { 10 }",
                expected: Some(10),
            },
            Test {
                input: "if (false) { 10 }",
                expected: None,
            },
            Test {
                input: "if (1) { 10 }",
                expected: Some(10),
            },
            Test {
                input: "if (1 < 2) { 10 }",
                expected: Some(10),
            },
            Test {
                input: "if (1 > 2) { 10 }",
                expected: None,
            },
            Test {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected: Some(20),
            },
            Test {
                input: "if (1 < 2) { 10 } else { 20 }",
                expected: Some(10),
            },
        ];
        for test in tests {
            match test_eval(test.input) {
                Ok(obj) => test_int_object(&obj, test.expected),
                Err(e) => panic!("received unexpected eval error {}", e.message),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: "return 10;",
                expected: 10,
            },
            Test {
                input: "return 10; 9;",
                expected: 10,
            },
            Test {
                input: "return 2 * 5; 9;",
                expected: 10,
            },
            Test {
                input: "9; return 2 * 5; 9;",
                expected: 10,
            },
            Test {
                input: "if (10 > 1) {
                          if (10 > 1) {
                            return 10;
                          }
                           return 1;
                        }",
                expected: 10,
            },
        ];
        for test in tests {
            match test_eval(test.input) {
                Ok(obj) => test_int_object(&obj, Some(test.expected)),
                Err(e) => panic!("received unexpected eval error {}", e.message),
            }
        }
    }

    #[test]
    fn test_error_handling() {
        struct Test<'a, 'b> {
            input: &'a str,
            expected: &'b str,
        }

        let tests = vec![
            Test {
                input: "5 + true;",
                expected: "type mismatch: INTEGER + BOOLEAN",
            },
            Test {
                input: "5 + true; 5;",
                expected: "type mismatch: INTEGER + BOOLEAN",
            },
            Test {
                input: "-true",
                expected: "unknown operator: -BOOLEAN",
            },
            Test {
                input: "true + false;",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "5; true + false; 5;",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "if (10 > 1) { true + false; }",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "if (10 > 1) {
                          if (10 > 1) {
                            return true + false;
                          }
                           return 1;
                        }",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
        ];
        for test in tests {
            match test_eval(test.input) {
                Ok(obj) => test_int_object(&obj, Some(test.expected)),
                Err(e) => panic!("received unexpected eval error {}", e.message),
            }
        }
    }
}
