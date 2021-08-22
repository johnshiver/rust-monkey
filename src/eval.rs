use crate::ast::{BlockStatement, Expression, IfExpression, Node, Program, Statement};
use crate::object;
use crate::object::Object::Null;
use crate::object::{Environment, Object};
use crate::token::Token;
use std::cell::RefCell;
use std::rc::Rc;

pub type EvalResult = Result<Rc<Object>, EvalError>;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

#[derive(Debug)]
pub struct EvalError {
    pub message: String,
}

pub fn eval(node: Node, env: Rc<RefCell<Environment>>) -> EvalResult {
    match node {
        Node::Program(prg) => eval_program(&prg, env),
        Node::Statement(stm) => eval_statement(&stm, env),
        Node::Expression(exp) => eval_expression(&exp, env),
    }
}

fn eval_program(prog: &Program, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Rc::new(Null);
    for stmt in &prog.statements {
        let res = match eval_statement(stmt, Rc::clone(&env)) {
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

fn eval_statement(statement: &Statement, env: Rc<RefCell<Environment>>) -> EvalResult {
    match statement {
        Statement::ExpressionStatement(exp) => eval_expression(exp, env),
        Statement::Return(ret) => {
            let value = match eval_expression(&ret.value, env) {
                Ok(v) => v,
                Err(e) => return Err(e),
            };
            return Ok(Rc::new(Object::Return(Rc::new(object::Return { value }))));
        }
        Statement::Let(let_stmt) => {
            let value = match eval_expression(&let_stmt.value, Rc::clone(&env)) {
                Ok(v) => v,
                Err(e) => return Err(e),
            };
            let obj = Rc::clone(&value);
            env.borrow_mut().set(let_stmt.name.clone().to_string(), obj);
            return Ok(value);
        }
        _ => panic!("not implemented"),
    }
}

fn eval_expression(exp: &Expression, env: Rc<RefCell<Environment>>) -> EvalResult {
    match exp {
        Expression::Integer(int) => return Ok(Rc::new(Object::Integer(*int))),
        Expression::Bool(b) => match *b {
            true => Ok(Rc::new(TRUE)),
            false => Ok(Rc::new(FALSE)),
        },
        Expression::Prefix(pfx) => {
            let right = match eval_expression(&pfx.right, env) {
                Ok(o) => o,
                Err(e) => return Err(e),
            };
            return eval_prefix_expression(&pfx.operator, right.as_ref());
        }
        Expression::Infix(ifx) => {
            let right = match eval_expression(&ifx.right, Rc::clone(&env)) {
                Ok(o) => o,
                Err(e) => return Err(e),
            };
            let left = match eval_expression(&ifx.left, Rc::clone(&env)) {
                Ok(o) => o,
                Err(e) => return Err(e),
            };
            return eval_infix_expression(&ifx.operator, &left, &right);
        }
        Expression::BlockStatement(blk) => eval_block(blk, env),
        Expression::IfStatement(if_stmt) => {
            // TODO: perhaps consequence should just be an expression
            let evaluated = match eval_expression(&if_stmt.condition, Rc::clone(&env)) {
                Ok(eval) => eval,
                Err(e) => return Err(e),
            };
            if is_truthy(evaluated) {
                return eval_block(&if_stmt.consequence, Rc::clone(&env));
            }

            match &if_stmt.alternative {
                Some(alt) => eval_block(&alt, Rc::clone(&env)),
                None => Ok(Rc::new(NULL)),
            }
        }
        _ => panic!("expression: {} not supported yet", exp.to_string()),
    }
}

fn eval_prefix_expression(operator: &Token, right: &Object) -> EvalResult {
    match operator {
        Token::Bang => Ok(Rc::new(eval_bang_prefix_operator_expression(right))),
        Token::Minus => eval_minus_prefix_operator_expression(right),
        _ => {
            let message = format!("unknown operator: {}{}", operator, right.Type());
            Err(EvalError { message })
        }
    }
}

fn eval_infix_expression(operator: &Token, left: &Object, right: &Object) -> EvalResult {
    if left.Type() != right.Type() {
        let message = format!(
            "type mismatch: {} {} {}",
            left.Type(),
            operator,
            right.Type()
        );
        return Err(EvalError { message });
    }
    match (operator, left, right) {
        (_, Object::Integer(l), Object::Integer(r)) => {
            eval_integer_infix_expression(operator, *l, *r)
        }
        // these work because we represent TRUE and FALSE as sentinel values
        (Token::EQ, Object::Boolean(_), Object::Boolean(_)) => {
            Ok(Rc::new(Object::Boolean(left == right)))
        }
        (Token::NEQ, Object::Boolean(_), Object::Boolean(_)) => {
            Ok(Rc::new(Object::Boolean(left != right)))
        }
        (_, _, _) => {
            let message = format!(
                "unknown operator: {} {} {}",
                left.Type(),
                operator,
                right.Type()
            );
            Err(EvalError { message })
        }
    }
}

fn eval_integer_infix_expression(operator: &Token, left: i64, right: i64) -> EvalResult {
    match operator {
        Token::Plus => Ok(Rc::new(Object::Integer(left + right))),
        Token::Minus => Ok(Rc::new(Object::Integer(left - right))),
        Token::Asterisk => Ok(Rc::new(Object::Integer(left * right))),
        Token::Slash => Ok(Rc::new(Object::Integer(left / right))),
        Token::LT => Ok(Rc::new(Object::Boolean(left < right))),
        Token::GT => Ok(Rc::new(Object::Boolean(left > right))),
        Token::EQ => Ok(Rc::new(Object::Boolean(left == right))),
        Token::NEQ => Ok(Rc::new(Object::Boolean(left != right))),
        _ => {
            let message = format!("unknown operator: {} {} {}", left, operator, right);
            Err(EvalError { message })
        }
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

fn eval_minus_prefix_operator_expression(right: &Object) -> EvalResult {
    match right {
        Object::Integer(i) => {
            let ni = *i;
            Ok(Rc::new(Object::Integer(-ni)))
        }
        _ => {
            let message = format!("unknown operator: -{}", right.Type());
            Err(EvalError { message })
        }
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

fn eval_block(block: &BlockStatement, env: Rc<RefCell<Environment>>) -> EvalResult {
    let mut result = Rc::new(Null);
    for stmt in &block.statements {
        let res = match eval_statement(stmt, Rc::clone(&env)) {
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
    use crate::object::{Environment, Object};
    use crate::parser::Parser;
    use std::cell::RefCell;
    use std::ops::Deref;
    use std::rc::Rc;

    fn test_eval(input: &str) -> EvalResult {
        let env = Rc::new(RefCell::new(Environment::new()));
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        eval(program, env)
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
            Test {
                input: "foobar",
                expected: "identifier not found: foobar",
            },
        ];
        for test in tests {
            match test_eval(test.input) {
                Ok(obj) => panic!("received object {}, expected error", obj),
                Err(e) => assert_eq!(e.message.as_str(), test.expected),
            }
        }
    }

    #[test]
    fn test_let_statements() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: "let a = 5; a;",
                expected: 5,
            },
            Test {
                input: "let a = 5 * 5; a;",
                expected: 25,
            },
            Test {
                input: "let a = 5; let b = a; b;",
                expected: 5,
            },
            Test {
                input: "let a = 5; let b = a; let c = a + b + 5; c;",
                expected: 15,
            },
        ];
        for test in tests {
            match test_eval(test.input) {
                Ok(obj) => test_int_object(&obj, Some(test.expected)),
                Err(e) => panic!(
                    "received an error {}, expected {}",
                    e.message, test.expected
                ),
            }
        }
    }
}
