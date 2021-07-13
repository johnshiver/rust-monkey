use crate::ast::{Expression, Node, Program, Statement};
use crate::object::Object;
use std::borrow::Borrow;

pub fn Eval(node: Node) -> Object {
    match node {
        Node::Program(prg) => eval_program(&prg),
        Node::Statement(stm) => eval_statement(&stm),
        Node::Expression(exp) => eval_expression(&exp),
    }
}

fn eval_program(program: &Program) -> Object {
    panic!("implement me")
}

fn eval_statement(statement: &Statement) -> Object {
    panic!("implement me")
}

fn eval_expression(exp: &Expression) -> Object {
    panic!("implement me")
}

#[cfg(test)]
mod tests {
    use crate::ast::Node;
    use crate::eval::Eval;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        Eval(Node::Program(Box::new(program)))
    }

    #[test]
    fn eval_integer_expression() {
        struct test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            test {
                input: "5",
                expected: 5,
            },
            test {
                input: "10",
                expected: 10,
            },
        ];

        for t in tests {}
    }
}
