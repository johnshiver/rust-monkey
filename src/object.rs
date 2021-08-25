use crate::ast::{BlockStatement, IdentExpression};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

type ObjectType = &'static str;

const INTEGER_OBJ: ObjectType = "INTEGER";
const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
const NULL_OBJ: ObjectType = "NULL";
const RETURN_VALUE_OBJ: ObjectType = "RETURN_VALUE";
const FUNCTION_OBJ: ObjectType = "FUNCTION";

#[derive(PartialEq, Eq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Rc<Return>),
    Function(Rc<Function>),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(val) => format!("{}", val),
            Object::Boolean(val) => format!("{}", val),
            Object::Null => "Null".to_string(),
            Object::Return(obj) => format!("{}", obj.value),
            Object::Function(f) => format!("{}", f),
        }
    }

    pub fn type_inspect(&self) -> ObjectType {
        match self {
            Object::Integer(_) => INTEGER_OBJ,
            Object::Boolean(_) => BOOLEAN_OBJ,
            Object::Null => NULL_OBJ,
            Object::Return(_) => RETURN_VALUE_OBJ,
            Object::Function(_) => FUNCTION_OBJ,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inspect())
    }
}

#[derive(Clone, Debug)]
pub struct Return {
    pub value: Rc<Object>,
}

impl PartialEq for Return {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}
impl Eq for Return {}

#[derive(Clone, Debug)]
pub struct Environment {
    pub store: HashMap<String, Rc<Object>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<Rc<Object>> {
        self.store.get(key).cloned()
    }

    pub fn set(&mut self, key: String, val: Rc<Object>) {
        self.store.insert(key, val);
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub parameters: Vec<IdentExpression>,
    pub body: BlockStatement,
    pub environment: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(
        params: Vec<IdentExpression>,
        body: BlockStatement,
        env: Rc<RefCell<Environment>>,
    ) -> Function {
        Function {
            parameters: params,
            body,
            environment: env,
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut param_strs = Vec::new();
        for p in &self.parameters {
            let param_str = format!("{}", p);
            param_strs.push(param_str);
        }
        write!(f, "fn");
        write!(f, "(");
        write!(f, "{}", param_strs.join(", "));
        write!(f, ") {} \n", "{");
        write!(f, "{}\n", self.body);
        write!(f, "{} \n", "}")
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}
impl Eq for Function {}
