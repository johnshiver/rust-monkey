use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

type ObjectType = &'static str;

const INTEGER_OBJ: ObjectType = "INTEGER";
const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
const NULL_OBJ: ObjectType = "NULL";
const RETURN_VALUE_OBJ: ObjectType = "RETURN_VALUE";

#[derive(PartialEq, Eq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Rc<Return>),
}

impl Object {
    pub fn Inspect(&self) -> String {
        match self {
            Object::Integer(val) => format!("{}", val),
            Object::Boolean(val) => format!("{}", val),
            Object::Null => "Null".to_string(),
            Object::Return(obj) => format!("{}", obj.value),
        }
    }

    pub fn Type(&self) -> ObjectType {
        match self {
            Object::Integer(_) => INTEGER_OBJ,
            Object::Boolean(_) => BOOLEAN_OBJ,
            Object::Null => NULL_OBJ,
            Object::Return(_) => RETURN_VALUE_OBJ,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.Inspect())
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

    pub fn set(mut self, key: &str, val: Rc<Object>) -> Rc<Object> {
        self.store.insert(key.to_string(), val.clone());
        val
    }
}
