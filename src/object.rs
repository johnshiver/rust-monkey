use std::fmt::{Display, Formatter};
use std::rc::Rc;

type ObjectType = &'static str;

const INTEGER_OBJ: ObjectType = "INTEGER";
const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
const NULL_OBJ: ObjectType = "NULL";
const RETURN_VALUE_OBJ: ObjectType = "RETURN_VALUE";
const ERROR_OBJ: ObjectType = "ERROR";

#[derive(PartialEq, Eq, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Rc<Return>),
    Error(String),
}

impl Object {
    pub fn Inspect(&self) -> String {
        match self {
            Object::Integer(val) => format!("{}", val),
            Object::Boolean(val) => format!("{}", val),
            Object::Null => "Null".to_string(),
            Object::Return(obj) => format!("{}", obj.value),
            Object::Error(e) => format!("{}", e),
        }
    }

    pub fn Type(&self) -> ObjectType {
        match self {
            Object::Integer(_) => INTEGER_OBJ,
            Object::Boolean(_) => BOOLEAN_OBJ,
            Object::Null => NULL_OBJ,
            Object::Return(_) => RETURN_VALUE_OBJ,
            Object::Error(_) => ERROR_OBJ,
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
