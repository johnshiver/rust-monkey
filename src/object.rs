type ObjectType = &'static str;

const INTEGER_OBJ: ObjectType = "INTEGER";
const BOOLEAN_OBJ: ObjectType = "BOOLEAN";
const NULL_OBJ: ObjectType = "NULL";

pub enum Object {
    Integer(int64),
    Boolean(bool),
    Null,
}

impl Object {
    pub fn Inspect(&self) -> String {
        match self {
            Object::Integer(val) => format!("{}", val),
            Object::Boolean(val) => format!("{}", val),
            Object::Null => "Null".to_string(),
        }
    }

    pub fn Type(&self) -> ObjectType {
        match self {
            Object::Integer(_) => INTEGER_OBJ,
            Object::Boolean(_) => BOOLEAN_OBJ,
            Object::Null => NULL_OBJ,
        }
    }
}
