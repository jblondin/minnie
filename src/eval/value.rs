
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Empty,
    Return(Box<Value>),

    Unimplemented,
}

impl Value {
    pub fn is_return(&self) -> bool {
        if let Value::Return(_) = *self { true } else { false }
    }
    pub fn ret(self) -> Value {
        if let Value::Return(value) = self { *value } else { self }
    }
}
