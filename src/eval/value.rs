use std::fmt;

use parse::ast::{Identifier, Block};
use eval::frame::Frame;

#[derive(Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Empty,
    Return(Box<Value>),
    Function {
        parameters: Vec<Identifier>,
        body: Block,
        context: Frame,
    },
    Error,
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

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Function { ref parameters, ref body, .. } => {
                write!(f, "Function {{ parameters: {:?}, body: {:?}, context: <hidden> }}",
                    parameters, body)
            },
            Value::Integer(i)     => { write!(f, "Integer({})", i) },
            Value::Float(fl)      => { write!(f, "Float({})", fl) },
            Value::Bool(b)        => { write!(f, "Bool({})", b) },
            Value::String(ref s)  => { write!(f, "String({})", s) },
            Value::Empty          => { write!(f, "Empty") },
            Value::Return(ref v)  => { write!(f, "Return({:?})", v) },
            Value::Error          => { write!(f, "Error") },
            Value::Unimplemented  => { write!(f, "Unimplemented") },
        }
    }
}
