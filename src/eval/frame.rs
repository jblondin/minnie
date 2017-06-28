use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use eval::value::Value;
use parse::ast::Identifier;

#[derive(Debug, Clone, PartialEq)]
pub struct Frame {
    data: HashMap<Identifier, Value>,
    parent: Option<Rc<RefCell<Frame>>>,
}

impl Frame {
    pub fn new() -> Frame {
        Frame {
            data: HashMap::new(),
            parent: None,
        }
    }

    pub fn push(parent: Rc<RefCell<Frame>>) -> Frame {
        Frame {
            data: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn set(&mut self, ident: Identifier, value: Value) {
        self.data.insert(ident, value);
    }

    pub fn get(&self, ident: &Identifier) -> Option<Value> {
        match self.data.get(ident) {
            Some(&ref value) => Some(value.clone()),
            None => {
                // head on up the frame stack
                match self.parent {
                    Some(ref parent_frame) => {
                        parent_frame.borrow().get(ident)
                    },
                    None => None,
                }
            }
        }
    }
}
