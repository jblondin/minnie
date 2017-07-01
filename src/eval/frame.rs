use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use eval::value::Value;
use parse::ast::Identifier;

#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    frame_data: Rc<RefCell<FrameData>>,
    parent: Option<Box<Frame>>,
}

impl Frame {
    pub fn new() -> Frame {
        Frame {
            frame_data: Rc::new(RefCell::new(FrameData::new())),
            parent: None,
        }
    }

    pub fn push(parent: &Frame) -> Frame {
        Frame {
            frame_data: Rc::new(RefCell::new(FrameData::new())),
            parent: Some(box parent.clone()),
        }
    }

    pub fn set(&mut self, ident: Identifier, value: Value) {
        self.frame_data.borrow_mut().insert(ident, value);
    }

    pub fn get(&self, ident: &Identifier) -> Option<Value> {
        match self.frame_data.borrow().get(ident) {
            Some(&ref value) => Some(value.clone()),
            None => {
                // head on up the frame stack
                match self.parent {
                    Some(ref parent_frame) => {
                        (*parent_frame).get(ident)
                    },
                    None => None,
                }
            }
        }
    }
}

pub type FrameData = HashMap<Identifier, Value>;
