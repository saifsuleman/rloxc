use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::object::{Function, Object};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
    Function(Rc<Function>),
    Object(Rc<RefCell<Object>>),
}

impl Value {
    pub fn truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            Value::Number(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Function(_) => true,
            Value::Object(_) => true,
        }
    }

    pub fn type_name(&self) -> &str {
        match self {
            Value::Bool(_) => "bool",
            Value::Nil => "nil",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Function(_) => "function",
            Value::Object(_) => "object",
        }
    }

    pub fn as_object(&self) -> Option<Rc<RefCell<Object>>> {
        match self {
            Value::Object(o) => Some(o.clone()),
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(func) => write!(f, "{}", func.name),
            Value::Object(o) => write!(f, "{}", o.borrow()),
        }
    }
}
