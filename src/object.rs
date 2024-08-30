use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::Deref,
    rc::Rc,
};

use crate::{chunk::Chunk, compiler, value::Value, vm::VM};

pub type NativeFn = fn(&mut [Value]) -> Value;

#[derive(Debug, PartialEq)]
pub enum Object {
    Closure(Closure),
    NativeFunction(NativeFn),
    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Closure(func) => write!(f, "{}", func),
            Object::NativeFunction(_) => write!(f, "<native fn>"),
            Object::Class(class) => write!(f, "{} class", class.name()),
            Object::Instance(instance) => write!(f, "{} instance", instance.class_name()),
            Object::BoundMethod(bound) => write!(
                f,
                "{}.{}",
                bound.receiver,
                bound.method.borrow().as_function().name
            ),
        }
    }
}

impl Object {
    pub fn as_function(&self) -> &Function {
        match self {
            Object::Closure(closure) => closure.function.as_ref(),
            _ => panic!("Expected function object"),
        }
    }

    pub fn as_closure(&self) -> &Closure {
        match self {
            Object::Closure(closure) => closure,
            _ => panic!("Expected closure object"),
        }
    }

    pub fn is_instance(&self) -> bool {
        matches!(*self, Object::Instance(_))
    }

    pub fn as_instance(&self) -> &Instance {
        match self {
            Object::Instance(instance) => instance,
            _ => panic!("Expected instance object"),
        }
    }

    pub fn as_instance_mut(&mut self) -> &mut Instance {
        match self {
            Object::Instance(instance) => instance,
            _ => panic!("Expected instance object"),
        }
    }

    pub fn as_class(&self) -> &Class {
        match self {
            Object::Class(class) => class,
            _ => panic!("Expected class object"),
        }
    }

    pub fn as_class_mut(&mut self) -> &mut Class {
        match self {
            Object::Class(class) => class,
            _ => panic!("Expected class object"),
        }
    }
}

impl Into<Value> for Object {
    fn into(self) -> Value {
        Value::Object(Rc::new(RefCell::new(self)))
    }
}

#[derive(Debug, PartialEq)]
pub enum Upvalue {
    LocalOpen(usize),
    ForeignOpen(Rc<RefCell<Upvalue>>),
    Closed(Value),
}

impl Upvalue {
    pub fn get_value(&self, vm: &VM) -> Value {
        match self {
            Upvalue::LocalOpen(index) => vm.get_value(*index),
            Upvalue::ForeignOpen(upvalue) => upvalue.borrow().get_value(vm),
            Upvalue::Closed(value) => value.clone(),
        }
    }

    pub fn set_value(&mut self, vm: &mut VM, value: Value) {
        match self {
            Upvalue::LocalOpen(index) => vm.set_value(*index, value),
            Upvalue::ForeignOpen(upvalue) => upvalue.as_ref().borrow_mut().set_value(vm, value),
            Upvalue::Closed(closed) => *closed = value,
        }
    }

    pub fn is_local(&self) -> bool {
        matches!(self, Upvalue::LocalOpen(_))
    }

    pub fn index(&self) -> usize {
        match self {
            Upvalue::LocalOpen(index) => *index,
            _ => panic!("Expected local upvalue"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Closure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

impl Closure {
    pub fn new(function: Rc<Function>) -> Self {
        Self {
            function,
            upvalues: Vec::new(),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.function)
    }
}

pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
    pub upvalues: Vec<compiler::Upvalue>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && self.name == other.name
    }
}

impl Function {
    pub fn new() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: String::new(),
            upvalues: Vec::new(),
        }
    }

    pub fn get_chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn get_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<fn {}>\n{:?}",
            if self.name.is_empty() {
                "<script>"
            } else {
                &self.name
            },
            self.chunk
        )
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<fn {}>",
            if self.name.is_empty() {
                "<script>"
            } else {
                &self.name
            }
        )
    }
}

impl Into<Chunk> for Function {
    fn into(self) -> Chunk {
        self.chunk
    }
}

#[derive(Debug, PartialEq)]
pub struct Class {
    name: String,
    methods: HashMap<String, Rc<RefCell<Object>>>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            methods: HashMap::new(),
        }
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn methods(&mut self) -> &mut HashMap<String, Rc<RefCell<Object>>> {
        &mut self.methods
    }

    pub fn insert_method(&mut self, name: String, method: Rc<RefCell<Object>>) {
        self.methods.insert(name, method);
    }

    pub fn get_method(&self, name: &str) -> Option<Rc<RefCell<Object>>> {
        self.methods.get(name).cloned()
    }
}

#[derive(Debug, PartialEq)]
pub struct Instance {
    class: Rc<RefCell<Object>>,
    fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: Rc<RefCell<Object>>) -> Self {
        assert!(matches!(*class.borrow(), Object::Class(_)));
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn class(&self) -> Rc<RefCell<Object>> {
        self.class.clone()
    }

    pub fn class_name(&self) -> String {
        match self.class.borrow().deref() {
            Object::Class(class) => class.name(),
            _ => panic!("Expected class object"),
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.fields.get(name).cloned()
    }

    pub fn set(&mut self, name: &str, value: Value) {
        self.fields.insert(name.to_string(), value);
    }
}

#[derive(Debug, PartialEq)]
pub struct BoundMethod {
    receiver: Value,
    method: Rc<RefCell<Object>>,
}

impl BoundMethod {
    pub fn new(receiver: Value, method: Rc<RefCell<Object>>) -> Object {
        Object::BoundMethod(Self { receiver, method })
    }

    pub fn method(&self) -> Rc<RefCell<Object>> {
        self.method.clone()
    }

    pub fn receiver(&self) -> Value {
        self.receiver.clone()
    }
}
