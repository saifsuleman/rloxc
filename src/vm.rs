#[allow(unused_imports)]
use std::io::{stdout, Write};

use std::{cell::RefCell, collections::HashMap, rc::Rc, result};

use crate::{
    chunk::OpCode,
    compiler, natives,
    object::{BoundMethod, Class, Closure, Function, Instance, NativeFn, Object, Upvalue},
    value::Value,
};

#[derive(Debug)]
pub enum InterpretError {
    CompileError,
    RuntimeError,
}

pub(crate) type Result<T> = result::Result<T, InterpretError>;

macro_rules! runtime_error {
    ($vm:ident, $( $arg:tt )*) => {
        {
            eprintln!($( $arg )*);
            $vm._error();
        }
    };
}

macro_rules! binary_op {
    ($vm:ident, $discriminant:ident, $op:tt) => {
        {
            let b = $vm.pop().unwrap();
            let a = $vm.pop().unwrap();
            if let (Value::Number(a), Value::Number(b)) = (a, b) {
                $vm.push(Value::$discriminant(a $op b));
            } else {
                runtime_error!($vm, "Operands must be numbers.");
                return Err(InterpretError::RuntimeError);
            }
        }
    };
}

#[derive(Clone)]
struct CallFrame {
    pub closure: Rc<RefCell<Object>>,
    pub ip: usize,
    pub stack_start: usize,
}

impl CallFrame {
    fn peek(&mut self) -> OpCode {
        self.closure.borrow().as_function().get_chunk().code[self.ip].clone()
    }

    fn read_instruction(&mut self) -> OpCode {
        self.ip += 1;
        self.closure.borrow().as_function().get_chunk().code[self.ip - 1]
    }

    fn get_constant(&mut self, index: usize) -> Value {
        self.closure
            .borrow()
            .as_function()
            .get_chunk()
            .get_constant(index)
            .clone()
    }
}

pub struct VM {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    open_upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

impl VM {
    pub fn new() -> Self {
        let mut vm = Self {
            frames: Vec::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
            open_upvalues: Vec::new(),
        };

        vm.define_native("clock", natives::clock);

        vm
    }

    pub fn get_value(&self, index: usize) -> Value {
        self.stack[index].clone()
    }

    pub fn set_value(&mut self, index: usize, value: Value) {
        self.stack[index] = value;
    }

    pub fn interpret_source(&mut self, source: &str) -> Result<()> {
        self.interpret_chunk(compiler::compile(source)?)
    }

    pub fn interpret_chunk(&mut self, function: Function) -> Result<()> {
        let frame = CallFrame {
            closure: Rc::new(RefCell::new(Object::Closure(Closure::new(Rc::new(
                function,
            ))))),
            ip: 0,
            stack_start: self.stack.len(),
        };

        let function = frame.closure.clone();
        self.push(Value::Object(function));
        self.frames.push(frame);

        self.run()
    }

    fn _error(&mut self) {
        for frame in self.frames.iter().rev() {
            let function = frame.closure.borrow();
            let function = function.as_function();

            eprintln!(
                "[line {}] in {}",
                function.get_chunk().line(frame.ip - 1),
                if function.name.is_empty() {
                    "script".to_owned()
                } else {
                    function.name.to_owned() + "()"
                }
            );
        }
        self.stack.clear();
        self.frames.clear();
    }

    pub fn define_native(&mut self, name: &str, function: NativeFn) {
        self.globals
            .insert(name.to_owned(), Object::NativeFunction(function).into());
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn peek(&mut self) -> Option<&Value> {
        self.stack.last()
    }

    fn peek_n(&mut self, n: usize) -> Option<&Value> {
        self.stack.get(self.stack.len() - n - 1)
    }

    fn call(&mut self, function: Rc<RefCell<Object>>, arg_count: usize) -> Result<()> {
        let arity = function.borrow().as_function().arity;
        if arg_count != arity {
            runtime_error!(self, "Expected {} arguments but got {}", arity, arg_count);
            return Err(InterpretError::RuntimeError);
        }
        if self.frames.len() == usize::MAX {
            runtime_error!(self, "Stack overflow.");
            return Err(InterpretError::RuntimeError);
        }
        self.frames.push(CallFrame {
            closure: function,
            ip: 0,
            stack_start: self.stack.len() - arg_count - 1,
        });
        Ok(())
    }

    fn call_value(&mut self, callee: Value, arg_count: usize) -> Result<()> {
        if let Value::Object(function) = callee {
            match *function.borrow() {
                Object::BoundMethod(ref bound) => {
                    let index = self.stack.len() - arg_count - 1;
                    self.stack[index] = bound.receiver();
                    self.call(bound.method(), arg_count)
                }
                Object::Class(ref class) => {
                    let index = self.stack.len() - arg_count - 1;
                    self.stack[index] = Object::Instance(Instance::new(function.clone())).into();

                    if let Some(initializer) = class.get_method("init") {
                        return self.call(initializer, arg_count);
                    } else if arg_count > 0 {
                        runtime_error!(self, "Expected 0 arguments but got {arg_count}");
                        return Err(InterpretError::RuntimeError);
                    }

                    Ok(())
                }
                Object::Closure(_) => self.call(function.clone(), arg_count),
                Object::NativeFunction(ref function) => {
                    let stack_start = self.stack.len() - arg_count;
                    let args = &mut self.stack[stack_start..];
                    let result = function(args);
                    self.stack.truncate(self.stack.len() - arg_count - 1);
                    self.push(result);
                    Ok(())
                }
                _ => {
                    runtime_error!(self, "Can only call functions and classes.");
                    Err(InterpretError::RuntimeError)
                }
            }
        } else {
            runtime_error!(self, "Can only call functions and classes.");
            Err(InterpretError::RuntimeError)
        }
    }

    fn invoke_from_class(&mut self, class: &Class, name: &str, arg_count: usize) -> Result<()> {
        let method = if let Some(method) = class.get_method(name) {
            method
        } else {
            runtime_error!(self, "Undefined property '{}'", name);
            return Err(InterpretError::RuntimeError);
        };

        self.call(method, arg_count)
    }

    fn invoke(&mut self, name: &str, arg_count: usize) -> Result<()> {
        let receiver = self.peek_n(arg_count).unwrap().clone();
        let receiver = receiver.as_object().map(|o| Ok(o)).unwrap_or_else(|| {
            runtime_error!(self, "Only instances have methods.");
            Err(InterpretError::RuntimeError)
        })?;

        if !receiver.borrow().is_instance() {
            runtime_error!(self, "Only instances have methods.");
            return Err(InterpretError::RuntimeError);
        }

        if let Some(value) = receiver.borrow().as_instance().get(name) {
            self.stack.truncate(self.stack.len() - arg_count - 1);
            let index = self.stack.len() - arg_count - 1;
            self.stack[index] = value.clone();
            return self.call_value(value, arg_count);
        }

        let class = receiver.borrow().as_instance().class().clone();
        let class = class.borrow();
        let class = class.as_class();
        self.invoke_from_class(class, name, arg_count)
    }

    fn bind_method(&mut self, class: &Class, name: &str) -> bool {
        if let Some(method) = class.get_method(name).clone() {
            let bound = BoundMethod::new(self.peek().unwrap().clone(), method.clone());
            self.pop();
            self.push(bound.into());
            true
        } else {
            runtime_error!(self, "Undefined property '{}'", name);
            false
        }
    }

    fn frame(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn close_upvalues(&mut self, last: usize) {
        while self.open_upvalues.len() > 0
            && self.open_upvalues.last().unwrap().borrow().index() >= last
        {
            let upvalue = self.open_upvalues.pop().unwrap();
            let value = { upvalue.borrow().get_value(self) };
            *upvalue.as_ref().borrow_mut() = Upvalue::Closed(value);
        }
    }

    fn run(&mut self) -> Result<()> {
        loop {
            let instruction = self.frame().read_instruction();
            let stack_start = self.frame().stack_start;

            #[cfg(debug_trace_execution)]
            {
                print!("\t");
                for value in &self.stack {
                    print!("[ {} ]", value);
                }
                println!();
                stdout().flush().unwrap();
                let ip = self.frame().ip - 1;
                let instruction = instruction
                    .to_string_resolved(self.frame().closure.borrow().as_function().get_chunk());
                let function = self.frame().closure.borrow().to_string();
                println!("({function}:{ip}){instruction}");
            }

            match instruction {
                OpCode::Add => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();

                    match (a, b) {
                        (Value::String(a), b) => self.push(Value::String(a + &b.to_string())),
                        (a, Value::String(b)) => self.push(Value::String(a.to_string() + &b)),
                        (Value::Number(a), Value::Number(b)) => self.push(Value::Number(a + b)),
                        (a, b) => {
                            runtime_error!(
                                self,
                                "Add operation not valid on types '{}' and '{}'",
                                a.type_name(),
                                b.type_name()
                            );
                            return Err(InterpretError::RuntimeError);
                        }
                    }
                }
                OpCode::Subtract => binary_op!(self, Number, -),
                OpCode::Multiply => binary_op!(self, Number, *),
                OpCode::Divide => binary_op!(self, Number, /),
                OpCode::Not => {
                    let value = self.pop().unwrap();
                    self.push(Value::Bool(!value.truthy()));
                }
                OpCode::Negate => {
                    let value = self.pop().unwrap();
                    if let Value::Number(n) = value {
                        self.push(Value::Number(-n));
                    } else {
                        runtime_error!(self, "Operand must be a number.");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Constant(index) => {
                    let value = self.frame().get_constant(index);
                    self.push(value);
                }
                OpCode::Nil => self.push(Value::Nil),
                OpCode::True => self.push(Value::Bool(true)),
                OpCode::False => self.push(Value::Bool(false)),
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::GetLocal(index) => {
                    let value = self.stack[stack_start + index].clone();
                    self.push(value);
                }
                OpCode::SetLocal(index) => {
                    let value = self.peek().unwrap().clone();
                    self.stack[stack_start + index] = value;
                }
                OpCode::GetGlobal(index) => {
                    let name = self.frame().get_constant(index);
                    if let Value::String(name) = name {
                        if let Some(value) = self.globals.get(&name) {
                            self.push(value.clone());
                        } else {
                            runtime_error!(self, "Undefined variable '{}'", name);
                            return Err(InterpretError::RuntimeError);
                        }
                    } else {
                        runtime_error!(self, "Global name must be a string.");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::SetGlobal(index) => {
                    let name = self.frame().get_constant(index);
                    if let Value::String(name) = name {
                        if self.globals.contains_key(&name) {
                            let value = self.peek().unwrap().clone();
                            self.globals.insert(name, value);
                        } else {
                            runtime_error!(self, "Undefined variable '{}'", name);
                            return Err(InterpretError::RuntimeError);
                        }
                    } else {
                        runtime_error!(self, "Global name must be a string.");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::DefineGlobal(index) => {
                    let name = self.frame().get_constant(index);
                    if let Value::String(name) = name {
                        let value = self.pop().unwrap();
                        self.globals.insert(name, value);
                    } else {
                        runtime_error!(self, "Global name must be a string.");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::GetUpvalue(index) => {
                    let upvalue =
                        self.frame().closure.borrow().as_closure().upvalues[index].clone();
                    let value = upvalue.borrow().get_value(self);
                    self.push(value);
                }
                OpCode::SetUpvalue(index) => {
                    let upvalue =
                        self.frame().closure.borrow().as_closure().upvalues[index].clone();
                    let value = self.peek().unwrap().clone();
                    upvalue.borrow_mut().set_value(self, value);
                }
                OpCode::Equal => {
                    let b = self.pop().unwrap();
                    let a = self.pop().unwrap();
                    self.push(Value::Bool(a == b));
                }
                OpCode::GetProperty(index) => {
                    let instance = self.peek().unwrap().clone();
                    let name = self.frame().get_constant(index);

                    let name = if let Value::String(name) = name {
                        name
                    } else {
                        runtime_error!(self, "Property name must be a string.");
                        return Err(InterpretError::RuntimeError);
                    };

                    let instance = instance.as_object();
                    if instance.is_none()
                        & !matches!(*instance.as_ref().unwrap().borrow(), Object::Instance(_))
                    {
                        runtime_error!(self, "Only instances have properties.");
                        return Err(InterpretError::RuntimeError);
                    };

                    let instance = instance.unwrap();
                    let class = instance.borrow().as_instance().class().clone();
                    let class = class.borrow();
                    let class = class.as_class();

                    if let Some(value) = instance.clone().borrow().as_instance().get(&name) {
                        self.pop();
                        self.push(value);
                    } else if !self.bind_method(class, &name) {
                        runtime_error!(self, "Undefined property '{}'", name);
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::SetProperty(index) => {
                    let value = self.pop().unwrap();
                    let instance = self.peek().unwrap().clone();
                    let name = self.frame().get_constant(index);

                    let name = if let Value::String(name) = name {
                        name
                    } else {
                        runtime_error!(self, "Property name must be a string.");
                        return Err(InterpretError::RuntimeError);
                    };

                    let instance = instance.as_object();
                    if instance.is_none()
                        & !matches!(*instance.as_ref().unwrap().borrow(), Object::Instance(_))
                    {
                        runtime_error!(self, "Only instances have fields.");
                        return Err(InterpretError::RuntimeError);
                    };

                    let instance = instance.unwrap();
                    let mut instance = instance.borrow_mut();

                    instance.as_instance_mut().set(&name, value);

                    let value = self.pop().unwrap();
                    self.pop();
                    self.push(value);
                }
                OpCode::GetSuper(index) => {
                    let name = self.frame().get_constant(index);
                    let name = name.to_string();

                    let superclass = self.pop().unwrap();
                    let superclass = superclass.as_object().unwrap();
                    let superclass = superclass.borrow();
                    let superclass = superclass.as_class();

                    if !self.bind_method(superclass, &name) {
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Greater => binary_op!(self, Bool, >),
                OpCode::Less => binary_op!(self, Bool, <),
                OpCode::Print => {
                    println!("{}", self.pop().unwrap());
                }
                OpCode::Jump(offset) => self.frame().ip += offset,
                OpCode::JumpIfFalse(offset) => {
                    let value = self.peek().unwrap();
                    if !value.truthy() {
                        self.frame().ip += offset;
                    }
                }
                OpCode::Loop(offset) => self.frame().ip -= offset,
                OpCode::Call(arg_count) => {
                    let callee = self.peek_n(arg_count).unwrap().clone();
                    self.call_value(callee, arg_count)?;
                }
                OpCode::Invoke(index, arity) => {
                    let name = self.frame().get_constant(index);
                    self.invoke(name.to_string().as_ref(), arity)?;
                }
                OpCode::SuperInvoke(index, arg_count) => {
                    let name = self.frame().get_constant(index);
                    let name = name.to_string();

                    let superclass = self.pop().unwrap();
                    let superclass = superclass.as_object().unwrap();
                    let superclass = superclass.borrow();
                    let superclass = superclass.as_class();

                    self.invoke_from_class(superclass, &name, arg_count)?;
                }
                OpCode::Closure(index) => {
                    let function = self.frame().get_constant(index);
                    if let Value::Function(function) = function {
                        let mut closure = Closure::new(function.clone());
                        while let OpCode::Upvalue(is_local, index) = self.frame().peek().clone() {
                            self.frame().read_instruction();
                            let upvalue = Rc::new(RefCell::new(if is_local {
                                Upvalue::LocalOpen(stack_start + index)
                            } else {
                                let upvalue = self.frame().closure.borrow().as_closure().upvalues
                                    [index]
                                    .clone();
                                Upvalue::ForeignOpen(upvalue)
                            }));
                            if upvalue.borrow().is_local() {
                                self.open_upvalues.push(upvalue.clone());
                            }
                            closure.upvalues.push(upvalue);
                        }
                        self.push(Object::Closure(closure).into());
                    } else {
                        runtime_error!(self, "Closure constant must be a function.");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Upvalue(_, _) => {
                    runtime_error!(self, "Upvalue declaration only valid immediately following closure or upvalue instruction.");
                    return Err(InterpretError::RuntimeError);
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.pop();
                }
                OpCode::Return => {
                    let result = self.pop().unwrap();
                    self.close_upvalues(stack_start);
                    let frame = self.frame();
                    let stack_len = frame.stack_start;
                    self.stack.truncate(stack_len);
                    self.frames.pop();
                    if self.frames.is_empty() {
                        self.pop();
                        return Ok(());
                    }

                    self.push(result);
                }
                OpCode::Class(index) => {
                    let name = self.frame().get_constant(index);
                    if let Value::String(name) = name {
                        let class = Class::new(name);
                        self.push(Object::Class(class).into());
                    } else {
                        runtime_error!(self, "Class name must be a string.");
                        return Err(InterpretError::RuntimeError);
                    }
                }
                OpCode::Inherit => {
                    let superclass = self.peek_n(1).unwrap().clone();
                    let subclass = self.peek().unwrap().clone();

                    let superclass = superclass
                        .as_object()
                        .clone()
                        .map(|o| Ok(o))
                        .unwrap_or_else(|| {
                            runtime_error!(self, "Superclass must be a class.");
                            Err(InterpretError::RuntimeError)
                        })?;
                    let subclass = subclass.as_object().clone().unwrap();

                    let mut superclass = superclass.borrow_mut();
                    let mut subclass = subclass.borrow_mut();

                    let superclass = if matches!(*superclass, Object::Class(_)) {
                        superclass.as_class_mut()
                    } else {
                        runtime_error!(self, "Superclass must be a class.");
                        return Err(InterpretError::RuntimeError);
                    };

                    let subclass = subclass.as_class_mut();

                    subclass.methods().extend(
                        superclass
                            .methods()
                            .iter()
                            .map(|(k, v)| (k.clone(), v.clone())),
                    );

                    self.pop();
                }
                OpCode::Method(index) => {
                    let name = self.frame().get_constant(index);
                    let name = if let Value::String(name) = name {
                        name
                    } else {
                        runtime_error!(self, "Method name must be a string.");
                        return Err(InterpretError::RuntimeError);
                    };
                    let method = self.peek().unwrap().as_object().unwrap();
                    if !matches!(*method.borrow(), Object::Closure(_)) {
                        runtime_error!(self, "Method must be a closure.");
                        return Err(InterpretError::RuntimeError);
                    }
                    let class = self.peek_n(1).unwrap().clone();
                    class
                        .as_object()
                        .unwrap()
                        .borrow_mut()
                        .as_class_mut()
                        .insert_method(name, method.clone());
                    self.pop();
                }
            }
        }
    }
}
