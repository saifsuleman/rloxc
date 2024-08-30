use std::{mem::discriminant, rc::Rc};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::{
    chunk::{Chunk, OpCode},
    object::Function,
    scanner::Scanner,
    token::{Token, TokenType},
    value::Value,
    vm::{InterpretError, Result},
};

#[derive(FromPrimitive, Clone, Copy)]
pub enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

type ParseFn = for<'a> fn(&'a mut Compiler<'_>, bool);

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    pub const fn new(
        prefix: Option<ParseFn>,
        infix: Option<ParseFn>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }

    pub const fn get(token: TokenType) -> &'static Self {
        &RULES[token as usize]
    }
}

macro_rules! parse_handler {
    ($fun:ident) => {
        Some(|compiler: &mut Compiler<'_>, can_assign: bool| compiler.$fun(can_assign))
    };
}

macro_rules! patch_jump {
    ($compiler:ident, $offset:expr, $discriminant:ident) => {{
        let jump = $compiler.get_chunk().code.len() - 1;
        $compiler.patch_instruction($offset, OpCode::$discriminant(jump - $offset));
    }};
}

const RULES: [ParseRule; 40] = [
    /* LeftParen    */
    ParseRule::new(
        parse_handler!(grouping),
        parse_handler!(call),
        Precedence::Call,
    ),
    /* RightParen   */ ParseRule::new(None, None, Precedence::None),
    /* LeftBrace    */ ParseRule::new(None, None, Precedence::None),
    /* RightBrace   */ ParseRule::new(None, None, Precedence::None),
    /* Comma        */ ParseRule::new(None, None, Precedence::None),
    /* Dot          */ ParseRule::new(None, parse_handler!(dot), Precedence::Call),
    /* Minus        */
    ParseRule::new(
        parse_handler!(unary),
        parse_handler!(binary),
        Precedence::Term,
    ),
    /* Plus         */ ParseRule::new(None, parse_handler!(binary), Precedence::Term),
    /* Semicolon    */ ParseRule::new(None, None, Precedence::None),
    /* Slash        */ ParseRule::new(None, parse_handler!(binary), Precedence::Factor),
    /* Star         */ ParseRule::new(None, parse_handler!(binary), Precedence::Factor),
    /* Bang         */ ParseRule::new(parse_handler!(unary), None, Precedence::None),
    /* BangEqual    */ ParseRule::new(None, parse_handler!(binary), Precedence::Equality),
    /* Equal        */ ParseRule::new(None, None, Precedence::None),
    /* EqualEqual   */ ParseRule::new(None, parse_handler!(binary), Precedence::Equality),
    /* Greater      */ ParseRule::new(None, parse_handler!(binary), Precedence::Comparison),
    /* GreaterEqual */ ParseRule::new(None, parse_handler!(binary), Precedence::Comparison),
    /* Less         */ ParseRule::new(None, parse_handler!(binary), Precedence::Comparison),
    /* LessEqual    */ ParseRule::new(None, parse_handler!(binary), Precedence::Comparison),
    /* Identifier   */ ParseRule::new(parse_handler!(variable), None, Precedence::None),
    /* String       */ ParseRule::new(parse_handler!(string), None, Precedence::None),
    /* Number       */ ParseRule::new(parse_handler!(number), None, Precedence::None),
    /* And          */ ParseRule::new(None, parse_handler!(and), Precedence::None),
    /* Class        */ ParseRule::new(None, None, Precedence::None),
    /* Else         */ ParseRule::new(None, None, Precedence::None),
    /* False        */ ParseRule::new(parse_handler!(literal), None, Precedence::None),
    /* For          */ ParseRule::new(None, None, Precedence::None),
    /* Fun          */ ParseRule::new(None, None, Precedence::None),
    /* If           */ ParseRule::new(None, None, Precedence::None),
    /* Nil          */ ParseRule::new(parse_handler!(literal), None, Precedence::None),
    /* Or           */ ParseRule::new(None, parse_handler!(or), Precedence::None),
    /* Print        */ ParseRule::new(None, None, Precedence::None),
    /* Return       */ ParseRule::new(None, None, Precedence::None),
    /* Super        */ ParseRule::new(parse_handler!(super_), None, Precedence::None),
    /* This         */ ParseRule::new(parse_handler!(this), None, Precedence::None),
    /* True         */ ParseRule::new(parse_handler!(literal), None, Precedence::None),
    /* Var          */ ParseRule::new(None, None, Precedence::None),
    /* While        */ ParseRule::new(None, None, Precedence::None),
    /* Error        */ ParseRule::new(None, None, Precedence::None),
    /* EOF          */ ParseRule::new(None, None, Precedence::None),
];

#[derive(Clone)]
struct Local {
    name: Token,
    depth: usize,
    captured: bool,
}

pub struct Upvalue {
    index: usize,
    is_local: bool,
}

impl Upvalue {
    pub fn new(index: usize, is_local: bool) -> Self {
        Self { index, is_local }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum FunctionType {
    Function,
    Method,
    Initializer,
}

struct CompileContext {
    parent: Option<Box<CompileContext>>,
    function: Function,
    locals: Vec<Local>,
    scope_depth: usize,
    function_type: FunctionType,
}

impl CompileContext {
    fn parent(&mut self) -> Option<&mut CompileContext> {
        self.parent.as_deref_mut()
    }
    fn resolve_local(&self, name: &Token) -> Option<usize> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if name.lexeme() == local.name.lexeme() {
                return if local.depth == usize::MAX {
                    None
                } else {
                    Some(i)
                };
            }
        }

        None
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        let upvalues = &mut self.function.upvalues;

        upvalues
            .iter()
            .enumerate()
            .find(|(_, upvalue)| upvalue.index == index && upvalue.is_local == is_local)
            .map(|(i, _)| i)
            .unwrap_or_else(|| {
                upvalues.push(Upvalue::new(index, is_local));
                upvalues.len() - 1
            })
    }

    fn resolve_upvalue(&mut self, name: Token) -> Option<usize> {
        let parent = self.parent()?;
        let local = parent.resolve_local(&name);

        if let Some(local) = local {
            parent.locals[local].captured = true;
            return Some(self.add_upvalue(local, true));
        }

        let index = parent.resolve_upvalue(name)?;
        return Some(self.add_upvalue(index, false));
    }
}

pub struct ClassContext {
    parent: Option<Box<ClassContext>>,
    has_superclass: bool,
}

pub struct Compiler<'a> {
    context: Option<CompileContext>,
    class: Option<ClassContext>,
    scanner: Scanner<'a>,
    current: Option<Token>,
    previous: Option<Token>,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        let scanner = Scanner::new(source);

        Self {
            context: None,
            class: None,
            scanner,
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
        }
    }

    fn add_instruction(&mut self, instruction: OpCode) {
        let line = self.previous.as_ref().unwrap().line();
        let chunk = self.get_chunk();
        chunk.write_chunk(instruction, line);
    }

    fn add_jump(&mut self, instruction: OpCode) -> usize {
        self.add_instruction(instruction);

        self.get_chunk().code.len() - 1
    }

    fn add_loop(&mut self, loop_start: usize) {
        let offset = self.get_chunk().code.len() - loop_start + 1;
        self.add_instruction(OpCode::Loop(offset))
    }

    fn patch_instruction(&mut self, address: usize, instruction: OpCode) {
        let chunk = self.get_chunk();
        chunk.code[address] = instruction;
    }

    fn add_return(&mut self) {
        let opcode = if self.context().function_type == FunctionType::Initializer {
            OpCode::GetLocal(0)
        } else {
            OpCode::Nil
        };
        self.add_instruction(opcode);
        self.add_instruction(OpCode::Return);
    }

    fn add_constant(&mut self, value: Value) {
        let index = self.get_chunk().set_constant(value);
        self.add_instruction(OpCode::Constant(index));
    }

    pub fn compile(&mut self) -> Result<Function> {
        self.context = Some(CompileContext {
            parent: None,
            function: Function::new(),
            locals: Vec::new(),
            scope_depth: 0,
            function_type: FunctionType::Function,
        });
        self.context().locals.push(Local {
            name: Self::synthetic_token(""),
            depth: 0,
            captured: false,
        });

        self.advance();

        while !self.match_token(TokenType::EOF) {
            self.declaration();
        }

        if self.had_error {
            Err(InterpretError::CompileError)
        } else {
            Ok(self.end_context())
        }
    }

    fn string(&mut self, _: bool) {
        let string = self.previous.as_ref().unwrap().lexeme();
        self.add_constant(Value::String(string[1..string.len() - 1].to_string()));
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) {
        let arg = self.context().resolve_local(&name);

        let context = self.context();
        let (set_op, get_op) = arg
            .map(|arg| (OpCode::SetLocal(arg), OpCode::GetLocal(arg)))
            .or_else(|| {
                context
                    .resolve_upvalue(name.clone())
                    .map(|index| (OpCode::SetUpvalue(index), OpCode::GetUpvalue(index)))
            })
            .unwrap_or_else(|| {
                let index = self
                    .get_chunk()
                    .set_constant(Value::String(name.lexeme().clone()));
                (OpCode::SetGlobal(index), OpCode::GetGlobal(index))
            });

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.add_instruction(set_op);
        } else {
            self.add_instruction(get_op);
        }
    }

    fn variable(&mut self, can_assign: bool) {
        let name = self.previous.as_ref().unwrap().clone();
        self.named_variable(name, can_assign);
    }

    fn synthetic_token(identifier: &str) -> Token {
        Token::new(TokenType::Identifier, identifier.to_string(), 0)
    }

    fn super_(&mut self, _: bool) {
        if self.class.is_none() {
            self.error("Cannot use 'super' outside of a class.");
        } else if !self.class.as_ref().unwrap().has_superclass {
            self.error("Cannot use 'super' in a class with no superclass.");
        }

        self.consume(TokenType::Dot, "Expect '.' after 'super'.");
        self.consume(TokenType::Identifier, "Expect superclass method name.");
        let name = self.previous.as_ref().unwrap().clone().lexeme();
        let name = self.get_chunk().set_constant(Value::String(name));

        self.named_variable(Self::synthetic_token("this"), false);
        if self.match_token(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.named_variable(Self::synthetic_token("super"), false);
            self.add_instruction(OpCode::SuperInvoke(name, arg_count));
        } else {
            self.named_variable(Self::synthetic_token("super"), false);
            self.add_instruction(OpCode::GetSuper(name));
        }
    }

    fn this(&mut self, _: bool) {
        if self.class.is_none() {
            self.error("Cannot use 'this' outside of a class.");
            return;
        }

        self.variable(false);
    }

    fn literal(&mut self, _: bool) {
        match self.previous.as_ref().unwrap().token_type() {
            TokenType::False => self.add_instruction(OpCode::False),
            TokenType::True => self.add_instruction(OpCode::True),
            TokenType::Nil => self.add_instruction(OpCode::Nil),
            _ => (),
        }
    }

    fn binary(&mut self, _: bool) {
        let operator = self.previous.as_ref().unwrap().token_type();
        let rule = ParseRule::get(operator);

        let index = rule.precedence as usize + 1;
        self.parse_precedence(Precedence::from_usize(index).unwrap());

        match operator {
            TokenType::BangEqual => {
                self.add_instruction(OpCode::Equal);
                self.add_instruction(OpCode::Not);
            }
            TokenType::EqualEqual => self.add_instruction(OpCode::Equal),
            TokenType::Greater => self.add_instruction(OpCode::Greater),
            TokenType::GreaterEqual => {
                self.add_instruction(OpCode::Less);
                self.add_instruction(OpCode::Not);
            }
            TokenType::Less => self.add_instruction(OpCode::Less),
            TokenType::LessEqual => {
                self.add_instruction(OpCode::Greater);
                self.add_instruction(OpCode::Not);
            }
            TokenType::Plus => self.add_instruction(OpCode::Add),
            TokenType::Minus => self.add_instruction(OpCode::Subtract),
            TokenType::Star => self.add_instruction(OpCode::Multiply),
            TokenType::Slash => self.add_instruction(OpCode::Divide),
            _ => (),
        }
    }

    fn call(&mut self, _: bool) {
        let arg_count = self.argument_list();
        self.add_instruction(OpCode::Call(arg_count));
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self.previous.as_ref().unwrap().clone().lexeme();
        let name = self.get_chunk().set_constant(Value::String(name));

        if can_assign && self.match_token(TokenType::Equal) {
            self.expression();
            self.add_instruction(OpCode::SetProperty(name))
        } else if self.match_token(TokenType::LeftParen) {
            let arg_count = self.argument_list();
            self.add_instruction(OpCode::Invoke(name, arg_count));
        } else {
            self.add_instruction(OpCode::GetProperty(name));
        }
    }

    fn unary(&mut self, _: bool) {
        let operator_type = self.previous.as_ref().unwrap().token_type();

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => self.add_instruction(OpCode::Negate),
            TokenType::Bang => self.add_instruction(OpCode::Not),
            _ => (),
        }
    }

    fn grouping(&mut self, _: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn number(&mut self, _: bool) {
        let number = self.previous.as_ref().unwrap().lexeme().parse().unwrap();
        self.add_constant(Value::Number(number));
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn function(&mut self, function_type: FunctionType) {
        self.begin_context(function_type);
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                self.context().function.arity += 1;
                let param = self.parse_variable("Expect parameter name.");
                self.define_variable(param);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();

        let function = Rc::new(self.end_context());
        let constant = self
            .get_chunk()
            .set_constant(Value::Function(function.clone()));
        self.add_instruction(OpCode::Closure(constant));

        function.upvalues.iter().for_each(|upvalue| {
            self.add_instruction(OpCode::Upvalue(upvalue.is_local, upvalue.index));
        });
    }

    fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expect method name.");
        let name = self.previous.as_ref().unwrap().lexeme().clone();
        let name = self.get_chunk().set_constant(Value::String(name));

        let function_type = if self.previous.as_ref().unwrap().lexeme() == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };
        self.function(function_type);
        self.add_instruction(OpCode::Method(name));
    }

    fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expect class name.");
        let name_token = self.previous.as_ref().unwrap().clone();
        let name = name_token.lexeme().clone();
        let name = self.get_chunk().set_constant(Value::String(name));
        self.declare_variable();

        self.add_instruction(OpCode::Class(name));
        self.define_variable(name);

        let class_context = ClassContext {
            parent: self.class.take().map(Box::new),
            has_superclass: false,
        };
        self.class = Some(class_context);

        if self.match_token(TokenType::Less) {
            self.consume(TokenType::Identifier, "Expect superclass name.");
            self.variable(false);

            let last = self.previous.clone().unwrap().lexeme();
            if name_token.lexeme() == last {
                self.error("A class cannot inherit from itself.");
            }

            self.begin_scope();
            self.add_local(Self::synthetic_token("super"));
            self.define_variable(0);

            self.named_variable(name_token.clone(), false);
            self.add_instruction(OpCode::Inherit);
            self.class.as_mut().unwrap().has_superclass = true;
        }

        self.named_variable(name_token, false);
        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");
        while !self.check(TokenType::RightBrace) & !self.check(TokenType::EOF) {
            self.method();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after class body.");
        self.add_instruction(OpCode::Pop);

        if self.class.as_ref().unwrap().has_superclass {
            self.end_scope();
        }

        self.class = self.class.take().unwrap().parent.map(|parent| *parent);
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.add_instruction(OpCode::Print);
    }

    fn return_statement(&mut self) {
        if self.context().scope_depth == 0 {
            self.error("Cannot return from top-level code.");
        }

        if self.match_token(TokenType::Semicolon) {
            self.add_return();
        } else {
            if self.context().function_type == FunctionType::Initializer {
                self.error("Cannot return a value from an initializer.");
            }

            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.add_instruction(OpCode::Return);
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.get_chunk().code.len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.add_jump(OpCode::Nil);
        self.add_instruction(OpCode::Pop);
        self.statement();
        self.add_loop(loop_start);

        patch_jump!(self, exit_jump, JumpIfFalse);
        self.add_instruction(OpCode::Pop);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.add_instruction(OpCode::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.match_token(TokenType::Semicolon) {
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.get_chunk().code.len();
        let exit_jump = if !self.match_token(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");
            let jump = self.add_jump(OpCode::Nil);
            self.add_instruction(OpCode::Pop);

            Some(jump)
        } else {
            None
        };

        if !self.match_token(TokenType::RightParen) {
            let body_jump = self.add_jump(OpCode::Nil);
            let increment_start = self.get_chunk().code.len();
            self.expression();
            self.add_instruction(OpCode::Pop);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.add_loop(loop_start);
            loop_start = increment_start;
            patch_jump!(self, body_jump, Jump);
        }

        self.statement();
        self.add_loop(loop_start);

        if let Some(exit_jump) = exit_jump {
            patch_jump!(self, exit_jump, JumpIfFalse);
            self.add_instruction(OpCode::Pop);
        }

        self.end_scope();
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.add_jump(OpCode::Nil);
        self.add_instruction(OpCode::Pop);
        self.statement();
        let else_jump = self.add_jump(OpCode::Nil);
        patch_jump!(self, then_jump, JumpIfFalse);
        self.add_instruction(OpCode::Pop);

        if self.match_token(TokenType::Else) {
            self.statement();
        }
        patch_jump!(self, else_jump, Jump);
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Print) {
            self.print_statement();
        } else if self.match_token(TokenType::For) {
            self.for_statement();
        } else if self.match_token(TokenType::If) {
            self.if_statement();
        } else if self.match_token(TokenType::Return) {
            self.return_statement();
        } else if self.match_token(TokenType::While) {
            self.while_statement();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.match_token(TokenType::Equal) {
            self.expression();
        } else {
            self.add_instruction(OpCode::Nil);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Class) {
            self.class_declaration();
        } else if self.match_token(TokenType::Fun) {
            self.fun_declaration();
        } else if self.match_token(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while !self.check(TokenType::EOF) {
            match self.previous.as_ref().unwrap().token_type() {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }
            self.advance();
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = match ParseRule::get(self.previous.as_ref().unwrap().token_type()).prefix
        {
            Some(prefix) => prefix,
            None => {
                self.error("Expect expression.");
                return;
            }
        };

        let can_assign = precedence as usize <= Precedence::Assignment as usize;
        prefix_rule(self, can_assign);

        while precedence as usize
            <= ParseRule::get(self.current.as_ref().unwrap().token_type()).precedence as usize
        {
            self.advance();
            if let Some(infix_rule) =
                ParseRule::get(self.previous.as_ref().unwrap().token_type()).infix
            {
                infix_rule(self, can_assign);
            }

            if can_assign && self.match_token(TokenType::Equal) {
                self.error("Invalid assignment target.");
            }
        }
    }

    fn parse_variable(&mut self, error_message: &str) -> usize {
        self.consume(TokenType::Identifier, error_message);

        self.declare_variable();

        if self.context().scope_depth > 0 {
            0
        } else {
            let previous = self.previous.as_ref().unwrap().lexeme().clone();
            self.get_chunk().set_constant(Value::String(previous))
        }
    }

    fn mark_initialized(&mut self) {
        if self.context().scope_depth == 0 {
            return;
        }
        self.context().locals.last_mut().unwrap().depth = self.context().scope_depth;
    }

    fn define_variable(&mut self, global: usize) {
        if self.context().scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.add_instruction(OpCode::DefineGlobal(global));
    }

    fn argument_list(&mut self) -> usize {
        let mut arg_count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                arg_count += 1;

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");

        arg_count
    }

    fn and(&mut self, _: bool) {
        let end_jump = self.add_jump(OpCode::Nil);

        self.add_instruction(OpCode::Pop);
        self.parse_precedence(Precedence::And);

        patch_jump!(self, end_jump, JumpIfFalse);
    }

    fn or(&mut self, _: bool) {
        let else_jump = self.add_jump(OpCode::Nil);
        let end_jump = self.add_jump(OpCode::Nil);

        patch_jump!(self, else_jump, JumpIfFalse);
        self.add_instruction(OpCode::Pop);

        self.parse_precedence(Precedence::Or);
        patch_jump!(self, end_jump, JumpIfFalse);
    }

    fn begin_context(&mut self, function_type: FunctionType) {
        let parent = self.context.take().unwrap();
        self.context = Some(CompileContext {
            parent: Some(Box::new(parent)),
            function: Function::new(),
            locals: Vec::new(),
            scope_depth: 0,
            function_type,
        });
        self.context().function.name = self.previous.as_ref().unwrap().lexeme().clone();
        let name = if matches!(
            function_type,
            FunctionType::Method | FunctionType::Initializer
        ) {
            "this"
        } else {
            ""
        };
        self.context().locals.push(Local {
            name: Self::synthetic_token(name),
            depth: 0,
            captured: false,
        });
    }

    fn end_context(&mut self) -> Function {
        self.add_return();

        #[cfg(debug_print_code)]
        {
            if !self.had_error {
                println!("{:?}", self.context().function);
            }
        }

        let context = self.context.take().unwrap();

        if let Some(parent) = context.parent {
            self.context = Some(*parent);
        }

        context.function
    }

    fn context(&mut self) -> &mut CompileContext {
        self.context.as_mut().unwrap()
    }

    fn get_chunk(&mut self) -> &mut Chunk {
        self.context().function.get_chunk_mut()
    }

    fn begin_scope(&mut self) {
        self.context().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.context().scope_depth -= 1;

        while self.context().locals.len() > 0
            && self.context().locals.last().unwrap().depth > self.context().scope_depth
        {
            let opcode = if self.context().locals.last().unwrap().captured {
                OpCode::CloseUpvalue
            } else {
                OpCode::Pop
            };

            self.add_instruction(opcode);
            self.context().locals.pop();
        }
    }

    fn add_local(&mut self, name: Token) {
        self.context().locals.push(Local {
            name,
            depth: usize::MAX,
            captured: false,
        })
    }

    fn declare_variable(&mut self) {
        if self.context().scope_depth == 0 {
            return;
        }

        let name = self.previous.as_ref().unwrap().clone();
        let mut err = false;
        let scope_depth = self.context().scope_depth;
        for local in self.context().locals.iter().rev() {
            if local.depth != usize::MAX && local.depth < scope_depth {
                break;
            }

            if name.lexeme() == local.name.lexeme() {
                err = true;
                break;
            }
        }
        if err {
            self.error("Variable with this name already declared in this scope.");
        }
        self.add_local(name);
    }

    fn advance(&mut self) {
        self.previous = self.current.take();

        loop {
            self.current = Some(self.scanner.scan_token());
            if self.current.as_ref().unwrap().token_type() != TokenType::Error {
                break;
            }

            let message = self.current.as_ref().unwrap().lexeme().clone();
            self.error_at_current(message.as_str());
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.current.as_ref().unwrap().token_type() == token_type {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            return false;
        }

        self.advance();
        true
    }

    fn check(&self, token_type: TokenType) -> bool {
        discriminant(&self.current.as_ref().unwrap().token_type()) == discriminant(&token_type)
    }

    fn error_at_current(&mut self, message: &str) {
        if let Some(current) = self.current.clone() {
            self.error_at(&current, message);
        }
    }

    fn error(&mut self, message: &str) {
        let token = self.previous.as_ref().unwrap().clone();
        self.error_at(&token, message);
    }

    fn error_at(&mut self, token: &Token, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;

        eprint!("[line {}] Error", token.line());

        match token.token_type() {
            TokenType::EOF => eprint!(" at EOF"),
            TokenType::Error => (),
            _ => eprint!(" at '{}'", token.lexeme()),
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }
}

pub fn compile(source: &str) -> Result<Function> {
    let mut compiler = Compiler::new(source);

    compiler.compile()
}
