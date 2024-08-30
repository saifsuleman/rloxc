use num_derive::FromPrimitive;

#[derive(Clone, Copy, PartialEq, Debug, FromPrimitive)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Self {
            token_type,
            lexeme,
            line,
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn lexeme(&self) -> String {
        self.lexeme.clone()
    }

    pub fn token_type(&self) -> TokenType {
        self.token_type
    }

    pub fn error(line: usize, message: String) -> Self {
        Self {
            token_type: TokenType::Error,
            lexeme: message,
            line,
        }
    }
}
