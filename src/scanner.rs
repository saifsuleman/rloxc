use crate::token::{Token, TokenType};

use fst::{Map, MapBuilder};
use num_traits::FromPrimitive;

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
    reserved_words: Map<Vec<u8>>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut buffer = Vec::new();
        let mut builder = MapBuilder::new(&mut buffer).unwrap();
        builder.insert("and", TokenType::And as u64).unwrap();
        builder.insert("class", TokenType::Class as u64).unwrap();
        builder.insert("else", TokenType::Else as u64).unwrap();
        builder.insert("false", TokenType::False as u64).unwrap();
        builder.insert("for", TokenType::For as u64).unwrap();
        builder.insert("fun", TokenType::Fun as u64).unwrap();
        builder.insert("if", TokenType::If as u64).unwrap();
        builder.insert("nil", TokenType::Nil as u64).unwrap();
        builder.insert("or", TokenType::Or as u64).unwrap();
        builder.insert("print", TokenType::Print as u64).unwrap();
        builder.insert("return", TokenType::Return as u64).unwrap();
        builder.insert("super", TokenType::Super as u64).unwrap();
        builder.insert("this", TokenType::This as u64).unwrap();
        builder.insert("true", TokenType::True as u64).unwrap();
        builder.insert("var", TokenType::Var as u64).unwrap();
        builder.insert("while", TokenType::While as u64).unwrap();
        builder.finish().unwrap();
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
            reserved_words: Map::new(buffer).unwrap(),
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if !self.more_tokens() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.advance();

        true
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();
        self.start = self.current;

        if !self.more_tokens() {
            return self.make_token(TokenType::EOF);
        }

        let c = self.advance();

        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' if self.match_char('=') => self.make_token(TokenType::BangEqual),
            '!' => self.make_token(TokenType::Bang),
            '=' if self.match_char('=') => self.make_token(TokenType::EqualEqual),
            '=' => self.make_token(TokenType::Equal),
            '<' if self.match_char('=') => self.make_token(TokenType::LessEqual),
            '<' => self.make_token(TokenType::Less),
            '>' if self.match_char('=') => self.make_token(TokenType::GreaterEqual),
            '>' => self.make_token(TokenType::Greater),
            '"' => self.string(),
            c if c.is_numeric() => self.number(),
            c if c.is_alphabetic() => self.identifier(),
            _ => Token::error(self.line, format!("Unexpected character '{c}'").to_owned()),
        }
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() || self.peek() == '_' || self.peek() == '-' {
            self.advance();
        }

        let slice = &self.source[self.start..self.current];

        let token_type = match self.reserved_words.get(slice.as_bytes()) {
            Some(token_type) => FromPrimitive::from_u64(token_type).unwrap(),
            None => TokenType::Identifier,
        };

        self.make_token(token_type)
    }

    fn number(&mut self) -> Token {
        while self.peek().is_numeric() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_numeric() {
            self.advance();

            while self.peek().is_numeric() {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' && self.more_tokens() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if !self.more_tokens() {
            return Token::error(self.line, "Unterminated string.".to_owned());
        }

        self.advance();

        self.make_token(TokenType::String)
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' if self.peek_next() == '/' => {
                    while self.peek() != '\n' && self.more_tokens() {
                        self.advance();
                    }
                }
                _ => return,
            }
        }
    }

    fn peek(&self) -> char {
        if !self.more_tokens() {
            return '\0';
        }
        self.source.chars().nth(self.current).unwrap()
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.chars().count() {
            return '\0';
        }
        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn make_token(&mut self, token_type: TokenType) -> Token {
        let lexeme = self
            .source
            .chars()
            .skip(self.start)
            .take(self.current - self.start)
            .collect();
        Token::new(token_type, lexeme, self.line)
    }

    fn more_tokens(&self) -> bool {
        self.current < self.source.chars().count()
    }
}
