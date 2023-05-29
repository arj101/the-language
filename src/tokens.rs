use crate::expr::LiteralType;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    LeftSquareBrace,
    RightSquareBrace,

    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Percentage,

    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentageEqual,

    StarStar,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(TIdentifier),
    StrLoose(String),
    StrStrict(String),
    Number(f64),

    Fun,
    This,

    If,
    Else,
    While,
    Do,
    For,
    Let,
    Print,
    True,
    False,
    Return,
    Null,

    And,
    Or,

    Newline,
    Eof,

    DebugTokenTime,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub t_type: TokenType,
    pub lexeme: String,
    pub literal: Option<LiteralType>,
    pub line: usize,
    pub col: (usize, usize),
}

impl Token {
    pub fn into_new(&self, t_type: TokenType) -> Self {
        let mut new = self.clone();
        new.t_type = t_type;
        new
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TIdentifier(pub Rc<String>);

impl TIdentifier {
    pub fn new(s: String) -> Self {
        Self(Rc::new(s))
    }
    pub fn inner(&self) -> &Rc<String> {
        &self.0
    }
}

pub mod tokens {
    use super::{TIdentifier, TokenType};
    pub fn empty_ident() -> TokenType {
        TokenType::Identifier(TIdentifier::new("".to_string()))
    }
}
