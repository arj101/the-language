use crate::expr::LiteralType;

#[derive(Debug, Clone, PartialEq)]
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
    Percentage,

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

#[derive(Debug, Clone, PartialEq)]
pub struct TIdentifier(pub String);
impl Default for TIdentifier {
    fn default() -> Self {
        TIdentifier("".to_owned())
    }
}

pub mod tokens {
    use super::{TIdentifier, TokenType};

    #[inline(always)]
    pub fn empty_ident() -> TokenType {
        TokenType::Identifier(TIdentifier::default())
    }
}
