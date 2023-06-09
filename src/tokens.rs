use crate::expr::LiteralType;
use std::rc::Rc;
use string_interner::backend::StringBackend;
use string_interner::symbol;
use string_interner::StringInterner;

pub type StrSymbol = symbol::SymbolUsize;
pub type StrInterner = StringInterner<StringBackend<StrSymbol>>;

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
    Str(String),
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
pub struct TIdentifier(pub StrSymbol);

impl TIdentifier {
    pub fn new(interner: &mut StrInterner, s: &str) -> Self {
        Self(interner.get_or_intern(s))
    }

    #[inline(always)]
    pub fn inner(&self) -> StrSymbol {
        self.0
    }
}

pub mod tokens {
    use super::{StrInterner, StrSymbol, TIdentifier, TokenType};
    pub fn empty_ident(interner: &mut StrInterner) -> TokenType {
        TokenType::Identifier(TIdentifier(interner.get_or_intern("")))
    }
}
