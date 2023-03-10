use crate::tokens::{
    TIdentifier, Token,
    TokenType::{self, *},
};

// use phf::phf_map;

static KEYWORDS: phf::Map<&'static str, TokenType> = phf::phf_map! {
    "and" => And,
    "else" => Else,
    "false" => False,
    "for" => For,
    "fun" => Fun,
    "if" => If,
    "null" => Null,
    "or" => Or,
    "print" => Print,
    "return" => Return,
    "this" => This,
    "true" => True,
    "let" => Let,
    "while" => While,
    "_time" => DebugTokenTime,
};

pub struct Lexer {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    line_start: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            line_start: 0,
        }
    }

    pub fn reset(&mut self, source: &str) {
        self.source = source.chars().collect();
        self.tokens.clear();
        self.start = 0;
        self.current = 0;
        self.line = 1;
        self.line_start = 0;
    }

    pub fn tokenise(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.add_token(Eof);
        &self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(LeftParen),
            ')' => self.add_token(RightParen),
            '{' => self.add_token(LeftBrace),
            '}' => self.add_token(RightBrace),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            '+' => self.add_token(Plus),
            ';' => self.add_token(Semicolon),
            '*' => self.add_token(Star),
            '-' => self.add_token(Minus),
            '%' => self.add_token(Percentage),
            '!' => self.add_token_if_matched('=', BangEqual, Bang),
            '=' => self.add_token_if_matched('=', EqualEqual, Equal),
            '<' => self.add_token_if_matched('=', LessEqual, Less),
            '>' => self.add_token_if_matched('=', GreaterEqual, Greater),
            '/' => {
                if self.match_c('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_c('*') {
                    self.multiline_comment();
                } else {
                    self.add_token(Slash)
                }
            }
            '\n' => {
                self.add_token(Newline);
                self.line += 1;
                self.line_start = self.current;
            }
            ' ' | '\r' | '\t' => (),
            '"' => self.string(true),
            '\'' => self.string(false),
            c => {
                if c.is_ascii_digit() {
                    self.number()
                } else if c.is_alphabetic() || c == '_' {
                    self.identifier()
                } else {
                    panic!("unexpected character '{}' at {}", c, self.current)
                }
            }
        }
    }

    fn string(&mut self, is_double_quote: bool) {
        while if is_double_quote {
            self.peek() != '"'
        } else {
            self.peek() != '\''
        } && !self.is_at_end()
        {
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            panic!("Unterminated string")
        }

        let is_strict = self.advance();
        let is_strict = is_strict == '"';

        let str = self.source[self.start + 1..self.current - 1]
            .iter()
            .collect();
        if is_strict {
            self.add_token(TokenType::StrStrict(str));
        } else {
            self.add_token(TokenType::StrLoose(str));
        }
    }

    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let number = self.source[self.start..self.current]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap();
        self.add_token(Number(number));
    }

    fn identifier(&mut self) {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = self.source[self.start..self.current]
            .iter()
            .collect::<String>();

        let token = if let Some(t) = KEYWORDS.get(&text) {
            t.clone()
        } else {
            Identifier(TIdentifier(text))
        };

        self.add_token(token);
    }

    fn multiline_comment(&mut self) {
        let mut nesting_depth = 1;

        while nesting_depth != 0 && !self.is_at_end() {
            if self.match_c('/') && self.peek() == '*' {
                self.advance();
                nesting_depth += 1;
            } else if self.match_c('*') && self.peek() == '/' && !self.is_at_end() {
                self.advance();
                nesting_depth -= 1;
            } else if !self.is_at_end() {
                self.advance();
            }
        }

        if nesting_depth != 0 {
            panic!("Unterminated multiline comment.");
        }
    }

    fn add_token_if_matched(&mut self, c: char, if_matched: TokenType, fallback: TokenType) {
        if self.is_at_end() {
            self.add_token(fallback.clone())
        }

        if self.source[self.current] != c {
            self.add_token(fallback)
        } else {
            self.current += 1;
            self.add_token(if_matched)
        }
    }

    fn add_token(&mut self, token: TokenType) {
        let text = self.source[self.start..self.current]
            .iter()
            .collect::<String>();

        self.tokens.push(Token {
            t_type: token,
            lexeme: text,
            literal: None,
            line: self.line,
            col: (
                self.start + 1 - self.line_start,
                self.current - self.line_start,
            ),
        })
    }

    // fn add_token_literal(&mut self, token: Token) {
    //     let literal = self.source[self.start..self.current+1].iter().collect();
    //     match token {
    //         Identifier(_) => self.add_token(Identifier(literal)),
    //         Str(_) => self.add_token(Str(literal)),
    //         Number(_) => self.add_token(Number(literal.parse().unwrap())),
    //         t => panic!("called add_token_literal on a token without value: {:?}", t),
    //     }
    // }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        c
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn match_c(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source[self.current] != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source[self.current]
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }
}
