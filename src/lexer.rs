use crate::tokens::{
    StrInterner, TIdentifier, Token,
    TokenType::{self, *},
};

// use phf::phf_map;

// static KEYWORDS: phf::Map<&'static str, TokenType> = phf::phf_map! ;

pub struct Lexer {
    keywords: rustc_hash::FxHashMap<String, TokenType>,
    source: Vec<char>,
    source_str: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    line_start: usize,
    interner: StrInterner,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let mut keywords = rustc_hash::FxHashMap::default();

        macro_rules! insert {
            ($($key:expr => $val:expr),* $(,)?) => {
                $(
                    keywords.insert($key.to_string(),$val);
                )*
            }
        }

        insert! {
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

        Self {
            keywords,
            source: source.chars().collect(),
            source_str: source.to_string(),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            line_start: 0,
            interner: StrInterner::new(),
        }
    }

    pub fn reset(&mut self, source: &str) {
        self.source = source.chars().collect();
        self.source_str = source.to_string();
        self.tokens.clear();
        self.start = 0;
        self.current = 0;
        self.line = 1;
        self.line_start = 0;
    }

    pub fn tokenise(&mut self) -> (&Vec<Token>, StrInterner) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.add_token(Eof);
        (&self.tokens, self.interner.clone())
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        macro_rules! add_if_matched {
            ($c1:expr => $t1:tt $(, $cx:expr => $tx:tt)* $(, else $default:tt)? $(,)?) => {
                if self.match_c($c1) {
                    self.add_token($t1)
                }
                $(else if self.match_c($cx) {
                    self.add_token($tx)
                })*
                $(else {
                    self.add_token($default)
                })?
            }
        }

        match c {
            '(' => self.add_token(LeftParen),
            ')' => self.add_token(RightParen),
            '{' => self.add_token(LeftBrace),
            '}' => self.add_token(RightBrace),
            '[' => self.add_token(LeftSquareBrace),
            ']' => self.add_token(RightSquareBrace),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            '+' => add_if_matched!('=' => PlusEqual, else Plus),
            ';' => self.add_token(Semicolon),
            '*' => add_if_matched!('=' => StarEqual, '*' => StarStar, else Star),
            '-' => add_if_matched!('=' => MinusEqual, else Minus),
            '%' => add_if_matched!('=' => PercentageEqual, else Percentage),
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
                    add_if_matched!('=' => SlashEqual, else Slash)
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
        self.add_token(TokenType::Str(str));
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

        let text = &self.source_str[self.start..self.current];

        let token = if let Some(t) = self.keywords.get(text) {
            t.clone()
        } else {
            Identifier(TIdentifier::new(&mut self.interner, text))
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
