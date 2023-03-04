mod parser {
    use crate::expr::{DebugVariable, Expr, LiteralType, Stmt, StrType};
    use crate::tokens::TIdentifier;
    use crate::tokens::Token;
    use crate::tokens::TokenType::{self, *};
    use idk::track;
    use idk::tracker_drive_struct;
    pub struct Parser {
        code_lines: Vec<String>,
        tokens: Vec<Token>,
        current: usize,
        parse_info: Vec<Vec<String>>,
        curr_tree: Vec<String>,
        tracking_stack: Vec<
            Vec<(String, std::time::Instant, Option<std::time::Duration>)>,
        >,
        curr_stack: Vec<(String, std::time::Instant, Option<std::time::Duration>)>,
    }
    impl Parser {
        pub fn new(tokens: Vec<Token>, source: String) -> Self {
            Self {
                tokens,
                current: 0,
                code_lines: source.lines().map(|l| l.to_owned()).collect(),
                parse_info: ::alloc::vec::Vec::new(),
                curr_tree: ::alloc::vec::Vec::new(),
                curr_stack: ::alloc::vec::Vec::new(),
                tracking_stack: ::alloc::vec::Vec::new(),
            }
        }
        pub fn reset(&mut self, tokens: Vec<Token>, source: String) {
            self.code_lines = source.lines().map(|l| l.to_owned()).collect();
            self.tokens = tokens;
            self.current = 0;
        }
        pub fn parse(&mut self) -> Vec<Stmt> {
            self.curr_stack.push(("parse".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("parse");
                let mut statements = ::alloc::vec::Vec::new();
                while !self.is_at_end() {
                    statements.push(self.declaration());
                    self.consume_end_of_line("expected EOL character");
                }
                self.exit_fn(statements)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn declaration(&mut self) -> Stmt {
            self.curr_stack
                .push(("declaration".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("declaration");
                let stmt = if self.match_t(&[TokenType::Let]) {
                    self.let_decl()
                } else {
                    self.statement()
                };
                self.exit_fn(stmt)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn let_decl(&mut self) -> Stmt {
            self.curr_stack
                .push(("let_decl".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("let_decl");
                let name = self
                    .consume(
                        &TokenType::Identifier(TIdentifier("".to_owned())),
                        "Expected variable name.",
                    );
                let name = if let TokenType::Identifier(name) = &name.t_type {
                    name.clone()
                } else {
                    ::core::panicking::panic("internal error: entered unreachable code")
                };
                let mut intializer = Expr::Literal(LiteralType::Null);
                if self.match_t(&[TokenType::Equal]) {
                    intializer = self.expression();
                }
                self.exit_fn(Stmt::Decl {
                    id: name,
                    expr: intializer,
                })
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn statement(&mut self) -> Stmt {
            self.curr_stack
                .push(("statement".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("statement");
                let stmt = if self.match_t(&[TokenType::If]) {
                    self.if_stmt()
                } else if self
                    .match_t(&[TokenType::While, TokenType::For, TokenType::Do])
                {
                    self.control_flow_loop()
                } else if self.match_t(&[TokenType::Print]) {
                    self.print_stmt()
                } else if self.match_t(&[TokenType::LeftBrace]) {
                    self.block()
                } else if self
                    .check_n(&TokenType::Identifier(TIdentifier("".to_owned())))
                {
                    self.assignment_stmt()
                } else {
                    self.match_t(&[TokenType::Fun]);
                    self.expr_stmt()
                };
                self.exit_fn(stmt)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn assignment_stmt(&mut self) -> Stmt {
            self.curr_stack
                .push(("assignment_stmt".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("assignment_stmt");
                let name = self
                    .consume(
                        &TokenType::Identifier(TIdentifier("".to_owned())),
                        "Expected variable name.",
                    );
                let name = if let TokenType::Identifier(name) = &name.t_type {
                    name.clone()
                } else {
                    ::core::panicking::panic("internal error: entered unreachable code")
                };
                let mut intializer = Expr::Literal(LiteralType::Null);
                if self.match_t(&[TokenType::Equal]) {
                    intializer = self.expression()
                } else {
                    self.report_token(&self.tokens[self.current]);
                    ::core::panicking::panic_fmt(
                        ::core::fmt::Arguments::new_v1(
                            &["Expected value in assignment statement."],
                            &[],
                        ),
                    );
                }
                self.exit_fn(Stmt::Assignment {
                    id: name,
                    expr: intializer,
                })
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn control_flow_loop(&mut self) -> Stmt {
            self.curr_stack
                .push((
                    "control_flow_loop".to_string(),
                    std::time::Instant::now(),
                    None,
                ));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("control_flow_loop");
                let stmt = match self.previous().t_type {
                    TokenType::While => {
                        let expr = self.expression();
                        self.consume(&TokenType::LeftBrace, "Expected block");
                        let body = Box::new(self.block());
                        Stmt::Loop {
                            entry_controlled: true,
                            init: None,
                            expr,
                            updation: None,
                            body,
                        }
                    }
                    TokenType::For => {
                        let init = Some(Box::new(self.declaration()));
                        self.consume_end_of_line(
                            "Expected end of line character after loop initialisation",
                        );
                        let expr = self.expression();
                        self.consume_end_of_line(
                            "Expected end of line character after loop expression",
                        );
                        let updation = Some(Box::new(self.statement()));
                        self.consume(&TokenType::LeftBrace, "Expected block");
                        let body = Box::new(self.block());
                        Stmt::Loop {
                            entry_controlled: true,
                            init,
                            expr,
                            updation,
                            body,
                        }
                    }
                    TokenType::Do => {
                        self.consume(&TokenType::LeftBrace, "Expected block");
                        let body = Box::new(self.block());
                        self.consume(
                            &TokenType::While,
                            "Expected 'while' after Do loop body",
                        );
                        let expr = self.expression();
                        self.consume_end_of_line("Expected EOL after loop condition");
                        Stmt::Loop {
                            entry_controlled: false,
                            init: None,
                            expr,
                            updation: None,
                            body,
                        }
                    }
                    _ => {
                        ::core::panicking::panic(
                            "internal error: entered unreachable code",
                        )
                    }
                };
                self.exit_fn(stmt)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn if_stmt(&mut self) -> Stmt {
            self.curr_stack
                .push(("if_stmt".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("if_stmt");
                let expr = self.expression();
                self.consume(&LeftBrace, "Expected Block");
                let if_block = Box::new(self.block());
                let else_block = if self.match_t(&[Else]) {
                    Some(Box::new(self.statement()))
                } else {
                    None
                };
                self.exit_fn(Stmt::If {
                    expr,
                    if_block,
                    else_block,
                })
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn print_stmt(&mut self) -> Stmt {
            self.curr_stack
                .push(("print_stmt".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("print_stmt");
                let value = self.expression();
                let val = Stmt::Print(value);
                self.exit_fn(val)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn expr_stmt(&mut self) -> Stmt {
            self.curr_stack
                .push(("expr_stmt".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("expr_stmt");
                let expression = self.expression();
                self.exit_fn(Stmt::ExprStmt(expression))
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn block(&mut self) -> Stmt {
            self.curr_stack.push(("block".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("block");
                let mut statements = ::alloc::vec::Vec::new();
                while !self.check_n(&RightBrace) && !self.is_at_end() {
                    statements.push(self.declaration());
                    if !self.check_n(&RightBrace) {
                        self.consume_end_of_line("Expected EOL character");
                    }
                }
                self.consume(&RightBrace, "Expected '}' after block.");
                self.exit_fn(Stmt::Block(statements))
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn expression(&mut self) -> Expr {
            self.curr_stack
                .push(("expression".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("expression");
                let expr = self.equality();
                self.exit_fn(expr)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn equality(&mut self) -> Expr {
            self.curr_stack
                .push(("equality".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("equality");
                let mut expr = self.comparison();
                while self.match_t(&[BangEqual, EqualEqual]) {
                    let operator = self.previous().clone();
                    let right = Box::new(self.comparison());
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
                        right,
                    };
                }
                self.exit_fn(expr)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn comparison(&mut self) -> Expr {
            self.curr_stack
                .push(("comparison".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("comparison");
                let mut expr = self.term();
                while self.match_t(&[Greater, GreaterEqual, Less, LessEqual]) {
                    let operator = self.previous().clone();
                    let right = Box::new(self.term());
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
                        right,
                    };
                }
                self.exit_fn(expr)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn term(&mut self) -> Expr {
            self.curr_stack.push(("term".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("term");
                let mut expr = self.factor();
                while self.match_t(&[Plus, Minus, Percentage]) {
                    let operator = self.previous().clone();
                    let right = Box::new(self.factor());
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
                        right,
                    };
                }
                self.exit_fn(expr)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn factor(&mut self) -> Expr {
            self.curr_stack
                .push(("factor".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("factor");
                let mut expr = self.unary();
                while self.match_t(&[Slash, Star]) {
                    let operator = self.previous().clone();
                    let right = Box::new(self.unary());
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        operator,
                        right,
                    };
                }
                self.exit_fn(expr)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn unary(&mut self) -> Expr {
            self.curr_stack.push(("unary".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("unary");
                let expr = if self.match_t(&[Bang, Minus]) {
                    let operator = self.previous().clone();
                    let right = Box::new(self.unary());
                    Expr::Unary { operator, right }
                } else {
                    self.primary()
                };
                self.exit_fn(expr)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn primary(&mut self) -> Expr {
            self.curr_stack
                .push(("primary".to_string(), std::time::Instant::now(), None));
            self.tracking_stack.push(self.curr_stack.clone());
            let rt_val = (|| {
                self.enter_fn("primary");
                let expr = {
                    if self.match_t(&[False]) {
                        return Expr::Literal(LiteralType::Bool(false));
                    }
                    if self.match_t(&[True]) {
                        return Expr::Literal(LiteralType::Bool(true));
                    }
                    if self.match_t(&[Null]) {
                        return Expr::Literal(LiteralType::Null);
                    }
                    if self.match_t(&[StrStrict(String::new()), StrLoose(String::new())])
                    {
                        return match &self.previous().t_type {
                            StrStrict(s) => {
                                Expr::Literal(LiteralType::Str(StrType::Strict(s.clone())))
                            }
                            StrLoose(s) => {
                                Expr::Literal(LiteralType::Str(StrType::Loose(s.clone())))
                            }
                            _ => {
                                ::core::panicking::panic(
                                    "internal error: entered unreachable code",
                                )
                            }
                        };
                    }
                    if self.match_t(&[Number(0.)]) {
                        if let Number(val) = self.previous().t_type {
                            return Expr::Literal(LiteralType::Number(val));
                        } else {
                            ::core::panicking::panic(
                                "internal error: entered unreachable code",
                            )
                        }
                    }
                    if self.match_t(&[LeftParen]) {
                        let expr = self.expression();
                        self.consume(&RightParen, "Expected ')' after expression.");
                        return Expr::Grouping(Box::new(expr));
                    }
                    if self.match_t(&[Identifier(TIdentifier(String::new()))]) {
                        if let Identifier(id) = &self.previous().t_type {
                            return Expr::Variable(id.to_owned());
                        }
                    }
                    if self.match_t(&[DebugTokenTime]) {
                        return Expr::DebugVariable(DebugVariable::Time);
                    }
                    Expr::Literal(LiteralType::Null)
                };
                self.exit_fn(expr)
            })();
            let mut last = self.curr_stack.pop();
            self.tracking_stack.push(self.curr_stack.clone());
            rt_val
        }
        fn consume_end_of_line(&mut self, msg: &str) {
            if self.check(&TokenType::Newline) || self.check(&TokenType::Semicolon) {
                while self.check(&TokenType::Newline)
                    || self.check(&TokenType::Semicolon)
                {
                    self.advance();
                }
                return;
            }
            if !self.is_at_end() {
                self.report_token(&self.tokens[self.current]);
                ::core::panicking::panic_display(&msg);
            }
        }
        fn consume(&mut self, t: &TokenType, msg: &str) -> &Token {
            if self.match_t(&[t.clone()]) {
                return self.previous();
            }
            self.report_token(&self.tokens[self.current]);
            ::core::panicking::panic_display(&msg)
        }
        fn match_t(&mut self, tokens: &[TokenType]) -> bool {
            let mut tmp_current = self.current;
            use std::mem::discriminant;
            let check_tmp = move |tokens: &[Token], idx: usize, t: &TokenType| {
                if &TokenType::Eof == &tokens[idx].t_type {
                    return false;
                }
                discriminant(&tokens[idx].t_type) == discriminant(t)
            };
            let advance_tmp = |idx| {
                let token: &Token = &self.tokens[idx];
                if !(&TokenType::Eof == &token.t_type) {
                    return idx + 1;
                }
                idx
            };
            while check_tmp(&self.tokens, tmp_current, &TokenType::Newline) {
                tmp_current = advance_tmp(tmp_current);
            }
            for t in tokens {
                if discriminant(&self.tokens[tmp_current].t_type) == discriminant(t) {
                    self.current = tmp_current;
                    self.advance();
                    return true;
                }
            }
            false
        }
        fn check(&self, t: &TokenType) -> bool {
            use std::mem::discriminant;
            if self.is_at_end() {
                return false;
            }
            discriminant(&self.peek().t_type) == discriminant(t)
        }
        fn check_n(&self, t: &TokenType) -> bool {
            use std::mem::discriminant;
            let mut tmp_current = self.current;
            while self.tokens[tmp_current].t_type == Newline {
                tmp_current += 1;
            }
            discriminant(&self.tokens[tmp_current].t_type) == discriminant(t)
        }
        fn advance(&mut self) -> &Token {
            if !self.is_at_end() {
                self.current += 1;
            }
            self.previous()
        }
        fn is_at_end(&self) -> bool {
            &TokenType::Eof == &self.peek().t_type
        }
        fn peek_n(&self) -> &Token {
            let mut tmp_current = self.current;
            while self.tokens[tmp_current].t_type == Newline {
                tmp_current += 1;
            }
            &self.tokens[tmp_current]
        }
        fn peek(&self) -> &Token {
            &self.tokens[self.current]
        }
        fn previous(&self) -> &Token {
            &self.tokens[self.current - 1]
        }
        fn enter_fn(&mut self, info: &str) {}
        fn exit_fn<T>(&mut self, data: T) -> T {
            data
        }
        fn report_token(&self, token: &Token) {
            let line = if let Some(line) = self.code_lines.get(token.line - 1) {
                line.to_owned()
            } else {
                "".to_owned()
            };
            let lexeme_len = token.lexeme.len();
            let (start, _end) = token.col;
            let line_num_len = {
                let res = ::alloc::fmt::format(
                    ::core::fmt::Arguments::new_v1(
                        &[""],
                        &[::core::fmt::ArgumentV1::new_display(&token.line)],
                    ),
                );
                res
            }
                .len();
            {
                ::std::io::_print(
                    ::core::fmt::Arguments::new_v1(
                        &["", "|\n"],
                        &[
                            ::core::fmt::ArgumentV1::new_display(
                                &" ".repeat(line_num_len),
                            ),
                        ],
                    ),
                );
            };
            {
                ::std::io::_print(
                    ::core::fmt::Arguments::new_v1(
                        &["", "| ", "\n"],
                        &[
                            ::core::fmt::ArgumentV1::new_display(&token.line),
                            ::core::fmt::ArgumentV1::new_display(&line),
                        ],
                    ),
                );
            };
            {
                ::std::io::_print(
                    ::core::fmt::Arguments::new_v1(
                        &["", "|", "", "\n"],
                        &[
                            ::core::fmt::ArgumentV1::new_display(
                                &" ".repeat(line_num_len),
                            ),
                            ::core::fmt::ArgumentV1::new_display(&" ".repeat(start)),
                            ::core::fmt::ArgumentV1::new_display(&"^".repeat(lexeme_len)),
                        ],
                    ),
                );
            };
            self.print_parse_info();
        }
        fn print_parse_info(&self) {
            {
                ::std::io::_print(
                    ::core::fmt::Arguments::new_v1(&["\nparse info: \n"], &[]),
                );
            };
            for line in &self.parse_info {
                for f in line {
                    {
                        ::std::io::_print(
                            ::core::fmt::Arguments::new_v1(
                                &["", " -> "],
                                &[::core::fmt::ArgumentV1::new_display(&f)],
                            ),
                        );
                    };
                }
                {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(&["\n"], &[]));
                };
            }
            {
                ::std::io::_print(::core::fmt::Arguments::new_v1(&["\n"], &[]));
            };
        }
        pub fn print_tracker_info(&self) {
            {
                ::std::io::_print(
                    ::core::fmt::Arguments::new_v1(&["\nparsing tracker info: \n"], &[]),
                );
            };
            for line in &self.tracking_stack {
                for f in line {
                    {
                        ::std::io::_print(
                            ::core::fmt::Arguments::new_v1(
                                &["", " [", "ns] ->"],
                                &[
                                    ::core::fmt::ArgumentV1::new_display(&f.0),
                                    ::core::fmt::ArgumentV1::new_display(
                                        &if let Some(dt) = f.2 { dt.as_nanos() } else { 0 },
                                    ),
                                ],
                            ),
                        );
                    }
                }
                {
                    ::std::io::_print(::core::fmt::Arguments::new_v1(&["\n"], &[]));
                };
            }
        }
    }
}
