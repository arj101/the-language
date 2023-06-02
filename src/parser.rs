use crate::expr::{DebugVariable, Expr, LiteralType, Stmt, StrType};
use crate::tokens::tokens;
use crate::tokens::TIdentifier;
use crate::tokens::Token;
use crate::tokens::TokenType::{self, *};

use crate::print_raw;
use crate::println_raw;
use idk::track;
use idk::tracker_drive_struct;
use rustc_hash::FxHashMap;

struct ParserState {
    had_error: bool,
    inside_function: bool,
    inside_loop: bool,
}

impl Default for ParserState {
    fn default() -> Self {
        Self {
            had_error: false,
            inside_function: false,
            inside_loop: false,
        }
    }
}

#[tracker_drive_struct]
pub struct Parser {
    code_lines: Vec<String>,
    tokens: Vec<Token>,
    current: usize,

    parse_info: Vec<Vec<String>>,
    curr_tree: Vec<String>,

    state: ParserState,

    errors: Vec<(usize, String)>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source: String) -> Self {
        Self {
            tokens,
            current: 0,
            code_lines: source.lines().map(|l| l.to_owned()).collect(),
            parse_info: vec![],
            curr_tree: vec![],
            curr_stack: vec![],
            tracking_stack: vec![],
            state: ParserState::default(),
            errors: vec![],
        }
    }

    pub fn reset(&mut self, tokens: Vec<Token>, source: String) {
        self.code_lines = source.lines().map(|l| l.to_owned()).collect();
        self.tokens = tokens;
        self.current = 0;
    }

    #[track]
    pub fn parse(&mut self) -> Result<Vec<Stmt>, ()> {
        self.state.had_error = false;
        let mut statements = vec![];

        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(_) => {
                    self.handle_last_error();
                }
            }
            if let Err(_) = self.consume_end_of_line("expected EOL character") {
                self.handle_last_error();
            }
        }

        if !self.state.had_error {
            Ok(statements)
        } else {
            Err(())
        }
    }

    fn print_error(msg: &str) {
        use termion::{color, style};

        println_raw!(
            "{}{}error{}: {msg}{}",
            style::Bold,
            color::Fg(color::Red),
            color::Fg(color::Reset),
            style::Reset
        );
    }

    fn handle_last_error(&mut self) {
        let (token_idx, msg) = self.errors.last().unwrap();

        Self::print_error(msg);
        self.report_token(&self.tokens[*token_idx]);
        self.recover_error();
    }

    #[track]
    fn declaration(&mut self) -> Result<Stmt, String> {
        if self.match_t(&[TokenType::Let]) {
            self.let_decl()
        } else {
            self.statement()
        }
    }

    #[track]
    fn let_decl(&mut self) -> Result<Stmt, String> {
        let name = self.consume(
            &TokenType::Identifier(TIdentifier::new("".to_string())),
            "Expected variable name.",
        )?;

        let name = if let TokenType::Identifier(name) = &name.t_type {
            name.clone()
        } else {
            unreachable!()
        };

        let mut intializer = Expr::Literal(LiteralType::Null);
        if self.match_t(&[TokenType::Equal]) {
            intializer = self.expression()?
        }

        Ok(Stmt::Decl {
            id: name,
            expr: intializer,
        })
    }

    #[track]
    fn statement(&mut self) -> Result<Stmt, String> {
        if self.match_t(&[TokenType::If]) {
            self.if_stmt()
        } else if self.match_t(&[TokenType::While, TokenType::For, TokenType::Do]) {
            self.control_flow_loop()
        } else if self.match_t(&[TokenType::Print]) {
            self.print_stmt()
        } else if self.match_t(&[TokenType::LeftBrace]) {
            self.block()
        } else if self.match_t(&[TokenType::Fun]) {
            self.fn_def()
        } else if self.match_t(&[TokenType::Return]) {
            self.return_stmt()
        } else if self.check_n(&tokens::empty_ident())
        // && (self.check_ahead_n(1, &TokenType::Equal) || self.check_ahead_n(1, &TokenType::PlusEqual) {
        {
            self.assignment_stmt()
        } else {
            self.expr_stmt()
        }
    }

    #[track]
    fn return_stmt(&mut self) -> Result<Stmt, String> {
        Ok(Stmt::Return(self.expression()?))
    }

    #[track]
    fn ident_expr(&mut self) -> Result<Expr, String> {
        let ident = self.consume(&tokens::empty_ident(), "Expected identifier")?;
        let ident = if let TokenType::Identifier(ident) = ident.t_type.clone() {
            ident
        } else {
            unreachable!()
        };

        if self.match_t(&[TokenType::LeftParen]) {
            let mut params = vec![];
            while !self.check_n(&RightParen) {
                let arg = self.expression()?;
                params.push(arg);
                self.match_t(&[TokenType::Comma]);
            }
            self.consume(&RightParen, "Expected ')'")?;
            Ok(Expr::FnCall(ident, params))
        } else {
            Ok(Expr::Variable(ident))
        }
    }

    #[track]
    fn fn_def(&mut self) -> Result<Stmt, String> {
        let ident = self.consume(
            &TokenType::Identifier(TIdentifier::new("".to_string())),
            "Expected function name",
        )?;
        let ident = if let TokenType::Identifier(ident) = &ident.t_type {
            ident.clone()
        } else {
            unreachable!()
        };

        self.consume(&TokenType::LeftParen, "Expected '('")?;
        let mut params = vec![];
        while self.check_n(&TokenType::Identifier(TIdentifier::new("".to_string()))) {
            let param = self.consume(
                &TokenType::Identifier(TIdentifier::new("".to_string())),
                "Expected parameter name",
            )?;
            let param = if let TokenType::Identifier(param) = &param.t_type {
                param.clone()
            } else {
                unreachable!()
            };
            params.push(param);
            self.match_t(&[TokenType::Comma]);
        }
        self.consume(&TokenType::RightParen, "Expected ')'")?;
        self.consume(&TokenType::LeftBrace, "Expected block")?;
        let body = Box::new(self.block()?);

        Ok(Stmt::FunctionDef {
            ident,
            params,
            body,
        })
    }

    #[track]
    fn assignment_stmt(&mut self) -> Result<Stmt, String> {
        let name = self
            .consume(
                &TokenType::Identifier(TIdentifier::new("".to_string())),
                "Expected variable name.",
            )?
            .clone();
        let name_pos = self.current - 1; //hax >:)

        let name = if let TokenType::Identifier(name) = &name.t_type {
            name.clone()
        } else {
            unreachable!()
        };

        let operator = self.peek_n().clone();

        macro_rules! expand {
            ($token_type: expr) => {
                Stmt::Assignment {
                    id: name.clone(),
                    expr: Expr::Binary {
                        left: Box::new(Expr::Variable(name)),
                        operator: operator.into_new($token_type),
                        right: Box::new(self.expression()?),
                    },
                }
            };
        }

        macro_rules! match_combined_operator {
            ($( $combined:tt => $expanded:tt),*  $(,)? ) => {
                match operator.t_type {
                    $($combined => {
                        self.match_t(&[$combined]);
                        Ok(expand!($expanded))
                    })*
                    _ => {
                        self.current = name_pos;
                        self.expr_stmt()
                    }
                }
            }
        }

        if operator.t_type == Equal {
            self.match_t(&[Equal]);
            Ok(Stmt::Assignment {
                id: name,
                expr: self.expression()?,
            })
        } else {
            match_combined_operator! {
                PlusEqual => Plus,
                MinusEqual => Minus,
                StarEqual => Star,
                SlashEqual => Slash,
                PercentageEqual => Percentage,
            }
        }
        // self.consume_end_of_line("Expected ';' after assignment statement.");
    }

    #[track]
    fn control_flow_loop(&mut self) -> Result<Stmt, String> {
        let stmt = match self.previous().t_type {
            TokenType::While => {
                let expr = self.expression()?;
                self.consume(&TokenType::LeftBrace, "Expected block")?;
                let body = Box::new(self.block()?);
                Ok(Stmt::Loop {
                    entry_controlled: true,
                    init: None,
                    expr,
                    updation: None,
                    body,
                })
            }
            TokenType::For => {
                let init = Some(Box::new(self.declaration()?));
                self.consume_end_of_line(
                    "Expected end of line character after loop initialisation",
                )?;
                let expr = self.expression()?;
                self.consume_end_of_line("Expected end of line character after loop expression")?;
                let updation = Some(Box::new(self.statement()?));
                self.consume(&TokenType::LeftBrace, "Expected block")?;
                let body = Box::new(self.block()?);
                Ok(Stmt::Loop {
                    entry_controlled: true,
                    init,
                    expr,
                    updation,
                    body,
                })
            }
            TokenType::Do => {
                self.consume(&TokenType::LeftBrace, "Expected block")?;
                let body = Box::new(self.block()?);
                self.consume(&TokenType::While, "Expected 'while' after Do loop body")?;
                let expr = self.expression()?;
                self.consume_end_of_line("Expected EOL after loop condition")?;
                Ok(Stmt::Loop {
                    entry_controlled: false,
                    init: None,
                    expr,
                    updation: None,
                    body,
                })
            }
            _ => unreachable!(),
        };

        self.exit_fn(stmt)
    }

    #[track]
    fn if_stmt(&mut self) -> Result<Stmt, String> {
        let expr = self.expression()?;
        let if_block = std::rc::Rc::new(if self.match_t(&[LeftBrace]) {
            self.block()?
        } else {
            self.statement()?
        });

        let else_block = if self.match_t(&[Else]) {
            Some(std::rc::Rc::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            expr,
            if_block,
            else_block,
        })
    }

    #[track]
    fn print_stmt(&mut self) -> Result<Stmt, String> {
        let value = self.expression()?;
        let val = Stmt::Print(value);
        // self.consume_end_of_line("Expected ';' after value.");

        Ok(val)
    }

    #[track]
    fn expr_stmt(&mut self) -> Result<Stmt, String> {
        let expression = self.expression()?;
        // self.consume_end_of_line("Expected ';' after expression.");

        Ok(Stmt::ExprStmt(expression))
    }

    #[track]
    fn block(&mut self) -> Result<Stmt, String> {
        let mut statements = vec![];

        while !self.check_n(&RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
            if !self.check_n(&RightBrace) {
                self.consume_end_of_line("Expected EOL character")?;
            }
        }

        self.consume(&RightBrace, "Expected '}' after block.")?;

        Ok(Stmt::Block(statements))
    }

    #[track]
    fn expression(&mut self) -> Result<Expr, String> {
        self.equality()
    }

    #[track]
    fn equality(&mut self) -> Result<Expr, String> {
        let mut expr = self.comparison()?;

        while self.match_t(&[BangEqual, EqualEqual]) {
            let operator = self.previous().clone();
            let right = Box::new(self.comparison()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right,
            };
        }

        Ok(expr)
    }

    #[track]
    fn comparison(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;

        while self.match_t(&[Greater, GreaterEqual, Less, LessEqual]) {
            let operator = self.previous().clone();
            let right = Box::new(self.term()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right,
            }
        }

        Ok(expr)
    }

    #[track]
    fn term(&mut self) -> Result<Expr, String> {
        let mut expr = self.factor()?;

        while self.match_t(&[Plus, Minus, Percentage]) {
            let operator = self.previous().clone();
            let right = Box::new(self.factor()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right,
            }
        }

        Ok(expr)
    }

    #[track]
    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.match_t(&[Slash, Star, StarStar]) {
            let operator = self.previous().clone();
            let right = Box::new(self.unary()?);
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right,
            }
        }

        Ok(expr)
    }

    #[track]
    fn unary(&mut self) -> Result<Expr, String> {
        let expr = if self.match_t(&[Bang, Minus]) {
            let operator = self.previous().clone();
            let right = Box::new(self.unary()?);
            Expr::Unary { operator, right }
        } else {
            self.primary()?
        };

        Ok(expr)
    }

    #[track]
    fn primary(&mut self) -> Result<Expr, String> {
        if self.match_t(&[False]) {
            return Ok(Expr::Literal(LiteralType::Bool(false)));
        }
        if self.match_t(&[True]) {
            return Ok(Expr::Literal(LiteralType::Bool(true)));
        }
        if self.match_t(&[Null]) {
            return Ok(Expr::Literal(LiteralType::Null));
        }

        if self.match_t(&[StrStrict(String::new()), StrLoose(String::new())]) {
            use std::rc::Rc;
            return match &self.previous().t_type {
                StrStrict(s) => Ok(Expr::Literal(LiteralType::Str(StrType::Strict(Rc::new(
                    s.clone(),
                ))))),
                StrLoose(s) => Ok(Expr::Literal(LiteralType::Str(StrType::Loose(Rc::new(
                    s.clone(),
                ))))),
                _ => unreachable!(),
            };
        }

        if self.match_t(&[Number(0.)]) {
            if let Number(val) = self.previous().t_type {
                return Ok(Expr::Literal(LiteralType::Number(val)));
            } else {
                unreachable!()
            }
        }

        if self.match_t(&[LeftParen]) {
            let expr = self.expression()?;
            self.consume(&RightParen, "Expected ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        if self.match_t(&[LeftSquareBrace]) {
            let mut array = vec![];

            if self.match_t(&[RightSquareBrace]) {
                return Ok(Expr::ArrayExpr(array));
            }
            array.push(self.expression()?);

            loop {
                self.match_t(&[Comma]);
                if self.match_t(&[RightSquareBrace]) {
                    return Ok(Expr::ArrayExpr(array));
                }

                array.push(self.expression()?);
            }
        }

        if self.check_n(&tokens::empty_ident()) {
            return self.ident_expr();
        }

        if self.match_t(&[DebugTokenTime]) {
            return Ok(Expr::DebugVariable(DebugVariable::Time));
        }

        self.create_error(format!("Unexpected token: {:?}", self.peek().lexeme))
    }

    fn consume_end_of_line(&mut self, msg: &str) -> Result<(), String> {
        if self.check(&TokenType::Newline) || self.check(&TokenType::Semicolon) {
            while self.check(&TokenType::Newline) || self.check(&TokenType::Semicolon) {
                self.advance();
            }
            return Ok(());
        }
        if !self.is_at_end() {
            return self.create_error(msg.to_owned());
        }
        Ok(())
    }

    fn consume(&mut self, t: &TokenType, msg: &str) -> Result<&Token, String> {
        if self.match_t(&[t.clone()]) {
            return Ok(self.previous());
        }
        self.create_error(msg.to_owned())
    }

    fn recover_error(&mut self) {
        while !self.is_at_end() {
            match self.tokens[self.current].t_type {
                TokenType::If
                | TokenType::Fun
                | TokenType::Let
                | TokenType::Do
                | TokenType::For
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => break,
                _ => {
                    self.advance();
                }
            }
        }
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
            //run over newline
            tmp_current = advance_tmp(tmp_current)
        }

        // println!(
        //     "[{}:{}] {:?} ==  {:?}",
        //     self.tokens[tmp_current].line,
        //     self.tokens[tmp_current].col.0,
        //     tokens,
        //     self.tokens[tmp_current].t_type
        // );

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
            tmp_current += 1
        }
        discriminant(&self.tokens[tmp_current].t_type) == discriminant(t)
    }

    fn check_ahead_n(&self, lookup_dist: usize, t: &TokenType) -> bool {
        use std::mem::discriminant;
        let mut tmp_current = self.current;
        for _ in 0..lookup_dist {
            tmp_current += 1;
            while self.tokens[tmp_current].t_type == Newline {
                tmp_current += 1
            }
        }
        discriminant(&self.tokens[tmp_current].t_type) == discriminant(t)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
            // println!(
            //     "advancing from {:?}({}) to {:?}({})",
            //     self.tokens[self.current - 1].t_type,
            //     self.current - 1,
            //     self.tokens[self.current].t_type,
            //     self.current
            // );
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

    fn enter_fn(&mut self, info: String) {
        // self.curr_tree.push(format!(
        //     "{} [{:?}]",
        //     info.to_owned(),
        //     // self.tokens[self.current].t_type,
        //     self.peek_n().t_type
        // ));
        // self.parse_info.push(self.curr_tree.clone());
    }

    fn exit_fn<T>(&mut self, data: T) -> T {
        // self.curr_tree.pop();

        // let last = self.curr_tree.pop();
        // if let Some(last) = last {
        //     let last_fn_name = last.split_ascii_whitespace().next().unwrap();
        //     self.curr_tree
        //         .push(format!("{} [{:?}]", last_fn_name, self.peek_n().t_type))
        // }
        // self.parse_info.push(self.curr_tree.clone());
        data
    }

    fn report_token(&self, token: &Token) {
        use termion::{color, style};

        let line = if let Some(line) = self.code_lines.get(token.line - 1) {
            line.to_owned()
        } else {
            "".to_owned() // likely a newline
        };
        let lexeme_len = token.lexeme.len();
        let (start, _end) = token.col;
        let line_num_len = format!("{}", token.line).len();
        println_raw!(
            " {} {}{}|{}{}",
            " ".repeat(line_num_len),
            color::Fg(color::LightBlue),
            style::Bold,
            style::Reset,
            color::Fg(color::Reset)
        );
        println_raw!(
            "{}{} {} |{}{} {}",
            color::Fg(color::LightBlue),
            style::Bold,
            token.line,
            style::Reset,
            color::Fg(color::Reset),
            line
        );
        println_raw!(
            " {} {}{}|{}{}{}{}{}{}",
            " ".repeat(line_num_len),
            color::Fg(color::LightBlue),
            style::Bold,
            color::Fg(color::Reset),
            " ".repeat(start),
            color::Fg(color::Red),
            "^".repeat(lexeme_len),
            color::Fg(color::Reset),
            style::Reset,
        );
    }

    fn create_error<T>(&mut self, msg: String) -> Result<T, String> {
        self.state.had_error = true;
        self.errors.push((self.current, msg.clone()));

        Err(msg.clone())
    }

    pub fn print_tracker_info(&self) {
        println_raw!("\nparsing tracker info: ");
        for line in &self.tracking_stack {
            for (i, f) in line.iter().enumerate() {
                if let Some(dt) = f.2 {
                    print_raw!(
                        "{}<-{}{} {} [{:.1}us]{} ",
                        termion::color::Fg(termion::color::LightMagenta),
                        termion::color::Fg(termion::color::Reset),
                        termion::style::Bold,
                        f.0,
                        dt.as_secs_f64() * 10.0f64.powi(6),
                        termion::style::Reset
                    )
                } else {
                    print_raw!(
                        "{}->{}",
                        termion::color::Fg(termion::color::LightGreen),
                        termion::color::Fg(termion::color::Reset),
                    );
                    if i == line.len() - 1 {
                        print_raw!("{}", termion::style::Bold)
                    }
                    print_raw!(" {} ", f.0);
                    if i == line.len() - 1 {
                        print_raw!("{}", termion::style::Reset)
                    }
                }
            }
            println_raw!();
        }
    }
}
