use syn::token::Fn;

use crate::environment::EnvVal;
use crate::environment::Environment;
use crate::expr::BindedExpr;
use crate::expr::BindedStmt;
use crate::expr::BindingExpr;
use crate::expr::BindingStmt;
use crate::expr::DebugVariable;
use crate::expr::{Expr, LiteralType, Stmt, StrType};
use crate::repl;
use crate::tokens::TIdentifier;
use crate::tokens::Token;
use crate::tokens::TokenType::{self};
use crate::utils::pretty_print_literal;
use crate::utils::print_literal;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time;

type EnvScope = Environment;

use std::rc::Rc;

struct InterpreterFlags {
    return_backtrack: bool,
}

impl Default for InterpreterFlags {
    fn default() -> Self {
        Self {
            return_backtrack: false,
        }
    }
}

pub struct Interpreter {
    repl_mode: bool,
    env: Environment,
    flags: InterpreterFlags,
    stop_signal: Arc<AtomicBool>,
}

impl Interpreter {
    pub fn new(repl_mode: bool, env: Environment, stop_signal: Arc<AtomicBool>) -> Self {
        Interpreter {
            repl_mode,
            env,
            flags: InterpreterFlags::default(),
            stop_signal,
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.execute_stmt(stmt);
        }
    }

    #[inline(always)]
    fn push_env(&mut self) {
        self.env.push_scope();
    }

    #[inline(always)]
    fn pop_env(&mut self) {
        self.env.pop_scope();
    }

    #[inline(always)]
    pub fn evaluate(&mut self, ast: &Expr) -> LiteralType {
        self.eval_expr(ast)
    }

    fn exec_block(&mut self, statements: &[Stmt]) -> LiteralType {
        self.push_env();
        let mut rt_val;
        for stmt in statements {
            if let Stmt::Return(expr) = stmt {
                let val = self.eval_expr(&expr);
                self.flags.return_backtrack = true;
                self.pop_env();
                return val;
            } else {
                rt_val = self.execute_stmt(&stmt);
                if self.flags.return_backtrack {
                    self.pop_env();
                    return rt_val;
                }
            }
        }
        self.pop_env();
        LiteralType::Null
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> LiteralType {
        match stmt {
            Stmt::ExprStmt(expr) => {
                let eval_result = self.evaluate(expr);
                if self.repl_mode {
                    pretty_print_literal(eval_result)
                }
                LiteralType::Null
            }
            Stmt::Print(expr) => {
                let eval_result = self.evaluate(expr);
                if self.repl_mode {
                    pretty_print_literal(eval_result);
                } else {
                    print_literal(eval_result)
                }
                LiteralType::Null
            }
            Stmt::Decl { id, expr } => {
                if self.env.has_var(&id.inner()) && !self.repl_mode {
                    panic!("Attempt to redeclare variable '{}'", id.inner())
                }
                let value = self.evaluate(expr);
                self.env
                    .define(Rc::clone(&id.0), EnvVal::Lt(value));
                LiteralType::Null
            }
            Stmt::Block(stmts) => self.exec_block(stmts),
            Stmt::If {
                expr,
                if_block,
                else_block: None,
            } => {
                let condition = self.evaluate(expr);
                let Stmt::Block(if_block) = if_block.as_ref() else {unreachable!()};
                if Self::to_bool(condition) {
                    return self.exec_block(&if_block);
                }
                LiteralType::Null
            }
            Stmt::If {
                expr,
                if_block,
                else_block: Some(else_block),
            } => {
                let condition = self.evaluate(expr);
                let Stmt::Block(if_block) = if_block.as_ref() else { unreachable!()};
                let Stmt::Block(else_block) = else_block.as_ref() else { unreachable!() };
                if Self::to_bool(condition) {
                    self.exec_block(&if_block)
                } else {
                    self.exec_block(&else_block)
                }
            }
            Stmt::Loop {
                entry_controlled,
                init,
                expr: condition_expr,
                updation,
                body,
            } => {
                self.push_env();

                if let Some(init) = init {
                    self.execute_stmt(init);
                }

                if !entry_controlled {
                    self.execute_stmt(body);
                }

                let mut rt_val;
                //to avoid checking for updation statement inside the loop
                if let Some(updation) = &updation {
                    while let LiteralType::Bool(true) = self.evaluate(condition_expr) {
                        rt_val = self.execute_stmt(body);
                        if self.flags.return_backtrack
                            || self.stop_signal.load(std::sync::atomic::Ordering::Relaxed)
                        {
                            self.pop_env();
                            return rt_val;
                        }
                        self.execute_stmt(updation);
                    }
                } else {
                    while let LiteralType::Bool(true) = self.evaluate(condition_expr) {
                        rt_val = self.execute_stmt(body);
                        if self.flags.return_backtrack
                            || self.stop_signal.load(std::sync::atomic::Ordering::Relaxed)
                        {
                            self.pop_env();
                            return rt_val;
                        }
                    }
                }

                self.pop_env();
                LiteralType::Null
            }
            Stmt::Assignment { id, expr } => {
                let value = self.evaluate(expr);
                self.env.update(&id.0, EnvVal::Lt(value));
                LiteralType::Null
            }
            Stmt::FunctionDef {
                ident,
                params,
                body,
            } => {
                if let Stmt::Block(stmts) = *body.clone() {
                    self.env.define(
                        Rc::clone(&ident.0),
                        EnvVal::Fn(
                            Rc::new(params.to_vec()),
                            Rc::new(stmts.iter().map(Self::bind).collect()),
                        ),
                    )
                };
                LiteralType::Null
            }
            Stmt::Return(_) => panic!("Unexpected 'return' statement"),
        }
    }

    fn exec_binded_expr(&mut self, expr: &BindingStmt) -> LiteralType {
        let BindingStmt::ExprStmt(expr) = expr else { unreachable!() };

        let eval_result = (expr.exec)(self, &expr.expr);
        if self.repl_mode {
            pretty_print_literal(eval_result)
        }

        LiteralType::Null
    }

    fn exec_binded_print(&mut self, stmt: &BindingStmt) -> LiteralType {
        let BindingStmt::Print(expr) = stmt else { unreachable!() };

        let eval_result = (expr.exec)(self, &expr.expr);
        if self.repl_mode {
            pretty_print_literal(eval_result);
        } else {
            print_literal(eval_result)
        }

        LiteralType::Null
    }

    fn exec_binded_decl(&mut self, decl: &BindingStmt) -> LiteralType {
        let BindingStmt::Decl { id, expr } = decl else { unreachable!() };

        if self.env.has_var(&id.inner()) && !self.repl_mode {
            panic!("Attempt to redeclare variable '{}'", id.inner())
        }
        let value = (expr.exec)(self, &expr.expr);
        self.env
            .define(Rc::clone(&id.0), EnvVal::Lt(value));
        LiteralType::Null
    }

    fn exec_binded_block(&mut self, block: &BindingStmt) -> LiteralType {
        let BindingStmt::Block(stmts) = block else { unreachable!() };

        Self::exec_binded(self, stmts)
    }

    fn exec_binded_if(&mut self, stmt: &BindingStmt) -> LiteralType {
        let BindingStmt::If { expr, if_block, .. } = stmt else { unreachable!() };

        let condition = (expr.exec)(self, &expr.expr);
        let BindingStmt::Block(if_block) = &if_block.stmt else {unreachable!()};
        if Self::to_bool(condition) {
            return self.exec_binded(&if_block);
        }

        LiteralType::Null
    }

    fn exec_binded_if_else(&mut self, stmt: &BindingStmt) -> LiteralType {
        let BindingStmt::If { expr, if_block, else_block: Some(else_block) } = stmt else { unreachable!() };

        let condition = (expr.exec)(self, &expr.expr);
        let BindingStmt::Block(if_block) = &if_block.stmt else { unreachable!()};
        let BindingStmt::Block(else_block) = &else_block.stmt else { unreachable!() };

        if Self::to_bool(condition) {
            self.exec_binded(&if_block)
        } else {
            self.exec_binded(&else_block)
        }
    }

    fn exec_binded_loop(&mut self, stmt: &BindingStmt) -> LiteralType {
        let BindingStmt::Loop { entry_controlled, init, expr: condition, updation, body } = stmt else { unreachable!() };

        self.push_env();

        if let Some(init) = init {
            (init.exec)(self, &init.stmt);
        }

        if !entry_controlled {
            (body.exec)(self, &body.stmt);
        }

        let mut rt_val;
        //to avoid checking for updation statement inside the loop
        if let Some(updation) = &updation {
            while let LiteralType::Bool(true) = (condition.exec)(self, &condition.expr) {
                rt_val = (body.exec)(self, &body.stmt);
                if self.flags.return_backtrack
                    || self.stop_signal.load(std::sync::atomic::Ordering::Relaxed)
                {
                    self.pop_env();
                    return rt_val;
                }
                (updation.exec)(self, &updation.stmt);
            }
        } else {
            while let LiteralType::Bool(true) = (condition.exec)(self, &condition.expr) {
                rt_val = (body.exec)(self, &body.stmt);
                if self.flags.return_backtrack
                    || self.stop_signal.load(std::sync::atomic::Ordering::Relaxed)
                {
                    self.pop_env();
                    return rt_val;
                }
            }
        }

        self.pop_env();
        LiteralType::Null
    }

    fn exec_binded_assignment(&mut self, stmt: &BindingStmt) -> LiteralType {
        let BindingStmt::Assignment { id, expr } = stmt else { unreachable!() };

        let value = (expr.exec)(self, &expr.expr);
        self.env.update(&id.0, EnvVal::Lt(value));

        LiteralType::Null
    }

    fn exec_binded_fn_def(&mut self, stmt: &BindingStmt) -> LiteralType {
        let BindingStmt::FunctionDef { ident, params, body } = stmt else { unreachable!() };

        if let BindingStmt::Block(stmts) = &body.stmt {
            self.env.define(
                Rc::clone(&ident.0),
                EnvVal::Fn(Rc::new(params.to_vec()), Rc::new(stmts.to_vec())),
            )
        };
        LiteralType::Null
    }

    fn exec_binded_return(&mut self, stmt: &BindingStmt) -> LiteralType {
        let BindingStmt::Return(val) = stmt else { unreachable!() };

        let rt_val = (val.exec)(self, &val.expr);
        self.flags.return_backtrack = true;
        rt_val
    }

    fn eval(&mut self, _: &BindingExpr) -> LiteralType {
        LiteralType::Null
    }

    fn eval2(&mut self, _: &BindingStmt) -> LiteralType {
        LiteralType::Null
    }

    fn bind(stmt: &Stmt) -> BindedStmt {
        macro_rules! bind {
            ($stmt:expr => $f:ident) => {
                BindedStmt {
                    stmt: $stmt,
                    exec: Self::$f,
                }
            };
        }

        match stmt {
            Stmt::Block(stmts) => BindedStmt {
                stmt: BindingStmt::Block(stmts.iter().map(Self::bind).collect()),
                exec: Self::exec_binded_block,
            },
            Stmt::ExprStmt(expr) => {
                bind!(BindingStmt::ExprStmt(Self::bind_expr(expr)) => exec_binded_expr)
            }
            Stmt::Print(expr) => {
                bind!(BindingStmt::Print(Self::bind_expr(expr)) => exec_binded_print)
            }
            Stmt::Decl { id, expr } => {
                bind!(BindingStmt::Decl{id: id.clone(), expr: Self::bind_expr(expr)} => exec_binded_decl)
            }
            Stmt::If {
                expr,
                if_block,
                else_block: None,
            } => {
                bind!(BindingStmt::If{expr: Self::bind_expr(expr), if_block: Rc::new(Self::bind(if_block)), else_block:None} => exec_binded_if)
            }
            Stmt::If {
                expr,
                if_block,
                else_block: Some(else_block),
            } => {
                bind!(BindingStmt::If{expr: Self::bind_expr(expr), if_block: Rc::new(Self::bind(if_block)), else_block: Some(Rc::new(Self::bind(else_block)))} => exec_binded_if_else)
            }
            Stmt::Loop {
                entry_controlled,
                init,
                expr,
                updation,
                body,
            } => bind!( BindingStmt::Loop{
            entry_controlled: *entry_controlled,
            init: init.clone().map(|init| Box::new(Self::bind(init.as_ref()))), expr: Self::bind_expr(expr), updation: updation.clone().map(|updation| Box::new(Self::bind(updation.as_ref()))), body: Box::new(Self::bind(body))} => exec_binded_loop),

            Stmt::Assignment { id, expr } => {
                bind!( BindingStmt::Assignment { id: id.clone(), expr: Self::bind_expr(expr) } => exec_binded_assignment)
            }

            Stmt::FunctionDef {
                ident,
                params,
                body,
            } => bind!(
            BindingStmt::FunctionDef { ident: ident.clone(), params: params.clone(), body: Box::new(Self::bind(body)) } => exec_binded_fn_def ),

            Stmt::Return(expr) => bind!(BindingStmt::Return(Self::bind_expr(expr)) => exec_binded_return ),

            _ => unreachable!(),
        }
    }

    fn eval_binded_literal(&mut self, lt: &BindingExpr) -> LiteralType {
        let BindingExpr::Literal(lt) = lt else { unreachable!() };
        lt.clone()
    }

    fn eval_binded_binary(&mut self, binary: &BindingExpr) -> LiteralType {
        let BindingExpr::Binary { left, operator, right } = binary else { unreachable!() };

        let mut left = (left.exec)(self, &left.expr);
        let mut right = (right.exec)(self, &right.expr);

        match (&left, &right) {
            (LiteralType::Number(_), LiteralType::Str(StrType::Loose(s))) => {
                if let Ok(n) = s.parse::<f64>() {
                    right = LiteralType::Number(n)
                }
            }
            (LiteralType::Str(StrType::Loose(s)), LiteralType::Number(_)) => {
                if let Ok(n) = s.parse::<f64>() {
                    left = LiteralType::Number(n)
                }
            }
            (LiteralType::Number(_), LiteralType::Bool(b)) => {
                right = LiteralType::Number(Self::bool_to_number(*b))
            }
            (LiteralType::Bool(b), LiteralType::Number(_)) => {
                left = LiteralType::Number(Self::bool_to_number(*b))
            }
            _ => (),
        }

        macro_rules! eval {
            ($( $token:pat => $function:ident ),* $(,)?) => {
                match operator.t_type {
                    $(
                         $token => Self::$function(left, right),
                     )*
                    _ => unreachable!()
                }
            };
        }

        use TokenType::*;
        eval! {
            Plus => addition,
            Minus => subtraction,
            Slash => division,
            Star => multiplication,
            StarStar => exponentiation,
            EqualEqual => equal,
            BangEqual => not_equal,
            Greater => greater,
            GreaterEqual => greater_equal,
            Less => less,
            LessEqual => less_equal,
            Percentage => modulus,
        }
    }

    fn eval_binded_unary(&mut self, unary: &BindingExpr) -> LiteralType {
        let BindingExpr::Unary { operator, right } = unary else { unreachable!() };
        let right = (right.exec)(self, &right.expr);
        let rt = match operator.t_type {
            TokenType::Minus => {
                let right = if let LiteralType::Number(n) = right {
                    n
                } else {
                    std::f64::NAN
                };
                LiteralType::Number(-right)
            }
            TokenType::Bang => match right {
                LiteralType::Bool(b) => LiteralType::Bool(!b),
                LiteralType::Number(n) => LiteralType::Bool(Self::number_to_bool(n)),
                LiteralType::Null => LiteralType::Bool(true),
                LiteralType::Str(s) => {
                    let s = Self::str_type_inner_ref(&s);
                    if s.is_empty() {
                        LiteralType::Bool(true)
                    } else {
                        LiteralType::Bool(false)
                    }
                }
                LiteralType::Array(a) => LiteralType::Bool(!a.is_empty()),
            },
            _ => unreachable!(),
        };
        rt
    }

    fn eval_binded_fn_call(&mut self, fn_call: &BindingExpr) -> LiteralType {
        let BindingExpr::FnCall(ident, args) = fn_call else { unreachable!() };
        self.push_env();

        let fn_def = self.env.get(&ident.0);
        let (params, stmts) = if let EnvVal::Fn(params, stmts) = fn_def {
            (Rc::clone(&params), Rc::clone(&stmts))
        } else {
            panic!("Cannot call a variable as a function")
        };

        for (i, param) in params.iter().enumerate() {
            if let Some(val) = args.get(i) {
                let expr = (val.exec)(self, &val.expr);
                self.env
                    .define(Rc::clone(&param.0), EnvVal::Lt((expr)));
                continue;
            }

            self.env
                .define(Rc::clone(&param.0), EnvVal::Lt((LiteralType::Null)));
        }

        let rt_val = self.exec_binded(&stmts);
        self.flags.return_backtrack = false;

        self.pop_env();
        rt_val
    }

    fn eval_binded_var(&mut self, var: &BindingExpr) -> LiteralType {
        let BindingExpr::Variable(ident) = var else { unreachable!() };

        match self.env.get(&ident.0) {
            EnvVal::Lt(literal) => literal.clone(),
            EnvVal::Fn(_, _) => {
                LiteralType::Str(StrType::Strict(Rc::new(format!("[fun {}]", ident.0.as_ref()))))
            }
        }
    }

    fn eval_binded_array(&mut self, array: &BindingExpr) -> LiteralType {
        let BindingExpr::ArrayExpr(array) = array else { unreachable!() };

        let mut evaled_array = vec![];
        for expr in array {
            evaled_array.push((expr.exec)(self, &expr.expr))
        }

        LiteralType::Array(Rc::new(evaled_array))
    }

    fn eval_binded_debug_var(&mut self, var: &BindingExpr) -> LiteralType {
        let BindingExpr::DebugVariable(var) = var else { unreachable!() };

        match var {
            DebugVariable::Time => LiteralType::Number(
                time::SystemTime::now()
                    .duration_since(time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs_f64(),
            ),
        }
    }

    fn bind_expr(expr: &Expr) -> BindedExpr {
        match expr {
            Expr::Literal(lt) => BindedExpr {
                expr: BindingExpr::Literal(lt.clone()),
                exec: Self::eval_binded_literal,
            },

            Expr::Grouping(expr) => Self::bind_expr(expr),

            Expr::Unary { operator, right } => BindedExpr {
                expr: BindingExpr::Unary {
                    operator: operator.clone(),
                    right: Box::new(Self::bind_expr(right)),
                },
                exec: Self::eval_binded_unary,
            },

            Expr::Binary {
                left,
                operator,
                right,
            } => BindedExpr {
                expr: BindingExpr::Binary {
                    left: Box::new(Self::bind_expr(left)),
                    operator: operator.clone(),
                    right: Box::new(Self::bind_expr(right)),
                },
                exec: Self::eval_binded_binary,
            },

            Expr::FnCall(ident, params) => BindedExpr {
                expr: BindingExpr::FnCall(
                    ident.clone(),
                    params.iter().map(Self::bind_expr).collect(),
                ),
                exec: Self::eval_binded_fn_call,
            },

            Expr::Variable(ident) => BindedExpr {
                expr: BindingExpr::Variable(ident.clone()),
                exec: Self::eval_binded_var,
            },

            Expr::ArrayExpr(arr) => BindedExpr {
                expr: BindingExpr::ArrayExpr(arr.iter().map(Self::bind_expr).collect()),
                exec: Self::eval_binded_array,
            },

            Expr::DebugVariable(debug_var) => BindedExpr {
                expr: BindingExpr::DebugVariable(debug_var.clone()),
                exec: Self::eval_binded_debug_var,
            },
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> LiteralType {
        match expr {
            Expr::Grouping(expr) => self.eval_expr(expr),
            Expr::Unary { operator, right } => self.eval_unary(operator, right),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.eval_binary(left, operator, right),
            Expr::Literal(l) => l.to_owned(),
            Expr::Variable(name) => match self.env.get(&name.0) {
                EnvVal::Lt(literal) => literal.clone(),
                EnvVal::Fn(_, _) => {
                    LiteralType::Str(StrType::Strict(Rc::new(format!("[fun {}]", name.0.as_ref()))))
                }
            },
            Expr::ArrayExpr(array) => {
                let mut array_evaled = vec![];
                for expr in array {
                    array_evaled.push(self.eval_expr(expr))
                }

                LiteralType::Array(Rc::new(array_evaled))
            }
            Expr::DebugVariable(var) => match var {
                DebugVariable::Time => LiteralType::Number(
                    time::SystemTime::now()
                        .duration_since(time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f64(),
                ),
            },
            Expr::FnCall(ident, args) => {
                self.push_env();

                let fn_def = self.env.get(&ident.0);
                let (params, stmts) = if let EnvVal::Fn(params, stmts) = fn_def {
                    (Rc::clone(&params), Rc::clone(&stmts))
                } else {
                    panic!("Cannot call a variable as a function")
                };

                for (i, param) in params.iter().enumerate() {
                    if let Some(val) = args.get(i) {
                        let expr = self.eval_expr(val);
                        self.env
                            .define(Rc::clone(&param.0), EnvVal::Lt((expr)));
                        continue;
                    }

                    self.env
                        .define(Rc::clone(&param.0), EnvVal::Lt((LiteralType::Null)));
                }

                let rt_val = self.exec_binded(&stmts);
                self.flags.return_backtrack = false;

                self.pop_env();
                rt_val
            }
        }
    }

    fn exec_binded(&mut self, stmts: &[BindedStmt]) -> LiteralType {
        self.push_env();

        let mut rt_val = LiteralType::Null;

        for stmt in stmts {
            rt_val = (stmt.exec)(self, &stmt.stmt);
            if self.flags.return_backtrack {
                self.pop_env();
                return rt_val;
            }
        }

        self.pop_env();
        LiteralType::Null
    }

    fn eval_binary(&mut self, left: &Expr, operator: &Token, right: &Expr) -> LiteralType {
        let mut left = self.eval_expr(left);
        let mut right = self.eval_expr(right);

        match (&left, &right) {
            (LiteralType::Number(_), LiteralType::Str(StrType::Loose(s))) => {
                if let Ok(n) = s.parse::<f64>() {
                    right = LiteralType::Number(n)
                }
            }
            (LiteralType::Str(StrType::Loose(s)), LiteralType::Number(_)) => {
                if let Ok(n) = s.parse::<f64>() {
                    left = LiteralType::Number(n)
                }
            }
            (LiteralType::Number(_), LiteralType::Bool(b)) => {
                right = LiteralType::Number(Self::bool_to_number(*b))
            }
            (LiteralType::Bool(b), LiteralType::Number(_)) => {
                left = LiteralType::Number(Self::bool_to_number(*b))
            }
            _ => (),
        }

        macro_rules! eval {
            ($( $token:pat => $function:ident ),* $(,)?) => {
                match operator.t_type {
                    $(
                         $token => Self::$function(left, right),
                     )*
                    _ => unreachable!()
                }
            };
        }

        use TokenType::*;
        eval! {
            Plus => addition,
            Minus => subtraction,
            Slash => division,
            Star => multiplication,
            StarStar => exponentiation,
            EqualEqual => equal,
            BangEqual => not_equal,
            Greater => greater,
            GreaterEqual => greater_equal,
            Less => less,
            LessEqual => less_equal,
            Percentage => modulus,
        }
    }

    fn modulus(left: LiteralType, right: LiteralType) -> LiteralType {
        let left = Self::try_to_number(left);
        let right = Self::try_to_number(right);
        LiteralType::Number(
            if let (LiteralType::Number(a), LiteralType::Number(b)) = (left, right) {
                a % b
            } else {
                std::f64::NAN
            },
        )
    }

    fn less_equal(left: LiteralType, right: LiteralType) -> LiteralType {
        let LiteralType::Bool(is_equal) = Self::equal(left.clone(), right.clone()) else { unreachable!() };
        let LiteralType::Bool(is_less) = Self::less(left, right) else { unreachable!() };
        LiteralType::Bool(is_equal || is_less)
    }

    fn less(left: LiteralType, right: LiteralType) -> LiteralType {
        match (left, right) {
            (LiteralType::Number(a), LiteralType::Number(b)) => LiteralType::Bool(a < b),
            (LiteralType::Str(a), LiteralType::Str(b)) => {
                let a = Self::str_type_inner(a);
                let b = Self::str_type_inner(b);
                LiteralType::Bool(a.len() < b.len())
            }
            (LiteralType::Bool(a), LiteralType::Bool(b)) => {
                LiteralType::Bool(Self::bool_to_number(a) < Self::bool_to_number(b))
            }
            (LiteralType::Str(_), LiteralType::Null) => LiteralType::Bool(true),
            (LiteralType::Null, LiteralType::Str(b)) => {
                LiteralType::Bool(!Self::str_type_inner(b).is_empty())
            }
            (l, r) => {
                if let (LiteralType::Number(a), LiteralType::Number(b)) =
                    (Self::try_to_number(l), Self::try_to_number(r))
                {
                    LiteralType::Bool(a < b)
                } else {
                    LiteralType::Bool(false)
                }
            }
        }
    }

    fn greater_equal(left: LiteralType, right: LiteralType) -> LiteralType {
        let LiteralType::Bool(is_equal) = Self::equal(left.clone(), right.clone()) else { unreachable!() };
        let LiteralType::Bool(is_greater) = Self::greater(left, right) else { unreachable!() };
        LiteralType::Bool(is_equal || is_greater)
    }

    fn greater(left: LiteralType, right: LiteralType) -> LiteralType {
        match (left, right) {
            (LiteralType::Number(a), LiteralType::Number(b)) => LiteralType::Bool(a > b),
            (LiteralType::Str(a), LiteralType::Str(b)) => {
                let a = Self::str_type_inner(a);
                let b = Self::str_type_inner(b);
                LiteralType::Bool(a.len() > b.len())
            }
            (LiteralType::Bool(a), LiteralType::Bool(b)) => {
                LiteralType::Bool(Self::bool_to_number(a) > Self::bool_to_number(b))
            }
            (LiteralType::Str(a), LiteralType::Null) => {
                LiteralType::Bool(!Self::str_type_inner(a).is_empty())
            }
            (LiteralType::Null, LiteralType::Str(_)) => LiteralType::Bool(false),
            (l, r) => {
                if let (LiteralType::Number(a), LiteralType::Number(b)) =
                    (Self::try_to_number(l), Self::try_to_number(r))
                {
                    LiteralType::Bool(a > b)
                } else {
                    LiteralType::Bool(false)
                }
            }
        }
    }

    fn not_equal(left: LiteralType, right: LiteralType) -> LiteralType {
        let LiteralType::Bool(b) = Self::equal(left, right) else { panic!() };
        LiteralType::Bool(!b)
    }

    fn equal(left: LiteralType, right: LiteralType) -> LiteralType {
        match (left, right) {
            (LiteralType::Number(a), LiteralType::Number(b)) => LiteralType::Bool(a == b),
            (LiteralType::Bool(a), LiteralType::Bool(b)) => LiteralType::Bool(a == b),
            (LiteralType::Str(a), LiteralType::Str(b)) => {
                let a = Self::str_type_inner(a);
                let b = Self::str_type_inner(b);

                LiteralType::Bool(a == b)
            }
            (LiteralType::Null, LiteralType::Null) => LiteralType::Bool(true),
            _ => LiteralType::Bool(false),
        }
    }

    fn multiplication(left: LiteralType, right: LiteralType) -> LiteralType {
        let left = Self::try_to_number(left);
        let right = Self::try_to_number(right);
        match (left, right) {
            (LiteralType::Number(a), LiteralType::Number(b)) => LiteralType::Number(a * b),
            (LiteralType::Str(s), LiteralType::Number(n)) => {
                LiteralType::Str(Self::str_multiplication(s, n))
            }
            _ => LiteralType::Number(std::f64::NAN),
        }
    }

    fn exponentiation(left: LiteralType, right: LiteralType) -> LiteralType {
        let left = Self::try_to_number(left);
        let right = Self::try_to_number(right);

        match (left, right) {
            (LiteralType::Number(a), LiteralType::Number(b)) => {
                if b.floor() == b {
                    LiteralType::Number(a.powi(b as i32))
                } else {
                    LiteralType::Number(a.powf(b))
                }
            }
            _ => LiteralType::Number(std::f64::NAN),
        }
    }

    fn str_multiplication(left: StrType, n: f64) -> StrType {
        let n = n.floor() as usize;
        match left {
            StrType::Loose(s) => StrType::Loose(Rc::new(s.repeat(n))),
            StrType::Strict(s) => StrType::Strict(Rc::new(s.repeat(n))),
        }
    }

    fn division(left: LiteralType, right: LiteralType) -> LiteralType {
        let left = Self::try_to_number(left);
        let right = Self::try_to_number(right);
        match (left, right) {
            (LiteralType::Number(a), LiteralType::Number(b)) => LiteralType::Number(a / b),
            (LiteralType::Str(s), LiteralType::Number(n)) => {
                let n = n.floor() as usize;

                let create_chunks = |s: String, strict_string: bool| {
                    let c = s.chars().collect::<Vec<char>>();
                    LiteralType::Array(Rc::new(
                        c.chunks(n)
                            .map(|c| {
                                let s = c.iter().collect::<String>();
                                if strict_string {
                                    LiteralType::Str(StrType::Strict(Rc::new(s)))
                                } else {
                                    LiteralType::Str(StrType::Loose(Rc::new(s)))
                                }
                            })
                            .collect::<Vec<LiteralType>>(),
                    ))
                };

                match s {
                    StrType::Loose(s) => create_chunks(s.to_string(), false),
                    StrType::Strict(s) => create_chunks(s.to_string(), true),
                }
            }
            _ => LiteralType::Number(std::f64::NAN),
        }
    }

    fn subtraction(left: LiteralType, right: LiteralType) -> LiteralType {
        let left = Self::try_to_number(left);
        let right = Self::try_to_number(right);
        LiteralType::Number(
            if let (LiteralType::Number(a), LiteralType::Number(b)) = (left, right) {
                a - b
            } else {
                std::f64::NAN
            },
        )
    }

    fn addition(left: LiteralType, right: LiteralType) -> LiteralType {
        match (&left, &right) {
            (LiteralType::Number(a), LiteralType::Number(b)) => LiteralType::Number(a + b),
            (LiteralType::Array(array), c) => {
                let mut array_new = array.as_ref().clone();
                if let LiteralType::Array(array2) = c {
                    for elt in array2.as_ref() {
                        array_new.push(elt.clone())
                    }
                } else {
                    array_new.push(c.clone());
                };
                LiteralType::Array(Rc::new(array_new))
            }
            (c, LiteralType::Array(array)) => {
                let mut array_new = array.as_ref().clone();
                if let LiteralType::Array(array2) = c {
                    for elt in array2.as_ref() {
                        array_new.push(elt.clone())
                    }
                } else {
                    if array_new.is_empty() {
                        array_new.push(c.clone())
                    } else {
                        array_new.insert(0, c.clone());
                    }
                };
                LiteralType::Array(Rc::new(array_new))
            }
            (LiteralType::Str(s), LiteralType::Str(s1)) => {
                LiteralType::Str(Self::str_addition(s.clone(), s1))
            }
            (LiteralType::Bool(b0), LiteralType::Bool(b1)) => {
                LiteralType::Number(Self::bool_to_number(*b0) + Self::bool_to_number(*b1))
            }
            (LiteralType::Str(StrType::Loose(s)), _left) => {
                LiteralType::Str(StrType::Loose(Rc::new(s.to_string() + &Self::literal_to_str(right))))
            }
            (_right, LiteralType::Str(StrType::Loose(s))) => {
                LiteralType::Str(StrType::Loose(Rc::new(Self::literal_to_str(left) + &s)))
            }
            (LiteralType::Str(StrType::Strict(_)), _)
            | (_, LiteralType::Str(StrType::Strict(_))) => LiteralType::Number(std::f64::NAN),
            (l, r) => {
                let (l, r) = (l.clone(), r.clone());
                LiteralType::Number(
                    if let (LiteralType::Number(a), LiteralType::Number(b)) =
                        (Self::try_to_number(l), Self::try_to_number(r))
                    {
                        a + b
                    } else {
                        std::f64::NAN
                    },
                )
            }
        }
    }

    fn str_addition(s0: StrType, s1: &StrType) -> StrType {
        match (&s0, s1) {
            (StrType::Strict(_), _) | (_, StrType::Strict(_)) => {
                StrType::Strict(Rc::new(Self::str_type_inner(s0) + Self::str_type_inner_ref(s1)))
            }
            _ => StrType::Loose(Rc::new(Self::str_type_inner(s0) + Self::str_type_inner_ref(s1))),
        }
    }

    fn eval_unary(&mut self, operator: &Token, right: &Expr) -> LiteralType {
        let right = self.eval_expr(right);
        let rt = match operator.t_type {
            TokenType::Minus => {
                let right = if let LiteralType::Number(n) = right {
                    n
                } else {
                    std::f64::NAN
                };
                LiteralType::Number(-right)
            }
            TokenType::Bang => match right {
                LiteralType::Bool(b) => LiteralType::Bool(!b),
                LiteralType::Number(n) => LiteralType::Bool(Self::number_to_bool(n)),
                LiteralType::Null => LiteralType::Bool(true),
                LiteralType::Str(s) => {
                    let s = Self::str_type_inner_ref(&s);
                    if s.is_empty() {
                        LiteralType::Bool(true)
                    } else {
                        LiteralType::Bool(false)
                    }
                }
                LiteralType::Array(a) => LiteralType::Bool(!a.is_empty()),
            },
            _ => unreachable!(),
        };
        rt
    }

    #[inline(always)]
    fn to_number(l: LiteralType) -> LiteralType {
        match l {
            LiteralType::Number(n) => LiteralType::Number(n),
            LiteralType::Bool(b) => LiteralType::Number(Self::bool_to_number(b)),
            LiteralType::Null => LiteralType::Number(0.0),
            LiteralType::Str(s) => {
                let s = if let StrType::Loose(s) = s {
                    s
                } else {
                    return LiteralType::Number(std::f64::NAN);
                };
                if let Ok(n) = s.parse() {
                    LiteralType::Number(n)
                } else {
                    LiteralType::Number(std::f64::NAN)
                }
            }
            LiteralType::Array(a) => LiteralType::Number(a.len() as f64),
        }
    }

    #[inline(always)]
    fn literal_to_str(l: LiteralType) -> String {
        match l {
            LiteralType::Str(s) => Self::str_type_inner(s),
            LiteralType::Bool(b) => b.to_string(),
            LiteralType::Number(n) => n.to_string(),
            LiteralType::Null => "null".to_owned(),
            LiteralType::Array(a) => format!("{a:?}"),
        }
    }

    #[inline(always)]
    fn try_to_number(l: LiteralType) -> LiteralType {
        match l {
            LiteralType::Number(number) => LiteralType::Number(number),
            LiteralType::Bool(b) => LiteralType::Number(Self::bool_to_number(b)),
            LiteralType::Null => LiteralType::Number(0.0),
            LiteralType::Str(s) => match &s {
                StrType::Loose(s1) => {
                    if let Ok(n) = s1.parse() {
                        LiteralType::Number(n)
                    } else {
                        LiteralType::Str(s)
                    }
                }
                StrType::Strict(_) => LiteralType::Str(s),
            },
            everything_else => everything_else,
        }
    }

    #[inline(always)]
    fn to_bool(l: LiteralType) -> bool {
        match l {
            LiteralType::Number(number) => number >= 1.0,
            LiteralType::Str(s) => Self::str_type_inner(s).len() > 0,
            LiteralType::Bool(b) => b,
            LiteralType::Null => false,
            LiteralType::Array(arr) => !arr.is_empty(),
        }
    }

    #[inline(always)]
    fn str_type_inner(s: StrType) -> String {
        let (StrType::Loose(s) | StrType::Strict(s)) = s;
        s.to_string()
    }

    #[inline(always)]
    fn str_type_inner_ref<'b>(s: &'b StrType) -> &'b String {
        let (StrType::Loose(s) | StrType::Strict(s)) = s;
        s
    }

    #[inline(always)]
    fn bool_to_number(b: bool) -> f64 {
        if b {
            1.0
        } else {
            0.0
        }
    }

    #[inline(always)]
    fn number_to_bool(n: f64) -> bool {
        n != 0.
    }
}
