use quote::__private::Literal;
use syn::token::Fn;

use crate::environment::EnvVal;
use crate::environment::Environment;
use crate::expr::DebugVariable;
use crate::expr::{Expr, LiteralType, Stmt, StrType};
use crate::repl;
use crate::tokens::TIdentifier;
use crate::tokens::Token;
use crate::tokens::TokenType::{self};
use crate::utils::pretty_print_literal;
use crate::utils::print_literal;
use std::time;

type EnvScope = Rc<Environment>;

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
    env: Rc<Environment>,
    flags: InterpreterFlags,
}

impl Interpreter {
    pub fn new(repl_mode: bool, env: Environment) -> Self {
        let env = Rc::new(env);
        Interpreter {
            repl_mode,
            env: Rc::clone(&env),
            flags: InterpreterFlags::default(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.execute_stmt(stmt);
        }
    }

    fn push_env(&mut self) {
        self.env = Rc::new(Environment::new_as_child(Rc::clone(&self.env)));
    }

    fn pop_env(&mut self) {
        self.env = Rc::get_mut(&mut self.env)
            .unwrap()
            .destroy()
            .take()
            .unwrap();
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
                self.flags.return_backtrack = true;
                let val = self.eval_expr(&expr);
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

    #[inline(always)]
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
                if self.env.has_var(&id.0) && !self.repl_mode {
                    panic!("Attempt to redeclare variable '{}'", id.0)
                }
                let value = self.evaluate(expr);
                Rc::get_mut(&mut self.env)
                    .unwrap()
                    .define(id.0.clone(), EnvVal::Lt(Rc::new(value)));
                LiteralType::Null
            }
            Stmt::Block(stmts) => self.exec_block(stmts),
            Stmt::If {
                expr,
                if_block,
                else_block: None,
            } => {
                let condition = self.evaluate(expr);
                let Stmt::Block(if_block) = *if_block.clone() else {unreachable!()};
                if let LiteralType::Bool(true) = condition {
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
                let Stmt::Block(if_block) = *if_block.clone() else { unreachable!()};
                let Stmt::Block(else_block) = *else_block.clone() else { unreachable!() };
                if let LiteralType::Bool(true) = condition {
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
                self.env = Rc::new(Environment::new_as_child(Rc::clone(&self.env)));

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
                        if self.flags.return_backtrack {
                            self.pop_env();
                            return rt_val;
                        }
                        self.execute_stmt(updation);
                    }
                } else {
                    while let LiteralType::Bool(true) = self.evaluate(condition_expr) {
                        rt_val = self.execute_stmt(body);
                        if self.flags.return_backtrack {
                            self.pop_env();
                            return rt_val;
                        }
                    }
                }

                self.env = Rc::get_mut(&mut self.env)
                    .unwrap()
                    .destroy()
                    .take()
                    .unwrap();
                LiteralType::Null
            }
            Stmt::Assignment { id, expr } => {
                let value = self.evaluate(expr);
                Rc::get_mut(&mut self.env)
                    .unwrap()
                    .update(&id.0, EnvVal::Lt(Rc::new(value)));
                LiteralType::Null
            }
            Stmt::FunctionDef {
                ident,
                params,
                body,
            } => {
                if let Stmt::Block(stmts) = *body.clone() {
                    Rc::get_mut(&mut self.env).unwrap().define(
                        ident.0.clone(),
                        EnvVal::Fn(Rc::new(params.to_vec()), Rc::new(stmts)),
                    )
                };
                LiteralType::Null
            }
            Stmt::Return(_) => panic!("Unexpected 'return' statement"),
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
            Expr::Variable(TIdentifier(name)) => match self.env.get(name) {
                EnvVal::Lt(literal) => literal.as_ref().clone(),
                EnvVal::Fn(_, _) => LiteralType::Str(StrType::Strict(format!("[fun {name}]"))),
            },
            Expr::DebugVariable(var) => match var {
                DebugVariable::Time => LiteralType::Number(
                    time::SystemTime::now()
                        .duration_since(time::UNIX_EPOCH)
                        .unwrap()
                        .as_secs_f64(),
                ),
            },
            Expr::FnCall(ident, args) => {
                let fn_def = Rc::get_mut(&mut self.env).unwrap().get(&ident.0);
                let (params, stmts) = if let EnvVal::Fn(params, stmts) = fn_def {
                    (Rc::clone(&params), Rc::clone(&stmts))
                } else {
                    panic!("Cannot call a variable as a function")
                };

                self.env = Rc::new(Environment::new_as_child(Rc::clone(&self.env)));
                for (i, param) in params.iter().enumerate() {
                    let val = if let Some(val) = args.get(&i.to_string()) {
                        let expr = self.eval_expr(val);
                        EnvVal::Lt(Rc::new(expr))
                    } else {
                        EnvVal::Lt(Rc::new(LiteralType::Null))
                    };
                    Rc::get_mut(&mut self.env)
                        .unwrap()
                        .define(param.0.clone(), val)
                }

                let rt_val = self.exec_block(&stmts);
                self.env = Rc::get_mut(&mut self.env)
                    .unwrap()
                    .destroy()
                    .take()
                    .unwrap();

                self.flags.return_backtrack = false;
                rt_val
            }
        }
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

        let rt_val = match operator.t_type {
            TokenType::Plus => Self::addition(left, right),
            TokenType::Minus => Self::subtraction(left, right),
            TokenType::Slash => Self::division(left, right),
            TokenType::Star => Self::multiplication(left, right),
            TokenType::EqualEqual => Self::equal(left, right),
            TokenType::BangEqual => Self::not_equal(left, right),
            TokenType::Greater => Self::greater(left, right),
            TokenType::GreaterEqual => Self::greater_equal(left, right),
            TokenType::Less => Self::less(left, right),
            TokenType::LessEqual => Self::less_equal(left, right),
            TokenType::Percentage => Self::modulus(left, right),
            _ => unreachable!(),
        };

        rt_val
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

    fn str_multiplication(left: StrType, n: f64) -> StrType {
        let n = n.floor() as usize;
        match left {
            StrType::Loose(s) => StrType::Loose(s.repeat(n)),
            StrType::Strict(s) => StrType::Strict(s.repeat(n)),
        }
    }

    fn division(left: LiteralType, right: LiteralType) -> LiteralType {
        let left = Self::try_to_number(left);
        let right = Self::try_to_number(right);
        match (left, right) {
            (LiteralType::Number(a), LiteralType::Number(b)) => LiteralType::Number(a / b),
            (LiteralType::Str(s), LiteralType::Number(n)) => {
                let n = n.floor() as usize;

                let create_chunks = |s: String| {
                    let c = s.chars().collect::<Vec<char>>();
                    LiteralType::Str(StrType::Loose(
                        c.chunks(n)
                            .map(|c| c.iter().collect::<String>())
                            .collect::<Vec<String>>()
                            .join(", ")
                            + " //arrays aren't implemented yet lol",
                    ))
                };

                match s {
                    StrType::Loose(s) => create_chunks(s),
                    StrType::Strict(s) => create_chunks(s),
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
            (LiteralType::Str(s), LiteralType::Str(s1)) => {
                LiteralType::Str(Self::str_addition(s.clone(), s1))
            }
            (LiteralType::Bool(b0), LiteralType::Bool(b1)) => {
                LiteralType::Number(Self::bool_to_number(*b0) + Self::bool_to_number(*b1))
            }
            (LiteralType::Str(StrType::Loose(s)), _left) => {
                LiteralType::Str(StrType::Loose(s.to_owned() + &Self::literal_to_str(right)))
            }
            (_right, LiteralType::Str(StrType::Loose(s))) => {
                LiteralType::Str(StrType::Loose(Self::literal_to_str(left) + s))
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
                StrType::Strict(Self::str_type_inner(s0) + Self::str_type_inner_ref(s1))
            }
            _ => StrType::Loose(Self::str_type_inner(s0) + Self::str_type_inner_ref(s1)),
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
        }
    }

    #[inline(always)]
    fn literal_to_str(l: LiteralType) -> String {
        match l {
            LiteralType::Str(s) => Self::str_type_inner(s),
            LiteralType::Bool(b) => b.to_string(),
            LiteralType::Number(n) => n.to_string(),
            LiteralType::Null => "null".to_owned(),
        }
    }

    #[inline(always)]
    fn try_to_number(l: LiteralType) -> LiteralType {
        match l {
            LiteralType::Number(n) => LiteralType::Number(n),
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
        }
    }

    #[inline(always)]
    fn str_type_inner(s: StrType) -> String {
        let (StrType::Loose(s) | StrType::Strict(s)) = s;
        s
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
