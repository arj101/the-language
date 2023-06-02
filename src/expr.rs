use crate::interpreter::Interpreter;
use crate::tokens::TIdentifier;
use crate::tokens::Token;
use crate::tokens::TokenType;
use rustc_hash::FxHashMap;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum StrType {
    Strict(Rc<String>),
    Loose(Rc<String>),
}

#[derive(Debug, Clone)]
pub enum LiteralType {
    Number(f64),
    Str(StrType),
    Bool(bool),
    Array(Rc<Vec<LiteralType>>),
    Null,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    ExprStmt(Expr),
    Print(Expr),
    Decl {
        id: TIdentifier,
        expr: Expr,
    },
    Assignment {
        id: TIdentifier,
        expr: Expr,
    },
    Block(Vec<Stmt>),
    // move the below into Expr after implementing `return`
    Return(Expr),
    If {
        expr: Expr,
        if_block: Rc<Stmt>,
        else_block: Option<Rc<Stmt>>,
    },
    Loop {
        entry_controlled: bool,
        init: Option<Box<Stmt>>,
        expr: Expr,
        updation: Option<Box<Stmt>>,
        body: Box<Stmt>,
    },
    FunctionDef {
        ident: TIdentifier,
        params: Vec<TIdentifier>,
        body: Box<Stmt>,
    },
}

#[derive(Clone)]
pub struct BindedStmt {
    pub stmt: BindingStmt,
    pub exec: fn(&mut Interpreter, &BindingStmt) -> LiteralType,
}

impl Debug for BindedStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BindedStmt")
            .field("stmt", &self.stmt)
            .finish()
    }
}

#[derive(Clone)]
pub struct BindedExpr {
    pub expr: BindingExpr,
    pub exec: fn(&mut Interpreter, &BindingExpr) -> LiteralType,
}

impl Debug for BindedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BindedStmt")
            .field("stmt", &self.expr)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum BindingStmt {
    ExprStmt(BindedExpr),
    Print(BindedExpr),
    Decl {
        id: TIdentifier,
        expr: BindedExpr,
    },
    Assignment {
        id: TIdentifier,
        expr: BindedExpr,
    },
    Block(Vec<BindedStmt>),
    // move the below into Expr after implementing `return`
    Return(BindedExpr),
    If {
        expr: BindedExpr,
        if_block: Rc<BindedStmt>,
        else_block: Option<Rc<BindedStmt>>,
    },
    Loop {
        entry_controlled: bool,
        init: Option<Box<BindedStmt>>,
        expr: BindedExpr,
        updation: Option<Box<BindedStmt>>,
        body: Box<BindedStmt>,
    },
    FunctionDef {
        ident: TIdentifier,
        params: Vec<TIdentifier>,
        body: Box<BindedStmt>,
    },
}

#[derive(Debug, Clone)]
pub enum BindingExpr {
    Grouping(Box<BindedExpr>),
    Binary {
        left: Box<BindedExpr>,
        operator: Token,
        right: Box<BindedExpr>,
    },
    Unary {
        operator: Token,
        right: Box<BindedExpr>,
    },
    Variable(TIdentifier),
    Literal(LiteralType),
    FnCall(TIdentifier, Vec<BindedExpr>),
    DebugVariable(DebugVariable),
    ArrayExpr(Vec<BindedExpr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Grouping(Box<Expr>),
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable(TIdentifier),
    Literal(LiteralType),
    FnCall(TIdentifier, Vec<Expr>),
    DebugVariable(DebugVariable),
    ArrayExpr(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum DebugVariable {
    Time,
}
