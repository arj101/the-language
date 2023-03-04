use crate::tokens::TIdentifier;
use crate::tokens::Token;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone)]
pub enum StrType {
    Strict(String),
    Loose(String),
}

#[derive(Debug, Clone)]
pub enum LiteralType {
    Number(f64),
    Str(StrType),
    Bool(bool),
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
        if_block: Box<Stmt>,
        else_block: Option<Box<Stmt>>,
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
    FnCall(TIdentifier, FxHashMap<String, Expr>),
    DebugVariable(DebugVariable),
}

#[derive(Debug, Clone)]
pub enum DebugVariable {
    Time,
}
