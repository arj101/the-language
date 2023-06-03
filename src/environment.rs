use crate::{
    expr::{BindedStmt, LiteralType, Stmt, StrType},
    tokens::StrSymbol,
    tokens::TIdentifier,
};
use rustc_hash::FxHashMap;
use std::{borrow::BorrowMut, collections::HashMap, rc::Rc, time::Instant};

use crate::println_raw;

#[derive(Debug)]
pub enum EnvVal {
    Lt(LiteralType),
    Fn(Rc<(Vec<TIdentifier>, Vec<BindedStmt>)>),
}

pub struct Environment {
    scopes: Vec<FxHashMap<StrSymbol, EnvVal>>,
    curr_scope: usize,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![FxHashMap::default()],
            curr_scope: 0,
        }
    }

    #[inline(always)]
    pub fn define(&mut self, name: StrSymbol, value: EnvVal) {
        self.scopes[self.curr_scope].insert(name, value);
    }

    #[inline]
    pub fn has_var(&self, name: &StrSymbol) -> bool {
        self.scopes[self.curr_scope].contains_key(name)
    }

    #[inline(always)]
    pub fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
        self.curr_scope = self.scopes.len() - 1;
    }

    #[inline(always)]
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
        self.curr_scope -= 1;
    }

    #[inline(always)]
    pub fn update(&mut self, name: &StrSymbol, value: EnvVal) {
        if let Some(old_val) = self.scopes[self.curr_scope].get_mut(name) {
            *old_val = value;
            return;
        }

        let mut scope_idx = self.curr_scope;
        while scope_idx > 0 {
            scope_idx -= 1;

            if let Some(old_val) = self.scopes[scope_idx].get_mut(name) {
                *old_val = value;
                return;
            }
        }

        println_raw!("{:?}: no such variable in scope", name);
    }

    #[inline(always)]
    pub fn get(&self, name: &StrSymbol) -> &EnvVal {
        if let Some(val) = self.scopes[self.curr_scope].get(name) {
            return val;
        }

        let mut scope_idx = self.curr_scope;
        while scope_idx > 0 {
            scope_idx -= 1;

            if let Some(val) = self.scopes[scope_idx].get(name) {
                return val;
            }
        }

        panic!("{:?}: no such variable in scope", name);
        // self.scopes[0]
        //     .get(&Rc::new("undefined".to_string()))
        //     .unwrap()
    }
}

#[inline(always)]
pub fn env_val_to_literal(env_val: &EnvVal) -> LiteralType {
    match env_val {
        EnvVal::Lt(literal) => literal.clone(),
        EnvVal::Fn(..) => LiteralType::Str(StrType::Strict(Rc::new("[fn]".to_owned()))),
    }
}
