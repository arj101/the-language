use crate::{
    expr::{LiteralType, Stmt, StrType},
    tokens::TIdentifier,
};
use rustc_hash::FxHashMap;
use std::{borrow::BorrowMut, collections::HashMap, rc::Rc, time::Instant};

use crate::println_raw;

#[derive(Debug)]
pub enum EnvVal {
    Lt(Rc<LiteralType>),
    Fn(Rc<Vec<TIdentifier>>, Rc<Vec<Stmt>>),
}

pub struct Environment {
    scopes: Vec<FxHashMap<Rc<String>, EnvVal>>,
    curr_scope: usize,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![FxHashMap::default()],
            curr_scope: 0,
        }
    }

    pub fn define(&mut self, name: Rc<String>, value: EnvVal) {
        self.scopes[self.curr_scope].insert(name, value);
    }

    pub fn has_var(&self, name: &Rc<String>) -> bool {
        self.scopes[self.curr_scope].contains_key(name)
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
        self.curr_scope = self.scopes.len() - 1;
    }

    pub fn pop_scope(&mut self) -> Result<(), ()> {
        if self.curr_scope <= 0 {
            println_raw!("cannot pop out of global scope");
            return Err(());
        }
        self.scopes.pop();
        self.curr_scope -= 1;
        Ok(())
    }

    pub fn update(&mut self, name: &Rc<String>, value: EnvVal) {
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

        println_raw!("{}: no such variable in scope", name);
    }

    pub fn get(&self, name: &Rc<String>) -> &EnvVal {
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

        println_raw!("{}: no such variable in scope", name);
        self.scopes[0].get(&Rc::new("undefined".to_string())).unwrap()
    }
}

pub fn env_val_to_literal(env_val: &EnvVal) -> LiteralType {
    match env_val {
        EnvVal::Lt(literal) => Rc::as_ref(literal).clone(),
        EnvVal::Fn(..) => LiteralType::Str(StrType::Strict("[fn]".to_owned())),
    }
}
