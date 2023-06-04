use crate::{
    expr::{BindedStmt, LiteralType, Stmt, StrType},
    tokens::StrSymbol,
    tokens::TIdentifier,
};
use rustc_hash::FxHashMap;
use std::{borrow::BorrowMut, collections::HashMap, rc::Rc, time::Instant};

use nohash_hasher::NoHashHasher;
use std::hash::BuildHasherDefault;
use crate::println_raw;

type NoHashHashMap<K, V> = HashMap<K, V, BuildHasherDefault<NoHashHasher<usize>>>;

#[derive(Debug)]
pub enum EnvVal {
    Lt(LiteralType),
    Fn(Rc<(Vec<TIdentifier>, Vec<BindedStmt>)>),
}

pub struct Environment {
    scopes: Vec<NoHashHashMap<StrSymbol, EnvVal>>,
    scope_map: NoHashHashMap<StrSymbol, Vec<usize>>,
    curr_scope: usize,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::with_capacity_and_hasher(16, BuildHasherDefault::default())],
            curr_scope: 0,
            scope_map: HashMap::with_capacity_and_hasher(16, BuildHasherDefault::default()),
        }
    }

    #[inline(always)]
    pub fn define(&mut self, name: StrSymbol, value: EnvVal) {
        self.scopes[self.curr_scope].insert(name, value);
        
        if let Some(scopes) = self.scope_map.get_mut(&name) {
            scopes.push(self.curr_scope);
        } else {
            let mut scopes = Vec::with_capacity(4);
            scopes.push(self.curr_scope);
            self.scope_map.insert(name, scopes);
        }
    }

    #[inline]
    pub fn has_var(&self, name: &StrSymbol) -> bool {
        self.scopes[self.curr_scope].contains_key(name)
    }

    #[inline(always)]
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::with_capacity_and_hasher(4, BuildHasherDefault::default()));
        self.curr_scope = self.scopes.len() - 1;
    }

    #[inline(always)]
    pub fn pop_scope(&mut self) {
        let prev_scope = self.scopes.pop().unwrap();
        self.curr_scope -= 1;

        for value in prev_scope.keys() {
            self.scope_map.get_mut(&value).unwrap().pop();
        }
    }

    #[inline(always)]
    pub fn update(&mut self, name: &StrSymbol, value: EnvVal) {
        if let Some(old_val) = self.scopes[self.curr_scope].get_mut(name) {
            *old_val = value;
            return;
        }

        if let Some(scopes) = self.scope_map.get(name) {
            if let Some(old_val) = self.scopes[*scopes.last().unwrap()].get_mut(name) {
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

        if let Some(scopes) = self.scope_map.get(name) {
            if let Some(val) = self.scopes[*scopes.last().unwrap()].get(name) {
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
