use crate::{
    expr::{BindedStmt, LiteralType, Stmt, StrType},
    tokens::StrSymbol,
    tokens::TIdentifier,
};
use rustc_hash::FxHashMap;
use std::{borrow::BorrowMut, collections::HashMap, rc::Rc, time::Instant};

use nohash_hasher::NoHashHasher;
use std::hash::BuildHasherDefault;

use std::cell::RefCell;

use crate::println_raw;

type NoHashHashMap<K, V> = HashMap<K, V, BuildHasherDefault<NoHashHasher<usize>>>;

#[derive(Debug)]
pub enum EnvVal {
    Lt(LiteralType),
    Fn(Rc<(Vec<TIdentifier>, Vec<BindedStmt>)>),
}

pub struct Environment {
    scopes: Vec<NoHashHashMap<StrSymbol, EnvVal>>,

    curr_scope: usize,

    var_scope_map: RefCell<NoHashHashMap<StrSymbol, Vec<usize>>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut env = Self {
            scopes: Vec::with_capacity(32),
            curr_scope: 0,

            var_scope_map: RefCell::new(HashMap::with_capacity_and_hasher(
                16,
                BuildHasherDefault::default(),
            )),
        };

        for _ in 0..32 {
            env.scopes.push(HashMap::with_capacity_and_hasher(
                16,
                BuildHasherDefault::default(),
            ));
        }

        env
    }

    #[inline(always)]
    pub fn define(&mut self, name: StrSymbol, value: EnvVal) {
        self.scopes[self.curr_scope].insert(name, value);

        if let Some(scopes) = self.var_scope_map.borrow_mut().get_mut(&name) {
            scopes.push(self.curr_scope);
            return;
        }

        let mut scopes = Vec::with_capacity(32);
        scopes.push(self.curr_scope);
        self.var_scope_map.borrow_mut().insert(name, scopes);
    }

    #[inline]
    pub fn has_var(&self, name: &StrSymbol) -> bool {
        self.scopes[self.curr_scope].contains_key(name)
    }

    #[inline(always)]
    pub fn push_scope(&mut self) {
        self.curr_scope += 1;

        if self.curr_scope >= self.scopes.len() {
            self.scopes.push(HashMap::with_capacity_and_hasher(
                16,
                BuildHasherDefault::default(),
            ));
            return;
        }

        if !self.scopes[self.curr_scope].is_empty() {
            self.scopes[self.curr_scope].clear()
        }
    }

    #[inline(always)]
    pub fn pop_scope(&mut self) {
        self.curr_scope -= 1;
    }

    #[inline(always)]
    pub fn update(&mut self, name: &StrSymbol, value: EnvVal) {
        if let Some(old_val) = self.scopes[self.curr_scope].get_mut(name) {
            *old_val = value;
            return;
        }

        if let Some(scopes) = self.var_scope_map.borrow_mut().get_mut(name) {
            while scopes.last().unwrap() > &self.curr_scope {
                scopes.pop();
            }
            while !self.scopes[*scopes.last().unwrap()].contains_key(name) {
                scopes.pop();
            }
            if let Some(old_val) = self.scopes[*scopes.last().unwrap()].get_mut(name) {
                *old_val = value;
            }
        }

        println_raw!("{:?}: no such variable in scope", name);
    }

    #[inline(always)]
    pub fn get(&self, name: &StrSymbol) -> &EnvVal {
        if let Some(val) = self.scopes[self.curr_scope].get(name) {
            return val;
        }

        if let Some(scopes) = self.var_scope_map.borrow_mut().get_mut(name) {
            while scopes.last().unwrap() > &self.curr_scope {
                scopes.pop();
            }

            'outer: loop {
                match self.scopes[*scopes.last().unwrap()].get(name) {
                    Some(val) => return val,
                    None => {
                        if let None = scopes.pop() { break 'outer }
                    }
                }
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
