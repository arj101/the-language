use crate::{
    expr::{LiteralType, Stmt},
    tokens::TIdentifier,
};
use rustc_hash::FxHashMap;
use std::{borrow::BorrowMut, collections::HashMap, rc::Rc, time::Instant};

#[derive(Debug)]
pub enum EnvVal {
    Lt(Rc<LiteralType>),
    Fn(Rc<Vec<TIdentifier>>, Rc<Vec<Stmt>>),
}

pub struct Environment {
    values: FxHashMap<String, EnvVal>,
    parent_scope: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: FxHashMap::default(),
            parent_scope: None,
        }
    }

    pub fn new_as_child(parent: Rc<Environment>) -> Self {
        Self {
            values: FxHashMap::default(),
            parent_scope: Some(parent),
        }
    }

    pub fn define(&mut self, name: String, value: EnvVal) {
        self.values.insert(name, value);
    }

    pub fn has_var(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    pub fn update(&mut self, name: &str, value: EnvVal) {
        if let Some(old_val) = self.values.get_mut(name) {
            *old_val = value;
        } else if let Some(parent) = &mut self.parent_scope {
            Rc::get_mut(parent).unwrap().update(name, value);
        } else {
            panic!("Attempt to assign to undefined variable")
        }
    }

    pub fn get(&self, name: &str) -> &EnvVal {
        if let Some(val) = self.values.get(name) {
            val
        } else if let Some(parent) = &self.parent_scope {
            parent.get(name)
        } else {
            panic!("Attempt to read from undefined variable: `{name}`")
        }
    }

    pub fn destroy(&mut self) -> Option<Rc<Environment>> {
        self.parent_scope.borrow_mut().take()
    }
}
