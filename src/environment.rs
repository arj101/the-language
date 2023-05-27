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
    values: FxHashMap<String, EnvVal>,
    parent_scope: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut values = FxHashMap::default();
        values.insert(
            "undefined".to_owned(),
            EnvVal::Lt(Rc::new(LiteralType::Null)),
        );
        Self {
            values,
            parent_scope: None,
        }
    }

    pub fn new_as_child(parent: Rc<Environment>) -> Self {
        let mut values = FxHashMap::default();
        values.insert(
            "undefined".to_owned(),
            EnvVal::Lt(Rc::new(LiteralType::Null)),
        );
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
            // panic!("Attempt to read from undefined variable: `{name}`")
            println_raw!("{name}: no such variable in scope");
            self.values.get("undefined").unwrap()
        }
    }

    pub fn destroy(&mut self) -> Option<Rc<Environment>> {
        self.parent_scope.borrow_mut().take()
    }
}

pub fn env_val_to_literal(env_val: &EnvVal) -> LiteralType {
    match env_val {
        EnvVal::Lt(literal) => Rc::as_ref(literal).clone(),
        EnvVal::Fn(..) => LiteralType::Str(StrType::Strict("[fn]".to_owned())),
    }
}
