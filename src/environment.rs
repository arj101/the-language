use crate::{
    expr::{BindedStmt, LiteralType, Stmt},
    tokens::StrSymbol,
    tokens::TIdentifier,
};
use rustc_hash::FxHashMap;
use std::{borrow::BorrowMut, collections::HashMap, mem::MaybeUninit, rc::Rc, time::Instant};
use string_interner::Symbol;

use nohash_hasher::NoHashHasher;
use std::hash::BuildHasherDefault;

use std::cell::RefCell;

use crate::println_raw;

type NoHashHashMap<K, V> = HashMap<K, V, BuildHasherDefault<NoHashHasher<usize>>>;

#[derive(Debug)]
pub enum EnvVal {
    Lt(LiteralType),
    Fn(*const (Vec<TIdentifier>, Vec<BindedStmt>)),
}

type ScopeVisitCounter = [usize; 65536];

type VisitingNumber = usize;
type ScopeDepth = usize;

pub struct VarStack {
    stack: [MaybeUninit<(EnvVal, ScopeDepth, VisitingNumber)>; 256],
    top: usize,

    visit_counter: *const ScopeVisitCounter,
}

impl VarStack {
    #[inline]
    pub fn create(val: EnvVal, curr_scope: usize, visit_counter: *const ScopeVisitCounter) -> Self {
        let mut stack: [MaybeUninit<(EnvVal, usize, usize)>; 256] =
            unsafe { MaybeUninit::uninit().assume_init() };

        stack[0] = MaybeUninit::new((EnvVal::Lt(LiteralType::Null), 0, 0)); //to avoid deallocation ;)
                                                                            //
        stack[1] = MaybeUninit::new((val, curr_scope, unsafe { (*visit_counter)[curr_scope] }));

        Self {
            stack,
            top: 1,
            visit_counter,
        }
    }

    #[inline(always)]
    pub fn visit_count(&self, scope_idx: usize) -> usize {
        unsafe { (*self.visit_counter)[scope_idx] }
    }

    #[inline(always)]
    pub fn var_at(&self, idx: usize) -> &(EnvVal, usize, usize) {
        unsafe { &*self.stack[idx].as_ptr() as &(EnvVal, usize, usize) }
    }

    #[inline(always)]
    pub fn define(&mut self, val: EnvVal, curr_scope: usize) {
        let mut scope_idx = self.var_at(self.top).1;
        while (scope_idx >= curr_scope || self.var_at(self.top).2 != self.visit_count(scope_idx))
            && self.top > 0
        {
            self.top -= 1;
            scope_idx = self.var_at(self.top).1;
        }

        self.top += 1;
        if self.top < self.stack.len() {
            self.stack[self.top].write((val, curr_scope, self.visit_count(curr_scope)));
            return;
        }

        panic!("Maximum shadowing depth exceeded");
    }

    #[inline(always)]
    pub fn get(&mut self, curr_scope: usize) -> &EnvVal {
        let mut scope_idx = self.var_at(self.top).1;
        while scope_idx > curr_scope || self.var_at(self.top).2 != self.visit_count(scope_idx) {
            self.top -= 1;
            scope_idx = self.var_at(self.top).1;
        }

        &self.var_at(self.top).0
    }
}

pub struct Environment {
    vars: Vec<Option<*mut VarStack>>,

    visit_counter: ScopeVisitCounter,

    curr_scope: usize,
    // var_scope_map: RefCell<NoHashHashMap<StrSymbol, Vec<usize>>>,
}

impl Environment {
    pub fn new(symbol_count: usize) -> Self {
        let mut vars = Vec::with_capacity(symbol_count);
        for _ in 0..symbol_count {
            vars.push(None)
        }

        Self {
            curr_scope: 0,
            visit_counter: [0; 65536],
            vars,
        }
    }

    pub fn expand_capacity(&mut self, symbol_count: usize) {
        let curr_len = self.vars.len() as isize;
        for _ in 0..(symbol_count as isize - curr_len).max(0) {
            self.vars.push(None);
        }
    }

    #[inline(always)]
    pub fn define(&mut self, name: StrSymbol, value: EnvVal) {
        if let Some(var_stack) = self.vars[name.to_usize()] {
            let var_stack: *mut VarStack = var_stack;
            unsafe { &mut *var_stack }.define(value, self.curr_scope);
            return;
        }

        let stack = Box::new(VarStack::create(
            value,
            self.curr_scope,
            &self.visit_counter as *const ScopeVisitCounter,
        ));

        //to avoid huge reallocations when self.vars need to grow
        let stack = Box::leak(stack) as *mut VarStack;

        self.vars[name.to_usize()] = Some(stack);
    }

    #[inline]
    pub fn has_var(&self, name: &StrSymbol) -> bool {
        self.vars[name.to_usize()].is_some()
    }

    #[inline(always)]
    pub fn push_scope(&mut self) {
        self.curr_scope += 1;
        if self.curr_scope >= 65536 {
            panic!("Nested scoping limit exceeded!")
        }

        self.visit_counter[self.curr_scope] += 1;
    }

    #[inline(always)]
    pub fn pop_scope(&mut self) {
        self.curr_scope -= 1;
    }

    #[inline(always)]
    pub fn update(&mut self, name: &StrSymbol, value: EnvVal) {
        if let Some(var_stack) = self.vars[name.to_usize()] {
            let var_stack: *mut VarStack = var_stack;
            return unsafe { &mut *var_stack }.define(value, self.curr_scope);
        }

        println_raw!("{:?}: no such variable in scope", name);
    }

    #[inline(always)]
    pub fn get(&mut self, name: &StrSymbol) -> &EnvVal {
        if let Some(var_stack) = self.vars[name.to_usize()] {
            let var_stack: *mut VarStack = var_stack;
            return unsafe { &mut *var_stack }.get(self.curr_scope);
        }

        panic!("{:?}: no such variable in scope", name);
    }
}

#[inline(always)]
pub fn env_val_to_literal(env_val: &EnvVal) -> LiteralType {
    match env_val {
        EnvVal::Lt(literal) => literal.clone(),
        EnvVal::Fn(..) => LiteralType::Str(Box::leak(Box::new("[fn]".to_owned()))),
    }
}
