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
    Fn(*const (Vec<TIdentifier>, BindedStmt)),
}

type ScopeVisitCounter = [usize; 65536];

type VisitingNumber = usize;
type ScopeDepth = usize;

pub struct VarStack {
    stack: [MaybeUninit<EnvVal>; 256],
    top: usize,
}

impl VarStack {
    #[inline]
    pub fn create(val: EnvVal) -> Self {
        let mut stack: [MaybeUninit<EnvVal>; 256] =
            unsafe { MaybeUninit::uninit().assume_init() };

        stack[0] = MaybeUninit::new(EnvVal::Lt(LiteralType::Null)); //to avoid deallocation ;)
                                                                            //
        stack[1] = MaybeUninit::new(val);

        Self {
            stack,
            top: 1,
        }
    }

    // #[inline(always)]
    // pub fn push(&mut self) {
    //     self.top += 1;
    //     self.stack[self.top] = MaybeUninit::new((EnvVal::Lt(LiteralType::Null), 0, 0));
    // }

    #[inline(always)]
    pub fn pop(&mut self) {
        self.top -= 1;
    }

    #[inline(always)]
    pub fn get_top(&self) -> &EnvVal {
        unsafe { &*self.stack[self.top].as_ptr() as &EnvVal }
    }

    #[inline(always)]
    pub fn define_top(&mut self, value: EnvVal) {
        self.top += 1;
        self.stack[self.top].write(value);
    }

    #[inline(always)]
    pub fn update_top(&mut self, value: EnvVal) {
        self.stack[self.top].write(value);
    }
}

pub struct Environment {
    vars: Vec<*mut VarStack>,

    visit_counter: ScopeVisitCounter,

    curr_scope: usize,
    // var_scope_map: RefCell<NoHashHashMap<StrSymbol, Vec<usize>>>,
}

macro_rules! leak {
    ($var:expr) => {
        Box::leak(Box::new($var)) as *mut _
    }
}

impl Environment {
    pub fn new(symbol_count: usize) -> Self {
        let mut vars = Vec::with_capacity(symbol_count);
        for _ in 0..symbol_count {
            vars.push(leak!(VarStack::create(EnvVal::Lt(LiteralType::Null))))
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
            self.vars.push(leak!(VarStack::create(EnvVal::Lt(LiteralType::Null))));
        }
    }

    #[inline(always)]
    pub fn define(&mut self, name: StrSymbol, value: EnvVal) {
        unsafe { &mut *(self.vars[name.to_usize()]) }.define_top(value);
    }

    #[inline]
    pub fn has_var(&self, name: &StrSymbol) -> bool {
        // self.vars[name.to_usize()].()
        //
        true
    }

    #[inline(always)]
    pub fn push_scope(&mut self) {
        // self.curr_scope += 1;
        // if self.curr_scope >= 65536 {
        //     panic!("Nested scoping limit exceeded!")
        // }
        //
        // self.visit_counter[self.curr_scope] += 1;
    }

    #[inline(always)]
    pub fn pop_scope(&mut self) {
        // self.curr_scope -= 1;
    }

    #[inline(always)]
    pub fn update(&mut self, name: &StrSymbol, value: EnvVal) {
        unsafe { &mut *(self.vars[name.to_usize()]) }.update_top(value);
    }

    #[inline(always)]
    pub fn get_varstack(&mut self, name: &StrSymbol) -> &mut VarStack {
         unsafe { &mut *self.vars[name.to_usize()] }
    }

    #[inline(always)]
    pub fn get(&mut self, name: &StrSymbol) -> &EnvVal {
         unsafe { &mut *(self.vars[name.to_usize()]) }.get_top()
    }
}

#[inline(always)]
pub fn env_val_to_literal(env_val: &EnvVal) -> LiteralType {
    match env_val {
        EnvVal::Lt(literal) => literal.clone(),
        EnvVal::Fn(..) => LiteralType::Str(Box::leak(Box::new("[fn]".to_owned()))),
    }
}
