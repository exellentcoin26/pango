#![allow(unused)]

pub use grammar::Grammar;

mod cfsm;
mod grammar;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol<V, T> {
    Terminal(T),
    Variable(V),
    Epsilon,
}

pub(crate) type Body<V, T> = Vec<Symbol<V, T>>;

struct LRParser {}
