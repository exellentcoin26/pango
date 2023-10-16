use std::fmt::Debug;

pub use cfsm::Cfsm;
pub use grammar::Grammar;
pub use parser::{
    slr::Slr,
    traits::{TerminalEq, TerminalHash},
};

mod cfsm;
mod cst;
mod grammar;
mod parser;

// TODO: Allow parse table terminal entries to be of a different type, but implement TerminalEq and TerminalHash.

/// Represents single character/lexeme/variable in a [body](https://en.wikipedia.org/wiki/Parsing_expression_grammar#Syntax)
/// of a rule.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol<V, T> {
    Terminal(T),
    Variable(V),
    // Note: `Eof` cannot be a public symbol, because it is implicitly added when interpreting the
    // grammar. Explicitly adding this would break the parser (generator).
    // Eof,
    Epsilon,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Terminal<T> {
    T(T),
    Eof,
}

/// List of [`Symbol`]s a `Variable` maps to.
pub(crate) type Body<V, T> = Vec<Symbol<V, T>>;

impl<T> From<T> for Terminal<T> {
    fn from(value: T) -> Self {
        Self::T(value)
    }
}
