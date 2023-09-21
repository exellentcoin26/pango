pub use cfsm::Cfsm;
pub use grammar::Grammar;
pub use parser::slr::Slr;

mod cfsm;
mod cst;
mod grammar;
mod parser;

/// Represents single character/lexeme/variable in a [body](https://en.wikipedia.org/wiki/Parsing_expression_grammar#Syntax)
/// of a rule.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol<V, T> {
    Terminal(T),
    Variable(V),
    Epsilon,
}

/// List of [`Symbol`]s a `Variable` maps to.
pub(crate) type Body<V, T> = Vec<Symbol<V, T>>;
