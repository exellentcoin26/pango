use super::table::ParseTable;
use crate::{cfsm::Cfsm, Grammar};

use std::hash::Hash;

#[derive(Debug)]
pub struct Slr<V, T> {
    table: ParseTable<V, T>,
}

impl<V, T> Slr<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    pub fn new(grammar: Grammar<V, T>) -> Self {
        let cfsm = Cfsm::from_grammar(grammar);

        let table = ParseTable::new_slr(cfsm).expect("grammar is not SLR");

        Self { table }
    }
}
