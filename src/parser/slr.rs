use super::table::ParseTable;
use crate::{cfsm::Cfsm, Grammar};

use std::{borrow::Cow, hash::Hash};

#[derive(Debug)]
pub struct Slr<'g, V, T>
where
    Grammar<V, T>: Clone,
{
    table: ParseTable<'g, V, T>,
}

impl<'g, V, T> Slr<'g, V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
    Grammar<V, T>: Clone,
{
    pub fn new(grammar: Grammar<V, T>) -> Self {
        let cfsm = Cfsm::from_grammar(Cow::Owned(grammar));

        let table = ParseTable::new_slr(cfsm).expect("grammar is not SLR");

        Self { table }
    }
}
