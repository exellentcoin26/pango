use super::item::ItemSet;
use crate::Symbol;

use std::collections::HashMap;

#[derive(Debug)]
pub(super) struct State<V, T> {
    pub(super) id: StateId,
    pub(super) item_set: ItemSet<V, T>,
    pub(super) transitions: HashMap<Symbol<V, T>, StateId>,
}

pub(super) type StateId = usize;

impl<V, T> State<V, T> {
    pub(super) fn new(id: StateId, item_set: ItemSet<V, T>) -> Self {
        Self {
            id,
            item_set,
            transitions: HashMap::new(),
        }
    }
}

impl<V, T> State<V, T>
where
    ItemSet<V, T>: Eq,
{
    pub(super) fn has_item_set(&self, item_set: &ItemSet<V, T>) -> bool {
        self.item_set == *item_set
    }
}

impl<V, T> PartialEq for State<V, T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<V, T> Eq for State<V, T> {}

impl<V, T> PartialOrd for State<V, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.id.cmp(&other.id))
    }
}

impl<V, T> Ord for State<V, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

#[derive(Default)]
pub(super) struct StateIdGenerator {
    current: StateId,
}

impl StateIdGenerator {
    pub(super) fn next(&mut self) -> StateId {
        let result = self.current;
        self.current += 1;
        result
    }
}
