use super::item::ItemSet;
use crate::Symbol;

use std::collections::HashMap;

/// Represents a [state](https://en.wikipedia.org/wiki/LR_parser#Finite_state_machine)
/// in the [`Cfsm`] containing an [`ItemSet`] and transitions based on symbols
/// to other states.
///
/// [`Cfsm`]: super::Cfsm
#[derive(Debug)]
pub(super) struct State<V, T> {
    /// Id of the [`State`].
    pub(super) id: StateId,
    /// Set of items this [`State`] currently represents.
    pub(super) item_set: ItemSet<V, T>,
    /// Transitions to other [`State`]s based on symbol input.
    pub(super) transitions: HashMap<Symbol<V, T>, StateId>,
}

pub(super) type StateId = usize;

impl<V, T> State<V, T> {
    /// Constructs a new [`State`] with the given id and [`ItemSet`].
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
    /// Checks whether two [`State`]s have the same [`ItemSet`].
    ///
    /// This is needed because [`State`] implements [`PartialEq`] and [`Eq`]
    /// based on the id and not the contents of the [`State`].
    /// [`PartialOrd`] and [`Ord`] require that [`PartialEq`] and [`Eq`] are
    /// implemented, thus implementing the latter based on [`ItemSet`]
    /// comparison would complicate trait bounds of functions using the
    /// [`Ord`] and [`PartialOrd`] implementations of [`State`].
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

/// Generator struct for the [`StateId`].
///
/// Note: This is overkill, but allows for cleaner code in the (already
/// non-trivial implementation) of the cfsm construction.
#[derive(Default)]
pub(super) struct StateIdGenerator {
    current: StateId,
}

impl StateIdGenerator {
    /// Returns the next [`StateId`] available.
    pub(super) fn next(&mut self) -> StateId {
        let result = self.current;
        self.current += 1;
        result
    }
}
