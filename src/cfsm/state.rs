use std::{collections::HashMap, fmt::Debug, hash::Hash, ptr::NonNull};

use super::item::ItemSet;
use crate::Symbol;

/// Represents a [state](https://en.wikipedia.org/wiki/LR_parser#Finite_state_machine)
/// in the [`Cfsm`] containing an [`ItemSet`] and transitions based on symbols
/// to other states.
///
/// [`Cfsm`]: super::Cfsm
#[derive(Debug)]
pub(crate) struct State<V, T> {
    /// Id of the [`State`].
    pub(crate) id: StateId,
    /// Set of items this [`State`] currently represents.
    pub(crate) item_set: ItemSet<V, T>,
    /// Transitions to other [`State`]s based on symbol input.
    pub(crate) transitions: HashMap<WSymbol<V, T>, StateId>,
}

pub(crate) struct WSymbol<V, T>(NonNull<Symbol<V, T>>);

pub(crate) type StateId = usize;

impl<V, T> State<V, T> {
    /// Constructs a new [`State`] with the given id and [`ItemSet`].
    pub(super) fn new(id: StateId, item_set: ItemSet<V, T>) -> Self {
        Self {
            id,
            item_set,
            transitions: HashMap::new(),
        }
    }

    /// Checks whether two [`State`]s have the same [`ItemSet`].
    ///
    /// This is needed because [`State`] implements [`PartialEq`] and [`Eq`]
    /// based on the id and not the contents of the [`State`].
    /// [`PartialOrd`] and [`Ord`] require that [`PartialEq`] and [`Eq`] are
    /// implemented, thus implementing the latter based on [`ItemSet`]
    /// comparison would complicate trait bounds of functions using the
    /// [`Ord`] and [`PartialOrd`] implementations of [`State`].
    pub(super) fn has_item_set(&self, item_set: &ItemSet<V, T>) -> bool
    where
        ItemSet<V, T>: Eq,
    {
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

impl<V, T> AsRef<Symbol<V, T>> for WSymbol<V, T> {
    fn as_ref(&self) -> &Symbol<V, T> {
        // SAFETY: `Symbol` comes from `Grammar` residing in a pinned `Cfsm` which is
        // *NOT* `Unpin`.
        unsafe { self.0.as_ref() }
    }
}

impl<V, T> From<&Symbol<V, T>> for WSymbol<V, T> {
    fn from(value: &Symbol<V, T>) -> Self {
        Self(NonNull::from(value))
    }
}

impl<V, T> Debug for WSymbol<V, T>
where
    Symbol<V, T>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", &self.as_ref())
    }
}

impl<V, T> PartialEq for WSymbol<V, T>
where
    Symbol<V, T>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<V, T> Eq for WSymbol<V, T> where Symbol<V, T>: Eq {}

impl<V, T> Hash for WSymbol<V, T>
where
    Symbol<V, T>: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
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
