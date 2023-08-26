use crate::{Body, Grammar, Symbol};

use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    hash::Hash,
    ptr::NonNull,
};

#[derive(Debug)]
pub(super) struct ItemSet<V, T> {
    items: HashMap<V, ItemBodies<V, T>>,
}

impl<V, T> PartialEq for ItemSet<V, T>
where
    V: Eq + Hash,
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}
impl<V, T> Eq for ItemSet<V, T>
where
    V: Eq + Hash,
    T: Eq + Hash,
{
}

pub(super) type ItemBodies<V, T> = HashSet<ItemBody<V, T>>;

#[derive(PartialEq, Eq, Hash)]
pub(super) struct ItemBody<V, T> {
    body: NonNull<Body<V, T>>,
    cursor: usize,
}

impl<V, T> Debug for ItemBody<V, T>
where
    V: Debug,
    T: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ItemBody")
            // SAFETY: The grammar this body points to should be pinned.
            .field("body", unsafe { self.body.as_ref() })
            .field("cursor", &self.cursor)
            .finish()
    }
}

impl<V, T> Clone for ItemBody<V, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<V, T> Copy for ItemBody<V, T> {}

impl<V, T> ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    Symbol<V, T>: Eq + Hash,
    ItemBody<V, T>: Eq + Hash,
{
    pub(super) fn iter_by_cursor_symbol(
        &self,
    ) -> impl Iterator<Item = (&Symbol<V, T>, HashMap<V, HashSet<&ItemBody<V, T>>>)> {
        self.items
            .iter()
            .fold(
                HashMap::new(),
                |mut group_by_symbol, (head, item_bodies)| {
                    for item_body in item_bodies {
                        let Some(symbol) = item_body.get_cursor_symbol() else {
                            continue;
                        };

                        group_by_symbol
                            .entry(symbol)
                            .or_insert_with(HashMap::new)
                            .entry(*head)
                            .or_insert_with(HashSet::new)
                            .insert(item_body);
                    }

                    group_by_symbol
                },
            )
            .into_iter()
    }
}

impl<V, T> ItemSet<V, T>
where
    V: Copy,
{
    pub(super) fn iter(&self) -> impl Iterator<Item = (V, ItemBody<V, T>)> + '_ {
        self.items
            .iter()
            .flat_map(|(head, bodies)| std::iter::repeat(*head).zip(bodies.iter().copied()))
    }
}

impl<V, T> ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    pub(super) fn from_incomplete_map(
        mut items: HashMap<V, ItemBodies<V, T>>,
        grammar: &Grammar<V, T>,
    ) -> Self {
        let mut pending_bodies =
            VecDeque::from_iter(items.values().flat_map(|bodies| bodies.iter()).copied());

        while let Some(item_body) = pending_bodies.pop_front() {
            let Some(head) = item_body.get_cursor_variable() else {
                continue;
            };

            let Some(new_item_bodies) = grammar
                .get_rule_bodies(head)
                .map(|bodies| bodies.iter().map(ItemBody::from))
            else {
                continue;
            };

            for new_item_body in new_item_bodies {
                if items
                    .entry(head)
                    .or_insert_with(ItemBodies::new)
                    .insert(new_item_body)
                {
                    pending_bodies.push_back(new_item_body);
                }
            }
        }

        Self { items }
    }
}

impl<V, T> From<((&V, &HashSet<Body<V, T>>), &Grammar<V, T>)> for ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    fn from(((head, bodies), grammar): ((&V, &HashSet<Body<V, T>>), &Grammar<V, T>)) -> Self {
        let items = HashMap::from([(*head, bodies.iter().map(ItemBody::from).collect())]);
        Self::from_incomplete_map(items, grammar)
    }
}

impl<V, T> From<(HashMap<V, ItemBodies<V, T>>, &Grammar<V, T>)> for ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    fn from((items, grammar): (HashMap<V, ItemBodies<V, T>>, &Grammar<V, T>)) -> Self {
        Self::from_incomplete_map(items, grammar)
    }
}

impl<V, T> ItemBody<V, T> {
    pub(super) fn get_body(&self) -> &Body<V, T> {
        // SAFETY: The struct containing the grammar the body is from, is pinned and
        // upholds the invariant of never being moved.
        unsafe { self.body.as_ref() }
    }

    pub(super) fn get_cursor_terminal(&self) -> Option<&T> {
        self.get_cursor_symbol().and_then(|s| match s {
            Symbol::Terminal(t) => Some(t),
            _ => None,
        })
    }

    pub(super) fn get_cursor_symbol(&self) -> Option<&Symbol<V, T>> {
        self.get_body().get(self.cursor)
    }

    pub(super) fn advance(mut self) -> Self {
        // advance the cursor by one, plus the amount of epsilon terminals (they are by
        // definition already read)
        self.cursor += 1 + self
            .get_body()
            .iter()
            .skip(self.cursor + 1)
            .take_while(|s| matches!(s, Symbol::Epsilon))
            .count();
        self.cursor = self.cursor.max(self.get_body().len());

        self
    }
}

impl<V, T> ItemBody<V, T>
where
    V: Copy,
{
    fn get_cursor_variable(&self) -> Option<V> {
        self.get_cursor_symbol().and_then(|s| match s {
            Symbol::Variable(v) => Some(*v),
            _ => None,
        })
    }
}

impl<V, T> From<&Body<V, T>> for ItemBody<V, T> {
    fn from(body: &Body<V, T>) -> Self {
        // `Symbol::Epsilon` has no meaning for the cursor, so when reading it, we
        // immediatly skip it.
        let cursor = body
            .iter()
            .take_while(|s| matches!(s, Symbol::Epsilon))
            .count();

        Self {
            body: NonNull::from(body),
            cursor,
        }
    }
}
