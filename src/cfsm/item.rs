use crate::{Body, Grammar, Symbol};

use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
    hash::Hash,
    ptr::NonNull,
};

/// Set of [`ItemBody`] structs.
pub(super) type ItemBodies<V, T> = HashSet<ItemBody<V, T>>;

/// Wrapper around [`Body`] containing a bullet/cursor for reading symbols.
pub(super) struct ItemBody<V, T> {
    body: NonNull<Body<V, T>>,
    pub(super) cursor: usize,
}

/// Set of [items](https://en.wikipedia.org/wiki/LR_parser#Items) in a state of the
/// [`Cfsm`].
///
/// [`Cfsm`]: super::Cfsm
#[derive(Debug)]
pub(super) struct ItemSet<V, T> {
    items: HashMap<V, ItemBodies<V, T>>,
}

impl<V, T> ItemBody<V, T> {
    /// Returns the [`Body`] the [`ItemBody`] references.
    pub(super) fn get_body(&self) -> &Body<V, T> {
        // SAFETY: The struct containing the grammar the body is from, is pinned and
        // upholds the invariant of never being moved.
        unsafe { self.body.as_ref() }
    }

    /// Returns the [`Symbol`] the bullet/cursor is currently reading.
    pub(super) fn get_cursor_symbol(&self) -> Option<&Symbol<V, T>> {
        self.get_body().get(self.cursor)
    }

    /// Returns the [`Variable`](Symbol::Variable) the bullet/cursor is
    /// currently reading.
    pub(super) fn get_cursor_variable(&self) -> Option<&V> {
        self.get_cursor_symbol().and_then(|s| match s {
            Symbol::Variable(v) => Some(v),
            Symbol::Terminal(_) => None,
            Symbol::Epsilon => unreachable!("cursor should never be on an epsilon"),
        })
    }

    /// Returns the [`Terminal`](Symbol::Terminal) the bullet/cursor is
    /// currently reading.
    pub(super) fn get_cursor_terminal(&self) -> Option<&T> {
        self.get_cursor_symbol().and_then(|s| match s {
            Symbol::Terminal(t) => Some(t),
            Symbol::Variable(_) => None,
            Symbol::Epsilon => unreachable!("cursor should never be on an epsilon"),
        })
    }

    /// Advances the bullet/cursor in the [`ItemBody`] by one (or more if the
    /// next symbols are [`Epsilon`](Symbol::Epsilon)).
    pub(super) fn advance(mut self) -> Self {
        // advance the cursor by one, plus the amount of epsilon terminals (they are by
        // definition already read)
        self.cursor += 1 + self
            .get_body()
            .iter()
            .skip(self.cursor + 1)
            .take_while(|s| matches!(s, Symbol::Epsilon))
            .count();
        self.cursor = self.cursor.min(self.get_body().len());

        self
    }
}

impl<V, T> ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    Symbol<V, T>: Eq + Hash,
{
    /// Groups the items in the [`ItemSet`] by the [`Symbol`] they are currently
    /// reading, and returns an iterator over them.
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
    /// Returns an iterator over the [`Variable`](Symbol::Variable)-[`ItemBody`]
    /// pairs in the [`ItemSet`].
    pub(super) fn iter(&self) -> impl Iterator<Item = (V, ItemBody<V, T>)> + '_ {
        self.items
            .iter()
            .flat_map(|(head, bodies)| std::iter::repeat(*head).zip(bodies.iter().copied()))
    }
}

impl<V, T> ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    ItemBody<V, T>: Eq + Hash,
{
    /// Constructs the closure of the incomplete [`ItemSet`] based on the
    /// [`Grammar`].
    pub(super) fn from_incomplete_map(
        mut items: HashMap<V, ItemBodies<V, T>>,
        grammar: &Grammar<V, T>,
    ) -> Self {
        let mut pending_bodies =
            VecDeque::from_iter(items.values().flat_map(|bodies| bodies.iter()).copied());

        while let Some(item_body) = pending_bodies.pop_front() {
            let Some(head) = item_body.get_cursor_variable().copied() else {
                continue;
            };

            let new_item_bodies = grammar
                .get_rule_bodies(head)
                .map(|bodies| bodies.iter().map(ItemBody::from))
                .expect("variable should have an associated rule");

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

impl<V, T> Clone for ItemBody<V, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<V, T> Copy for ItemBody<V, T> {}

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

impl<V, T> Debug for ItemBody<V, T>
where
    Body<V, T>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ItemBody")
            // SAFETY: The grammar this body points to should be pinned.
            .field("body", unsafe { self.body.as_ref() })
            .field("cursor", &self.cursor)
            .finish()
    }
}

// Implementing these manually prevents trait bounds on the struct, which are
// bubbled up to all types using the struct (e.g., `State`).
impl<V, T> PartialEq for ItemSet<V, T>
where
    V: Eq + Hash,
    ItemBody<V, T>: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}
impl<V, T> Eq for ItemSet<V, T>
where
    V: Eq + Hash,
    ItemBody<V, T>: Eq + Hash,
{
}

impl<V, T> PartialEq for ItemBody<V, T>
where
    Body<V, T>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.get_body() == other.get_body() && self.cursor == other.cursor
    }
}
impl<V, T> Eq for ItemBody<V, T> where Body<V, T>: Eq {}

impl<V, T> Hash for ItemBody<V, T>
where
    Body<V, T>: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // hash the body instead of the pointer to the body
        self.get_body().hash(state);
        self.cursor.hash(state);
    }
}

impl<V, T> From<((&V, &HashSet<Body<V, T>>), &Grammar<V, T>)> for ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    ItemBody<V, T>: Eq + Hash,
{
    fn from(((head, bodies), grammar): ((&V, &HashSet<Body<V, T>>), &Grammar<V, T>)) -> Self {
        let items = HashMap::from([(*head, bodies.iter().map(ItemBody::from).collect())]);
        Self::from_incomplete_map(items, grammar)
    }
}

impl<V, T> From<(HashMap<V, ItemBodies<V, T>>, &Grammar<V, T>)> for ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    ItemBody<V, T>: Eq + Hash,
{
    fn from((items, grammar): (HashMap<V, ItemBodies<V, T>>, &Grammar<V, T>)) -> Self {
        Self::from_incomplete_map(items, grammar)
    }
}

#[cfg(test)]
mod tests {
    use super::{ItemBody, ItemSet};
    use crate::{Grammar, Symbol};

    use std::collections::{HashMap, HashSet};

    #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
    enum Variable {
        Function,
        Body,
        Prototype,
        Statement,
    }

    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    enum Terminal {
        Bracket,
        Identifier(String),
        Semi,
    }

    impl<T> From<Variable> for Symbol<Variable, T> {
        fn from(v: Variable) -> Self {
            Symbol::Variable(v)
        }
    }

    impl<V> From<Terminal> for Symbol<V, Terminal> {
        fn from(t: Terminal) -> Self {
            Symbol::Terminal(t)
        }
    }

    #[test]
    fn item_body_epsilon() {
        let body = vec![Symbol::<Variable, Terminal>::Epsilon];
        let item_body = ItemBody::from(&body);

        assert_eq!(item_body.cursor, 1);
        assert_eq!(item_body.get_cursor_symbol(), None);
    }

    #[test]
    fn item_body_epsilon_advance() {
        let body = vec![
            Symbol::Variable(Variable::Function),
            Symbol::Epsilon,
            Symbol::Epsilon,
            Symbol::Epsilon,
            Symbol::Terminal(Terminal::Semi),
        ];
        let item_body = ItemBody::from(&body);

        assert_eq!(item_body.cursor, 0);
        assert_eq!(
            item_body.get_cursor_symbol(),
            Some(&Symbol::Variable(Variable::Function))
        );
        assert_eq!(item_body.get_cursor_variable(), Some(&Variable::Function));
        assert_eq!(item_body.get_cursor_terminal(), None);

        let item_body = item_body.advance();
        assert_eq!(
            item_body.get_cursor_symbol(),
            Some(&Symbol::Terminal(Terminal::Semi))
        );
        assert_eq!(item_body.get_cursor_terminal(), Some(&Terminal::Semi));
        assert_eq!(item_body.get_cursor_variable(), None);
        assert_eq!(item_body.cursor, 4);
    }

    #[test]
    fn item_set_from_incomplete_map() {
        let function_body = vec![Variable::Prototype.into(), Variable::Body.into()];
        let prototype_bodies = (
            vec![Terminal::Identifier(String::new()).into()],
            vec![Variable::Statement.into(), Terminal::Semi.into()],
        );
        let statement_body = vec![Terminal::Identifier(String::new()).into()];
        let body_bodies = (
            vec![
                Terminal::Bracket.into(),
                Variable::Function.into(),
                Terminal::Bracket.into(),
            ],
            vec![
                Terminal::Bracket.into(),
                Terminal::Identifier(String::new()).into(),
                Terminal::Semi.into(),
                Terminal::Bracket.into(),
            ],
        );

        let grammar = Grammar::builder()
            .with_start_variable(Variable::Function)
            .with_rule(Variable::Function, function_body.clone())
            .with_rules(
                Variable::Prototype,
                [prototype_bodies.0.clone(), prototype_bodies.1.clone()],
            )
            .with_rule(Variable::Statement, statement_body.clone())
            .with_rules(
                Variable::Body,
                [body_bodies.0.clone(), body_bodies.1.clone()],
            )
            .build();

        let item_set = ItemSet::from((grammar.get_start_variable_rules(), &grammar));

        assert_eq!(
            item_set.items,
            HashMap::from([
                (
                    Variable::Function,
                    HashSet::from([ItemBody::from(&function_body)])
                ),
                (
                    Variable::Prototype,
                    HashSet::from([
                        ItemBody::from(&prototype_bodies.0),
                        ItemBody::from(&prototype_bodies.1)
                    ])
                ),
                (
                    Variable::Statement,
                    HashSet::from([ItemBody::from(&statement_body)])
                )
            ])
        )
    }
}
