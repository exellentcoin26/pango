use crate::{Body, Grammar, Symbol};

use std::{
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    hash::Hash,
    marker::PhantomPinned,
    pin::Pin,
    ptr::NonNull,
};

struct Cfsm<V, T> {
    start_state: StateId,
    states: BTreeSet<State<V, T>>,
    grammar: Grammar<V, T>,
    _pin: PhantomPinned,
}

type StateId = usize;

struct State<V, T> {
    id: StateId,
    item_set: ItemSet<V, T>,
    transitions: HashMap<Symbol<V, T>, StateId>,
}

struct ItemSet<V, T> {
    items: HashMap<V, ItemBodies<V, T>>,
}

type ItemBodies<V, T> = HashSet<ItemBody<V, T>>;

#[derive(Debug, PartialEq, Eq, Hash)]
struct ItemBody<V, T> {
    body: NonNull<Body<V, T>>,
    cursor: usize,
}

impl<V, T> Clone for ItemBody<V, T> {
    fn clone(&self) -> Self {
        Self {
            body: self.body,
            cursor: self.cursor,
        }
    }
}
impl<V, T> Copy for ItemBody<V, T> {}

impl<V, T> Cfsm<V, T> {
    fn builder(grammar: Grammar<V, T>) -> CfsmBuilder<V, T> {
        CfsmBuilder::new(grammar)
    }

    fn get_grammar(&self) -> &Grammar<V, T> {
        &self.grammar
    }
}

impl<V, T> Cfsm<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    fn from_grammar(grammar: Grammar<V, T>) -> Pin<Box<Self>> {
        let mut builder = Self::builder(grammar);

        let mut state_id_generator = StateIdGenerator::default();

        let start_state_id = state_id_generator.next();
        builder.set_start_state_id(start_state_id);
        let mut pending_states = VecDeque::from([State::new(
            start_state_id,
            ItemSet::from((
                builder.get_grammar().get_start_variable_rules(),
                builder.get_grammar(),
            )),
            builder.get_grammar(),
        )]);

        while let Some(mut state) = pending_states.pop_front() {
            // try all symbols with a bullet reading them

            let State {
                ref item_set,
                ref mut transitions,
                ..
            } = state;

            todo!();

            builder.add_state(state);
        }

        builder.build()
    }
}

impl<V, T> From<Grammar<V, T>> for Pin<Box<Cfsm<V, T>>>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    fn from(grammar: Grammar<V, T>) -> Self {
        Cfsm::from_grammar(grammar)
    }
}

impl<V, T> State<V, T> {
    fn new(id: StateId, item_set: ItemSet<V, T>, grammar: &Grammar<V, T>) -> Self {
        Self {
            id,
            item_set,
            transitions: HashMap::new(),
        }
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

impl<V, T> ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    fn from_incomplete_item_set(
        mut items: HashMap<V, ItemBodies<V, T>>,
        grammar: &Grammar<V, T>,
    ) -> Self {
        let mut pending_bodies =
            VecDeque::from_iter(items.values().flat_map(|bodies| bodies.iter()).copied());

        while let Some(item_body) = pending_bodies.pop_front() {
            let Some(head) = item_body.get_cursor_variable() else {
                continue;
            };

            let Some(new_item_bodies) = grammar.get_rule_bodies(head).map(|bodies| bodies.iter().map(ItemBody::from)) else {
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
        Self::from_incomplete_item_set(items, grammar)
    }
}

impl<V, T> ItemBody<V, T> {
    fn get_body(&self) -> &Body<V, T> {
        // SAFETY: The struct containing the grammar the body is from, is pinned and
        // upholds the invariant of never being moved.
        unsafe { self.body.as_ref() }
    }
}

impl<V, T> ItemBody<V, T>
where
    V: Copy,
{
    fn get_cursor_variable(&self) -> Option<V> {
        self.get_body().get(self.cursor).and_then(|s| match s {
            Symbol::Variable(v) => Some(*v),
            Symbol::Terminal(_) => None,
            Symbol::Epsilon => None,
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

struct CfsmBuilder<V, T> {
    start_state: Option<StateId>,
    cfsm: Pin<Box<Cfsm<V, T>>>,
}

impl<V, T> CfsmBuilder<V, T> {
    fn new(grammar: Grammar<V, T>) -> Self {
        Self {
            start_state: None,
            cfsm: Box::pin(Cfsm {
                start_state: 0,
                states: BTreeSet::new(),
                grammar,
                _pin: PhantomPinned,
            }),
        }
    }

    fn get_grammar(&self) -> &Grammar<V, T> {
        &self.cfsm.grammar
    }

    fn set_start_state_id(&mut self, state_id: StateId) {
        self.start_state = Some(state_id);
        *self.get_start_state_id_mut() = state_id;
    }

    fn add_state(&mut self, state: State<V, T>) {
        self.get_states_mut().insert(state);
    }

    fn get_states_mut(&mut self) -> &mut BTreeSet<State<V, T>> {
        // SAFETY: Returning a mutable reference to the `states` field, does not move
        // the struct, nor does moving out of the mutable reference. The
        // `states` field does not contain referenced data, only references.
        &mut unsafe { self.cfsm.as_mut().get_unchecked_mut() }.states
    }

    fn get_start_state_id_mut(&mut self) -> &mut StateId {
        &mut unsafe { self.cfsm.as_mut().get_unchecked_mut() }.start_state
    }

    fn build(mut self) -> Pin<Box<Cfsm<V, T>>> {
        // `Cfsm` is a self-referential struct, thus the pin implementation is used.
        // This means that it needs to be constructed first with the grammar and
        // then modified.

        let (state_ids, destination_ids) = self.get_states_mut().iter().fold(
            (HashSet::new(), HashSet::new()),
            |(mut state_ids, mut destination_ids),
             State {
                 id, transitions, ..
             }| {
                state_ids.insert(*id);
                destination_ids.extend(transitions.values().copied());

                (state_ids, destination_ids)
            },
        );

        if !destination_ids.is_subset(&state_ids) {
            panic!("one or more destination states found that do not exist");
        }

        // the `start_state` on the inner cfsm is already set
        match self.start_state {
            Some(start_state) => {
                if !state_ids.contains(&start_state) {
                    panic!("start state is not a valid state");
                }
                start_state
            }
            None => panic!("start state not set"),
        };

        self.cfsm
    }
}

#[derive(Default)]
struct StateIdGenerator {
    current: StateId,
}

impl StateIdGenerator {
    fn next(&mut self) -> StateId {
        let result = self.current;
        self.current += 1;
        result
    }
}
