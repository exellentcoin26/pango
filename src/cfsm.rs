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
    bullet: usize,
}

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
            ItemSet::from(builder.get_grammar().get_start_variable_rules()),
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

impl<V, T> From<(&V, &HashSet<Body<V, T>>)> for ItemSet<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    fn from((head, bodies): (&V, &HashSet<Body<V, T>>)) -> Self {
        // TODO: Calculate item closure
        todo!("calculate item closure")
    }
}

impl<V, T> From<&Body<V, T>> for ItemBody<V, T> {
    fn from(body: &Body<V, T>) -> Self {
        Self {
            body: NonNull::from(body),
            bullet: 0,
        }
    }
}

struct CfsmBuilder<V, T> {
    start_state: Option<StateId>,
    states: BTreeSet<State<V, T>>,
    cfsm: Pin<Box<Cfsm<V, T>>>,
}

impl<V, T> CfsmBuilder<V, T> {
    fn new(grammar: Grammar<V, T>) -> Self {
        Self {
            start_state: None,
            states: BTreeSet::new(),
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
    }

    fn add_state(&mut self, state: State<V, T>) {
        self.states.insert(state);
    }

    fn build(mut self) -> Pin<Box<Cfsm<V, T>>> {
        // `Cfsm` is a self-referential struct, thus the pin implementation is used. This means
        // that it needs to be constructed first with the grammar and then modified.

        let (state_ids, destination_ids) = self.states.iter().fold(
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

        let start_state = match self.start_state {
            Some(start_state) => {
                if !state_ids.contains(&start_state) {
                    panic!("start state is not a valid state");
                }
                start_state
            }
            None => panic!("start state not set"),
        };

        unsafe {
            // SAFETY: modifying the `states` and `start_state` fields do not move the struct
            // insert `states`
            self.cfsm
                .as_mut()
                .get_unchecked_mut()
                .states
                .extend(self.states.into_iter());
            // set `start_state`
            self.cfsm.as_mut().get_unchecked_mut().start_state = start_state;
        }

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
