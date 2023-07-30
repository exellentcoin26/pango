use crate::regex::{ast, tokenizer::QuantifierKind};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};

// TODO: Clean up expects and panics to be `Option` and `Result` types.

pub(super) type StateId = usize;

/// Needs to be a `BTreeMap` for `std::hash::Hash` to be implemented.
pub(super) type StateCounters = BTreeMap<StateId, usize>;

pub(crate) struct Nfa {
    pub(super) start_state: StateId,
    pub(super) states: Vec<State>,
}

/// A state in the NFA.
///
/// Note: A state can only be constructed by the `NfaBuilder`. It is assumed that the state id is
/// unique within an `Nfa`, thus checking equality is implemented using the id and NOT the other
/// fields. This results in possibly incorrect behaviour when comparing with states from other
/// `Nfa`'s.
pub(super) struct State {
    /// Id of the state used by other states as a pointer.
    pub(super) id: StateId,
    /// Whether the state is final.
    pub(super) fin: bool,
    /// Transitions to other states based on regular and quantified input.
    pub(super) transitions: Transitions,
}

type Transitions = HashMap<Input, HashSet<StateId>>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) enum Input {
    /// Regular input to the NFA.
    Literal(ast::LiteralKind),
    /// Epsilon input, meaning no input needed (the transition can be made at any time).
    Eps,
    /// Epsilon input, but based on the amount of times the state has been visited.
    Quantified(QuantifierKind),
}

impl Default for Nfa {
    fn default() -> Self {
        NfaBuilder::new(false).with_state(true).build()
    }
}

impl Nfa {
    pub(super) fn builder(start_state_final: bool) -> NfaBuilder {
        NfaBuilder::new(start_state_final)
    }

    pub(super) fn get_state(&self, state_id: StateId) -> &State {
        self.states
            .iter()
            .find(|State { id, .. }| *id == state_id)
            .expect("requested state does not exist")
    }

    pub(super) fn get_final_states(&self) -> impl Iterator<Item = &State> + '_ {
        self.states.iter().filter(|s| s.fin)
    }

    /// Returns an iterator over references to all states in the epsilon closure of the given
    /// state.
    ///
    /// When `state_counters` is not provided, it is assumed that the state is visited 0 times.
    pub(super) fn eps_closure(&self, state_id: StateId) -> impl Iterator<Item = StateId> {
        let mut not_visited = VecDeque::from([state_id]);
        let mut result = BTreeSet::from([state_id]);

        while let Some(state_id) = not_visited.pop_front() {
            let State { transitions, .. } = self.get_state(state_id);

            let new_state_ids = transitions
                .iter()
                .filter_map(|(input, states)| -> Option<Box<dyn Iterator<Item = _>>> {
                    match input {
                        Input::Literal(_) => None,
                        Input::Eps => Some(Box::new(states.iter())),
                        Input::Quantified(quantifier) => {
                            if quantifier.is_satisfied(0) {
                                Some(Box::new(states.iter()))
                            } else {
                                None
                            }
                        }
                    }
                })
                .flatten();

            for state_id in new_state_ids {
                if result.insert(*state_id) {
                    not_visited.push_back(*state_id)
                }
            }
        }

        result.into_iter()
    }
}

impl Input {
    pub(super) fn can_take(&self, input: char) -> bool {
        match self {
            Input::Literal(lit) => lit.contains(input),
            Input::Eps => false,
            Input::Quantified(_) => false,
        }
    }
}

impl State {
    fn new(id: StateId, fin: bool, transitions: Transitions) -> Self {
        Self {
            id,
            fin,
            transitions,
        }
    }

    fn with_id(id: StateId, fin: bool) -> Self {
        Self::new(id, fin, Transitions::new())
    }
}

pub(super) struct NfaBuilder {
    pub(super) start_state: StateId,
    pub(super) states: Vec<State>,
}

impl NfaBuilder {
    pub(super) fn new(start_state_final: bool) -> Self {
        Self {
            start_state: 0,
            states: Vec::from([State::with_id(0, start_state_final)]),
        }
    }

    /// Returns the next generated state should have. If the state is not created, the id will not
    /// be taken.
    fn new_state_id(&self) -> StateId {
        self.states.len()
    }

    pub(super) fn build(self) -> Nfa {
        // Create a set of StateIds present in the NFA and a set of transition destination ids.

        let (state_ids, destination_ids) = self.states.iter().fold(
            (HashSet::new(), HashSet::new()),
            |(mut state_ids, mut destination_ids),
             State {
                 id, transitions, ..
             }| {
                state_ids.insert(id);
                destination_ids.extend(transitions.values().flatten());
                (state_ids, destination_ids)
            },
        );

        if !destination_ids.is_subset(&state_ids) {
            panic!("one or more destination states found that do not exist");
        }

        if !state_ids.contains(&self.start_state) {
            panic!("start state is not a valid state");
        }

        Nfa {
            start_state: self.start_state,
            states: self.states,
        }
    }

    pub(super) fn with_state(mut self, fin: bool) -> Self {
        self.add_state(fin);
        self
    }

    pub(super) fn with_transition(mut self, start: StateId, end: StateId, input: Input) -> Self {
        self.add_transition(start, end, input);
        self
    }

    pub(super) fn add_state(&mut self, fin: bool) -> StateId {
        let id = self.new_state_id();
        self.states.push(State::with_id(id, fin));
        id
    }

    pub(super) fn add_transition(&mut self, start: StateId, end: StateId, input: Input) {
        self.get_state_mut(start)
            .transitions
            .entry(input)
            .or_insert(HashSet::new())
            .insert(end);
    }

    pub(super) fn add_quantified_state(
        &mut self,
        fin: bool,
        quantifier: QuantifierKind,
        quantifier_done: StateId,
    ) -> StateId {
        let id = self.new_state_id();
        self.states.push(State::with_id(id, fin));
        self.get_state_mut(id)
            .transitions
            .entry(Input::Quantified(quantifier))
            .or_insert_with(HashSet::new)
            .insert(quantifier_done);

        id
    }

    pub(super) fn get_state(&self, state_id: StateId) -> &State {
        self.states
            .iter()
            .find(|State { id, .. }| *id == state_id)
            .expect("state does not exist")
    }

    pub(super) fn get_state_mut(&mut self, state_id: StateId) -> &mut State {
        self.states
            .iter_mut()
            .find(|State { id, .. }| *id == state_id)
            .expect("state does not exist")
    }

    pub(super) fn get_final_state_ids(&self) -> impl Iterator<Item = StateId> + '_ {
        self.states
            .iter()
            .filter_map(|State { fin, id, .. }| if *fin { Some(*id) } else { None })
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for State {}
