use crate::regex::{
    ast::{self},
    tokenizer::QuantifierKind,
};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

// TODO: Refactor quantifiers and destination states of the quantifier to be more readable in
// function definitions.

// TODO (IMPORTANT): Quantifier guard states should have an end guard as well. This will always go
// back to the begin guard which can then decide to continue or to redo the quantified part. Right
// now, quantified expressions can reach the final state no matter what the quantifier is for the
// expression.

// TODO: Clean up expects and panics to be `Option` and `Result` types.

pub(super) type StateId = usize;

pub(super) struct Nfa {
    pub(super) start_state: StateId,
    pub(super) states: Vec<State>,
}

#[derive(PartialEq, Eq)]
pub(super) enum State {
    /// A regular automata state.
    Reg {
        /// Id of the state used by other states as a pointer.
        id: StateId,
        /// Whether the state is final.
        fin: bool,
        /// Transitions to other states that can be made based on an input.
        transitions: HashMap<Input, HashSet<StateId>>,
    },
    /// A expression quantifier guard state used to prevent the exponential blowup of quantfied
    /// regular expressions, like, `{23, 60}`.
    ///
    /// Note: A guard state can never be final.
    QuantGuard {
        /// Id of the state used by other states as a pointer.
        id: StateId,
        /// Quantifier the guard represents, when it is fullfilled, the a transition to the
        /// transition state _can_ be made.
        quantifier: QuantifierKind,
        /// The state to which the automata _can_ transition when the quantifier is fullfilled.
        quantifier_done: StateId,
        /// Transitions to other states on epsilon.
        transitions: HashSet<StateId>,
    },
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) enum Input {
    Literal(ast::LiteralKind),
    Eps,
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

    pub(super) fn get_state(&self, id: StateId) -> &State {
        self.states
            .iter()
            .find(|s| s.has_id(id))
            .expect("requested state does not exist")
    }

    pub(super) fn get_final_states(&self) -> impl Iterator<Item = &State> + '_ {
        self.states.iter().filter(|s| match s {
            State::Reg { fin, .. } => *fin,
            State::QuantGuard { .. } => false,
        })
    }

    /// Returns an iterator over references to all states in the epsilon closure of the given
    /// state.
    ///
    /// Note: This does not use guard states in the closure, because this requires simulation time
    /// information which is not encoded in the model of the NFA.
    pub(super) fn eps_closure(&self, state_id: StateId) -> impl Iterator<Item = &State> {
        /// Enum for iterator types used to return different iterator types from the same match
        /// exression as long as they have the same `Item`. This can also be done by boxing the
        /// iterators, but would require indirection and runtime performance loss.
        ///
        /// Note: This is a really over-engineered solution to code repitition inside the match
        /// arms. However, not looking at the boiler-plate code (which could be turned into a
        /// macro) is, in my opinion, the cleanest.
        enum EpsClosureIter<Item, R, Q>
        where
            R: Iterator<Item = Item>,
            Q: Iterator<Item = Item>,
        {
            Reg(R),
            Quant(Q),
        }

        impl<Item, R, Q> Iterator for EpsClosureIter<Item, R, Q>
        where
            R: Iterator<Item = Item>,
            Q: Iterator<Item = Item>,
        {
            type Item = Item;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    EpsClosureIter::Reg(reg) => reg.next(),
                    EpsClosureIter::Quant(quant) => quant.next(),
                }
            }
        }

        let state = self.get_state(state_id);

        let mut not_visited = VecDeque::from([state]);
        let mut result = BTreeSet::from([state]);

        while let Some(state) = not_visited.pop_front() {
            let new_states = match state {
                State::Reg { transitions, .. } => EpsClosureIter::Reg(
                    transitions
                        .iter()
                        .filter_map(|(input, states)| {
                            if *input == Input::Eps {
                                Some(states.iter().map(|state_id| self.get_state(*state_id)))
                            } else {
                                None
                            }
                        })
                        .flatten(),
                ),
                State::QuantGuard { transitions, .. } => {
                    EpsClosureIter::Quant(transitions.iter().map(|id| self.get_state(*id)))
                }
            };

            for state in new_states {
                if result.insert(state) {
                    not_visited.push_back(state)
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
        }
    }
}

impl State {
    fn new_quantifier_guard(
        id: StateId,
        quantifier: QuantifierKind,
        quantifier_done: StateId,
        transitions: HashSet<StateId>,
    ) -> Self {
        Self::QuantGuard {
            id,
            quantifier,
            quantifier_done,
            transitions,
        }
    }

    fn new_regular(id: StateId, transitions: HashMap<Input, HashSet<StateId>>, fin: bool) -> Self {
        Self::Reg {
            id,
            fin,
            transitions,
        }
    }

    fn regular_with_id(id: StateId, fin: bool) -> Self {
        Self::new_regular(id, HashMap::new(), fin)
    }

    fn quantifier_with_id(id: StateId, quantifier: QuantifierKind, transition: StateId) -> Self {
        Self::new_quantifier_guard(id, quantifier, transition, HashSet::new())
    }

    pub(super) fn get_id(&self) -> StateId {
        match self {
            State::Reg { id, .. } => *id,
            State::QuantGuard { id, .. } => *id,
        }
    }

    pub(super) fn has_id(&self, state_id: StateId) -> bool {
        match self {
            State::Reg { id, .. } => *id == state_id,
            State::QuantGuard { id, .. } => *id == state_id,
        }
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
            states: Vec::from([State::regular_with_id(0, start_state_final)]),
        }
    }

    pub(super) fn build(self) -> Nfa {
        // Create a set of StateIds present in the NFA and a set of transition destination ids.

        let (state_ids, destination_ids) = self.states.iter().fold(
            (HashSet::new(), HashSet::new()),
            |(mut state_ids, mut destination_ids), state| {
                state_ids.insert(state.get_id());
                match state {
                    State::Reg { transitions, .. } => {
                        destination_ids.extend(transitions.values().flatten())
                    }
                    State::QuantGuard {
                        quantifier_done,
                        transitions,
                        ..
                    } => {
                        destination_ids.insert(*quantifier_done);
                        destination_ids.extend(transitions)
                    }
                }

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

    pub(super) fn get_final_states(&self) -> impl Iterator<Item = StateId> + '_ {
        self.states.iter().filter_map(|s| match s {
            State::Reg { id, fin: true, .. } => Some(*id),
            _ => None,
        })
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
        self.states.push(State::regular_with_id(id, fin));
        id
    }

    pub(super) fn add_quantified_state(
        &mut self,
        quantifier: QuantifierKind,
        quantifier_done: StateId,
    ) -> StateId {
        let id = self.new_state_id();
        self.states
            .push(State::quantifier_with_id(id, quantifier, quantifier_done));
        id
    }

    pub(super) fn get_state_mut(&mut self, state_id: StateId) -> &mut State {
        self.states
            .iter_mut()
            .find(|s| s.has_id(state_id))
            .expect("state does not exist")
    }

    pub(super) fn add_transition(&mut self, start: StateId, end: StateId, input: Input) {
        match self.get_state_mut(start) {
            State::Reg {
                ref mut transitions,
                ..
            } => {
                transitions
                    .entry(input)
                    .or_insert(HashSet::new())
                    .insert(end);
            }
            State::QuantGuard {
                ref mut transitions,
                ..
            } => {
                transitions.insert(end);
            }
        }
    }

    /// Returns the next generated state should have. If the state is not created, the id will not
    /// be taken.
    fn new_state_id(&self) -> StateId {
        self.states.len()
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.get_id().partial_cmp(&other.get_id())
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.get_id().cmp(&other.get_id())
    }
}

#[cfg(test)]
mod foo {
    use super::Nfa;
    use crate::regex::parser::Parser;

    #[test]
    fn to_dot() {
        println!(
            "{}",
            Nfa::from(Parser::new("(ab)c🔥🌘").parse().unwrap()).to_dot()
        );
        println!("{}", Nfa::from(Parser::new("").parse().unwrap()).to_dot());
        println!(
            "{}",
            Nfa::from(Parser::new("((a|b)c🔥🌘|foo)").parse().unwrap()).to_dot()
        );
        println!(
            "{}",
            Nfa::from(Parser::new("((ab)*c{3})?").parse().unwrap()).to_dot()
        );
    }
}
