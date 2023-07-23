use super::{
    super::traits::Simulatable,
    model::{Nfa, State, StateId},
};
use std::collections::HashMap;

// TODO: Optimize this implementation. For example, states connected with eps transitions do not
// need to be stored in seperate runs, but can be seen as one.

#[derive(Clone)]
struct NfaSimulator<'a> {
    /// Nfa we are simulating.
    nfa: &'a Nfa,
    /// A list of runs the simulator is currently in.
    runs: Vec<Run<'a>>,
}

#[derive(Clone)]
struct Run<'a> {
    state: &'a State,
    state_counters: HashMap<StateId, usize>,
}

impl<'a> Run<'a> {
    fn new(state: &'a State) -> Self {
        Self {
            state,
            state_counters: HashMap::new(),
        }
    }

    fn update(mut self, new_state: &'a State) -> Self {
        *self.state_counters.entry(self.state.get_id()).or_insert(0) += 1;

        Self {
            state: new_state,
            state_counters: self.state_counters,
        }
    }
}

impl<'a> NfaSimulator<'a> {
    fn new(nfa: &'a Nfa) -> Self {
        Self {
            nfa,
            runs: nfa
                .eps_closure(nfa.start_state)
                .map(Run::new)
                .collect::<Vec<_>>(),
        }
    }
}

impl Simulatable for NfaSimulator<'_> {
    fn is_accepting(&self) -> bool {
        self.runs.iter().any(|r| match r.state {
            State::Reg { fin, .. } => *fin,
            _ => false,
        })
    }

    fn feed(&mut self, input: char) -> bool {
        /// Enum for iterator types used to return different iterator types from the same match
        /// exression as long as they have the same `Item`. This can also be done by boxing the
        /// iterators, but would require indirection and runtime performance loss.
        ///
        /// Note: This is a really over-engineered solution to code repitition inside the match
        /// arms. However, not looking at the boiler-plate code (which could be turned into a
        /// macro) is, in my opinion, the cleanest.
        enum FeedIter<Item, R, Q>
        where
            R: Iterator<Item = Item>,
            Q: Iterator<Item = Item>,
        {
            Reg(R),
            Quant(Q),
        }

        impl<Item, R, Q> Iterator for FeedIter<Item, R, Q>
        where
            R: Iterator<Item = Item>,
            Q: Iterator<Item = Item>,
        {
            type Item = Item;

            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    FeedIter::Reg(reg) => reg.next(),
                    FeedIter::Quant(quant) => quant.next(),
                }
            }
        }

        let mut updated_runs = Vec::new();

        for run @ Run {
            state,
            state_counters,
        } in self.runs.iter()
        {
            // Check for every transition if the input can make that transition.

            let new_states = match state {
                State::Reg { transitions, .. } => FeedIter::Reg(
                    transitions
                        .iter()
                        .filter_map(|(expected, states)| {
                            if expected.can_take(input) {
                                Some(states.iter())
                            } else {
                                None
                            }
                        })
                        .flatten(),
                ),

                State::QuantGuard {
                    quantifier,
                    quantifier_done,
                    transitions,
                    ..
                } => {
                    let new_states = transitions.iter().chain(
                        match quantifier
                            .is_satisfied(*state_counters.get(&state.get_id()).unwrap_or(&0))
                        {
                            true => Some(quantifier_done).into_iter(),
                            false => None.into_iter(),
                        },
                    );

                    FeedIter::Quant(new_states)
                }
            };

            // For every run, this clones one time too many. This is not a problem as `Run` is
            // relatively cheap to clone, because it only contains references and usize's.
            updated_runs.extend(
                new_states
                    .flat_map(|s| self.nfa.eps_closure(*s))
                    .map(|s| run.clone().update(s)),
            )
        }

        self.runs = updated_runs;

        self.is_accepting()
    }
}
