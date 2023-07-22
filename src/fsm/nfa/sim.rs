use super::{
    super::traits::Simulatable,
    model::{Nfa, State, StateId},
};
use std::collections::HashMap;

// TODO: Optimize this implementation. For example, states connected with eps transitions do not
// need to be stored in seperate runs, but can be seen as one.

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
        let mut updated_runs = Vec::new();

        for run in self.runs.iter() {
            // Check for every transition if the input can make that transition.

            let new_states = match run.state {
                State::Reg { transitions, .. } => transitions
                    .iter()
                    .filter_map(|(expected, states)| {
                        if expected.can_take(input) {
                            Some(states.iter().map(|state_id| self.nfa.get_state(*state_id)))
                        } else {
                            None
                        }
                    })
                    .flatten(),

                _ => panic!("runs can only be in a regular state, not in quantifier guards"),
            };

            // For every run, this clones one time too many. This is not a problem as `Run` is
            // relatively cheap to clone, because it only contains references and usize's.
            updated_runs.extend(
                new_states
                    .flat_map(|s| self.nfa.eps_closure(s.get_id()))
                    .map(|s| run.clone().update(s)),
            )
        }

        self.runs = updated_runs;

        self.is_accepting()
    }
}
