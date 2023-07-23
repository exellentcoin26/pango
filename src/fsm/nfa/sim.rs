use super::{
    super::traits::Simulatable,
    model::{Nfa, State, StateCounters},
};
use std::collections::HashMap;

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
    state_counters: StateCounters,
}

impl<'a> Run<'a> {
    fn new(state: &'a State) -> Self {
        Self {
            state,
            state_counters: HashMap::new(),
        }
    }

    fn update(mut self, new_state: &'a State) -> Self {
        *self.state_counters.entry(self.state.id).or_insert(0) += 1;

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
                .eps_closure(nfa.start_state, None)
                .map(Run::new)
                .collect::<Vec<_>>(),
        }
    }
}

impl Simulatable for NfaSimulator<'_> {
    fn is_accepting(&self) -> bool {
        self.runs.iter().any(|r| r.state.fin)
    }

    fn feed(&mut self, input: char) -> bool {
        let mut updated_runs = Vec::new();

        for run @ Run {
            state: State { transitions, .. },
            state_counters,
        } in self.runs.iter()
        {
            // Check for every transition if the input can make that transition.

            let new_states = transitions
                .iter()
                .filter_map(|(expected, states)| {
                    if expected.can_take(input) {
                        Some(states.iter())
                    } else {
                        None
                    }
                })
                .flatten();

            // First, update the run with the new information, then calculate the epsilon closure
            // with the updated state counters.
            //
            // Note: For every run, this clones one time too many. This is not a problem as `Run` is
            // relatively cheap to clone, because it only contains references and usize's.

            for new_state_id in new_states {
                let new_run = run.clone().update(self.nfa.get_state(*new_state_id));

                let eps_closure = self
                    .nfa
                    .eps_closure(new_run.state.id, Some(&new_run.state_counters))
                    // Do include the original state
                    .filter(|State { id, .. }| *id != new_run.state.id);

                updated_runs.extend(eps_closure.map(|s| new_run.clone().update(s)));
            }
        }

        self.runs = updated_runs;

        self.is_accepting()
    }
}
