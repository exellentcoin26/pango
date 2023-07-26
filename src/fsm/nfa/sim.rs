use super::{
    super::traits::Simulatable,
    model::{Input, Nfa, State, StateCounters, StateId},
};
use std::collections::{HashSet, VecDeque};

// TODO: Refactor runs to use hash as an id for keeping track instead of cloning. Note that this
// can result in collisions.

#[derive(Clone)]
pub(super) struct NfaSimulator<'a> {
    /// Nfa we are simulating.
    nfa: &'a Nfa,
    /// A list of runs the simulator is currently in.
    runs: Vec<Run>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Run {
    state_id: StateId,
    state_counters: StateCounters,
}

impl Run {
    fn new(state_id: StateId) -> Self {
        Self {
            state_id,
            state_counters: StateCounters::new(),
        }
    }

    fn update(mut self, new_state_id: StateId) -> Self {
        *self.state_counters.entry(self.state_id).or_insert(0) += 1;
        self.state_id = new_state_id;

        self
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

    fn eps_closure(&self, run: Run) -> impl Iterator<Item = Run> {
        let mut not_visited = VecDeque::from([run.clone()]);
        let mut result = HashSet::from([run]);

        while let Some(run) = not_visited.pop_front() {
            let Run {
                state_id,
                state_counters,
            } = &run;

            let State { transitions, .. } = self.nfa.get_state(*state_id);

            let new_state_ids = transitions
                .iter()
                .filter_map(|(input, states)| -> Option<Box<dyn Iterator<Item = _>>> {
                    match input {
                        Input::Literal(_) => None,
                        Input::Eps => Some(Box::new(states.iter())),
                        Input::Quantified(quantifier) => {
                            if quantifier.is_satisfied(*state_counters.get(state_id).unwrap_or(&0))
                            {
                                Some(Box::new(states.iter()))
                            } else {
                                None
                            }
                        }
                    }
                })
                .flatten();

            let new_runs = new_state_ids
                .into_iter()
                .map(|new_state| run.clone().update(*new_state));

            for new_run in new_runs {
                if result.insert(new_run.clone()) {
                    not_visited.push_back(new_run);
                }
            }
        }

        result.into_iter()
    }
}

impl Simulatable for NfaSimulator<'_> {
    fn is_accepting(&self) -> bool {
        self.runs
            .iter()
            .any(|Run { state_id, .. }| self.nfa.get_state(*state_id).fin)
    }

    fn feed(&mut self, input: char) -> bool {
        let mut updated_runs = Vec::new();

        for run @ Run { state_id, .. } in self.runs.iter() {
            let State { transitions, .. } = self.nfa.get_state(*state_id);

            // Check for every transition if the input can make that transition.

            let new_state_ids = transitions
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

            let new_runs = new_state_ids
                .into_iter()
                .map(|new_state_id| run.clone().update(*new_state_id))
                .flat_map(|new_run| self.eps_closure(new_run));

            updated_runs.extend(new_runs);
        }

        self.runs = updated_runs;

        dbg!(input, &self.runs);

        dbg!(self.is_accepting())
    }
}

impl Nfa {
    pub(super) fn simulate(&self, input: &str) -> bool {
        NfaSimulator::new(self).feed_str(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::{fsm::nfa::model::Nfa, regex::parser::Parser};

    #[test]
    fn accepts() {
        let nfa = Nfa::from(Parser::new("(ab){3}c{2}").parse().unwrap());

        // eprintln!("{}", nfa);

        assert!(!nfa.simulate("aa"));
        assert!(nfa.simulate("abababcc"));
        assert!(!nfa.simulate("abababccc"));
    }

    #[test]
    fn or() {
        let nfa = Nfa::from(Parser::new("(a|b){3,4}").parse().unwrap());

        // eprintln!("{}", nfa);

        assert!(nfa.simulate("aaaa"));
        assert!(nfa.simulate("bbbb"));
        assert!(nfa.simulate("abab"));
        assert!(nfa.simulate("bbaa"));
        assert!(nfa.simulate("baba"));
        assert!(nfa.simulate("aabb"));
        assert!(nfa.simulate("aaab"));
        assert!(nfa.simulate("bbba"));
    }
}
