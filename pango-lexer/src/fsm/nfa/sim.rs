use super::{
    super::{
        traits::{NDSimulate, Simulatable, Simulate},
        StateId,
    },
    model::{Input, Nfa, State, StateCounters},
};

use std::{
    borrow::Cow,
    collections::{BTreeSet, HashSet, VecDeque},
};

// TODO: Refactor runs to use hash as an id for keeping track instead of
// cloning. Note that this can result in collisions.

/// Simulator for the [`Nfa`].
#[derive(Clone)]
pub struct NfaSimulator<'a> {
    /// [`Nfa`] we are simulating.
    nfa: Cow<'a, Nfa>,
    /// A list of runs the simulator is currently in.
    runs: Vec<Run>,
}

/// A single decision branch in the simulation of the [`Nfa`].
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct Run {
    state_id: StateId,
    state_counters: StateCounters,
}

impl Run {
    /// Creates a new [`Run`] which starts in the `state_id` state.
    fn new(state_id: StateId) -> Self {
        Self {
            state_id,
            state_counters: StateCounters::new(),
        }
    }

    /// Updates the [`Run`] to the new state and updates the needed state
    /// counters.
    fn take_transition(mut self, new_state_id: StateId, quantified: bool) -> Self {
        let count = self.state_counters.entry(self.state_id).or_insert(0);
        if quantified {
            // Reset the state count if quantified transition is used
            *count = 0;
        } else {
            *count += 1;
        }
        self.state_id = new_state_id;

        self
    }
}

impl<'a> NfaSimulator<'a> {
    /// Creates a new [`NfaSimulator`].
    fn new(nfa: Cow<'a, Nfa>) -> Self {
        let runs = nfa
            .eps_closure(nfa.start_state)
            .map(Run::new)
            .collect::<Vec<_>>();

        Self { nfa, runs }
    }

    /// Returns an iterator over the runtime epsilon closure. This is an epsilon
    /// closure of [`Run`]s, NOT of [`State`]s. Because runs contain state
    /// counters, this will also take quantified transitions.
    fn eps_closure(&self, run: Run) -> impl Iterator<Item = Run> {
        let mut not_visited = VecDeque::from([run.clone()]);
        let mut result = HashSet::from([run]);

        while let Some(run) = not_visited.pop_front() {
            let Run {
                state_id,
                state_counters,
            } = &run;

            let State { transitions, .. } = self.nfa.get_state(*state_id);

            let transitions_taken = transitions
                .iter()
                .filter_map(|(input, states)| -> Option<Box<dyn Iterator<Item = _>>> {
                    match input {
                        Input::Literal(_) => None,
                        Input::Eps => Some(Box::new(states.iter().map(|state| (state, false)))),
                        Input::Quantified(quantifier) => {
                            if quantifier.is_satisfied(*state_counters.get(state_id).unwrap_or(&0))
                            {
                                Some(Box::new(states.iter().map(|state| (state, true))))
                            } else {
                                None
                            }
                        }
                    }
                })
                .flatten();

            let new_runs = transitions_taken
                .into_iter()
                .map(|(new_state, quantified)| run.clone().take_transition(*new_state, quantified));

            for new_run in new_runs {
                if result.insert(new_run.clone()) {
                    not_visited.push_back(new_run);
                }
            }
        }

        result.into_iter()
    }
}

impl Simulatable for Nfa {
    type Simulator<'a> = NfaSimulator<'a>;

    fn simulate(&self, input: &str) -> bool {
        NfaSimulator::new(Cow::Borrowed(self)).feed_str(input)
    }

    fn to_simulator(&self) -> Self::Simulator<'_> {
        NfaSimulator::new(Cow::Borrowed(self))
    }
}

impl Simulate for NfaSimulator<'_> {
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

            // First, update the run with the new information, then calculate the epsilon
            // closure with the updated state counters.
            //
            // Note: For every run, this clones one time too many. This is not a problem as
            // `Run` is relatively cheap to clone, because it only contains
            // references and usize's.

            let new_runs = new_state_ids
                .into_iter()
                .map(|new_state_id| run.clone().take_transition(*new_state_id, false))
                .flat_map(|new_run| self.eps_closure(new_run));

            updated_runs.extend(new_runs);
        }

        self.runs = updated_runs;

        #[cfg(debug_assertions)]
        {
            dbg!(input, &self.runs);
            dbg!(self.is_accepting());
        }

        self.is_accepting()
    }

    fn can_feed(&self, input: char) -> bool {
        for Run {
            state_id,
            state_counters: _,
        } in self.runs.iter()
        {
            let State { transitions, .. } = self.nfa.get_state(*state_id);

            // Check for every transition if the input can make that transition.

            if transitions.keys().any(|expected| expected.can_take(input)) {
                return true;
            }
        }

        false
    }
}

impl NDSimulate for NfaSimulator<'_> {
    fn get_current_final_states(&self) -> BTreeSet<StateId> {
        let final_state_ids = self
            .nfa
            .get_final_states()
            .map(|s| s.id)
            .collect::<HashSet<StateId>>();

        self.runs
            .iter()
            .filter_map(|Run { state_id, .. }| {
                if final_state_ids.contains(state_id) {
                    Some(state_id)
                } else {
                    None
                }
            })
            .copied()
            .collect::<BTreeSet<_>>()
    }
}

#[cfg(test)]
mod tests {
    use super::Simulatable;
    use crate::{fsm::nfa::model::Nfa, regex::parser::Parser};

    #[test]
    fn accepts() {
        let nfa = Nfa::from(Parser::new("(ab){3}c{2}").parse().unwrap());

        eprintln!("{}", nfa);

        assert!(!nfa.simulate("aa"));
        assert!(nfa.simulate("abababcc"));
        assert!(!nfa.simulate("abababccc"));
    }

    #[test]
    fn concat() {
        let nfa = Nfa::from(Parser::new("abc🌕").parse().unwrap());

        eprintln!("{}", nfa);

        assert!(nfa.simulate("abc🌕"));
        assert!(!nfa.simulate("abd🌕"));
        assert!(!nfa.simulate("abc🌕🌕"));
        assert!(!nfa.simulate("aabc🌕"));
    }

    #[test]
    fn empty() {
        let nfa = Nfa::from(Parser::new("").parse().unwrap());

        eprintln!("{}", nfa);

        assert!(nfa.simulate(""));
        assert!(!nfa.simulate("a"));
    }

    #[test]
    fn alt() {
        let nfa = Nfa::from(Parser::new("(a|b){3,4}").parse().unwrap());

        eprintln!("{}", nfa);

        assert!(nfa.simulate("aaaa"));
        assert!(nfa.simulate("bbbb"));
        assert!(nfa.simulate("abab"));
        assert!(nfa.simulate("bbaa"));
        assert!(nfa.simulate("baba"));
        assert!(nfa.simulate("aabb"));
        assert!(nfa.simulate("aaab"));
        assert!(nfa.simulate("bbba"));
    }

    #[test]
    fn lit() {
        let nfa = Nfa::from(Parser::new("a").parse().unwrap());

        eprintln!("{}", nfa);

        assert!(nfa.simulate("a"));
        assert!(!nfa.simulate("b"));
    }

    #[test]
    fn group() {
        let nfa = Nfa::from(Parser::new("(abc)(ac)🌕").parse().unwrap());

        eprintln!("{}", nfa);

        assert!(nfa.simulate("abcac🌕"));
        assert!(!nfa.simulate("bcac🌕"));
        assert!(!nfa.simulate("abcac🌕🌕"));
    }

    #[test]
    fn range_quantifier() {
        let nfa = Nfa::from(Parser::new(r"a{3}(bc){2,}\}{2,4}").parse().unwrap());

        eprintln!("{}", nfa);

        assert!(nfa.simulate("aaabcbcbc}}}"));
        assert!(nfa.simulate("aaabcbcbcbcbcbcbcbcbcbcbc}}}}"));
        assert!(nfa.simulate("aaabcbc}}"));
        assert!(!nfa.simulate("aaabcbc}"));
        assert!(!nfa.simulate("aaaabcbc}}"));
        assert!(!nfa.simulate("aaabc}}"));
        assert!(!nfa.simulate("aaaabcbcbcbc}}}}}"));
    }

    #[test]
    fn symbol_quantifiers() {
        let nfa = Nfa::from(Parser::new(r"a*(bc?)+c?").parse().unwrap());

        eprintln!("{}", nfa);

        assert!(nfa.simulate("b"));
        assert!(nfa.simulate("aaaaaaaabcbbbbbbcc"));
        assert!(nfa.simulate("bbcc"));
        assert!(!nfa.simulate("aaaaabcbccc"));
    }
}
