use super::model::{Input, Nfa, State, StateId};

impl Nfa {
    /// Converts the NFA to dot language using the grahviz dot language format.
    #[allow(unused)]
    pub(super) fn to_dot(&self) -> String {
        let final_dot = format!(
            "node [shape = doublecircle]; {}",
            self.get_final_states()
                .map(|s| s.get_id().to_string())
                .collect::<Vec<String>>()
                .join(" ")
        );

        let guard_dot = self
            .states
            .iter()
            .filter_map(|s| match s {
                State::QuantGuard { id, quantifier, .. } => {
                    Some(format!("{} [label = \"{} ({:?})\"];\n", id, id, quantifier))
                }
                _ => None,
            })
            .collect::<String>();

        format!(
            "digraph nfa {{\n\
                \trankdir = LR;\n\
            \n\
                \t{}\n\
                {}\n\
                \tnode [shape = circle]; 0;\n\
            \n\
                {}\n\
            }}",
            final_dot,
            guard_dot
                .lines()
                .map(|l| format!("\t{}", l))
                .collect::<Vec<String>>()
                .join("\n"),
            self.transition_dot()
                .map(|l| format!("\t{}", l))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }

    fn transition_dot(&self) -> impl Iterator<Item = String> + '_ {
        self.states.iter().flat_map(|state| {
            state.transition_tuples().map(|(start, dest, label)| {
                format!("{} -> {} [label = \"{}\"];", start, dest, label)
            })
        })
    }
}

impl State {
    fn transition_tuples(&self) -> Box<dyn Iterator<Item = (StateId, StateId, String)> + '_> {
        match self {
            State::Reg {
                id, transitions, ..
            } => Box::new(transitions.iter().flat_map(move |(input, dest_states)| {
                dest_states
                    .iter()
                    .map(move |dest| (*id, *dest, format!("{:?}", input)))
            })),
            State::QuantGuard {
                id,
                transitions,
                quantifier_done,
                ..
            } => Box::new(
                std::iter::once((*id, *quantifier_done, "quant".to_string())).chain(
                    transitions
                        .iter()
                        .map(|dest| (*id, *dest, format!("{:?}", Input::Eps))),
                ),
            ),
        }
    }
}
