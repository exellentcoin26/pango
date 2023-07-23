use super::model::{Input, Nfa, State, StateId};

impl Nfa {
    /// Converts the NFA to dot language using the grahviz dot language format.
    #[allow(unused)]
    pub(super) fn to_dot(&self) -> String {
        let final_dot = format!(
            "node [shape = doublecircle]; {}",
            self.get_final_states()
                .map(|State { id, .. }| id.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        );

        format!(
            "digraph nfa {{\n\
                \trankdir = LR;\n\
            \n\
                \t// final states\n\
                \t{}\n\
                \tnode [shape = circle]; 0;\n\
            \n\
                {}\n\
            }}",
            final_dot,
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
    fn transition_tuples(&self) -> impl Iterator<Item = (StateId, StateId, String)> + '_ {
        self.transitions
            .iter()
            .flat_map(move |(input, dest_states)| {
                dest_states
                    .iter()
                    .map(move |dest| (self.id, *dest, format!("{:?}", input)))
            })
    }
}
