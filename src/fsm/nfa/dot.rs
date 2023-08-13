use super::{
    model::{Nfa, State},
    StateId,
};

impl std::fmt::Display for Nfa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_dot())
    }
}

impl Nfa {
    /// Converts the NFA to dot language using the [grahviz](https://graphviz.org/docs/layouts/dot/)
    /// dot language format.
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

    /// Converts the transitions to the dot format and returns an iterator over it.
    fn transition_dot(&self) -> impl Iterator<Item = String> + '_ {
        self.states.iter().flat_map(|state| {
            state.transition_tuples().map(|(start, dest, label)| {
                format!("{} -> {} [label = \"{}\"];", start, dest, label)
            })
        })
    }
}

impl State {
    /// Creates an flattened iterator over the transitions from a `State`.
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
