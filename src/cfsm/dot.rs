use super::{item::ItemBody, state::State, Cfsm};
use crate::Symbol;

use std::fmt::{Debug, Display};

impl<V, T> Cfsm<V, T>
where
    V: Copy + Debug,
    T: Debug,
{
    /// Converts the CFSM to dot language using the [grahviz](https://graphviz.org/docs/layouts/dot/)
    /// dot language format.
    #[allow(unused)]
    pub fn to_dot(&self) -> String {
        format!(
            "digraph cfsm {{\n\
                \trankdir=LR;\n\
            \n\
                \tnode [shape=rectangle];\n\
                {}\n\
            \n\
                {}\n\
            }}",
            self.node_labels_dot()
                .map(|l| format!("\t{}\n", l))
                .collect::<String>(),
            self.transitions_dot()
                .map(|l| format!("\t{}\n", l))
                .collect::<String>(),
        )
    }

    fn node_labels_dot(&self) -> impl Iterator<Item = String> + '_ {
        self.states.iter().map(|State { id, item_set, .. }| {
            let label = item_set
                .iter()
                .map(|(head, item_body)| format!("{:?} ➞ {}", head, item_body))
                .collect::<Vec<_>>()
                .join(r"\n");

            format!("{} [label=\"{}\"];", id, label)
        })
    }

    fn transitions_dot(&self) -> impl Iterator<Item = String> + '_ {
        self.states.iter().flat_map(
            |State {
                 id, transitions, ..
             }| {
                transitions.iter().map(|(symbol, state)| {
                    let symbol = symbol.as_ref();

                    format!(
                        "{} -> {} [label = \"{}\"]",
                        *id,
                        state,
                        match symbol {
                            Symbol::Terminal(t) => format!("{:?}", t),
                            Symbol::Variable(v) => format!("{:?}", v),
                            eps @ Symbol::Epsilon => format!("{:?}", eps),
                        }
                    )
                })
            },
        )
    }
}

impl<V, T> Display for ItemBody<V, T>
where
    V: Debug,
    T: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // every symbol in the body plus the bullet.
        let mut result = Vec::with_capacity(self.get_body().len() + 1);

        for symbol in self.get_body().iter() {
            if result.len() == self.cursor {
                result.push(String::from("•"));
            }
            result.push(match symbol {
                Symbol::Terminal(t) => format!("{:?}", t),
                Symbol::Variable(v) => format!("{:?}", v),
                eps @ Symbol::Epsilon => format!("{:?}", eps),
            });
        }

        if self.cursor == result.len() {
            result.push(String::from("•"));
        }

        write!(f, "{}", result.join(" "))
    }
}
