use crate::regex::{
    ast::{self, Ast, ExprKind},
    tokenizer::QuantifierKind,
};
use std::collections::{BTreeSet, HashMap, HashSet};

type StateId = usize;
type Input = ast::LiteralKind;

struct Nfa {
    start_state: StateId,
    states: Vec<State>,
}

struct State {
    id: usize,
    fin: bool,
    transitions: HashMap<Input, BTreeSet<StateId>>,
    quantifier: Option<QuantifierKind>,
}

impl Default for Nfa {
    fn default() -> Self {
        Self {
            start_state: 0,
            states: Vec::from([State::with_id(0, false), State::with_id(1, true)]),
        }
    }
}

impl Nfa {
    fn builder(start_state_final: bool) -> NfaBuilder {
        NfaBuilder::new(start_state_final)
    }

    fn get_final_states(&self) -> impl Iterator<Item = StateId> + '_ {
        self.states
            .iter()
            .filter_map(|s| if s.fin { Some(s.id) } else { None })
    }

    /// Converts the NFA to dot language using the grahviz dot language format.
    fn to_dot(&self) -> String {
        let transition_dot = self
            .states
            .iter()
            .flat_map(|state| {
                state
                    .transitions
                    .iter()
                    .flat_map(move |(input, dest_states)| {
                        dest_states.iter().map(move |dest| {
                            format!("{} -> {} [label = \"{:?}\"]\n", state.id, dest, input)
                        })
                    })
            })
            .collect::<String>();

        let final_dot = format!(
            "node [shape = doublecircle]; {}",
            self.get_final_states()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        );

        format!(
            "digraph nfa {{\n\
                \trankdir = LR;\n\
            \n\
                \t{}\n\
                \tnode [shape = circle]
            \n\
                \t{}\n\
            }}",
            final_dot, transition_dot
        )
    }
}

impl State {
    fn new(
        id: StateId,
        transitions: HashMap<Input, BTreeSet<StateId>>,
        fin: bool,
        quantifier: Option<QuantifierKind>,
    ) -> Self {
        Self {
            id,
            transitions,
            fin,
            quantifier,
        }
    }

    fn with_id(id: StateId, fin: bool) -> Self {
        Self::new(id, HashMap::new(), fin, None)
    }
}

impl From<Ast> for Nfa {
    fn from(value: Ast) -> Self {
        Compiler::new().compile(&value.0)
    }
}

struct NfaBuilder {
    start_state: StateId,
    states: Vec<State>,
}

impl NfaBuilder {
    fn new(start_state_final: bool) -> Self {
        Self {
            start_state: 0,
            states: Vec::from([State::with_id(0, start_state_final)]),
        }
    }

    fn build(self) -> Nfa {
        let (state_ids, destination_ids) = self.states.iter().fold(
            (HashSet::new(), HashSet::new()),
            |(mut state_ids, mut destination_ids), state| {
                state_ids.insert(state.id);
                destination_ids.extend(state.transitions.values().flatten());

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

    fn get_final_states(&self) -> impl Iterator<Item = StateId> + '_ {
        self.states
            .iter()
            .filter_map(|s| if s.fin { Some(s.id) } else { None })
    }

    fn with_state(mut self, fin: bool) -> Self {
        self.add_state(fin);
        self
    }

    fn with_transition(mut self, start: StateId, end: StateId, input: Input) -> Self {
        self.add_transition(start, end, input);
        self
    }

    fn add_state(&mut self, fin: bool) -> StateId {
        let id = self.new_state_id();
        self.states.push(State::with_id(id, fin));
        id
    }

    fn add_transition(&mut self, start: StateId, end: StateId, input: Input) {
        self.states
            .get_mut(start)
            .expect("state index does not exist")
            .transitions
            .entry(input)
            .or_insert(BTreeSet::new())
            .insert(end);
    }

    /// Returns the next generated state should have. If the state is not created, the id will not
    /// be taken.
    fn new_state_id(&self) -> StateId {
        self.states.len()
    }
}

struct Compiler {
    /// Current NFA being compiled from the regex syntax tree.
    nfa: NfaBuilder,
}

impl Compiler {
    fn new() -> Self {
        Self {
            nfa: Nfa::builder(false).with_state(true),
        }
    }

    fn compile(mut self, expr: &ExprKind) -> Nfa {
        let end_state = self
            .nfa
            .get_final_states()
            .next()
            .expect("exected at least one final state for the NFA to start with");

        self.expr(expr, self.nfa.start_state, Some(end_state));
        self.nfa.build()
    }

    #[allow(clippy::only_used_in_recursion)]
    fn expr(&mut self, expr: &ExprKind, start: StateId, end: Option<StateId>) {
        match expr {
            ExprKind::Concat(exprs) => {
                // Run once for the first expression so that it is connected to the expected start
                // state. Run the intermediate expressions to connect them in a chain. Run once for
                // the last expression so it is connected to the expected end state.
                let current_state = start;

                for expr in exprs.iter().take(exprs.len() - 1) {
                    self.expr(expr, current_state, None)
                }

                self.expr(
                    exprs.last().expect(
                        "expected at least one expressions in a concatenation of expressions",
                    ),
                    current_state,
                    end,
                )
            }
            ExprKind::Empty => (),
            ExprKind::Alt(lhs, rhs) => {
                // Connect both the expect start and end state
                self.expr(lhs, start, end);
                self.expr(rhs, start, end);
            }
            ExprKind::Lit(lit, _quantifier) => {
                // TODO: Decide on how to implement quantification of states. Right now I think it
                // might be possible to combine quantifiers and take min/max values of the range
                // values to decide the new quantifier.

                let dest_state = match end {
                    Some(dest) => dest,
                    None => self.nfa.add_state(false),
                };

                self.nfa.add_transition(start, dest_state, lit.clone());
            }
            ExprKind::Group(expr, _quantifier) => {
                // TODO: Decide on how to implement quantification of expressions. A quantification
                // can be expressed as a wrapped expression with a gateway state that counts how
                // many times it is passed and can both go to the end state for the quantification
                // wrapper and redo the expression when the quantification is still or not yet
                // valid.

                self.expr(expr, start, end);
            }
        }
    }
}

#[cfg(test)]
mod foo {
    use super::Nfa;
    use crate::regex::ast::LiteralKind;

    #[test]
    fn to_dot() {
        let nfa = Nfa::builder(false)
            .with_state(true)
            .with_transition(0, 1, LiteralKind::Match('a'))
            .build();

        println!("{}", nfa.to_dot());
    }
}
