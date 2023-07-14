#![allow(unused)]

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

    fn get_final_states(&self) -> Vec<StateId> {
        self.states
            .iter()
            .filter_map(|s| if s.fin { Some(s.id) } else { None })
            .collect()
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
        self.expr(
            expr,
            self.nfa.start_state,
            Some(
                *self
                    .nfa
                    .get_final_states()
                    .first()
                    .expect("exected at least one final state for the NFA to start with"),
            ),
        );
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
            ExprKind::Lit(lit, quantifier) => {
                // TODO: Decide on how to implement quantification of states. Right now I think it
                // might be possible to combine quantifiers and take min/max values of the range
                // values to decide the new quantifier.
                todo!()
            }
            ExprKind::Group(expr, quantifier) => todo!(),
        }
    }
}
