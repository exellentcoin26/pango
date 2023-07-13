#![allow(unused)]

use crate::regex::{
    ast::{self, Ast, ExprKind},
    tokenizer::QuantifierKind,
};
use std::collections::BTreeMap;

type StateId = usize;
type Input = ast::LiteralKind;

struct Nfa {
    start_state: StateId,
    states: Vec<State>,
}

struct State {
    id: usize,
    fin: bool,
    transitions: BTreeMap<Input, Vec<StateId>>,
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
    fn get_final_states(&self) -> Vec<StateId> {
        self.states
            .iter()
            .filter_map(|s| if s.fin { Some(s.id) } else { None })
            .collect()
    }
}

impl State {
    fn new(
        id: StateId,
        transitions: BTreeMap<Input, Vec<StateId>>,
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
        Self::new(id, BTreeMap::new(), fin, None)
    }
}

impl From<Ast> for Nfa {
    fn from(value: Ast) -> Self {
        Compiler::new().build(&value.0)
    }
}

struct Compiler {
    /// Current NFA being compiled from the regex syntax tree.
    nfa: Nfa,
}

impl Compiler {
    fn new() -> Self {
        Self {
            nfa: Nfa::default(),
        }
    }

    /// Returns the next generated state should have. If the state is not created, the id will not
    /// be taken.
    fn new_state_id(&self) -> StateId {
        self.nfa.states.len()
    }

    fn build(mut self, expr: &ExprKind) -> Nfa {
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
        self.nfa
    }

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
            ExprKind::Lit(lit, quantifier) => todo!(),
            ExprKind::Group(expr, quantifier) => todo!(),
        }
    }
}
