use super::{
    model::{Input, Nfa, NfaBuilder},
    StateId,
};
use crate::regex::{
    ast::{Ast, ExprKind},
    tokenizer::QuantifierKind,
};

/// Regex AST to NFA compiler.
pub(crate) struct Compiler {
    /// Current NFA being compiled from the regex syntax tree.
    nfa: NfaBuilder,
    /// `StateId` of the final state of the NFA used by the compilation.
    final_state: StateId,
}

impl From<Ast> for Nfa {
    fn from(value: Ast) -> Self {
        Compiler::new().with_expression(value).compile()
    }
}

impl From<Vec<Ast>> for Nfa {
    fn from(value: Vec<Ast>) -> Self {
        value
            .into_iter()
            .fold(Compiler::new(), |compiler, expr| {
                compiler.with_expression(expr)
            })
            .compile()
    }
}

impl Compiler {
    pub(crate) fn new() -> Self {
        let nfa = Nfa::builder(false).with_state(true);
        let final_state = nfa
            .get_final_state_ids()
            .next()
            .expect("exected at least one final state for the NFA to start with");

        Self { nfa, final_state }
    }

    pub(crate) fn new_final(&mut self) -> StateId {
        self.final_state = self.nfa.add_state(true);
        self.final_state
    }

    pub(crate) fn with_expression(mut self, expr: Ast) -> Self {
        self.expr(&expr.0, self.nfa.start_state, self.final_state);
        self
    }

    pub(crate) fn add_expression(&mut self, expr: Ast) {
        self.expr(&expr.0, self.nfa.start_state, self.final_state);
    }

    pub(crate) fn compile(self) -> Nfa {
        self.nfa.build()
    }

    fn insert_quantifier_state(
        &mut self,
        quantifier: QuantifierKind,
        start_state: StateId,
        end_state: StateId,
    ) -> StateId {
        let quantifier_state = self.nfa.add_quantified_state(false, quantifier, end_state);

        self.nfa
            .add_transition(start_state, quantifier_state, Input::Eps);

        quantifier_state
    }

    #[allow(clippy::only_used_in_recursion)]
    fn expr(&mut self, expr: &ExprKind, start: StateId, end: StateId) {
        match expr {
            ExprKind::Concat(exprs) => {
                // Run once for the first expression so that it is connected to the expected start
                // state. Run the intermediate expressions to connect them in a chain. Run once for
                // the last expression so it is connected to the expected end state.

                let mut current_state = start;

                for expr in exprs.iter().take(exprs.len() - 1) {
                    let new_state = self.nfa.add_state(false);
                    self.expr(expr, current_state, new_state);
                    current_state = new_state;
                }

                self.expr(
                    exprs.last().expect(
                        "expected at least one expressions in a concatenation of expressions",
                    ),
                    current_state,
                    end,
                )
            }
            ExprKind::Empty => self.nfa.add_transition(start, end, Input::Eps),
            ExprKind::Alt(lhs, rhs) => {
                // Connect both the expected start and end state
                self.expr(lhs, start, end);
                self.expr(rhs, start, end);
            }
            ExprKind::Lit(lit, quantifier) => {
                let (start, end) = match quantifier {
                    Some(quantifier) => {
                        let quantifier_state =
                            self.insert_quantifier_state(*quantifier, start, end);
                        (quantifier_state, quantifier_state)
                    }
                    None => (start, end),
                };

                self.nfa
                    .add_transition(start, end, Input::Literal(lit.clone()));
            }
            ExprKind::Group(expr, quantifier) => {
                let (start, end) = match quantifier {
                    Some(quantifier) => {
                        let quantifier_state =
                            self.insert_quantifier_state(*quantifier, start, end);
                        (quantifier_state, quantifier_state)
                    }
                    None => (start, end),
                };

                self.expr(expr, start, end);
            }
        }
    }
}
