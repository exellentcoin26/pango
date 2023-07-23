use super::model::{Input, Nfa, NfaBuilder, StateId};
use crate::regex::{
    ast::{Ast, ExprKind},
    tokenizer::QuantifierKind,
};

/// Regex AST to NFA compiler.
pub(super) struct Compiler {
    /// Current NFA being compiled from the regex syntax tree.
    nfa: NfaBuilder,
}

impl From<Ast> for Nfa {
    fn from(value: Ast) -> Self {
        Compiler::new().compile(&value.0)
    }
}

impl Compiler {
    pub(super) fn new() -> Self {
        Self {
            nfa: Nfa::builder(false).with_state(true),
        }
    }

    pub(super) fn compile(mut self, expr: &ExprKind) -> Nfa {
        let end_state = self
            .nfa
            .get_final_states()
            .next()
            .expect("exected at least one final state for the NFA to start with");

        self.expr(expr, self.nfa.start_state, end_state);
        self.nfa.build()
    }

    fn insert_quantifier_state(
        &mut self,
        quantifier: QuantifierKind,
        start_state: StateId,
        end_state: StateId,
    ) -> StateId {
        let quantifier_state = self.nfa.add_quantified_state(quantifier, end_state);

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
                // Connect both the expect start and end state
                self.expr(lhs, start, end);
                self.expr(rhs, start, end);
            }
            ExprKind::Lit(lit, quantifier) => {
                // TODO: Decide on how to implement quantification of states. Right now I think it
                // might be possible to combine quantifiers and take min/max values of the range
                // values to decide the new quantifier.
                //
                // Quantification can be implemented using an extra gateway state and a

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
                // TODO: Decide on how to implement quantification of expressions. A quantification
                // can be expressed as a wrapped expression with a gateway state that counts how
                // many times it is passed and can both go to the end state for the quantification
                // wrapper and redo the expression when the quantification is still or not yet
                // valid.

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
