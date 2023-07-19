use crate::regex::{
    ast::{self, Ast, ExprKind},
    tokenizer::QuantifierKind,
};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

// TODO: Refactor quantifiers and destination states of the quantifier to be more readable in
// function definitions.

// TODO (IMPORTANT): Quantifier guard states should have an end guard as well. This will always go
// back to the begin guard which can then decide to continue or to redo the quantified part. Right
// now, quantified expressions can reach the final state no matter what the quantifier is for the
// expression.

pub(super) type StateId = usize;

pub(super) struct Nfa {
    pub(super) start_state: StateId,
    pub(super) states: Vec<State>,
}

#[derive(PartialEq, Eq)]
pub(super) struct State {
    /// Id of the state used by other states as a pointer.
    pub(super) id: StateId,
    /// Whether the state is final.
    pub(super) fin: bool,
    /// Transitions to other states that can be made based on an input.
    pub(super) transitions: HashMap<Input, HashSet<StateId>>,
    /// Whether the state is a quantifier guard. If it is, what is the quantifier and if the
    /// quantifier is satisfied, what shortcut in the NFA can be taken to "fullfil" the quantifier.
    ///
    /// Note: The transition that is used as the shortcut does not need to be inserted into the map of
    /// transitions.
    pub(super) quantifier: Option<(QuantifierKind, StateId)>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub(super) enum Input {
    Literal(ast::LiteralKind),
    Eps,
}

impl Default for Nfa {
    fn default() -> Self {
        NfaBuilder::new(false).with_state(true).build()
    }
}

impl Nfa {
    fn builder(start_state_final: bool) -> NfaBuilder {
        NfaBuilder::new(start_state_final)
    }

    pub(super) fn get_state(&self, id: StateId) -> &State {
        self.states
            .iter()
            .find(|s| s.id == id)
            .expect("requested state does not exist")
    }

    fn get_final_states(&self) -> impl Iterator<Item = &State> + '_ {
        self.states.iter().filter(|s| s.fin)
    }

    /// Returns an iterator over references to all states in the epsilon closure of the given
    /// state.
    pub(super) fn eps_closure(&self, state_id: StateId) -> impl Iterator<Item = &State> {
        let state = self
            .states
            .iter()
            .find(|s| s.id == state_id)
            .expect("state should always exist");

        let mut not_visited = VecDeque::from([state]);
        let mut result = BTreeSet::from([state]);

        while let Some(state) = not_visited.pop_front() {
            let new_states = state
                .transitions
                .iter()
                .filter_map(|(input, states)| {
                    if *input == Input::Eps {
                        Some(states.iter().map(|state_id| {
                            self.states
                                .iter()
                                .find(|state| state.id == *state_id)
                                .expect("state should always exist otherwise the NFA is invalid")
                        }))
                    } else {
                        None
                    }
                })
                .flatten();

            // This clone _should_ be cheap, as it is a clone of references and not actual states.
            not_visited.extend(new_states.clone());
            result.extend(new_states);
        }

        result.into_iter()
    }

    /// Converts the NFA to dot language using the grahviz dot language format.
    #[allow(unused)]
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
                            format!("{} -> {} [label = \"{:?}\"];\n", state.id, dest, input)
                        })
                    })
            })
            .collect::<String>();

        let final_dot = format!(
            "node [shape = doublecircle]; {}",
            self.get_final_states()
                .map(|s| s.id.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        );

        let guard_dot = self
            .states
            .iter()
            .filter_map(|s| {
                s.quantifier
                    .map(|q| format!("{} [label = \"{} ({:?})\"];\n", s.id, s.id, q))
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
            transition_dot
                .lines()
                .map(|l| format!("\t{}", l))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

impl Input {
    pub(crate) fn can_take(&self, input: char) -> bool {
        match self {
            Input::Literal(lit) => lit.contains(input),
            Input::Eps => false,
        }
    }
}

impl State {
    fn new(
        id: StateId,
        transitions: HashMap<Input, HashSet<StateId>>,
        fin: bool,
        quantifier: Option<(QuantifierKind, StateId)>,
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

    fn with_id_and_quantifier(
        id: StateId,
        fin: bool,
        quantifier: (QuantifierKind, StateId),
    ) -> Self {
        Self::new(id, HashMap::new(), fin, Some(quantifier))
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

    fn add_state_with_quantifier(
        &mut self,
        fin: bool,
        quantifier: (QuantifierKind, StateId),
    ) -> StateId {
        let id = self.new_state_id();
        self.states
            .push(State::with_id_and_quantifier(id, fin, quantifier));
        id
    }

    fn add_transition(&mut self, start: StateId, end: StateId, input: Input) {
        self.states
            .get_mut(start)
            .expect("state index does not exist")
            .transitions
            .entry(input)
            .or_insert(HashSet::new())
            .insert(end);
    }

    /// Returns the next generated state should have. If the state is not created, the id will not
    /// be taken.
    fn new_state_id(&self) -> StateId {
        self.states.len()
    }
}

// Regex AST to NFA compiler.
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

        self.expr(expr, self.nfa.start_state, end_state);
        self.nfa.build()
    }

    fn insert_quantifier_state(
        &mut self,
        quantifier: QuantifierKind,
        start_state: StateId,
        end_state: StateId,
    ) -> StateId {
        let quantifier_state = self
            .nfa
            .add_state_with_quantifier(false, (quantifier, end_state));

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

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

#[cfg(test)]
mod foo {
    use super::Nfa;
    use crate::regex::parser::Parser;

    #[test]
    fn to_dot() {
        println!(
            "{}",
            Nfa::from(Parser::new("(ab)c🔥🌘").parse().unwrap()).to_dot()
        );
        println!("{}", Nfa::from(Parser::new("").parse().unwrap()).to_dot());
        println!(
            "{}",
            Nfa::from(Parser::new("((a|b)c🔥🌘|foo)").parse().unwrap()).to_dot()
        );
        println!(
            "{}",
            Nfa::from(Parser::new("((ab)*c{3})?").parse().unwrap()).to_dot()
        );
    }
}
