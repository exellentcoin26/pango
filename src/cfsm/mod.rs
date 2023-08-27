use self::{
    item::{ItemBody, ItemSet},
    state::{State, StateId, StateIdGenerator},
};
use crate::{Grammar, Symbol};

use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    fmt::Debug,
    hash::Hash,
    marker::PhantomPinned,
    pin::Pin,
};

mod item;
mod state;

#[derive(Debug)]
pub struct Cfsm<'g, V, T>
where
    Grammar<V, T>: Clone,
{
    start_state: StateId,
    states: BTreeSet<State<V, T>>,
    grammar: Cow<'g, Grammar<V, T>>,
    _pin: PhantomPinned,
}

impl<'g, V, T> Cfsm<'g, V, T>
where
    Grammar<V, T>: Clone,
{
    fn builder(grammar: Cow<'g, Grammar<V, T>>) -> CfsmBuilder<'g, V, T> {
        CfsmBuilder::new(grammar)
    }

    pub fn get_grammar(&self) -> &Grammar<V, T> {
        &self.grammar
    }
}

impl<'g, V, T> Cfsm<'g, V, T>
where
    V: Copy + Eq + Hash,
    Symbol<V, T>: Clone + Eq + Hash,
    Grammar<V, T>: Clone,
{
    pub fn from_grammar(grammar: impl Into<Cow<'g, Grammar<V, T>>>) -> Pin<Box<Self>> {
        let mut builder = Self::builder(grammar.into());

        let mut state_id_generator = StateIdGenerator::default();

        let start_state_id = state_id_generator.next();
        builder.set_start_state_id(start_state_id);
        let mut pending_states = VecDeque::from([State::new(
            start_state_id,
            ItemSet::from((
                builder.get_grammar().get_start_variable_rules(),
                builder.get_grammar(),
            )),
        )]);

        while let Some(mut state) = pending_states.pop_front() {
            // try all symbols with a bullet reading them

            let State {
                ref item_set,
                ref mut transitions,
                ..
            } = state;

            for (symbol, item_set) in item_set.iter_by_cursor_symbol() {
                // advance the items which read the same symbol

                let new_item_set = item_set
                    .into_iter()
                    .map(|(head, item_bodies)| {
                        let new_item_bodies = item_bodies
                            .into_iter()
                            .copied()
                            .map(ItemBody::advance)
                            .collect::<HashSet<_>>();
                        (head, new_item_bodies)
                    })
                    .collect::<HashMap<_, _>>();

                let item_set = ItemSet::from((new_item_set, builder.get_grammar()));

                let new_state_id = {
                    match builder
                        .get_states_mut()
                        .iter()
                        .find(|s| s.has_item_set(&item_set))
                    {
                        Some(State { id, .. }) => *id,
                        None => {
                            let state = State::new(state_id_generator.next(), item_set);
                            let state_id = state.id;
                            pending_states.push_back(state);
                            state_id
                        }
                    }
                };

                transitions.insert(symbol.clone(), new_state_id);
            }

            builder.add_state(state);
        }

        builder.build()
    }
}

struct CfsmBuilder<'g, V, T>
where
    Grammar<V, T>: Clone,
{
    start_state: Option<StateId>,
    cfsm: Pin<Box<Cfsm<'g, V, T>>>,
}

impl<'g, V, T> CfsmBuilder<'g, V, T>
where
    Grammar<V, T>: Clone,
{
    fn new(grammar: Cow<'g, Grammar<V, T>>) -> Self {
        Self {
            start_state: None,
            cfsm: Box::pin(Cfsm {
                start_state: 0,
                states: BTreeSet::new(),
                grammar,
                _pin: PhantomPinned,
            }),
        }
    }

    fn get_grammar(&self) -> &Grammar<V, T> {
        &self.cfsm.grammar
    }

    fn set_start_state_id(&mut self, state_id: StateId) {
        self.start_state = Some(state_id);
        *self.get_start_state_id_mut() = state_id;
    }

    fn add_state(&mut self, state: State<V, T>) {
        self.get_states_mut().insert(state);
    }

    fn get_states_mut(&mut self) -> &mut BTreeSet<State<V, T>> {
        // SAFETY: Returning a mutable reference to the `states` field, does not move
        // the struct, nor does moving out of the mutable reference. The
        // `states` field does not contain referenced data, only references.
        &mut unsafe { self.cfsm.as_mut().get_unchecked_mut() }.states
    }

    fn get_start_state_id_mut(&mut self) -> &mut StateId {
        &mut unsafe { self.cfsm.as_mut().get_unchecked_mut() }.start_state
    }

    fn build(mut self) -> Pin<Box<Cfsm<'g, V, T>>> {
        // `Cfsm` is a self-referential struct, thus the pin implementation is used.
        // This means that it needs to be constructed first with the grammar and
        // then modified.

        let (state_ids, destination_ids) = self.get_states_mut().iter().fold(
            (HashSet::new(), HashSet::new()),
            |(mut state_ids, mut destination_ids),
             State {
                 id, transitions, ..
             }| {
                state_ids.insert(*id);
                destination_ids.extend(transitions.values().copied());

                (state_ids, destination_ids)
            },
        );

        assert!(
            destination_ids.is_subset(&state_ids),
            "one or more destination states found that do not exist",
        );

        // the `start_state` on the inner cfsm is already set
        match self.start_state {
            Some(start_state) => {
                assert!(
                    state_ids.contains(&start_state),
                    "start state is not a valid state"
                );

                start_state
            }
            None => unreachable!("start state not set"),
        };

        self.cfsm
    }
}

#[cfg(test)]
mod tests {
    use super::Cfsm;
    use crate::{Grammar, Symbol};

    #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
    enum Variable {
        Function,
        Body,
        Prototype,
        Statement,
    }

    #[derive(Debug, Clone, Hash, PartialEq, Eq)]
    enum Terminal {
        Bracket,
        Identifier(String),
        Semi,
    }

    impl<T> From<Variable> for Symbol<Variable, T> {
        fn from(v: Variable) -> Self {
            Symbol::Variable(v)
        }
    }

    impl<V> From<Terminal> for Symbol<V, Terminal> {
        fn from(t: Terminal) -> Self {
            Symbol::Terminal(t)
        }
    }

    #[test]
    fn cfsm() {
        let grammar = Grammar::builder()
            .with_start_variable(Variable::Function)
            .with_rule(
                Variable::Function,
                [Variable::Prototype.into(), Variable::Body.into()],
            )
            .with_rules(
                Variable::Prototype,
                [
                    vec![Terminal::Identifier(String::new()).into()],
                    vec![Variable::Statement.into(), Terminal::Semi.into()],
                ],
            )
            .with_rule(
                Variable::Statement,
                [Terminal::Identifier(String::new()).into()],
            )
            .with_rules(
                Variable::Body,
                [
                    vec![
                        Terminal::Bracket.into(),
                        Variable::Function.into(),
                        Terminal::Bracket.into(),
                    ],
                    vec![
                        Terminal::Bracket.into(),
                        Terminal::Identifier(String::new()).into(),
                        Terminal::Semi.into(),
                        Terminal::Bracket.into(),
                    ],
                ],
            )
            .build();

        Cfsm::from_grammar(grammar.clone());
        Cfsm::from_grammar(&grammar);
    }
}