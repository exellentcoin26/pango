use std::{collections::HashMap, fmt::Debug, hash::Hash, pin::Pin, ptr::NonNull};

use super::traits::{TerminalEq, TerminalHash};
use crate::{
    cfsm::{self, Cfsm, StateId},
    Grammar, Symbol,
};

// #[derive(Debug)]
pub struct ParseTable<V, T> {
    pub(super) action: ActionTable<V, T>,
    pub(super) goto: GotoTable<V>,
    _cfsm: Pin<Box<Cfsm<V, T>>>,
}

type ActionTable<V, T> = Vec<HashMap<Terminal<T>, ActionKind<V>>>;
type GotoTable<V> = Vec<HashMap<V, StateId>>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ActionKind<V> {
    Shift { dest_state: StateId },
    Reduce { variable: V, amount: usize },
    Accept { variable: V, amount: usize },
}

pub(super) enum Terminal<T> {
    T(NonNull<T>),
    Eof,
}

impl<V: Debug, T: Debug> Debug for ParseTable<V, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseTable")
            .field("action", &self.action)
            .field("goto", &self.goto)
            // .field("cfsm", &self.cfsm)
            .finish()
    }
}

impl<V, T> ParseTable<V, T>
where
    V: Copy + Eq + Hash,
    T: TerminalEq + TerminalHash + Eq + Hash,
{
    pub fn new_slr(cfsm_pin: Pin<Box<Cfsm<V, T>>>) -> Result<Self, ()> {
        let cfsm = cfsm_pin.as_ref();
        let grammar = cfsm.get_grammar();

        let follow_set = grammar.follow_set();

        let (mut action, mut goto) = (ActionTable::new(), GotoTable::new());

        for cfsm::State {
            id: state_id,
            item_set,
            transitions,
        } in cfsm.iter()
        {
            for (head, body) in item_set.iter() {
                match body.get_cursor_symbol() {
                    Some(s @ Symbol::Terminal(t)) => {
                        // body of the form `S -> x •t y`
                        let dest_state = *transitions
                            .get(&s.into())
                            .expect("no transition found for symbol");
                        Self::insert_action(
                            &mut action,
                            *state_id,
                            Terminal::T(t.into()),
                            ActionKind::Shift { dest_state },
                        )?;
                    }
                    Some(s @ Symbol::Variable(v)) => {
                        // body of the form `S -> x •v y`
                        let dest_state = *transitions
                            .get(&s.into())
                            .expect("no transition found for symbol");
                        Self::insert_goto(&mut goto, *state_id, *v, dest_state);
                    }
                    None => {
                        // body of the form `S -> w •`
                        let Some(follow) = follow_set.get(&head) else {
                            continue;
                        };

                        for s in follow.iter() {
                            let t = match s {
                                Symbol::Terminal(t) => Terminal::T(t.into()),
                                Symbol::Variable(_) => {
                                    unreachable!("variables should not exist in the follow set")
                                }
                                Symbol::Epsilon => continue,
                            };

                            Self::insert_action(
                                &mut action,
                                *state_id,
                                t,
                                ActionKind::Reduce {
                                    variable: head,
                                    amount: body.get_body().len(),
                                },
                            )?;
                        }

                        if head == grammar.get_start_variable() {
                            // reduce once to the start variable on a lookahead of eof, then
                            // accept
                            Self::insert_action(
                                &mut action,
                                *state_id,
                                Terminal::Eof,
                                ActionKind::Accept {
                                    variable: head,
                                    amount: body.get_body().len(),
                                },
                            )?;
                        }
                    }
                    Some(Symbol::Epsilon) => {
                        unreachable!("cursor should not be able to read epsilon")
                    }
                }
            }
        }

        Ok(Self {
            action,
            goto,
            _cfsm: cfsm_pin,
        })
    }
}

impl<V, T> ParseTable<V, T>
where
    T: TerminalEq + TerminalHash,
{
    fn insert_action(
        table: &mut ActionTable<V, T>,
        state: StateId,
        terminal: Terminal<T>,
        action: ActionKind<V>,
    ) -> Result<(), ()> {
        if table.len() <= state {
            table.resize_with(state + 1, Default::default);
        }

        let state = table
            .get_mut(state)
            .expect("state should exist as the table has just been resized");
        let mut occupied = true;
        let _entry = state.entry(terminal).or_insert_with(|| {
            occupied = false;
            action
        });

        // TODO: Return type of conflict
        match occupied {
            true => Err(()),
            false => Ok(()),
        }
    }
}

impl<V, T> ParseTable<V, T>
where
    V: Eq + Hash,
{
    fn insert_goto(table: &mut GotoTable<V>, state: StateId, variable: V, new_state: StateId) {
        if table.len() <= state {
            table.resize_with(state + 1, Default::default);
        }

        let old_dest_state = table
            .get_mut(state)
            .expect("state should exist as the table has just been resized")
            .insert(variable, new_state);

        #[cfg(debug_assertions)]
        {
            if let Some(old_dest_state) = old_dest_state {
                assert!(
                    old_dest_state == new_state,
                    "goto conflict should not be possible"
                );
            }
        }
    }
}

impl<V, T> ParseTable<V, T> {
    pub(crate) fn get_grammar(&self) -> &Grammar<V, T> {
        self._cfsm.as_ref().get_grammar()
    }
}

impl<T: Debug> Debug for Terminal<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::T(t) => {
                // SAFETY: `t` comes from the `Cfsm` which is in a `Pin`.
                f.debug_tuple("T").field(unsafe { t.as_ref() }).finish()
            }
            Self::Eof => write!(f, "Eof"),
        }
    }
}

impl<T> From<&T> for Terminal<T> {
    fn from(value: &T) -> Self {
        Self::T(NonNull::from(value))
    }
}

impl<T> PartialEq for Terminal<T>
where
    T: TerminalEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // SAFETY: Both pointers point to a terminal which reside in the pinnned `Cfsm`, which
            // is NOT `Unpin`.
            (Terminal::T(lhs), Terminal::T(rhs)) => unsafe {
                lhs.as_ref().terminal_eq(rhs.as_ref())
            },
            (Terminal::Eof, Terminal::Eof) => true,
            _ => false,
        }
    }
}

impl<T> Eq for Terminal<T> where T: TerminalEq {}

impl<T> Hash for Terminal<T>
where
    T: TerminalHash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            // SAFETY: `t` points to a terminal which resides in the pinnned `Cfsm`, which
            // is NOT `Unpin`.
            Terminal::T(t) => unsafe { t.as_ref() }.terminal_hash(state),
            Terminal::Eof => 0.hash(state),
        }
    }
}
