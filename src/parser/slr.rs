use std::hash::Hash;

use pango_lexer::{Lexer, Token};

use super::table::{ActionKind, ParseTable, Terminal};
use crate::{
    cfsm::{Cfsm, StateId},
    cst::{Cst, Node},
    Grammar, Symbol,
};

#[derive(Debug)]
pub struct Slr<V, T> {
    table: ParseTable<V, T>,
}

struct ParseNode<V, T> {
    node: Node<V, T>,
    state: StateId,
}

impl<V, T> Slr<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    pub fn new(grammar: Grammar<V, T>) -> Self {
        let cfsm = Cfsm::from_grammar(grammar);

        let table = ParseTable::new_slr(cfsm).expect("grammar is not SLR");

        Self { table }
    }
}

impl<V, T> Slr<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    pub fn parse(&self, mut input: Lexer<T>) -> Result<Cst<V, T>, ()> {
        // TODO: Store position information about the token.

        let mut next_token = None;

        let mut current_state = StateId::default();
        let mut stack = Vec::from([ParseNode {
            node: Node {
                symbol: Symbol::Variable(self.table.get_grammar().get_start_variable()),
                children: Vec::with_capacity(0),
            },
            state: current_state,
        }]);

        while let Some(token) = next_token.or_else(|| input.next()) {
            next_token = Some(token);
            let (parse_node, accept) =
                self.handle_token(&mut next_token, &mut stack, current_state)?;

            if accept {
                return Ok(Cst {
                    root: parse_node.node,
                });
            };

            current_state = parse_node.state;
            stack.push(parse_node);
        }

        // eof
        next_token = None;
        while let Ok((parse_node, accept)) =
            self.handle_token(&mut next_token, &mut stack, current_state)
        {
            if accept {
                return Ok(Cst {
                    root: parse_node.node,
                });
            };

            current_state = parse_node.state;
            stack.push(parse_node);
        }

        // never reached the accept call
        Err(())
    }

    fn handle_token(
        &self,
        token: &mut Option<Token<T>>,
        stack: &mut Vec<ParseNode<V, T>>,
        current_state: StateId,
    ) -> Result<(ParseNode<V, T>, bool), ()> {
        let terminal = match token {
            Some(Token { ref kind, .. }) => kind.into(),
            None => Terminal::Eof,
        };

        let action = self
            .table
            .action
            .get(current_state)
            .expect("state does not exist in the action table");

        let Some(action_kind) = action.get(&terminal) else {
            return Err(());
        };

        let mut accept = false;
        let parse_node = match action_kind {
            ActionKind::Shift { dest_state } => ParseNode {
                node: Node::from_symbol(Symbol::Terminal(
                    token.take().expect("expected a token to shift").kind,
                )),
                state: *dest_state,
            },
            ActionKind::Reduce { variable, amount } => {
                let children = stack
                    .drain((stack.len() - amount)..)
                    .map(|parse_node| parse_node.node)
                    .rev()
                    .collect();

                let goto = self
                    .table
                    .goto
                    .get(
                        stack
                            .last()
                            .expect("at least one element should be remaining on the stack")
                            .state,
                    )
                    .expect("state does not exist in the goto table");

                let Some(new_state) = goto.get(variable) else {
                    return Err(());
                };

                ParseNode {
                    node: Node {
                        symbol: Symbol::Variable(*variable),
                        children,
                    },
                    state: *new_state,
                }
            }
            ActionKind::Accept { variable, amount } => {
                let children = stack
                    .drain((stack.len() - amount)..)
                    .map(|parse_node| parse_node.node)
                    .rev()
                    .collect();

                accept = true;
                ParseNode {
                    node: Node {
                        symbol: Symbol::Variable(*variable),
                        children,
                    },
                    // State does not matter here as the action is to accept
                    state: StateId::default(),
                }
            }
        };

        Ok((parse_node, accept))
    }
}
