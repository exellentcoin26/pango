use std::hash::Hash;

use pango_lexer::{Lexer, Token};

use super::{
    error::ParseError,
    table::{ActionKind, ParseTable},
    traits::{TerminalEq, TerminalHash},
};
use crate::{
    cfsm::{Cfsm, StateId},
    cst::{Cst, Node},
    Grammar, Symbol, Terminal,
};

// TODO: Add mechanism for recovering parsing errors.

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
    T: TerminalEq + TerminalHash + Eq + Hash,
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
    T: TerminalEq + TerminalHash + Eq + Hash,
{
    pub fn parse(&self, mut input: Lexer<T>) -> Result<Cst<V, T>, ParseError<T>> {
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

        while let Some(token) = next_token.map(Ok).or_else(|| input.next()) {
            let token = match token {
                Ok(token) => token,
                Err(err) => return Err(ParseError::Lexer(err)),
            };

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
        Err(ParseError::Parser {
            received_token: None,
            expected_tokens: self.get_expected_tokens(current_state),
        })
    }

    fn handle_token(
        &self,
        token: &mut Option<Token<T>>,
        stack: &mut Vec<ParseNode<V, T>>,
        current_state: StateId,
    ) -> Result<(ParseNode<V, T>, bool), ParseError<T>> {
        let terminal = match token {
            Some(Token { ref kind, .. }) => kind.into(),
            None => Terminal::Eof,
        };

        let action = self
            .table
            .action
            .get(current_state)
            .expect("state does not exist in the action table");

        let Some(action_kind) = action.get(&terminal.into()) else {
            return Err(ParseError::Parser {
                received_token: token.take(),
                expected_tokens: self.get_expected_tokens(current_state),
            });
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

                let new_state = goto
                    .get(variable)
                    .expect("could not find variable in goto table");
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

    fn get_expected_tokens(&self, current_state: StateId) -> Vec<Terminal<&'_ T>> {
        let action = self
            .table
            .action
            .get(current_state)
            .expect("state does not exist in the action table");
        action.keys().copied().map(|k| k.into()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::{Cst, Grammar, Lexer, Node, Slr, Symbol, TerminalEq, TerminalHash};

    #[test]
    fn parse() -> Result<(), Box<dyn std::error::Error>> {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        enum Variables {
            Expr,
        }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        enum Terminals {
            Mul,
            Plus,
            Val(usize),
        }

        impl TerminalEq for Terminals {
            fn terminal_eq(&self, other: &Self) -> bool {
                match (self, other) {
                    // ignore the value when matching terminal kinds
                    (Terminals::Val(_), Terminals::Val(_)) => true,
                    _ => self == other,
                }
            }
        }
        impl TerminalHash for Terminals {
            fn terminal_hash<H>(&self, state: &mut H)
            where
                H: std::hash::Hasher,
            {
                use std::hash::Hash;
                match self {
                    // ignore the value when hashing terminal kinds
                    Terminals::Val(_) => Terminals::Val(0).hash(state),
                    _ => self.hash(state),
                }
            }
        }

        impl<T> From<Variables> for Symbol<Variables, T> {
            fn from(value: Variables) -> Self {
                Symbol::Variable(value)
            }
        }

        impl<V> From<Terminals> for Symbol<V, Terminals> {
            fn from(value: Terminals) -> Self {
                Self::Terminal(value)
            }
        }

        use {Terminals::*, Variables::*};

        let grammar = Grammar::builder()
            .with_start_variable(Expr)
            .with_rules(
                Expr,
                [
                    vec![Expr.into(), Mul.into(), Val(0).into()],
                    vec![Expr.into(), Plus.into(), Val(0).into()],
                    vec![Val(0).into()],
                ],
            )
            .build();

        let parser = Slr::new(grammar);

        let lexer = Lexer::builder()
            .with_token_map(r"\d", |source| {
                Val(source.parse().expect("failed to parse digit as usize"))
            })?
            .with_token_unit(r"\+", Plus)?
            .with_token_unit(r"\*", Mul)?
            .tokenize("3+4*3+1");

        assert_eq!(
            parser.parse(lexer),
            Ok(Cst {
                root: Node {
                    symbol: Expr.into(),
                    children: vec![
                        Node::from_symbol(Val(1).into()),
                        Node::from_symbol(Plus.into()),
                        Node {
                            symbol: Expr.into(),
                            children: vec![
                                Node::from_symbol(Val(3,).into()),
                                Node::from_symbol(Mul.into()),
                                Node {
                                    symbol: Expr.into(),
                                    children: vec![
                                        Node::from_symbol(Val(4,).into()),
                                        Node::from_symbol(Plus.into()),
                                        Node {
                                            symbol: Expr.into(),
                                            children: vec![Node::from_symbol(Val(3,).into(),)],
                                        },
                                    ],
                                },
                            ],
                        },
                    ],
                },
            },)
        );

        Ok(())
    }
}
