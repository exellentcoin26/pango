use self::input::{InputIter, InputIterToken};
use crate::{
    fsm::{NDSimulate, Nfa, NfaCompiler, Simulatable, Simulate, StateId},
    regex::{self, parser::ParseResult},
};

use std::collections::BTreeMap;

mod input;

pub struct Lexer<'input, TokenKind, Fsm: Simulatable> {
    /// Input to be lexed.
    pub input: &'input str,
    /// Iterator over the input.
    iter: InputIter<'input>,
    /// Base configuration of the finite-state machine to simulate. The final states of this
    /// machine decide the token kind.
    fsm: Fsm,
    /// Map of `StateId` from the FSM to a `TokenKind`. Usually this will be an enum.
    tokens: BTreeMap<StateId, TokenKindGenerator<TokenKind>>,
}

enum TokenKindGenerator<TokenKind> {
    Map(Box<dyn FnMut(&str) -> TokenKind>),
    Unit(TokenKind),
}

#[derive(Debug)]
pub struct Token<TokenKind>
where
    TokenKind: Clone,
{
    kind: TokenKind,
    source: String,
    pos: (usize, usize),
}

impl<TokenKind> Token<TokenKind>
where
    TokenKind: Clone,
{
    pub(self) fn from_input_iter_token(
        InputIterToken { source, pos }: InputIterToken,
        kind: TokenKind,
    ) -> Self {
        Self { kind, source, pos }
    }
}

impl<TokenKind, Fsm> Iterator for Lexer<'_, TokenKind, Fsm>
where
    for<'a> Fsm: Simulatable + 'a,
    for<'a> Fsm::Simulator<'a>: NDSimulate,
    TokenKind: Clone,
{
    type Item = Token<TokenKind>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut sim = self.fsm.to_simulator();
        let mut token_kind = None;

        while let Some(ch) = self.iter.next() {
            if !sim.can_feed(ch) {
                // the current token is the longest token we can 'munch'

                return token_kind.map(|token_kind| {
                    Token::from_input_iter_token(self.iter.consume_token(), token_kind)
                });
            }

            if sim.feed(ch) {
                // The finite-state machine currently accepts. Append the last unaccepted
                // characters to the token and store the token kind.
                self.iter.accept_suffix();

                // Get the first final state the Simulator is in.
                //
                // Note: this assumes that the lower final state ids, have the highest precedence.
                // This is enforced by the order of expression compilation.
                let final_state = sim
                    .get_current_final_states()
                    .into_iter()
                    .next()
                    .expect("expected a single final state");

                token_kind = Some(
                    match self
                        .tokens
                        .get_mut(&final_state)
                        .expect("final state should match a single token")
                    {
                        TokenKindGenerator::Map(token_kind_gen) => {
                            token_kind_gen(&self.iter.get_token().source)
                        }

                        TokenKindGenerator::Unit(token_kind) => token_kind.clone(),
                    },
                );
            }
        }

        if sim.is_accepting() {
            token_kind.map(|token_kind| {
                Token::from_input_iter_token(self.iter.consume_token(), token_kind)
            })
        } else {
            None
        }
    }
}

impl<TokenKind> Lexer<'_, TokenKind, Nfa> {
    pub fn builder() -> LexerGenerator<TokenKind> {
        LexerGenerator::new()
    }
}

pub struct LexerGenerator<TokenKind> {
    fsm_compiler: NfaCompiler,
    tokens: BTreeMap<StateId, TokenKindGenerator<TokenKind>>,
}

impl<TokenKind> Default for LexerGenerator<TokenKind> {
    fn default() -> Self {
        Self {
            fsm_compiler: NfaCompiler::new(),
            tokens: BTreeMap::new(),
        }
    }
}

impl<TokenKind> LexerGenerator<TokenKind> {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn with_token(mut self, token: &str, token_kind: TokenKind) -> ParseResult<Self> {
        self.add_token(token, TokenKindGenerator::Unit(token_kind))?;
        Ok(self)
    }

    #[inline]
    pub fn with_token_map(
        mut self,
        token: &str,
        token_kind_map: Box<dyn FnMut(&str) -> TokenKind>,
    ) -> ParseResult<Self> {
        self.add_token(token, TokenKindGenerator::Map(token_kind_map))?;
        Ok(self)
    }

    fn add_token(
        &mut self,
        token: &str,
        token_kind: TokenKindGenerator<TokenKind>,
    ) -> ParseResult<()> {
        // start a new token in the finite-state machine
        let token_final_state = self.fsm_compiler.new_final();

        self.fsm_compiler
            .add_expression(regex::Parser::new(token).parse()?);

        // bind the new final state to the token_kind.
        self.tokens.insert(token_final_state, token_kind);
        Ok(())
    }

    pub fn tokenize(self, input: &str) -> Lexer<TokenKind, Nfa> {
        Lexer {
            input,
            iter: InputIter::new(input),
            fsm: self.fsm_compiler.compile(),
            tokens: self.tokens,
        }
    }
}
