use self::input::{InputIter, InputIterToken};
use crate::{
    fsm::{NDSimulate, Nfa, NfaCompiler, Simulatable, Simulate, StateId},
    regex::{self, parser::ParseResult},
};

use std::collections::BTreeMap;

mod input;

/// Finite-state machine based lexer.
pub struct Lexer<'input, TokenKind, Fsm: Simulatable = Nfa> {
    /// Input to be lexed.
    pub input: &'input str,
    /// Iterator over the input.
    iter: InputIter<'input>,
    /// Base configuration of the finite-state machine to simulate. The final
    /// states of this machine decide the token kind.
    fsm: Fsm,
    /// Map of `StateId` from the FSM to a `TokenKind`. Usually this will be an
    /// enum.
    tokens: BTreeMap<StateId, TokenKindGenerator<TokenKind>>,
}

/// `TokenKind`s can be generated based on the token or can just be created
/// (e.g., unit structs).
///
/// This is useful for doing some parsing of the internal structure of the
/// token, for example, converting to [si units](https://en.wikipedia.org/wiki/International_System_of_Units). In rust,
/// numbers can have a suffix to predeclare the type of the number, this can
/// also be used for parsing out that type.
enum TokenKindGenerator<TokenKind> {
    /// Generate a `TokenKind` based on the token source.
    Map(Box<dyn FnMut(&str) -> TokenKind>),
    /// Create the `TokenKind`.
    Unit(Box<dyn FnMut() -> TokenKind>),
}

/// [`Token`] returned by the [`Lexer`].
#[derive(Debug, PartialEq, Eq)]
pub struct Token<TokenKind> {
    /// Kind of token configured by the user.
    pub kind: TokenKind,
    /// Source string representation of the token.
    pub source: String,
    /// Position of the token in the input. The end points one position beyond
    /// the end of the token.
    pub pos: (usize, usize),
}

impl<TokenKind> Token<TokenKind> {
    /// Creates a new [`Token`] from the [`InputIterToken`].
    fn from_input_iter_token(
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
                // Note: this assumes that the lower final state ids, have the highest
                // precedence. This is enforced by the order of expression
                // compilation.
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

                        TokenKindGenerator::Unit(token_kind_gen) => token_kind_gen(),
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
    /// Creates a `LexerGenerator`.
    pub fn builder() -> LexerGenerator<TokenKind> {
        LexerGenerator::new()
    }
}

/// Builder struct for the [`Lexer`].
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
    /// Creates a new empty [`LexerGenerator`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a single token to the [`Lexer`]. The function is called when
    /// generating the token. It is not given the source representation of
    /// the token.
    ///
    /// # Fails
    ///
    /// When the provided `token` is invalid regex. If you need this use [`with_token_map`] instead.
    ///
    /// [`with_token_map`]: Self::with_token_map
    #[inline]
    pub fn with_token(mut self, token: &str, token_kind: fn() -> TokenKind) -> ParseResult<Self>
    where
        TokenKind: 'static,
    {
        self.add_token(token, TokenKindGenerator::Unit(Box::new(token_kind)))?;
        Ok(self)
    }

    /// Adds a single token to the [`Lexer`].
    ///
    /// # Fails
    ///
    /// When the provided `token` is invalid regex.
    #[inline]
    pub fn with_token_unit(mut self, token: &str, token_kind: TokenKind) -> ParseResult<Self>
    where
        TokenKind: Copy + 'static,
    {
        self.add_token(
            token,
            TokenKindGenerator::Unit(Box::new(move || token_kind)),
        )?;
        Ok(self)
    }

    /// Adds a single token mapping function to the [`Lexer`]. This function is
    /// called when generating the token and is given the source string
    /// representation of the token.
    ///
    /// # Fails
    ///
    /// When the provided `token` is invalid regex.
    #[inline]
    pub fn with_token_map(
        mut self,
        token: &str,
        // token_kind_map: Box<dyn FnMut(&str) -> TokenKind>,
        token_kind_map: fn(&str) -> TokenKind,
    ) -> ParseResult<Self>
    where
        TokenKind: 'static,
    {
        self.add_token(token, TokenKindGenerator::Map(Box::new(token_kind_map)))?;
        Ok(self)
    }

    /// Adds a single token to the [`Lexer`].
    ///
    /// # Fails
    ///
    /// When the provided `token` is invalid regex.
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

    /// Builds the [`Lexer`].
    pub fn tokenize(self, input: &str) -> Lexer<TokenKind, Nfa> {
        Lexer {
            input,
            iter: InputIter::new(input),
            fsm: self.fsm_compiler.compile(),
            tokens: self.tokens,
        }
    }
}

#[cfg(test)]
mod tests {
    macro_rules! tokens {
        ($(($start:expr, $end:expr, $source:expr) => $token_kind:expr),*) => {
           [$(Token {kind: $token_kind, source: $source.into(), pos: ($start, $end) }),*]
        };
    }

    macro_rules! assert_eq_tokens {
        ($lhs:expr, $rhs:expr) => {
            for (expected, actual) in $lhs.into_iter().zip($rhs) {
                assert_eq!(expected, actual);
            }
        };
    }

    #[allow(unused)]
    macro_rules! print_tokens {
        ($tokens:expr) => {
            for Token {
                kind,
                source,
                pos: (start, end),
            } in $tokens
            {
                println!("({}, {}, \"{}\") => {:?},", start, end, source, kind);
            }
        };
    }

    use super::{Lexer, Token};

    #[test]
    fn lexer() -> Result<(), Box<dyn std::error::Error>> {
        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
        enum Foo {
            #[default]
            A,
            B,
            C,
            D {
                len: usize,
            },
            E,
        }

        let tokens = Lexer::builder()
            .with_token_unit("aaaa", Foo::A)?
            .with_token_unit("b{4,}", Foo::B)?
            .with_token_unit("b{4}", Foo::C)?
            .with_token_map("d*", |token_source| Foo::D {
                len: token_source.len(),
            })?
            .with_token_unit(r"/\* .*", Foo::A)?
            .with_token_unit(r"/\* .* \*/", Foo::E)?
            .with_token(r"a", Default::default)?
            .tokenize(
                "bbbbbbbaaaabbbbddddddddddddddddd/* foo bar baz **** * /* foo bar baz ***** */",
            );

        let expected = tokens! [
            (0, 7, "bbbbbbb") => Foo::B,
            (7, 11, "aaaa") => Foo::A,
            (11, 15, "bbbb") => Foo::B,
            (15, 32, "ddddddddddddddddd") => Foo::D { len: 17 },
            (32, 77, "/* foo bar baz **** * /* foo bar baz ***** */") => Foo::A
        ];

        assert_eq_tokens!(expected, tokens);

        Ok(())
    }
}
