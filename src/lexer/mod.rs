use crate::{
    fsm::Nfa,
    regex::{self, parser::ParseResult},
};

pub struct Lexer {
    fsm: Nfa,
}

pub struct LexerGenerator {
    tokens: Vec<regex::Ast>,
}

impl LexerGenerator {
    pub fn new() -> Self {
        Self { tokens: Vec::new() }
    }

    pub fn with_token(mut self, token: &str) -> ParseResult<Self> {
        self.tokens.push(regex::Parser::new(token).parse()?);
        Ok(self)
    }

    pub fn generate(self) -> Lexer {
        Lexer {
            fsm: Nfa::from(self.tokens),
        }
    }
}
