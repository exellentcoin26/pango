use pango_lexer::{LexError, Token};

use super::traits::TerminalEq;
use crate::Terminal;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError<'a, T>
where
    T: TerminalEq,
{
    Lexer(LexError),
    Parser {
        /// Token received by the parser.
        ///
        /// Note: No token being received is the same as receiving `Eof`.
        received_token: Option<Token<T>>,
        expected_tokens: Vec<Terminal<&'a T>>,
    },
}

impl<T> std::fmt::Display for ParseError<'_, T>
where
    T: TerminalEq,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Lexer(err) => writeln!(f, "{}", err),
            ParseError::Parser { .. } => writeln!(f, "[ERROR] (_, _): parse error"),
        }
    }
}
