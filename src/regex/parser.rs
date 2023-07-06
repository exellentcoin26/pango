//! Regex recursive descent parser based on this grammar: https://github.com/kean/Regex/blob/main/grammar.ebnf

use super::tokenizer::{OperatorKind, Token, TokenKind, Tokenizer};
use crate::iter::{CachedPeekable, CachedPeekableable, Peekableable};

/// Recursive descent regex parser.
pub struct Parser<'a> {
    /// Stream of tokens being parsed.
    tokens: CachedPeekable<Tokenizer<'a>>,
}

type ParseResult = core::result::Result<(), ParseError>;

#[derive(Debug)]
pub struct ParseError {
    message: String,
    pos: (usize, usize),
}

impl core::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "[ERROR] ({}, {}): {}",
            self.pos.0, self.pos.1, self.message
        )
    }
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            tokens: Tokenizer::new(input).cached_peekable(),
        }
    }

    pub fn parse(&mut self) -> ParseResult {
        self.expression()
    }

    /// Rule: `expression ::= sub_expression (VERTICAL expression)?`
    fn expression(&mut self) -> ParseResult {
        self.sub_expression()?;

        match self.tokens.peek() {
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::Vertical),
            }) => {
                self.tokens.next();
                self.expression()
            }
            _ => Ok(()),
        }
    }

    /// Rule: `sub_expression ::= sub_exrpession_item+`
    fn sub_expression(&mut self) -> ParseResult {
        if self.tokens.peek().is_none() {
            return Err(ParseError {
                message: "expected at least one sub expression".to_string(),
                pos: self.get_current_token_position(),
            });
        }

        while !matches!(
            self.tokens.peek(),
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::Vertical)
                    | TokenKind::Operator(OperatorKind::RightParen)
            }) | None
        ) {
            self.sub_expression_item()?;
        }

        Ok(())
    }

    /// Rule: `sub_expression_item ::= match | group`
    fn sub_expression_item(&mut self) -> ParseResult {
        let Some(Token {pos: _, kind}) = self.tokens.peek() else {
            return Err(ParseError {message: "expected at least one token for the subexpression".to_string(), pos: self.get_current_token_position()})
        };

        match kind {
            TokenKind::Operator(OperatorKind::LeftParen) => self.group(),
            _ => self.match_(),
        }
    }

    /// Rule: `group ::= LEFT_PAREN expresion RIGHT_PAREN QUANTFIER?`
    fn group(&mut self) -> ParseResult {
        // LEFT_PAREN
        if !matches!(
            self.tokens.next(),
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::LeftParen)
            })
        ) {
            return Err(ParseError {
                message: "expected LEFT_PAREN".to_string(),
                pos: self.get_current_token_position(),
            });
        }

        // expression
        self.expression()?;

        // RIGHT_PAREN
        if !matches!(
            self.tokens.next(),
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::RightParen)
            })
        ) {
            return Err(ParseError {
                message: "expected RIGTH_PAREN".to_string(),
                pos: self.get_current_token_position(),
            });
        }

        // QUANTIFIER
        if let Some(Token {
            pos: _,
            kind: TokenKind::Quantifier(_),
        }) = self.tokens.peek()
        {
            self.tokens.next();
        };

        Ok(())
    }

    /// Rule: `match ::= match_item QUANTIFIER?`
    fn match_(&mut self) -> ParseResult {
        self.match_item()?;

        if let Some(Token {
            pos: _,
            kind: TokenKind::Quantifier(_),
        }) = self.tokens.peek()
        {
            self.tokens.next();
        };

        Ok(())
    }

    /// Rule: `match_item ::= CHARACTER_CLASS | CHARACTER | character_group`
    fn match_item(&mut self) -> ParseResult {
        match self.tokens.peek() {
            Some(Token {
                pos: _,
                kind: TokenKind::Match(_),
            }) => {
                self.tokens.next();
            }
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::LeftBracket),
            }) => self.character_group()?,
            Some(Token {
                pos: _,
                kind: TokenKind::Class(_),
            }) => {
                self.tokens.next();
            }

            _ => {
                return Err(ParseError {
                    message: "expected MATCH, CHARACTER_CLASS or LEFT_BRACKET".to_string(),
                    pos: self.get_current_token_position(),
                });
            }
        };

        Ok(())
    }

    /// Rule: `character_group ::= LEFT_BRACKET CARRET? character_group_item+ RIGHT_BRACKET`
    fn character_group(&mut self) -> ParseResult {
        // LEFT_BRACHET
        if !matches!(
            self.tokens.next(),
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::LeftBracket)
            })
        ) {
            return Err(ParseError {
                message: "expected LEFT_BRACKET".to_string(),
                pos: self.get_current_token_position(),
            });
        }

        // CARRET?
        let negated = match self.tokens.peek() {
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::Carret),
            }) => {
                self.tokens.next();
                true
            }
            _ => false,
        };

        // character_group_item+

        if self.tokens.peek().is_none() {
            return Err(ParseError {
                message: "expected at least one character group item".to_string(),
                pos: self.get_current_token_position(),
            });
        }

        while let Some(token) = self.tokens.peek() {
            match *token {
                Token {
                    pos: _,
                    kind: TokenKind::Operator(OperatorKind::RightBracket),
                } => {
                    self.tokens.next();
                    break;
                }
                _ => self.character_group_item()?,
            };
        }

        Ok(())
    }

    /// Rule: `character_group_item ::= CHARACTER_CLASS | character_range | CHARACTER`
    fn character_group_item(&mut self) -> ParseResult {
        if let Some(Token { pos, kind }) = self.tokens.next() {
            use TokenKind::*;

            match kind {
                Class(_) => (),
                Match(_) => {
                    // Regular character or character range

                    if let Some(Token {
                        pos: _,
                        kind: Operator(OperatorKind::Minus),
                    }) = self.tokens.peek()
                    {
                        self.tokens.next();

                        if !matches!(
                            self.tokens.next(),
                            Some(Token {
                                pos: _,
                                kind: Match(_)
                            })
                        ) {
                            return Err(ParseError {
                                message: "expected MATCH".to_string(),
                                pos: self.get_current_token_position(),
                            });
                        }
                    }
                }

                _ => {
                    return Err(ParseError {
                        message: "expected CHARACTER_CLASS or CHARACTER".to_string(),
                        pos,
                    })
                }
            }
        } else {
            return Err(ParseError {
                message: "expected at least one token for the character group item".to_string(),
                pos: self.get_current_token_position(),
            });
        }

        Ok(())
    }

    fn get_current_token_position(&mut self) -> (usize, usize) {
        let current = self.tokens.current();
        match (self.tokens.peek(), current) {
            (
                Some(Token {
                    pos: (begin_cursor_pos, end_cursor_pos),
                    kind: _,
                }),
                _,
            ) => (*begin_cursor_pos, *end_cursor_pos),
            (
                None,
                Some(Token {
                    pos: (_, end_cursor_pos),
                    kind: _,
                }),
            ) => (end_cursor_pos - 1, end_cursor_pos),
            _ => (0, 1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;

    #[test]
    fn parse() {
        let mut parser = Parser::new(r"\w\d[q-z](0|4+)*.🙃#");
        parser.parse().unwrap();
    }
}
