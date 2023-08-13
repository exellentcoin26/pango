//! Regex recursive descent parser based on this grammar: <https://github.com/kean/Regex/blob/main/grammar.ebnf>

use super::{
    ast::{self, Ast},
    tokenizer::{OperatorKind, Token, TokenKind, Tokenizer},
};
use crate::{
    iter::{CachedPeekable, CachedPeekableable, Peekableable},
    prelude::W,
};

/// Recursive descent regex parser.
pub(crate) struct Parser<'a> {
    /// Stream of tokens being parsed.
    tokens: CachedPeekable<Tokenizer<'a>>,
    errors: Vec<ParseError>,
}

/// Whether the paring of the regex succeeded.
pub type ParseResult<T> = core::result::Result<T, W<Vec<ParseError>>>;

// TODO: Refactor error messages to be enum variants with the
// [`std::fmt::Display`] trait.

/// Information about the error occurred during parsing.
#[derive(Debug, Clone)]
pub struct ParseError {
    message: String,
    pos: (usize, usize),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "[ERROR] ({}, {}): {}",
            self.pos.0, self.pos.1, self.message
        )
    }
}

impl std::fmt::Display for W<Vec<ParseError>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in self.0.iter() {
            writeln!(f, "{}", err)?;
        }

        Ok(())
    }
}

impl std::error::Error for W<Vec<ParseError>> {}

impl<'a> Parser<'a> {
    /// Creates a new regex parser from the `input`.
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            tokens: Tokenizer::new(input).cached_peekable(),
            errors: Vec::new(),
        }
    }

    /// Parses the `input` into a regex [`Ast`].
    ///
    /// [`Ast`]: regex::Ast
    pub(crate) fn parse(&mut self) -> ParseResult<ast::Ast> {
        if self.tokens.peek().is_none() {
            return Ok(Ast(ast::ExprKind::Empty));
        }

        match self.expression() {
            Ok(expr) => Ok(Ast(expr)),
            Err(mut errs) => {
                self.errors.append(&mut errs.0);
                Err(W(self.errors.clone()))
            }
        }
    }

    /// Rule: `expression ::= sub_expression (VERTICAL expression)?`
    fn expression(&mut self) -> ParseResult<ast::ExprKind> {
        let mut lhs = self.sub_expression()?;
        let lhs = if lhs.len() == 1 {
            lhs.remove(0)
        } else {
            ast::ExprKind::Concat(lhs)
        };

        match self.tokens.peek() {
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::Vertical),
            }) => {
                self.tokens.next();
                Ok(ast::ExprKind::Alt(
                    Box::new(lhs),
                    Box::new(self.expression()?),
                ))
            }
            _ => Ok(lhs),
        }
    }

    /// Rule: `sub_expression ::= sub_exrpession_item+`
    fn sub_expression(&mut self) -> ParseResult<Vec<ast::ExprKind>> {
        if self.tokens.peek().is_none() {
            return Err(W(vec![ParseError {
                message: "expected at least one sub expression".to_string(),
                pos: self.get_current_token_position(),
            }]));
        }

        let mut literals = Vec::new();
        while !matches!(
            self.tokens.peek(),
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::Vertical)
                    | TokenKind::Operator(OperatorKind::RightParen)
            }) | None
        ) {
            literals.push(self.sub_expression_item()?);
        }

        Ok(literals)
    }

    /// Rule: `sub_expression_item ::= match | group`
    fn sub_expression_item(&mut self) -> ParseResult<ast::ExprKind> {
        let Some(Token {pos: _, kind}) = self.tokens.peek() else {
            return Err(W(vec![ParseError {message: "expected at least one token for the subexpression".to_string(), pos: self.get_current_token_position()}]))
        };

        match kind {
            TokenKind::Operator(OperatorKind::LeftParen) => self.group(),
            _ => self.match_(),
        }
    }

    /// Rule: `group ::= LEFT_PAREN expresion RIGHT_PAREN QUANTFIER?`
    fn group(&mut self) -> ParseResult<ast::ExprKind> {
        // LEFT_PAREN
        if !matches!(
            self.tokens.next(),
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::LeftParen)
            })
        ) {
            return Err(W(vec![ParseError {
                message: "expected LEFT_PAREN".to_string(),
                pos: self.get_current_token_position(),
            }]));
        }

        // expression
        let expr = self.expression()?;

        // RIGHT_PAREN
        if !matches!(
            self.tokens.next(),
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::RightParen)
            })
        ) {
            return Err(W(vec![ParseError {
                message: "expected RIGTH_PAREN".to_string(),
                pos: self.get_current_token_position(),
            }]));
        }

        // QUANTIFIER
        let quantifier = match self.tokens.peek().copied() {
            Some(Token {
                pos: _,
                kind: TokenKind::Quantifier(quant_kind),
            }) => {
                self.tokens.next();
                Some(quant_kind)
            }
            _ => None,
        };

        Ok(ast::ExprKind::Group(Box::new(expr), quantifier))
    }

    /// Rule: `match ::= match_item QUANTIFIER?`
    fn match_(&mut self) -> ParseResult<ast::ExprKind> {
        let literal = self.match_item()?;

        let quantifier = match self.tokens.peek().copied() {
            Some(Token {
                pos: _,
                kind: TokenKind::Quantifier(quant_kind),
            }) => {
                self.tokens.next();
                Some(quant_kind)
            }
            _ => None,
        };

        Ok(ast::ExprKind::Lit(literal, quantifier))
    }

    /// Rule: `match_item ::= CHARACTER_CLASS | CHARACTER | character_group`
    fn match_item(&mut self) -> ParseResult<ast::LiteralKind> {
        let literal = match self.tokens.peek().copied() {
            Some(Token {
                pos: _,
                kind: TokenKind::Match(match_),
            }) => {
                self.tokens.next();
                ast::LiteralKind::Match(match_)
            }
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::LeftBracket),
            }) => self.character_group()?,
            Some(Token {
                pos: _,
                kind: TokenKind::Class(kind),
            }) => {
                self.tokens.next();
                ast::LiteralKind::Class(kind)
            }

            _ => {
                return Err(W(vec![ParseError {
                    message: "expected MATCH, CHARACTER_CLASS or LEFT_BRACKET".to_string(),
                    pos: self.get_current_token_position(),
                }]));
            }
        };

        Ok(literal)
    }

    /// Rule: `character_group ::= LEFT_BRACKET CARRET? character_group_item+
    /// RIGHT_BRACKET`
    fn character_group(&mut self) -> ParseResult<ast::LiteralKind> {
        // LEFT_BRACHET
        if !matches!(
            self.tokens.next(),
            Some(Token {
                pos: _,
                kind: TokenKind::Operator(OperatorKind::LeftBracket)
            })
        ) {
            return Err(W(vec![ParseError {
                message: "expected LEFT_BRACKET".to_string(),
                pos: self.get_current_token_position(),
            }]));
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
            return Err(W(vec![ParseError {
                message: "expected at least one character group item".to_string(),
                pos: self.get_current_token_position(),
            }]));
        }

        let mut literals = Vec::new();
        while let Some(token) = self.tokens.peek() {
            match *token {
                Token {
                    pos: _,
                    kind: TokenKind::Operator(OperatorKind::RightBracket),
                } => {
                    self.tokens.next();
                    break;
                }
                _ => literals.push(self.character_group_item()?),
            };
        }

        Ok(ast::LiteralKind::Group {
            negated,
            literals: W(literals),
        })
    }

    /// Rule: `character_group_item ::= CHARACTER_CLASS | character_range |
    /// CHARACTER`
    fn character_group_item(&mut self) -> ParseResult<ast::GroupedLiteralKind> {
        if let Some(Token { pos, kind }) = self.tokens.next() {
            use TokenKind::*;

            let grouped_literal_kind = match kind {
                Class(kind) => ast::GroupedLiteralKind::Class(kind),
                Match(start) => {
                    // Regular character or character range

                    if let Some(Token {
                        pos: _,
                        kind: Operator(OperatorKind::Minus),
                    }) = self.tokens.peek()
                    {
                        self.tokens.next();

                        match self.tokens.next() {
                            Some(Token {
                                pos: _,
                                kind: Match(end),
                            }) => ast::GroupedLiteralKind::Range(start, end),
                            _ => {
                                return Err(W(vec![ParseError {
                                    message: "expected MATCH".to_string(),
                                    pos: self.get_current_token_position(),
                                }]));
                            }
                        }
                    } else {
                        ast::GroupedLiteralKind::Match(start)
                    }
                }

                _ => {
                    return Err(W(vec![ParseError {
                        message: "expected CHARACTER_CLASS or CHARACTER".to_string(),
                        pos,
                    }]));
                }
            };

            Ok(grouped_literal_kind)
        } else {
            Err(W(vec![ParseError {
                message: "expected at least one token for the character group item".to_string(),
                pos: self.get_current_token_position(),
            }]))
        }
    }

    /// Returns the postion of the current token.
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

    #[test]
    fn foo() {
        let mut parser = Parser::new(r"¡0¡¡¡¡(\\)¡¡0");
        parser.parse().unwrap();
    }
}
