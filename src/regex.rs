#![allow(dead_code)]
#![allow(unused)]

use std::{iter::Peekable, str::Chars};

pub struct Tokenizer<'a> {
    input: &'a str,
    cursor_pos: usize,
    it: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    pos: usize,
}

impl Token {
    pub fn new(kind: TokenKind, pos: usize) -> Self {
        Self { kind, pos }
    }
}

#[derive(Debug)]
pub enum TokenKind {
    Literal(char),
    Class(ClassKind),
    Operator(OperatorKind),
    Invalid,
}

#[derive(Debug)]
pub enum ClassKind {
    Wildcard,
    Word,
    Whitespace,
    Digit,
    NonWord,
    NonDigit,
    NonWhitespace,
}

#[derive(Debug)]
pub enum OperatorKind {
    LeftSquareBracket,
    RightSquareBracket,
    Carret,
    Minus,
    Asterisk,
    QuestionMark,
    Plus,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            cursor_pos: 0,
            it: input.chars().peekable(),
            tokens: Vec::new(),
        }
    }

    pub fn tokenize(mut self) -> Vec<Token> {
        while let Some(ch) = self.it.next() {
            match ch {
                '\\' => {
                    let token_kind = self.take_class_or_escape_character();
                    self.tokens.push(Token::new(token_kind, self.cursor_pos));
                }

                _ => unimplemented!("character not handled! ({})", ch),
            }

            self.cursor_pos += 1;
        }

        self.tokens
    }

    fn take_class_or_escape_character(&mut self) -> TokenKind {
        let Some(ch) = self.it.next() else {
            /// The last token of the input was a '\', which should always be pared. 
            return TokenKind::Invalid;
        };

        match ch {
            'w' | 'W' | 'd' | 'D' | 's' | 'S' => {
                let class = self.handle_class(ch);
                TokenKind::Class(class)
            }
            'f' | 'n' | 'r' | 't' | 'v' | 'c' | '0' | '^' | '$' | '\\' | '.' | '*' | '+' | '?'
            | '(' | ')' | '[' | ']' | '{' | '}' | '|' | '/' => {
                match self.handle_escape_sequence(ch) {
                    Some(literal) => TokenKind::Literal(literal),
                    None => TokenKind::Invalid,
                }
            }

            _ => TokenKind::Invalid,
        }
    }

    fn handle_class(&self, ch: char) -> ClassKind {
        match ch {
            'w' => ClassKind::Word,
            'W' => ClassKind::NonWord,
            'd' => ClassKind::Digit,
            'D' => ClassKind::NonDigit,
            's' => ClassKind::Whitespace,
            'S' => ClassKind::NonWhitespace,
            _ => unreachable!("unhandled character class ({})", ch),
        }
    }

    fn handle_escape_sequence(&mut self, ch: char) -> Option<char> {
        match ch {
            'f' => Some(unsafe { char::from_u32_unchecked(0xc) }),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            'v' => Some(unsafe { char::from_u32_unchecked(0xb) }),
            '0' => Some('\0'),
            '^' | '$' | '\\' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '|'
            | '/' => Some(ch),
            'c' => {
                // 'c' is followed by a letter from 'A'..='Z' or 'a'..='z'. The code point of the following letter modulo 32 is the code point of the escape sequence. This way control characters 1-26 can be used.
                let Some(ch) = self.it.next() else {
                    return None;
                };

                match ch {
                    'a'..='z' | 'A'..='Z' => {
                        Some(char::from_u32(ch as u32 % 32).expect("failed to convert code point"))
                    }
                    _ => None,
                }
            }

            _ => unreachable!("unhandled escape sequence ({})", ch),
        }
    }
}
