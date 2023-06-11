#![allow(dead_code)]
#![allow(unused)]

use std::{
    iter::{Enumerate, Peekable},
    str::Chars,
};

pub struct Tokenizer<'a> {
    /// Input the tokenizer will tokenize.
    input: &'a str,
    /// Iterator over the characters in the input (as defined in the rust `char`
    /// type), along with their position in the input.
    it: Peekable<Enumerate<Chars<'a>>>,
    /// Temporary list of tokens from the tokenized input.
    /// TODO: Convert to iterator.
    tokens: Vec<Token>,
}

#[derive(Debug)]
pub struct Token {
    /// Information about the kind of token along with the value of the token.
    /// The value can already be parsed (e.g. unicode escape sequences)
    kind: TokenKind,
    /// Start and end position of the token in the input text. The end position
    /// is one further than the end of the current token.
    pos: (usize, usize),
}

impl Token {
    pub fn new(kind: TokenKind, pos: (usize, usize)) -> Self {
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
            it: input.chars().enumerate().peekable(),
            tokens: Vec::new(),
        }
    }

    pub fn tokenize(mut self) -> Vec<Token> {
        while let Some((start_cursor_pos, ch)) = self.it.next() {
            match ch {
                '\\' => {
                    let token_kind = self.take_class_or_escape_character();
                    let end_cursor_pos = match self.it.peek() {
                        Some((end_cursor_pos, _)) => *end_cursor_pos,
                        None => start_cursor_pos,
                    };
                    self.tokens
                        .push(Token::new(token_kind, (start_cursor_pos, end_cursor_pos)));
                }

                _ => unimplemented!("character not handled! ({})", ch),
            }
        }

        self.tokens
    }

    fn take_class_or_escape_character(&mut self) -> TokenKind {
        let Some((_, ch)) = self.it.next() else {
            // The last token of the input was a '\', which should always be
            // pared.
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
            'x' | 'u' => match self.handle_unicode_escape_sequence(ch) {
                Some(literal) => TokenKind::Literal(literal),
                None => TokenKind::Invalid,
            },

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
            'f' => Some(char::from_u32(0xc).expect("failed to convert code point to character")),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            'v' => Some(char::from_u32(0xb).expect("failed to convert code point to character")),
            '0' => Some('\0'),
            '^' | '$' | '\\' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '|'
            | '/' => Some(ch),
            'c' => {
                // 'c' is followed by a letter from 'A'..='Z' or 'a'..='z'. The
                // code point of the following letter modulo 32 is the code
                // point of the escape sequence. This way control characters
                // 1-26 can be used.

                let (_, ch) = self.it.next()?;

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

    fn handle_unicode_escape_sequence(&mut self, ch: char) -> Option<char> {
        match ch {
            'x' => {
                // Take the next two characters and interpret them as hexadecimals.
                // Only take the second if the first could be converted to a
                // hexadecimal, else early return to not take up the next
                // character as well.

                char::from_u32(self.take_next_hexadecimals_and_convert_to_u32(2)?)
            }
            'u' => {
                // Two forms are possible, either `\uHHHH` or `\u{HHH}`.

                let (_, ch) = self.it.next()?;

                match ch {
                    '{' => {
                        // Take the next three characters, interpret them as
                        // hexadecimal and return the unicode character. Only
                        // take the next character if the previous was valid.

                        let result =
                            char::from_u32(self.take_next_hexadecimals_and_convert_to_u32(3)?);

                        let (_, last_bracket) = self.it.next()?;

                        if last_bracket == '}' {
                            result
                        } else {
                            None
                        }
                    }

                    _ => {
                        // Take the next four characters, interpret them as
                        // hexadecimal and return the unicode character. Only
                        // take the next character if the previous was valid.

                        let hex1 = ch.to_digit(16)?;

                        char::from_u32(
                            (hex1 << 12) + self.take_next_hexadecimals_and_convert_to_u32(3)?,
                        )
                    }
                }
            }

            _ => unreachable!("unhandled unicode escape sequence"),
        }
    }

    fn take_next_hexadecimals_and_convert_to_u32(&mut self, amount: u8) -> Option<u32> {
        if amount > 8 {
            panic!("cannot take more hexadecimals than 8, because the result should fit in a u32");
        }

        let mut digits = vec![0u32; amount as usize];
        for i in 0..amount {
            digits[i as usize] = self.it.next().and_then(|(_, ch)| ch.to_digit(16))?;
        }

        Some(digits.into_iter().fold(0, |acc, d| (acc << 4) + d))
    }
}
