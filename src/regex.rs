#![allow(unused)]

use std::{iter::Peekable, str::CharIndices};

pub struct Tokenizer<'a> {
    /// Input the tokenizer will tokenize.
    input: &'a str,
    /// Iterator over the characters in the input (as defined in the rust `char`
    /// type), along with their position in the input.
    it: Peekable<CharIndices<'a>>,
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
    Quantifier(QuantifierKind),
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
    Vertical,
}

#[derive(Debug)]
pub enum QuantifierKind {
    Asterisk,
    Plus,
    QuestionMark,
    Range(QuantifierRangeKind),
}

#[derive(Debug)]
pub enum QuantifierRangeKind {
    Max(u32),
    Min(u32),
    Range(u32, u32),
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            it: input.char_indices().peekable(),
            tokens: Vec::new(),
        }
    }

    pub fn tokenize(mut self) -> Vec<Token> {
        while let Some((start_cursor_pos, ch)) = self.it.next() {
            let token_kind = match ch {
                '\\' => self.handle_class_or_escape_sequence(),
                '.' => TokenKind::Class(ClassKind::Wildcard),
                '[' | ']' | '^' | '|' => self.handle_operators(ch),
                '*' | '+' | '?' | '{' => self.handle_quantifier(ch),

                _ => unimplemented!("character not handled! ({})", ch),
            };

            let end_cursor_pos = self.get_token_end_cursor_pos();
            self.tokens
                .push(Token::new(token_kind, (start_cursor_pos, end_cursor_pos)))
        }

        self.tokens
    }

    fn handle_operators(&self, ch: char) -> TokenKind {
        let operator_kind = match ch {
            '[' => OperatorKind::LeftSquareBracket,
            ']' => OperatorKind::RightSquareBracket,
            '^' => OperatorKind::Carret,
            '|' => OperatorKind::Vertical,

            _ => unreachable!("unhandled operator ({})", ch),
        };

        TokenKind::Operator(operator_kind)
    }

    fn handle_quantifier(&mut self, ch: char) -> TokenKind {
        let quantifier_kind = match ch {
            '*' => QuantifierKind::Asterisk,
            '+' => QuantifierKind::Plus,
            '?' => QuantifierKind::QuestionMark,
            '{' => {
                // Handle range quantifiers
                let Some((_, ch)) = self.it.peek() else {
                    // the token was `{`
                    return TokenKind::Invalid
                };

                if !ch.is_ascii_digit() {
                    return TokenKind::Invalid;
                }

                let first_value = self.take_next_decimals_and_convert_to_u32();

                let Some((_, mut ch)) = self.it.next() else {
                            return TokenKind::Invalid;
                        };

                let comma_found = if ch == ',' {
                    let Some((_, next_ch)) = self.it.next() else {
                                // the token is `{<number>,`
                                return TokenKind::Invalid;
                            };
                    ch = next_ch;
                    true
                } else {
                    false
                };

                let range_kind = match ch {
                    '}' => {
                        let Some(first_value) = first_value else {
                                    return TokenKind::Invalid;
                                };
                        if !comma_found {
                            QuantifierRangeKind::Max(first_value)
                        } else {
                            QuantifierRangeKind::Min(first_value)
                        }
                    }
                    '0'..='9' => {
                        let second_value = self.take_next_decimals_and_convert_to_u32();

                        let Some((_, ch)) = self.it.next() else {
                            return TokenKind::Invalid;
                        };

                        if ch != '}' {
                            return TokenKind::Invalid;
                        } else {
                            let (Some(min_value), Some(max_value)) = (first_value, second_value) else {
                                return TokenKind::Invalid;
                            };

                            QuantifierRangeKind::Range(min_value, max_value)
                        }
                    }

                    _ => return TokenKind::Invalid,
                };

                QuantifierKind::Range(range_kind)
            }

            _ => unreachable!("unhandled quantifier start character ({})", ch),
        };

        TokenKind::Quantifier(quantifier_kind)
    }

    fn handle_class_or_escape_sequence(&mut self) -> TokenKind {
        let Some((_, ch)) = self.it.next() else {
            // The last token of the input was a '\', which should always be pared.
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

    fn take_next_hexadecimals_and_convert_to_u32(&mut self, amount: usize) -> Option<u32> {
        if amount > 8 {
            panic!("cannot take more hexadecimals than 8, because the result should fit in a u32");
        }

        (&mut self.it)
            .scan(0, |count, (_, ch)| {
                if *count == amount {
                    None
                } else {
                    ch.to_digit(16)
                }
            })
            .reduce(|acc, d| (acc << 4) + d)
    }

    /// Take all decimal characters from the input iterator even if they overflow the usize. If they overflow, return None.
    fn take_next_decimals_and_convert_to_u32(&mut self) -> Option<u32> {
        const U32_DIGIT_COUNT: u32 = 10;
        (&mut self.it)
            .scan(0, |count, (_, ch)| {
                if *count == U32_DIGIT_COUNT {
                    None
                } else {
                    ch.to_digit(10)
                }
            })
            .reduce(|acc, d| (acc * 10) + d)
    }

    fn get_token_end_cursor_pos(&mut self) -> usize {
        match self.it.peek() {
            Some((end_cursor_pos, _)) => *end_cursor_pos - 1,
            None => self.input.chars().count() - 1,
        }
    }
}
