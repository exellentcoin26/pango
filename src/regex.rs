#![allow(unused)]

use crate::iter::{CachedPeekable, CachedPeekableable, CautiousMapWhileable, Peekableable};
use std::{
    iter::{Enumerate, Peekable},
    str::Chars,
};

pub struct Tokenizer<'a> {
    /// The input the tokenizer is going to tokenize.
    input: &'a str,
    /// Iterator over the characters in the input (as defined in the rust `char`
    /// type), along with their position in the input.
    it: CachedPeekable<Enumerate<Chars<'a>>>,
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Class(ClassKind),
    Operator(OperatorKind),
    Quantifier(QuantifierKind),
    Match(char),
    Invalid,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ClassKind {
    Wildcard,
    Word,
    Whitespace,
    Digit,
    NonWord,
    NonDigit,
    NonWhitespace,
}

#[derive(Debug, PartialEq, Eq)]
pub enum OperatorKind {
    LeftSquareBracket,
    RightSquareBracket,
    LeftParen,
    RightParan,
    Carret,
    Vertical,
    Minus,
}

#[derive(Debug, PartialEq, Eq)]
pub enum QuantifierKind {
    Asterisk,
    Plus,
    QuestionMark,
    Range(QuantifierRangeKind),
}

#[derive(Debug, PartialEq, Eq)]
pub enum QuantifierRangeKind {
    Max(u32),
    Min(u32),
    Range(u32, u32),
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.it.next() {
            Some((start_cursor_pos, ch)) => {
                let token_kind = match ch {
                    '\\' => self.handle_class_or_escape_sequence(),
                    '.' => TokenKind::Class(ClassKind::Wildcard),
                    '[' | ']' | '(' | ')' | '^' | '|' | '-' => self.handle_operators(ch),
                    '*' | '+' | '?' | '{' => self.handle_quantifier(ch),

                    a => TokenKind::Match(a),
                };

                Some(Token::new(
                    token_kind,
                    (start_cursor_pos, self.get_token_end_cursor_pos()),
                ))
            }
            None => None,
        }
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            it: input.chars().enumerate().cached_peekable(),
        }
    }

    fn handle_operators(&self, ch: char) -> TokenKind {
        let operator_kind = match ch {
            '[' => OperatorKind::LeftSquareBracket,
            ']' => OperatorKind::RightSquareBracket,
            '(' => OperatorKind::LeftParen,
            ')' => OperatorKind::RightParan,
            '^' => OperatorKind::Carret,
            '|' => OperatorKind::Vertical,
            '-' => OperatorKind::Minus,

            _ => unreachable!("unhandled operator (`{}`)", ch),
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
                let Some((_, ch)) = self.it.next() else {
                    // the token was `{`
                    return TokenKind::Invalid
                };

                if !ch.is_ascii_digit() {
                    return TokenKind::Invalid;
                }

                let first_value = self.take_next_decimals_and_convert_to_u32(Some(
                    ch.to_digit(10)
                        .expect("failed to convert character to ascii digit"),
                ));

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
                        let second_value = self.take_next_decimals_and_convert_to_u32(Some(
                            ch.to_digit(10)
                                .expect("failed to convert character to ascii digit"),
                        ));

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

            _ => unreachable!("unhandled quantifier start character (`{}`)", ch),
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
            | '(' | ')' | '[' | ']' | '{' | '}' | '|' | '/' => self
                .handle_escape_sequence(ch)
                .map_or(TokenKind::Invalid, TokenKind::Match),
            'x' | 'u' => self
                .handle_unicode_escape_sequence(ch)
                .map_or(TokenKind::Invalid, TokenKind::Match),

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

            _ => unreachable!("unhandled character class (`{}`)", ch),
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

            _ => unreachable!("unhandled escape sequence (`{}`)", ch),
        }
    }

    fn handle_unicode_escape_sequence(&mut self, ch: char) -> Option<char> {
        match ch {
            'x' => {
                // Take the next two characters and interpret them as hexadecimals.
                // Only take the second if the first could be converted to a
                // hexadecimal, else early return to not take up the next
                // character as well.

                let (digit_amount, character_code) =
                    self.take_next_hexadecimals_and_convert_to_u32(2, None)?;

                if digit_amount != 2 {
                    return None;
                }

                char::from_u32(character_code)
            }
            'u' => {
                // Two forms are possible, either `\uHHHH` or `\u{HHH}`.

                let (_, ch) = self.it.next()?;

                match ch {
                    '{' => {
                        // Take the next four to five characters, interpret them as
                        // hexadecimal and return the unicode character.

                        let (digit_amount, character_code) =
                            self.take_next_hexadecimals_and_convert_to_u32(5, None)?;

                        if digit_amount < 4 {
                            return None;
                        }

                        let result = char::from_u32(character_code);

                        let (_, last_bracket) = self.it.next()?;

                        if last_bracket == '}' {
                            result
                        } else {
                            None
                        }
                    }

                    _ => {
                        // Take the next four characters, interpret them as
                        // hexadecimal and return the unicode character.

                        let hex1 = ch.to_digit(16)?;

                        // Note: The first character has already been consumed.
                        let (digit_count, character_code) =
                            self.take_next_hexadecimals_and_convert_to_u32(4, Some(hex1))?;

                        char::from_u32(character_code)
                    }
                }
            }

            _ => unreachable!("unhandled unicode escape sequence"),
        }
    }

    /// Returns the count of digits and converts them to u32 including the starting digit if present.
    /// Takes all hexadecimals it can take.
    /// It only returns Some when the amount was the expected amount.
    fn take_next_hexadecimals_and_convert_to_u32(
        &mut self,
        max_digits: usize,
        start: Option<u32>,
    ) -> Option<(u32, u32)> {
        assert!(max_digits < 8, "amount of digits should fit in u32");

        let (digit_count, start) = match start {
            Some(d) => {
                assert!(
                    d < 16,
                    "starting digit should be less than 16 (hexadecimal)"
                );
                (1, d)
            }
            None => (0, 0),
        };

        self.it
            .by_ref()
            .cautious_map_while(|(_, ch)| ch.to_digit(16))
            .fold(Some((digit_count, start)), |acc, d| {
                acc.map(|(digit_count, n)| (digit_count + 1, (n << 4) + d))
            })
    }

    /// Take all decimal characters from the input iterator even if they
    /// overflow the u32. If they overflow, return None.
    fn take_next_decimals_and_convert_to_u32(&mut self, start: Option<u32>) -> Option<u32> {
        (&mut self.it)
            .map_while(|(_, ch)| ch.to_digit(10))
            .fold(start.or(Some(0)), |acc, d| Some(acc?.checked_mul(10)? + d))
    }

    fn get_token_end_cursor_pos(&mut self) -> usize {
        match self.it.current() {
            Some((cursor_pos, _)) => cursor_pos + 1,
            None => 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        ClassKind::{self, *},
        OperatorKind::{self, *},
        Token,
        TokenKind::{self, *},
        Tokenizer,
    };

    macro_rules! tokens {
        ($(($start:expr, $end:expr) => $token_kind:expr),*) => {
           [$(Token::new($token_kind.into(), ($start, $end))),*]
        };
    }

    macro_rules! assert_eq_tokens {
        ($lhs:expr, $rhs:expr) => {
            for (expected, actual) in $lhs.into_iter().zip($rhs) {
                assert_eq!(expected, actual);
            }
        };
    }

    macro_rules! print_tokens {
        ($tokens:expr) => {
            for Token {
                kind,
                pos: (start, end),
            } in $tokens
            {
                println!("({}, {}) => {:?}", start, end, kind);
            }
        };
    }

    #[test]
    fn matches() {
        let mut tokenizer = Tokenizer::new("Hello, World!");
        let tokens = tokens![
            (0, 1) => Match('H'),
            (1, 2) => Match('e'),
            (2, 3) => Match('l'),
            (3, 4) => Match('l'),
            (4, 5) => Match('o'),
            (5, 6) => Match(','),
            (6, 7) => Match(' '),
            (7, 8) => Match('W'),
            (8, 9) => Match('o'),
            (9, 10) => Match('r'),
            (10, 11) => Match('l'),
            (11, 12) => Match('d'),
            (12, 13) => Match('!')
        ];

        assert_eq_tokens!(tokens, tokenizer);
    }

    #[test]
    fn unicode_literals() {
        let mut tokenizer = Tokenizer::new("⬛α🌟🔥ž 🍎ж🐶日!3ç🌺💡ś 🎉ë🌞🐧");
        let tokens = tokens![
            (0, 1) => Match('⬛'),
            (1, 2) => Match('α'),
            (2, 3) => Match('🌟'),
            (3, 4) => Match('🔥'),
            (4, 5) => Match('ž'),
            (5, 6) => Match(' '),
            (6, 7) => Match('🍎'),
            (7, 8) => Match('ж'),
            (8, 9) => Match('🐶'),
            (9, 10) => Match('日'),
            (10, 11) => Match('!'),
            (11, 12) => Match('3'),
            (12, 13) => Match('ç'),
            (13, 14) => Match('🌺'),
            (14, 15) => Match('💡'),
            (15, 16) => Match('ś'),
            (16, 17) => Match(' '),
            (17, 18) => Match('🎉'),
            (18, 19) => Match('ë'),
            (19, 20) => Match('🌞'),
            (20, 21) => Match('🐧')
        ];

        assert_eq_tokens!(tokens, tokenizer);
    }

    #[test]
    fn unicode_constructs() {
        let mut tokenizer = Tokenizer::new(
            r"\u{27A1}\u{1F319}\u{1F4A1}\u{1F34E}\u{2328}\u27A1\u27B7\u1CA1\u27BE\xF0\x9F\x8C\x99",
        );
        let tokens = tokens![
            (0, 8) => Match('➡'),
            (8, 17) => Match('🌙'),
            (17, 26) => Match('💡'),
            (26, 35) => Match('🍎'),
            (35, 43) => Match('⌨'),
            (43, 49) => Match('➡'),
            (49, 55) => Match('➷'),
            (55, 61) => Match('Ს'),
            (61, 67) => Match('➾'),
            (67, 71) => Match('ð'),
            (71, 75) => Match('\u{9f}'),
            (75, 79) => Match('\u{8c}'),
            (79, 83) => Match('\u{99}')
        ];

        assert_eq_tokens!(tokens, tokenizer)
    }

    #[test]
    fn control_character_constructs() {
        let mut tokenizer = Tokenizer::new(r"\cA\cZ\cJ");
        let tokens = tokens![
            (0, 3) => Match('\u{1}'),
            (3, 6) => Match('\u{1a}'),
            (6, 9) => Match('\n')
        ];

        assert_eq_tokens!(tokens, tokenizer);
    }

    #[test]
    fn escape_characters() {
        let mut tokenizer = Tokenizer::new(r"\t\r\n\v\f\0");
        let tokens = tokens![
            (0, 2) => Match('\t'),
            (2, 4) => Match('\r'),
            (4, 6) => Match('\n'),
            (6, 8) => Match('\u{b}'),
            (8, 10) => Match('\u{c}'),
            (10, 12) => Match('\0')
        ];

        assert_eq_tokens!(tokens, tokenizer);
    }

    #[test]
    fn character_classes() {
        let mut tokenizer = Tokenizer::new(r".\d\D\w\W\s\S");
        let mut tokens = tokens![
            (0, 1) => Class(Wildcard),
            (1, 3) => Class(Digit),
            (3, 5) => Class(NonDigit),
            (5, 7) => Class(Word),
            (7, 9) => Class(NonWord),
            (9, 11) => Class(Whitespace),
            (11, 13) => Class(NonWhitespace)
        ];

        assert_eq_tokens!(tokens, tokenizer);
    }

    #[test]
    fn operators() {
        let mut tokenizer = Tokenizer::new("[]()^|-");
        let tokens = tokens![
            (0, 1) => Operator(LeftSquareBracket),
            (1, 2) => Operator(RightSquareBracket),
            (2, 3) => Operator(LeftParen),
            (3, 4) => Operator(RightParan),
            (4, 5) => Operator(Carret),
            (5, 6) => Operator(Vertical),
            (6, 7) => Operator(Minus)
        ];

        assert_eq_tokens!(tokens, tokenizer);
    }
}
