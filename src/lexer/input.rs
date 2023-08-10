use std::{iter::Enumerate, str::Chars};

pub(super) struct InputIter<'a> {
    /// Character iterator over the input.
    iter: Enumerate<Chars<'a>>,
    /// Token we are building from the input.
    current_token: InputIterToken,
    /// Characters that have been consumed from the input, but have not been accepted into the
    /// token.
    token_suffix: Vec<(usize, char)>,
    /// Owned iterator over the previous token suffix.
    prev_characters_iter: std::vec::IntoIter<(usize, char)>,
}

#[derive(Clone)]
pub(super) struct InputIterToken {
    pub(super) source: String,
    pub(super) pos: (usize, usize),
}

impl Default for InputIterToken {
    fn default() -> Self {
        Self::new(0)
    }
}

impl InputIterToken {
    pub(self) fn new(token_start: usize) -> Self {
        Self {
            source: String::new(),
            pos: (token_start, token_start),
        }
    }
    pub(self) fn update_pos_end(&mut self, new_end: usize) {
        self.pos.1 = new_end;
    }
}

impl Iterator for InputIter<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        // First, use the characters from the previous token suffix that was not accepted.
        self.prev_characters_iter
            .next()
            .or_else(|| self.iter.next())
            .map(|(pos, ch)| {
                self.token_suffix.push((pos, ch));
                ch
            })
    }
}

impl<'a> InputIter<'a> {
    pub(super) fn new(input: &'a str) -> Self {
        Self {
            iter: input.chars().enumerate(),
            current_token: InputIterToken::new(0),
            token_suffix: Vec::new(),
            prev_characters_iter: std::vec::IntoIter::default(),
        }
    }

    pub(super) fn consume_token(&mut self) -> InputIterToken {
        self.prev_characters_iter = std::mem::take(&mut self.token_suffix).into_iter();
        let new_token = InputIterToken {
            // OPTIMIZATION: Assume that the string length of the new token will at least be half
            // of the previous token.
            //
            // TODO(benchmark): Check if this is an actual optimization.
            source: String::with_capacity(self.current_token.source.len() / 2),
            pos: (self.current_token.pos.1, 0),
        };
        std::mem::replace(&mut self.current_token, new_token)
    }

    pub(super) fn get_token(&self) -> &InputIterToken {
        &self.current_token
    }

    pub(super) fn accept_suffix(&mut self) {
        let mut last_pos = 0;

        self.current_token.source.push_str(
            &std::mem::take(&mut self.token_suffix)
                .into_iter()
                .map(|(pos, ch)| {
                    last_pos = pos;
                    ch
                })
                .collect::<String>(),
        );
        // The end position is one beyond the last character of the token.
        self.current_token.update_pos_end(last_pos + 1);
    }
}
