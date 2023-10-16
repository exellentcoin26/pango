/// Whether the paring of the regex succeeded.
pub type ParseResult<'a, T> = core::result::Result<T, ParseErrors<'a>>;

#[derive(Debug, Clone, Default)]
pub struct ParseErrors<'a> {
    source: Option<&'a str>,
    errors: Vec<ParseError>,
}

/// Information about the error occurred during parsing.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub pos: (usize, usize),
}

#[derive(Debug, Clone, Copy)]
pub enum ParseErrorKind {
    LeftParan,
    RightParan,
    LeftBracket,
    RightBracket,
    SubExpression,
    TokenForSubExpression,
    Match,
    MatchCharacterClassLeftBracket,
    CharacterClassCharacter,
    CharacterGroupItem,
    TokenForCharacterGroupItem,
}

impl<'a> ParseErrors<'a> {
    pub(super) fn append(&mut self, mut other: Self) {
        self.errors.append(&mut other.errors)
    }

    pub(super) fn set_source(mut self, source: impl Into<&'a str>) -> Self {
        self.source = Some(source.into());
        self
    }
}

impl From<Vec<ParseError>> for ParseErrors<'_> {
    fn from(errors: Vec<ParseError>) -> Self {
        Self {
            source: None,
            errors,
        }
    }
}

impl From<ParseError> for ParseErrors<'_> {
    fn from(error: ParseError) -> Self {
        Self {
            source: None,
            errors: Vec::from([error]),
        }
    }
}

impl std::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParseErrorKind::*;
        write!(
            f,
            "{}",
            match self {
                SubExpression | TokenForSubExpression =>
                    "expected at least one token for a sub expression",
                LeftParan => "expected LEFT_PAREN",
                RightParan => "expected RIGTH_PAREN",
                MatchCharacterClassLeftBracket => "expected MATCH, CHARACTER_CLASS or LEFT_BRACKET",
                LeftBracket => "expected LEFT_BRACKET",
                RightBracket => "expected RIGHT_BRACKET",
                Match => "expected CHARACTER_CLASS, CHARACTER, or character_group",
                CharacterClassCharacter => "expected CHARACTER_CLASS or CHARACTER",
                CharacterGroupItem | TokenForCharacterGroupItem =>
                    "expected at least one item in the character group",
            }
        )
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "[ERROR] ({}, {}): {}", self.pos.0, self.pos.1, self.kind)
    }
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseErrors<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in self.errors.iter() {
            writeln!(f, "{}", err)?;
        }

        Ok(())
    }
}

impl std::error::Error for ParseErrors<'_> {}
