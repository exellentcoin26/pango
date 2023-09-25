pub type LexResult<T> = Result<T, LexError>;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct LexError(pub(super) usize, pub(super) usize);

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "[ERROR] ({}, {}): Lexer could not identify token",
            self.0, self.1
        )
    }
}

impl std::error::Error for LexError {}
