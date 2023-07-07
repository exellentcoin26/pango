//! Regex ast based on this grammar: https://github.com/kean/Regex/blob/main/grammar.ebnf
//!
//! # Grammar
//!
//! ```ebnf
//!     expression ::= sub_expression (VERTICAL expression)?;
//!     sub_expression ::= sub_exrpession_item+;
//!     sub_expression_item ::= match | group;
//!     group ::= LEFT_PAREN expresion RIGHT_PAREN QUANTFIER?;s
//!     match ::= match_item QUANTIFIER?;
//!     match_item ::= CHARACTER_CLASS | CHARACTER | character_group;
//!     character_group ::= LEFT_BRACKET CARRET? character_group_item+ RIGHT_BRACKET;
//!     character_group_item ::= CHARACTER_CLASS | character_range | CHARACTER;
//! ```

use super::tokenizer;

/// Regular expression ast.
#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Concatenation of regular expressions.
    Concat(Vec<ExprKind>),
    /// An empty regex expresion.
    Empty,
    /// An alternative expression (e.g., `<expression> | <expression>`).
    Alt(Box<ExprKind>, Box<ExprKind>),
    /// A literal (e.g., `a`, `[^ca]`, `[a-z]`, `[0-1]*`).
    Lit(LiteralKind, Option<tokenizer::QuantifierKind>),
    /// A grouped expression (e.g., `([a-z] | foo)`, `(ab[ac]){3,}`).
    Group(Box<ExprKind>, Option<tokenizer::QuantifierKind>),
}

/// Literal kind that appears in an expression (e.g., `b`, `[^ab]`).
#[derive(Debug, Clone)]
pub enum LiteralKind {
    /// A single token (unicode character constructs can consist multiple characters).
    Match(char),
    /// A shorthand for character groups (e.g., `\w`, `\D`, `.`).
    Class(tokenizer::ClassKind),
    /// A group of characters (e.g. `[^a-cl47i]`).
    Group {
        negated: bool,
        literals: Vec<GroupedLiteralKind>,
    },
}

/// literal that appears in a group.
#[derive(Debug, Clone, Copy)]
pub enum GroupedLiteralKind {
    /// A single token (unicode character constructs can consist multiple characters).
    Match(char),
    /// A shorthand for character groups (e.g., `\w`, `\D`, `.`).
    Class(tokenizer::ClassKind),
    /// A character range (e.g., `0-1`, `a-z`).
    Range(char, char),
}
