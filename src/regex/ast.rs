//! Regex ast based on this grammar: https://github.com/kean/Regex/blob/main/grammar.ebnf
//!
//! # Grammar
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

use std::collections::HashSet;

use super::tokenizer;

/// Regular expression ast.
#[derive(Debug)]
pub enum Expr {
    /// An alternative expression (e.g., `<expression> | <expression>`).
    Alt(Box<Expr>, Box<Expr>),
    /// A literal (e.g., `a`, `[^ca]`, `[a-z]`).
    Lit(LiteralKind),
    /// A quantified expresion (e.g., `(ab[ac]){3,}`, `[0-1]*`).
    Quant(Box<Expr>, Option<tokenizer::QuantifierKind>),
}

/// Literal kind that appears in an expression (e.g., `b`, `[^ab]`).
#[derive(Debug)]
pub enum LiteralKind {
    /// A single token (unicode character constructs can consist multiple characters).
    Match(char),
    /// A group of characters (e.g. `[^a-cl47i]`).
    Group {
        negated: bool,
        kind: HashSet<GroupedLiteralKind>,
    },
}

/// literal that appears in a group.
#[derive(Debug)]
pub enum GroupedLiteralKind {
    Match(char),
    Range(char, char),
}
