use pango::{Grammar, Slr, Symbol, TerminalEq, TerminalHash};
use pango_lexer::Lexer;

// Note: Be careful when deriving `Eq`, `PartialEq` and `Hash` for terminals, the lookup of actions in the parse table is
// done using the `Eq` and `Hash` implementation. This means that if the terminal were to hold
// data (e.g., named fields), this would be taken into account. Mostly resulting in incorrect
// behaviour.

// TODO: Find a way to solve this.
//
// Ideas:
//  - Using a different trait: This is possible because the table does not store a terminal
//    directly. Instead it stores a wrapper type which would allow for `Eq` and `Hash` to be passed
//    through to something like `TerminalEq` and `TerminalHash`.

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Variables {
    Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Terminals {
    Mul,
    Plus,
    Val(usize),
}

impl TerminalEq for Terminals {
    fn terminal_eq(&self, other: &Self) -> bool {
        match (self, other) {
            // ignore the value when matching terminal kinds
            (Terminals::Val(_), Terminals::Val(_)) => true,
            _ => self == other,
        }
    }
}
impl TerminalHash for Terminals {
    fn terminal_hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        use std::hash::Hash;
        match self {
            // ignore the value when hashing terminal kinds
            Terminals::Val(_) => Terminals::Val(0).hash(state),
            _ => self.hash(state),
        }
    }
}

impl<T> From<Variables> for Symbol<Variables, T> {
    fn from(value: Variables) -> Self {
        Symbol::Variable(value)
    }
}

impl<V> From<Terminals> for Symbol<V, Terminals> {
    fn from(value: Terminals) -> Self {
        Self::Terminal(value)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use {Terminals::*, Variables::*};

    let grammar = Grammar::builder()
        .with_start_variable(Expr)
        .with_rules(
            Expr,
            [
                vec![Expr.into(), Mul.into(), Val(0).into()],
                vec![Expr.into(), Plus.into(), Val(0).into()],
                vec![Val(0).into()],
            ],
        )
        .build();

    println!("{:#?}", grammar);

    let parser = Slr::new(grammar);

    let lexer = Lexer::builder()
        .with_token_map(r"\d", |source| {
            Val(source.parse().expect("failed to parse digit as usize"))
        })?
        .with_token_unit(r"\+", Plus)?
        .with_token_unit(r"\*", Mul)?
        .tokenize("3+4*3+1");

    println!("{:#?}", parser.parse(lexer));

    Ok(())
}
