use pango_lexer::Lexer;
use pango_parser::{Grammar, Slr, Symbol, TerminalEq, TerminalHash};

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
        .tokenize("3+4_");

    println!("{:#?}", parser.parse(lexer));

    Ok(())
}
