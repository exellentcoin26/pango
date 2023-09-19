use pango::{Grammar, Slr, Symbol};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Variables {
    Expr,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Terminals {
    Mul,
    Plus,
    Val,
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

fn main() {
    use {Terminals::*, Variables::*};

    let grammar = Grammar::builder()
        .with_start_variable(Expr)
        .with_rules(
            Expr,
            [
                vec![Expr.into(), Mul.into(), Val.into()],
                vec![Expr.into(), Plus.into(), Val.into()],
                vec![Val.into()],
            ],
        )
        .build();

    println!("{:#?}", grammar);

    let parser = Slr::new(grammar);

    println!("{:#?}", parser);
}
