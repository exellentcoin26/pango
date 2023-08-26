use pango::{Cfsm, Grammar, Symbol};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Variables {
    Function,
    Prototype,
    Body,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Terminals {
    Brackets,
    Identifier,
    Simicolon,
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
    let grammar = Grammar::builder()
        .with_start_variable(Variables::Function)
        .with_rule(
            Variables::Function,
            Vec::from([
                Variables::Prototype.into(),
                Terminals::Brackets.into(),
                Variables::Body.into(),
            ]),
        )
        .with_rule(
            Variables::Prototype,
            Vec::from([Terminals::Brackets.into()]),
        )
        .with_rule(
            Variables::Body,
            Vec::from([Terminals::Identifier.into(), Terminals::Simicolon.into()]),
        )
        .build();

    println!("{:#?}", grammar.clone());

    let cfsm = Cfsm::from_grammar(grammar);

    println!("{:#?}", cfsm);
}
