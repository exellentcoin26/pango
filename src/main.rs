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
    Semicolon,
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
            Vec::from([Terminals::Identifier.into(), Terminals::Semicolon.into()]),
        )
        .build();

    println!("{:#?}", grammar);

    let cfsm0 = Cfsm::from_grammar(&grammar);
    let cfsm1 = Cfsm::from_grammar(&grammar);
    let cfsm2 = Cfsm::from_grammar(grammar.clone());

    println!("{:#?}", cfsm0);
    println!("{:#?}", cfsm1);
    println!("{:#?}", cfsm2);
}
