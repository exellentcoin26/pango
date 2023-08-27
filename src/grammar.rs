use crate::{Body, Symbol};

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    hash::Hash,
};

// TODO: Convert runtime errors with start variable to type-state builder pattern.

#[derive(Debug, Clone)]
pub struct Grammar<V, T> {
    start_variable: V,
    rules: Rules<V, T>,
}

type Rules<V, T> = HashMap<V, HashSet<Body<V, T>>>;

impl<V, T> Grammar<V, T> {
    pub fn builder() -> GrammarBuilder<V, T> {
        GrammarBuilder::new()
    }
}

impl<V, T> Grammar<V, T>
where
    V: Copy + Eq + Hash,
{
    pub(super) fn get_rule_bodies(&self, head: V) -> Option<&HashSet<Body<V, T>>> {
        self.rules.get(&head)
    }
}

impl<V, T> Grammar<V, T>
where
    V: Eq + Hash,
    T: Eq + Hash,
{
    pub(super) fn get_start_variable_rules(&self) -> (&V, &HashSet<Body<V, T>>) {
        self.rules
            .get_key_value(&self.start_variable)
            .expect("start variable should have an associated rule")
    }
}

impl<V, T> From<Grammar<V, T>> for Cow<'static, Grammar<V, T>>
where
    Grammar<V, T>: Clone,
{
    fn from(grammar: Grammar<V, T>) -> Self {
        Cow::Owned(grammar)
    }
}

impl<'g, V, T> From<&'g Grammar<V, T>> for Cow<'g, Grammar<V, T>>
where
    Grammar<V, T>: Clone,
{
    fn from(grammar: &'g Grammar<V, T>) -> Self {
        Cow::Borrowed(grammar)
    }
}

pub struct GrammarBuilder<V, T> {
    start_variable: Option<V>,
    rules: Rules<V, T>,
}

impl<V, T> GrammarBuilder<V, T> {
    fn new() -> Self {
        Self {
            start_variable: None,
            rules: Rules::new(),
        }
    }
}

impl<V, T> GrammarBuilder<V, T>
where
    V: Eq + Hash,
    T: Eq + Hash,
{
    pub fn with_start_variable(mut self, variable: V) -> Self {
        self.set_start_variable(variable);
        self
    }

    pub fn with_rule(mut self, variable: V, body: impl Into<Body<V, T>>) -> Self {
        self.add_rule(variable, body);
        self
    }

    pub fn set_start_variable(&mut self, variable: V) {
        self.start_variable = Some(variable);
    }

    pub fn add_rule(&mut self, variable: V, body: impl Into<Body<V, T>>) -> bool {
        let body = body.into();
        // default empty bodies to `Symbol::Epsilon`
        let body = if !body.is_empty() {
            body
        } else {
            Body::from([Symbol::Epsilon])
        };

        self.rules
            .entry(variable)
            .or_insert_with(HashSet::new)
            .insert(body)
    }
}

impl<V, T> GrammarBuilder<V, T>
where
    V: Eq + Hash,
{
    pub fn build(self) -> Grammar<V, T> {
        // check whether all variables have a rule associated with them
        let mut variables = HashSet::new();
        let mut heads = HashSet::new();

        for (head, bodies) in self.rules.iter() {
            heads.insert(head);

            variables.extend(
                bodies
                    .iter()
                    .flat_map(|body| body.iter())
                    .filter_map(|symbol| match symbol {
                        Symbol::Variable(v) => Some(v),
                        Symbol::Terminal(_) => None,
                        Symbol::Epsilon => None,
                    }),
            )
        }

        let start_variable = self.start_variable.expect("start variable not set");

        variables.insert(&start_variable);

        assert!(
            variables.is_subset(&heads),
            "all variables should have a rule associated with them",
        );

        Grammar {
            start_variable,
            rules: self.rules,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Grammar;
    use crate::Symbol;

    #[derive(Hash, PartialEq, Eq)]
    enum Variable {
        Function,
        Body,
        Prototype,
    }

    #[derive(Hash, PartialEq, Eq)]
    enum Terminal {
        Bracket,
        Identifier(String),
        Semi,
    }

    impl<T> From<Variable> for Symbol<Variable, T> {
        fn from(v: Variable) -> Self {
            Symbol::Variable(v)
        }
    }

    impl<V> From<Terminal> for Symbol<V, Terminal> {
        fn from(t: Terminal) -> Self {
            Symbol::Terminal(t)
        }
    }

    #[test]
    fn grammar() {
        Grammar::builder()
            .with_start_variable(Variable::Function)
            .with_rule(
                Variable::Function,
                [
                    Variable::Prototype.into(),
                    Terminal::Bracket.into(),
                    Variable::Body.into(),
                ],
            )
            .with_rule(Variable::Prototype, Vec::from([Terminal::Bracket.into()]))
            .with_rule(
                Variable::Body,
                [
                    Terminal::Identifier(String::new()).into(),
                    Terminal::Semi.into(),
                ],
            )
            .build();
    }

    #[test]
    #[should_panic(expected = "start variable not set")]
    fn no_start_variable() {
        Grammar::builder()
            .with_rule(
                Variable::Function,
                [
                    Terminal::Bracket.into(),
                    Terminal::Identifier(String::new()).into(),
                    Terminal::Semi.into(),
                ],
            )
            .build();
    }

    #[test]
    #[should_panic(expected = "variables should have a rule associated")]
    fn variable_with_no_rule() {
        Grammar::builder()
            .with_start_variable(Variable::Function)
            .with_rule(
                Variable::Function,
                [Variable::Body.into(), Terminal::Semi.into()],
            )
            .build();
    }

    #[test]
    #[should_panic(expected = "variables should have a rule associated")]
    fn start_variable_without_rule() {
        Grammar::<Variable, Terminal>::builder()
            .with_start_variable(Variable::Function)
            .build();
    }
}
