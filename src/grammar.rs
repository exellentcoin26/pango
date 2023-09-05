use crate::{Body, Symbol};

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    hash::Hash,
};

// TODO: Convert runtime errors with start variable to type-state builder
// pattern.

/// Represents a [parsing expression grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar).
#[derive(Debug, Clone)]
pub struct Grammar<V, T> {
    /// Start variable of the [`Grammar`].
    start_variable: V,
    /// Mapping of `Variables` to a set of [`Bodies`](Body).
    rules: Rules<V, T>,
}

/// Map of `Variables` to a set of [`Bodies`](Body).
type Rules<V, T> = HashMap<V, HashSet<Body<V, T>>>;

impl<V, T> Grammar<V, T> {
    /// Creates a new `GrammarBuilder`.
    pub fn builder() -> GrammarBuilder<V, T> {
        GrammarBuilder::new()
    }

    pub(crate) fn iter_variables(&self) -> impl Iterator<Item = &V> {
        self.rules.keys()
    }
}

impl<V, T> Grammar<V, T>
where
    T: Eq + Hash,
{
    pub(crate) fn iter_terminals(&self) -> impl Iterator<Item = &T> {
        let mut terminals = HashSet::new();

        self.rules.values().for_each(|item_body_set| {
            item_body_set
                .iter()
                .flat_map(|b| {
                    b.iter().filter_map(|s| match s {
                        Symbol::Terminal(t) => Some(t),
                        _ => None,
                    })
                })
                .for_each(|t| {
                    terminals.insert(t);
                });
        });

        terminals.into_iter()
    }
}

impl<V, T> Grammar<V, T>
where
    V: Copy + Eq + Hash,
{
    /// Returns a set of [`Bodies`](Body) the `Variable` maps to.
    pub(super) fn get_rule_bodies(&self, head: V) -> Option<&HashSet<Body<V, T>>> {
        self.rules.get(&head)
    }
}

impl<V, T> Grammar<V, T>
where
    V: Eq + Hash,
    Symbol<V, T>: Eq + Hash,
{
    /// Returns a set of [`Bodies`](Body) associated with the start variable of
    /// the [`Grammar`].
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

/// Builder struct for the [`Grammar`].
pub struct GrammarBuilder<V, T> {
    /// Start variable of the [`Grammar`].
    start_variable: Option<V>,
    /// Mapping of `Variables` to a set of [`Bodies`](Body).
    rules: Rules<V, T>,
}

impl<V, T> GrammarBuilder<V, T> {
    /// Creates a new [`GrammarBuilder`] with no `start_variable` and no
    /// associated [`Rules`].
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
    Symbol<V, T>: Eq + Hash,
{
    /// Sets the `start_variable` of the [`Grammar`].
    pub fn with_start_variable(mut self, variable: V) -> Self {
        self.set_start_variable(variable);
        self
    }

    /// Adds a [`Rule`](Rules) to the [`Grammar`].
    pub fn with_rule(mut self, variable: V, body: impl Into<Body<V, T>>) -> Self {
        self.add_rule(variable, body);
        self
    }

    /// Sets the `start_variable` of the [`Grammar`].
    pub fn set_start_variable(&mut self, variable: V) {
        self.start_variable = Some(variable);
    }

    /// Adds a [`Rule`](Rules) to the [`Grammar`].
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
    V: Copy + Eq + Hash,
    Symbol<V, T>: Eq + Hash,
{
    /// Adds a set of [`Bodies`](Body) associated with a `Variable` to the grammar.
    pub fn with_rules<B>(mut self, variable: V, bodies: impl IntoIterator<Item = B>) -> Self
    where
        B: Into<Body<V, T>>,
    {
        self.add_rules(variable, bodies);
        self
    }

    /// Adds a set of [`Bodies`](Body) associated with a `Variable` to the grammar.
    pub fn add_rules<B>(&mut self, variable: V, bodies: impl IntoIterator<Item = B>)
    where
        B: Into<Body<V, T>>,
    {
        bodies.into_iter().for_each(|b| {
            let _ = self.add_rule(variable, b.into());
        });
    }
}

impl<V, T> GrammarBuilder<V, T>
where
    V: Eq + Hash,
{
    /// Builds the [`Grammar`] and does runtime checks (used variables have at least one
    /// associated rule, a start variable is set).
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

    #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
    enum Variable {
        Function,
        Body,
        Prototype,
    }

    #[derive(Debug, Hash, PartialEq, Eq)]
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
    fn multi_rule() {
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
            .with_rules(
                Variable::Body,
                [
                    [
                        Terminal::Identifier(String::new()).into(),
                        Terminal::Semi.into(),
                    ],
                    [Terminal::Bracket.into(), Terminal::Bracket.into()],
                ],
            )
            .build();
    }

    #[test]
    fn empty_body() {
        let grammar = Grammar::<Variable, Terminal>::builder()
            .with_start_variable(Variable::Function)
            .with_rule(Variable::Function, [])
            .build();

        assert_eq!(
            grammar.rules[&Variable::Function].iter().next().unwrap(),
            &vec![Symbol::Epsilon]
        )
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
