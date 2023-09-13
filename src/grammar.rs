use crate::{Body, Symbol};

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet, VecDeque},
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
        self.rules
            .values()
            .fold(HashSet::new(), |mut terminals, item_body_set| {
                terminals.extend(item_body_set.iter().flat_map(|b| {
                    b.iter().filter_map(|s| match s {
                        Symbol::Terminal(t) => Some(t),
                        _ => None,
                    })
                }));
                terminals
            })
            .into_iter()
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

mod body_iter {
    use dyn_clone::{clone_trait_object, DynClone};

    use super::Symbol;

    // NOTE: This trait is needed because rust does not allow implementing traits for non-local
    // types (e.g., `Box<dyn Iterator<Item = _>>`).
    pub(super) trait BodyIter<'a, V: 'a, T: 'a>:
        Iterator<Item = &'a Symbol<V, T>> + 'a + DynClone
    {
    }

    impl<'a, V: 'a, T: 'a, I> BodyIter<'a, V, T> for I where
        I: Iterator<Item = &'a Symbol<V, T>> + Clone + 'a
    {
    }

    clone_trait_object!(<'a, V: 'a, T: 'a> BodyIter<'a, V, T>);
}

impl<V, T> Grammar<V, T>
where
    V: Copy + Eq + Hash,
    T: Eq + Hash,
{
    fn first(&self, variable: V) -> HashSet<&Symbol<V, T>> {
        self.first_bodies(
            Some(variable),
            self.get_rule_bodies(variable)
                .expect("variable does not exist"),
        )
    }

    fn first_body<'a, B>(&'a self, variable: Option<V>, body: B) -> HashSet<&'a Symbol<V, T>>
    where
        B: IntoIterator<Item = &'a Symbol<V, T>>,
        B::IntoIter: body_iter::BodyIter<'a, V, T> + Clone,
    {
        self.first_bodies(variable, std::iter::once(body))
    }

    fn first_bodies<'a, Bs, B>(
        &'a self,
        variable: Option<V>,
        bodies: Bs,
    ) -> HashSet<&'a Symbol<V, T>>
    where
        Bs: IntoIterator<Item = B>,
        B: IntoIterator<Item = &'a Symbol<V, T>>,
        B::IntoIter: body_iter::BodyIter<'a, V, T> + Clone,
    {
        use body_iter::BodyIter;

        type PendingQueue<'a, V, T> = VecDeque<(Option<V>, Vec<Box<dyn BodyIter<'a, V, T>>>)>;
        type DependencyMap<'a, V, T> =
            HashMap<V, HashMap<Option<V>, Vec<Box<dyn BodyIter<'a, V, T>>>>>;

        let mut result = HashSet::new();
        let mut first = HashMap::new();
        let mut pending_bodies = VecDeque::from([(
            variable,
            bodies
                .into_iter()
                .map(|b| Box::new(b.into_iter()) as Box<dyn BodyIter<'a, V, T>>)
                .collect::<Vec<_>>(),
        )]);
        let mut dependencies = HashMap::new();
        let mut visited_variables = HashSet::new();

        fn extend_for_variable<'a, V, T>(
            variable: Option<V>,
            symbols: impl IntoIterator<Item = &'a Symbol<V, T>>,
            pending_queue: &mut PendingQueue<'a, V, T>,
            result: &mut HashSet<&'a Symbol<V, T>>,
            first: &mut HashMap<V, HashSet<&'a Symbol<V, T>>>,
            dependencies: &mut DependencyMap<'a, V, T>,
        ) where
            V: Copy + Eq + Hash,
            Symbol<V, T>: Eq + Hash,
        {
            let result_len = result.len();

            let changed = (match variable {
                Some(v) => {
                    let entry = first.entry(v).or_default();
                    let first_len = entry.len();
                    entry.extend(symbols);
                    first_len != entry.len()
                }
                None => {
                    result.extend(symbols);
                    false
                }
            }) || result_len != result.len();

            if changed {
                if let Some(dependencies) = variable.and_then(|v| dependencies.remove(&v)) {
                    pending_queue.extend(dependencies);
                }
            }
        }

        while let Some((variable, bodies)) = pending_bodies.pop_front() {
            if let Some(variable) = variable {
                visited_variables.insert(variable);
            }
            for body in bodies {
                let mut last_body = body.clone();
                let mut body = body.peekable();
                while let Some(symbol) = body.next() {
                    match symbol {
                        t @ Symbol::Terminal(_) => {
                            extend_for_variable(
                                variable,
                                std::iter::once(t),
                                &mut pending_bodies,
                                &mut result,
                                &mut first,
                                &mut dependencies,
                            );

                            break;
                        }
                        Symbol::Variable(v) => {
                            // make ourselves a dependency
                            //
                            // NOTE: Even if we depend on ourselves this still works, a dependency
                            // is *NOT* from a variable to a variable, rather it is from a variable
                            // to a rule set associated with another variable. Meaning changing the
                            // first set of ourselves, could require other rules, in which we
                            // depend on ourselves, to be revisited.
                            dependencies
                                .entry(*v)
                                .or_insert_with(HashMap::new)
                                .entry(variable)
                                .or_insert_with(Vec::new)
                                .push(last_body.clone());

                            if !visited_variables.contains(v) {
                                // make the bodies of the variable pending
                                pending_bodies.push_back((
                                    Some(*v),
                                    self.get_rule_bodies(*v)
                                        .expect("variable does not exist")
                                        .iter()
                                        .map(|b| Box::new(b.iter()) as Box<dyn BodyIter<V, T>>)
                                        .collect(),
                                ));

                                break;
                            }

                            let Some(first_v) = first.get(v).cloned() else {
                                // the first set of v is empty
                                break;
                            };

                            let last_symbol_in_body = body.peek().is_none();
                            let first_v_contains_eps = first_v.contains(&Symbol::Epsilon);

                            extend_for_variable(
                                variable,
                                first_v.into_iter().filter(|s| {
                                    // only keep epsilon if v was the last variable in the body
                                    !matches!(s, Symbol::Epsilon) || last_symbol_in_body
                                }),
                                &mut pending_bodies,
                                &mut result,
                                &mut first,
                                &mut dependencies,
                            );

                            if !first_v_contains_eps {
                                // only continue iterating the body if the the first of v contains
                                // epsilon

                                break;
                            }
                        }
                        Symbol::Epsilon if body.peek().is_none() => {
                            // only insert epsilon(s) if it is the only symbol in the body
                            extend_for_variable(
                                variable,
                                std::iter::once(&Symbol::Epsilon),
                                &mut pending_bodies,
                                &mut result,
                                &mut first,
                                &mut dependencies,
                            );

                            // next is none so break is more efficient
                            break;
                        }
                        _ => (),
                    }
                    last_body.next();
                }
            }
        }

        match variable {
            Some(variable) => {
                // first set of variable has been stored in `first`, not `result`
                first.remove(&variable).unwrap_or_default()
            }
            None => result,
        }
    }

    pub(super) fn follow_set(&self) -> HashMap<V, HashSet<&Symbol<V, T>>> {
        let mut result: HashMap<V, HashSet<&Symbol<V, T>>> = HashMap::new();
        // map of dependencies (values are dependent on keys)
        let mut follow_dependencies: HashMap<V, HashSet<V>> = HashMap::new();

        for (head, body) in self
            .rules
            .iter()
            .flat_map(|(head, bodies)| std::iter::repeat(*head).zip(bodies.iter()))
        {
            let mut iter = body.iter().peekable();

            while let Some(Symbol::Variable(variable)) =
                iter.find(|s| matches!(s, Symbol::Variable(_)))
            {
                let variable = *variable;
                let entry = result.entry(variable);

                match iter.peek() {
                    Some(_) => {
                        // calculate the first of the remaining body slice

                        let first = self.first_body(None, iter.clone());

                        if first.contains(&Symbol::Epsilon) {
                            // keep track of the dependency
                            follow_dependencies
                                .entry(head)
                                .or_default()
                                .insert(variable);
                        }

                        entry
                            .or_default()
                            .extend(first.into_iter().filter(|s| !matches!(s, Symbol::Epsilon)))
                    }
                    None => {
                        // keep track of the dependency
                        follow_dependencies
                            .entry(head)
                            .or_default()
                            .insert(variable);
                    }
                }
            }
        }

        // keep iterating dependencies until the sets do not change anymore
        // follow[lhs] = follow[rhs]
        // the dependency is from rhs -> lhs (if rhs changes, lhs needs to be updated)
        let mut follow_dependencies_queue = VecDeque::from_iter(
            follow_dependencies
                .iter()
                .flat_map(|(rhs, lhs)| lhs.iter().cloned().zip(std::iter::repeat(*rhs))),
        );

        while let Some((lhs, rhs)) = follow_dependencies_queue.pop_front() {
            // insert follow[rhs] into follow[lhs]

            fn extend_set<'a, V, T>(
                set: &mut HashSet<&'a Symbol<V, T>>,
                iter: impl Iterator<Item = &'a Symbol<V, T>>,
            ) -> bool
            where
                Symbol<V, T>: Eq + Hash,
            {
                let len = set.len();
                set.extend(iter);
                len != set.len()
            }

            if let Some(rhs_follow) = result.get(&rhs).cloned() {
                if extend_set(
                    result.entry(lhs).or_default(),
                    rhs_follow.clone().into_iter(),
                ) {
                    // retry the dependencies on lhs
                    if let Some(deps) = follow_dependencies.get(&lhs).cloned() {
                        follow_dependencies_queue
                            .extend(deps.into_iter().zip(std::iter::repeat(lhs)));
                    }
                }
            }
        }

        result
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

    mod grammar_construction {
        use super::{Grammar, Symbol, Terminal, Variable};

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

        mod first_follow {}

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

    mod first_follow {
        use std::collections::{HashMap, HashSet};

        use super::{Grammar, Symbol, Terminal, Variable};

        #[test]
        fn first() {
            let grammar = Grammar::builder()
                .with_start_variable(Variable::Function)
                .with_rule(
                    Variable::Function,
                    [
                        Variable::Prototype.into(),
                        // Terminal::Bracket.into(),
                        Variable::Body.into(),
                    ],
                )
                .with_rules(
                    Variable::Prototype,
                    [vec![Terminal::Bracket.into()], vec![]],
                )
                .with_rules(
                    Variable::Body,
                    [
                        vec![
                            Terminal::Identifier(String::new()).into(),
                            Terminal::Semi.into(),
                        ],
                        vec![Terminal::Semi.into()],
                    ],
                )
                .build();

            assert_eq!(
                grammar.first(Variable::Function),
                HashSet::from([
                    &Terminal::Identifier(String::new()).into(),
                    &Terminal::Semi.into(),
                    &Terminal::Bracket.into()
                ])
            );
            assert_eq!(
                grammar.first(Variable::Body),
                HashSet::from([
                    &Terminal::Identifier(String::new()).into(),
                    &Terminal::Semi.into()
                ])
            );
            assert_eq!(
                grammar.first(Variable::Prototype),
                HashSet::from([&Terminal::Bracket.into(), &Symbol::Epsilon])
            );
        }

        #[test]
        fn first_recursive() {
            use Variable::*;
            let grammar = Grammar::<_, Terminal>::builder()
                .with_start_variable(Function)
                .with_rule(Body, [Function.into()])
                .with_rule(Function, [Prototype.into()])
                .with_rules(Prototype, [vec![Body.into()], vec![]])
                .build();

            assert_eq!(grammar.first(Body), HashSet::from([&Symbol::Epsilon]));
            assert_eq!(grammar.first(Function), HashSet::from([&Symbol::Epsilon]));
            assert_eq!(grammar.first(Prototype), HashSet::from([&Symbol::Epsilon]));
        }

        #[test]
        fn first_recursive_self() {
            use self::{Terminal::*, Variable::*};

            let grammar = Grammar::builder()
                .with_start_variable(Function)
                .with_rules(
                    Function,
                    [
                        vec![
                            Function.into(),
                            Symbol::Epsilon,
                            Body.into(),
                            Bracket.into(),
                        ],
                        vec![Prototype.into()],
                    ],
                )
                .with_rule(Prototype, [Symbol::Epsilon])
                .with_rule(Body, [Symbol::Epsilon])
                .build();

            assert_eq!(
                grammar.first(Function),
                HashSet::from([&Bracket.into(), &Symbol::Epsilon])
            );
        }

        #[test]
        fn follow() {
            #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
            enum V {
                S,
                B,
                C,
                D,
                E,
                F,
            }
            #[allow(non_camel_case_types)]
            #[derive(Debug, PartialEq, Eq, Hash)]
            enum T {
                a,
                b,
                c,
                f,
                g,
                h,
            }

            impl<T> From<V> for Symbol<V, T> {
                fn from(value: V) -> Self {
                    Self::Variable(value)
                }
            }
            impl<V> From<T> for Symbol<V, T> {
                fn from(value: T) -> Self {
                    Self::Terminal(value)
                }
            }

            use {T::*, V::*};

            let grammar = Grammar::builder()
                .with_start_variable(S)
                .with_rule(S, [a.into(), B.into(), D.into(), h.into()])
                .with_rule(B, [c.into(), C.into()])
                .with_rules(C, [vec![b.into(), C.into()], vec![]])
                .with_rule(D, [E.into(), F.into()])
                .with_rules(E, [vec![g.into()], vec![]])
                .with_rules(F, [vec![f.into()], vec![]])
                .build();

            println!("{:#?}", grammar.follow_set());
            assert_eq!(
                grammar.follow_set(),
                HashMap::from([
                    (B, HashSet::from([&g.into(), &f.into(), &h.into()])),
                    (C, HashSet::from([&g.into(), &f.into(), &h.into()])),
                    (D, HashSet::from([&h.into()])),
                    (E, HashSet::from([&f.into(), &h.into()])),
                    (F, HashSet::from([&h.into()])),
                ])
            )
        }
    }
}
