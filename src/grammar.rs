use std::collections::{HashMap, HashSet};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Symbol<V, T> {
    Terminal(T),
    Variable(V),
}

type Body<V, T> = Vec<Symbol<V, T>>;

type Rules<V, T> = HashMap<V, HashSet<Body<V, T>>>;

#[derive(Debug)]
pub struct Grammar<V, T>
where
    V: Copy + Eq + std::hash::Hash,
    T: Eq + std::hash::Hash,
{
    rules: Rules<V, T>,
}

impl<V, T> Grammar<V, T>
where
    V: Copy + Eq + std::hash::Hash,
    T: Eq + std::hash::Hash,
{
    pub fn builder() -> GrammarBuilder<V, T> {
        GrammarBuilder::new()
    }
}

pub struct GrammarBuilder<V, T>
where
    V: Copy + Eq + std::hash::Hash,
    T: Eq + std::hash::Hash,
{
    rules: Rules<V, T>,
}

impl<V, T> GrammarBuilder<V, T>
where
    V: Copy + Eq + std::hash::Hash,
    T: Eq + std::hash::Hash,
{
    fn new() -> Self {
        Self {
            rules: Rules::new(),
        }
    }

    pub fn with_rule(mut self, variable: V, body: Body<V, T>) -> Self {
        self.add_rule(variable, body);
        self
    }

    pub fn add_rule(&mut self, variable: V, body: Body<V, T>) -> bool {
        self.rules
            .entry(variable)
            .or_insert_with(HashSet::new)
            .insert(body.into())
    }

    pub fn build(self) -> Grammar<V, T> {
        // check whether all variables have a rule associated with them
        let mut variables = HashSet::new();
        let mut heads = HashSet::new();

        for (head, bodies) in self.rules.iter() {
            heads.insert(*head);

            variables.extend(
                bodies
                    .iter()
                    .flat_map(|body| body.iter())
                    .filter_map(|symbol| match symbol {
                        Symbol::Variable(v) => Some(*v),
                        Symbol::Terminal(_) => None,
                    }),
            )
        }

        if !variables.is_subset(&heads) {
            panic!("all variables should have a rule associated with them")
        }

        Grammar { rules: self.rules }
    }
}
