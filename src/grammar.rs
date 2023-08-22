use crate::{Body, Symbol};

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    thread::panicking,
};

#[derive(Debug)]
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
    V: Eq + Hash,
    T: Eq + Hash,
{
    pub(crate) fn get_start_variable_rules(&self) -> (&V, &HashSet<Body<V, T>>) {
        self.rules
            .get_key_value(&self.start_variable)
            .expect("start variable should have an associated rule")
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

    pub fn with_rule(mut self, variable: V, body: Body<V, T>) -> Self {
        self.add_rule(variable, body);
        self
    }

    pub fn set_start_variable(&mut self, variable: V) {
        self.start_variable = Some(variable);
    }

    pub fn add_rule(&mut self, variable: V, body: Body<V, T>) -> bool {
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
                    }),
            )
        }

        if !variables.is_subset(&heads) {
            panic!("all variables should have a rule associated with them");
        }

        let start_variable = match self.start_variable {
            Some(start_variable) => {
                if !heads.contains(&start_variable) {
                    panic!("start variable does not have an associated rule");
                }
                start_variable
            }
            None => panic!("start variable not set"),
        };

        Grammar {
            start_variable,
            rules: self.rules,
        }
    }
}
