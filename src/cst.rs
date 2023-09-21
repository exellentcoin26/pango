use crate::Symbol;

#[derive(Debug)]
pub struct Cst<V, T> {
    pub(crate) root: Node<V, T>,
}

#[derive(Debug)]
pub(crate) struct Node<V, T> {
    pub(crate) symbol: Symbol<V, T>,
    pub(crate) children: Vec<Node<V, T>>,
}

impl<V, T> Node<V, T> {
    pub(crate) fn from_symbol(symbol: Symbol<V, T>) -> Self {
        Self {
            symbol,
            children: Vec::new(),
        }
    }
}
