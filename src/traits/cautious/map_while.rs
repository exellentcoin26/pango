//! Cautious map while implementation making use of [`std::iter::Peekable`].
//! Inspired by https://stackoverflow.com/questions/28776630/implementing-a-cautious-take-while-using-peekable

use std::iter::Peekable;

pub struct CautiousMapWhile<'a, I: Iterator, P, T>
where
    P: Fn(&I::Item) -> Option<T>,
{
    inner: &'a mut Peekable<I>,
    predicate: P,
}

pub trait CautiousMapWhileable<I: Iterator, P, T>
where
    P: Fn(&I::Item) -> Option<T>,
{
    fn cautious_map_while(&mut self, predicate: P) -> CautiousMapWhile<I, P, T>;
}

impl<I: Iterator, P, T> Iterator for CautiousMapWhile<'_, I, P, T>
where
    P: Fn(&I::Item) -> Option<T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.peek() {
            Some(next) => (self.predicate)(next),
            None => None,
        }
    }
}

impl<I: Iterator, P, T> CautiousMapWhileable<I, P, T> for &mut Peekable<I>
where
    P: Fn(&I::Item) -> Option<T>,
{
    fn cautious_map_while(&mut self, predicate: P) -> CautiousMapWhile<I, P, T> {
        CautiousMapWhile {
            inner: self,
            predicate,
        }
    }
}
