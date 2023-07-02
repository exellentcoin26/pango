//! Cautious map while implementation making use of [`std::iter::Peekable`].
//! Inspired by https://stackoverflow.com/questions/28776630/implementing-a-cautious-take-while-using-peekable

use crate::iter::Peekableable;

pub struct CautiousMapWhile<'a, P, Pr, T>
where
    P: Peekableable,
    Pr: Fn(&P::Item) -> Option<T>,
{
    inner: &'a mut P,
    predicate: Pr,
}

pub trait CautiousMapWhileable<P: Peekableable, Pr, T>
where
    Pr: Fn(&P::Item) -> Option<T>,
{
    fn cautious_map_while(&mut self, predicate: Pr) -> CautiousMapWhile<P, Pr, T>;
}

impl<P: Peekableable, Pr, T> Iterator for CautiousMapWhile<'_, P, Pr, T>
where
    Pr: Fn(&P::Item) -> Option<T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.peek() {
            Some(next) => {
                let result = (self.predicate)(next);

                if result.is_some() {
                    self.inner.next();
                }

                result
            }
            None => None,
        }
    }
}

impl<P: Peekableable, Pr, T> CautiousMapWhileable<P, Pr, T> for &mut P
where
    Pr: Fn(&P::Item) -> Option<T>,
{
    fn cautious_map_while(&mut self, predicate: Pr) -> CautiousMapWhile<P, Pr, T> {
        CautiousMapWhile {
            inner: self,
            predicate,
        }
    }
}
