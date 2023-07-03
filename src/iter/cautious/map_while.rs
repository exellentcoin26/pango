//! Cautious map while implementation making use of [`std::iter::Peekable`].
//! Inspired by https://stackoverflow.com/questions/28776630/implementing-a-cautious-take-while-using-peekable

use crate::iter::Peekableable;

pub struct CautiousMapWhile<'a, I, P, T>
where
    I: Peekableable,
    P: Fn(&I::Item) -> Option<T>,
{
    iter: &'a mut I,
    predicate: P,
}

pub trait CautiousMapWhileable<I, P, T>
where
    I: Peekableable,
    P: Fn(&I::Item) -> Option<T>,
{
    fn cautious_map_while(&mut self, predicate: P) -> CautiousMapWhile<I, P, T>;
}

impl<I, P, T> Iterator for CautiousMapWhile<'_, I, P, T>
where
    I: Peekableable,
    P: Fn(&I::Item) -> Option<T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.peek() {
            Some(next) => {
                let result = (self.predicate)(next);

                if result.is_some() {
                    self.iter.next();
                }

                result
            }
            None => None,
        }
    }
}

impl<I, P, T> CautiousMapWhileable<I, P, T> for &mut I
where
    I: Peekableable,
    P: Fn(&I::Item) -> Option<T>,
{
    fn cautious_map_while(&mut self, predicate: P) -> CautiousMapWhile<I, P, T> {
        CautiousMapWhile {
            iter: self,
            predicate,
        }
    }
}
