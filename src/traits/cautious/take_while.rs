//! Cautious take while implementation making use of [`std::iter::Peekable`].
//! Inspired by https://stackoverflow.com/questions/28776630/implementing-a-cautious-take-while-using-peekable

use std::iter::Peekable;

pub struct CautiousTakeWhile<'a, I: Iterator, P>
where
    P: Fn(&I::Item) -> bool,
{
    inner: &'a mut Peekable<I>,
    predicate: P,
}

pub trait CautiousTakeWhileable<I: Iterator, P>
where
    P: Fn(&I::Item) -> bool,
{
    fn cautious_take_while(&mut self, predicate: P) -> CautiousTakeWhile<I, P>;
}

impl<I: Iterator, P> Iterator for CautiousTakeWhile<'_, I, P>
where
    P: Fn(&I::Item) -> bool,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let return_next = match self.inner.peek() {
            Some(next) => (self.predicate)(next),
            None => false,
        };

        if return_next {
            self.inner.next()
        } else {
            None
        }
    }
}

impl<I: Iterator, P> CautiousTakeWhileable<I, P> for &mut Peekable<I>
where
    P: Fn(&I::Item) -> bool,
{
    fn cautious_take_while(&mut self, predicate: P) -> CautiousTakeWhile<I, P> {
        CautiousTakeWhile {
            inner: self,
            predicate,
        }
    }
}
