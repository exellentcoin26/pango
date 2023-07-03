//! Cautious take while implementation making use of [`std::iter::Peekable`].
//! Inspired by https://stackoverflow.com/questions/28776630/implementing-a-cautious-take-while-using-peekable

use crate::iter::Peekableable;

pub struct CautiousTakeWhile<'a, I, P>
where
    I: Peekableable,
    P: Fn(&I::Item) -> bool,
{
    iter: &'a mut I,
    predicate: P,
}

pub trait CautiousTakeWhileable<I, P>
where
    I: Peekableable,
    P: Fn(&I::Item) -> bool,
{
    fn cautious_take_while(&mut self, predicate: P) -> CautiousTakeWhile<I, P>;
}

impl<I, P> Iterator for CautiousTakeWhile<'_, I, P>
where
    I: Peekableable,
    P: Fn(&I::Item) -> bool,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let return_next = match self.iter.peek() {
            Some(next) => (self.predicate)(next),
            None => false,
        };

        if return_next {
            self.iter.next()
        } else {
            None
        }
    }
}

impl<I, P> CautiousTakeWhileable<I, P> for &mut I
where
    I: Peekableable,
    P: Fn(&I::Item) -> bool,
{
    fn cautious_take_while(&mut self, predicate: P) -> CautiousTakeWhile<I, P> {
        CautiousTakeWhile {
            iter: self,
            predicate,
        }
    }
}
