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

#[cfg(test)]
mod tests {
    use super::CautiousTakeWhileable;

    #[test]
    fn cautious_take_while() {
        let mut it = "foobar!".chars().peekable();

        let first = it
            .by_ref()
            .cautious_take_while(|i| *i != 'b')
            .collect::<Vec<_>>();
        assert_eq!(first.len(), 3);
        assert_eq!(first.last(), Some(&'o'));

        assert_eq!(it.next(), Some('b'));
        assert_eq!(
            it.by_ref()
                .cautious_take_while(|n| *n != '\n')
                .collect::<Vec<_>>()
                .len(),
            3
        );
    }
}
