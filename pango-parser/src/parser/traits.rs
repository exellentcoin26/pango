use std::hash::Hasher;

pub trait TerminalEq {
    fn terminal_eq(&self, other: &Self) -> bool;
}

pub trait TerminalHash {
    fn terminal_hash<H>(&self, state: &mut H)
    where
        H: Hasher;
}
