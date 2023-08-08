use super::StateId;

use std::collections::BTreeSet;

pub trait Simulate {
    /// Returns whether the finite-state machine accepts when no input is given.
    fn is_accepting(&self) -> bool;

    /// Feeds a single character to the finite-state machine and returns whether it has reached an
    /// accepting state.
    fn feed(&mut self, input: char) -> bool;

    /// Checks whether the character can be fed to the finite-state machine.
    fn can_feed(&self, input: char) -> bool;

    /// Feeds an entire string to the finite-state machine at once and returns whether it has
    /// reached an accepting state.
    fn feed_str(&mut self, input: &str) -> bool {
        input
            .chars()
            .map(|c| self.feed(c))
            .last()
            .unwrap_or(self.is_accepting())
    }
}

pub(crate) trait NDSimulate {
    /// Get the final states the simulator is currently in.
    fn get_current_final_states(&self) -> BTreeSet<StateId>;
}

pub trait Simulatable {
    type Simulator<'a>: Simulate
    where
        Self: 'a;

    /// Simulate the finite-state machine on the input and return whether it accepts.
    fn simulate(&self, input: &str) -> bool;

    /// Create a simulator from the finite-state machine.
    fn to_simulator(&self) -> Self::Simulator<'_>;
}
