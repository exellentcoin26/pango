pub(crate) trait Simulate {
    /// Simulates the finite-state machine from start to finish and returns whether it accepts the
    /// input.
    fn run(mut self, input: &str) -> bool
    where
        Self: Sized,
    {
        let accept = self.is_accepting();
        input.chars().map(|c| self.feed(c)).last().unwrap_or(accept)
    }

    /// Returns whether the finite-state machine accepts when no input is given.
    fn is_accepting(&self) -> bool;

    /// Feeds a single character to the finite-state machine and returns whether it has reached an
    /// accepting state.
    fn feed(&mut self, input: char) -> bool;

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

pub(crate) trait Simulatable {
    type Simulator<'a>
    where
        Self: 'a;

    /// Simulate the finite-state machine on the input and return whether it accepts.
    fn simulate(&self, input: &str) -> bool;

    /// Create a simulator from the finite-state machine.
    fn to_simulator<'a>(&'a self) -> Self::Simulator<'a>;
}
