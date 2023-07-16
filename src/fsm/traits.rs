pub(super) trait Simulateable {
    /// Simulates the finite-state machine from start to finish and returns whether it accepts the
    /// input.
    fn run(mut self, input: &str) -> bool
    where
        Self: Sized,
    {
        let accept = self.start_closure();
        input.chars().map(|c| self.feed(c)).last().unwrap_or(accept)
    }

    /// Returns whether the epsilon closure of the start state has an accepting state.
    fn start_closure(&self) -> bool;

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
            .expect("cannot feed an empty string")
    }
}
