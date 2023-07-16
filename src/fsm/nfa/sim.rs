use super::{super::traits::Simulateable, model::Nfa};

struct NfaSimulator<'a> {
    nfa: &'a Nfa,
}

impl<'a> NfaSimulator<'a> {
    fn new(nfa: &'a Nfa) -> Self {
        Self { nfa }
    }
}

impl Simulateable for NfaSimulator<'_> {
    fn start_closure(&self) -> bool {
        todo!()
    }

    fn feed(&mut self, input: char) -> bool {
        todo!()
    }

    fn feed_str(&mut self, input: &str) -> bool {
        todo!()
    }
}
