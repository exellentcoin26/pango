pub(crate) use self::{
    nfa::{compiler::Compiler as NfaCompiler, model::Nfa},
    traits::{NDSimulate, Simulatable, Simulate},
};

pub(crate) type StateId = usize;

mod nfa;
mod traits;
