extern crate vec_map;
extern crate bit_set;

pub use regex::Regex;
pub use dfa::{State, Dfa};

/// The Regular Expression (regex) module.
pub mod regex;
/// Deterministic Finite Automata (dfa) module.
pub mod dfa;
/// A module for computing derivatives based on
/// "Regular-expression derivatives reexamined" by Owens et al.
pub mod derivatives;
/// A module which represents Sets as iterators over
/// iterators of ordered non-duplicate elements.
pub mod iter_set;

#[cfg(test)] mod tests;
