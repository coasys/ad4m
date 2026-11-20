//! The two scryer-prolog answer types ring 2 names (`perspectives/utils.rs`,
//! `perspectives/perspective_instance.rs`), for a build without scryer-prolog. Same shape as
//! scryer's `lib_machine` (`Term`, `LeafAnswer`). `lib_wasm.rs` makes them reachable as
//! `scryer_prolog::Term` and `scryer_prolog::LeafAnswer`, so ring 2 compiles unchanged.
//!
//! Prolog never runs in the web executor, so no value of these types is ever made by an
//! engine; they only keep the marshalling code (`perspectives/utils.rs`) compiling.

use std::collections::BTreeMap;

/// scryer's arbitrary precision integer. `i128` prints the same way (`Display`).
pub type Integer = i128;
/// scryer's arbitrary precision rational. Only printed (`Display`) by ring 2.
pub type Rational = f64;

/// A Prolog term.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Integer(Integer),
    Rational(Rational),
    Float(f64),
    Atom(String),
    String(String),
    List(Vec<Term>),
    Compound(String, Vec<Term>),
    Var(String),
}

/// A leaf answer from a query.
#[derive(Debug, Clone, PartialEq)]
pub enum LeafAnswer {
    True,
    False,
    Exception(Term),
    #[non_exhaustive]
    LeafAnswer {
        bindings: BTreeMap<String, Term>,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn ring_2_reaches_the_types_by_their_scryer_path() {
        let term: scryer_prolog::Term = Term::Integer(42);
        assert_eq!(term.to_owned(), Term::Integer(42));
        let answer = scryer_prolog::LeafAnswer::LeafAnswer {
            bindings: BTreeMap::from([("X".to_string(), Term::Atom("a".to_string()))]),
        };
        assert!(matches!(answer, LeafAnswer::LeafAnswer { .. }));
    }
}
