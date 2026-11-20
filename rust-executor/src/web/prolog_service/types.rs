//! Query answers, as on native (`src/prolog_service/types.rs`), over the stand-in scryer types.

use scryer_prolog::Term;

#[derive(Clone, Debug, PartialEq)]
pub struct QueryMatch {
    pub bindings: std::collections::BTreeMap<String, Term>,
}

#[derive(Debug, PartialEq)]
pub enum QueryResolution {
    True,
    False,
    Matches(Vec<QueryMatch>),
}

pub type QueryResult = Result<QueryResolution, String>;
