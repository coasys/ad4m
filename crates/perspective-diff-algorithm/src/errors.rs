//! Algorithm-crate error type.
//!
//! p-diff-sync has its own HDK-flavored `SocialContextError`. The
//! algorithm crate needs a smaller error type that adapter code can
//! convert into whatever the host-side error is. p-diff-sync's
//! `SocialContextError::from(AlgoError)` impl handles the conversion;
//! the holograph runtime would do the same for whatever host-side
//! error it uses.

use thiserror::Error;

#[derive(Debug, Error)]
pub enum AlgoError {
    #[error("retriever error: {0}")]
    Retriever(String),
    #[error("no common ancestor found")]
    NoCommonAncestorFound,
    #[error("internal algorithm error: {0}")]
    Internal(&'static str),
    #[error("topo-sort error: {0}")]
    TopoSort(#[from] crate::topo_sort::TopoSortError),
}

pub type AlgoResult<T> = Result<T, AlgoError>;
