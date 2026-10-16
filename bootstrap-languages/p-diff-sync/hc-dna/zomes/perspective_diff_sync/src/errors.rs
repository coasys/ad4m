use hdk::prelude::*;
use holo_hash::HoloHashError;
use perspective_diff_algorithm::AlgoError;
use std::convert::Infallible;

#[derive(thiserror::Error, Debug)]
pub enum SocialContextError {
    #[error(transparent)]
    Serialization(#[from] SerializedBytesError),
    #[error(transparent)]
    Infallible(#[from] Infallible),
    #[error(transparent)]
    EntryError(#[from] EntryError),
    #[error(transparent)]
    Wasm(#[from] WasmError),
    #[error(transparent)]
    HoloHashError(#[from] HoloHashError),
    #[error("Internal Error. Error: {0}")]
    InternalError(&'static str),
    #[error("No common ancestor found")]
    NoCommonAncestorFound,
    #[error("No did found")]
    NoDidFound,
    #[error("Algorithm error: {0}")]
    Algo(String),
}

// Step 13b-C phase 2 (wake-15): bridge algorithm-crate errors into the
// p-diff-sync error type so `?` works at workspace call sites. The
// algorithm crate also has a `NoCommonAncestorFound` variant; surface it
// distinctly so existing `match`es on `NoCommonAncestorFound` keep firing.
impl From<AlgoError> for SocialContextError {
    fn from(e: AlgoError) -> Self {
        match e {
            AlgoError::NoCommonAncestorFound => SocialContextError::NoCommonAncestorFound,
            other => SocialContextError::Algo(format!("{}", other)),
        }
    }
}

pub type SocialContextResult<T> = Result<T, SocialContextError>;
