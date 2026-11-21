//! HDK-side shim onto the algorithm-crate `Workspace`.
//!
//! Step 13b-C phase 2 (wake-15): the substrate-agnostic
//! `perspective_diff_algorithm::Workspace` is now the only Workspace
//! implementation. This module exists purely so legacy import paths
//! (`crate::link_adapter::workspace::{Workspace, NULL_NODE}`) keep
//! working — the actual algorithm + tests live in the algorithm crate.
//!
//! `NULL_NODE()` here returns the HDK-typed `ActionHash` form of the
//! sentinel (`from_raw_36(vec![0xdb; 36])`). Algorithm-crate code uses
//! `perspective_diff_algorithm::null_node()` which returns the mirror
//! `algo::Hash`. Both encode the same 36-byte payload.

use hdk::prelude::ActionHash;

pub use perspective_diff_algorithm::Workspace;

#[allow(non_snake_case)]
pub fn NULL_NODE() -> ActionHash {
    ActionHash::from_raw_36(vec![0xdb; 36])
}
