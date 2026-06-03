//! Substrate-agnostic mirrors of the p-diff-sync integrity-zome wire types.
//!
//! These types are byte-for-byte compatible with their counterparts in
//! `perspective_diff_sync_integrity` (same serde shape), but live in the
//! algorithm crate so the DAG-walk modules can manipulate them without
//! dragging in HDK / HDI / `holo_hash` / `SerializedBytes`.
//!
//! p-diff-sync provides `From<integrity::T>` / `Into<integrity::T>`
//! conversions at the HDK boundary. The algorithm operates on these
//! pure-serde types internally.
//!
//! Step 13a of the holograph spike: introduced as the foundation for
//! widening the Step 1.5 algorithm-crate extraction beyond `topo_sort`.

use serde::{Deserialize, Serialize};

/// Triple (source/target/predicate) carried by every link expression.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Triple {
    pub source: Option<String>,
    pub target: Option<String>,
    pub predicate: Option<String>,
}

/// Signature/key pair attached to expressions for AD4M's
/// expression-proof scheme.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}

/// A single signed link expression — the atomic unit of a perspective.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct LinkExpression {
    pub author: String,
    pub data: Triple,
    pub timestamp: String,
    pub proof: ExpressionProof,
}

/// A diff between two perspective states: which links to add and remove.
#[derive(Serialize, Deserialize, Clone, Debug, Default, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}

impl PerspectiveDiff {
    pub fn new() -> Self {
        Self::default()
    }

    /// Total number of additions + removals in this diff. Used by the
    /// chunking logic to know when to start a new chunk.
    pub fn total_diff_number(&self) -> usize {
        self.additions.len() + self.removals.len()
    }
}
