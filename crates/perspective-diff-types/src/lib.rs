//! Shared wire types for the p-diff-sync DAG.
//!
//! This crate exists to give `perspective-diff-algorithm` and the
//! `perspective_diff_sync_integrity` zome a single source of truth for
//! the types both crates need to manipulate (`PerspectiveDiff`,
//! `PerspectiveDiffEntryReference`, `Snapshot`, `HashReference`,
//! `LocalHashReference`, `HashBroadcast`, `LinkExpression`, `Triple`,
//! `ExpressionProof`, `Hash`).
//!
//! Wake-23 (Step 1 of the deeper PR-A extraction): replaces the
//! parallel "mirror types" that previously lived in
//! `perspective-diff-algorithm::diff_types` and the conversion shim in
//! `p-diff-sync/.../link_adapter/conversions.rs`.
//!
//! The `hdi` cargo feature decorates the entry-shaped types with
//! `holochain_serialized_bytes::SerializedBytes` and registers them
//! with HDI via `app_entry!`. The integrity zome turns this feature on;
//! the algorithm crate and its standalone tests build without it.
//! Because Cargo unifies features per build graph, a single workspace
//! build that pulls in the integrity zome ends up using the
//! `hdi`-flavored version everywhere — and crucially that's still the
//! same struct shape, so consumers don't need to gate on the feature.

use serde::{Deserialize, Serialize};

#[cfg(feature = "hdi")]
use hdi::prelude::*;
#[cfg(feature = "hdi")]
use holochain_serialized_bytes::SerializedBytes;

pub extern crate holo_hash;

/// 39-byte HoloHash<Action> — the on-DHT identity for every entry in
/// the perspective-diff DAG. Lives at this crate's surface so both the
/// algorithm crate and the integrity zome refer to the same concrete
/// type with zero conversion.
pub type Hash = holo_hash::HoloHash<holo_hash::hash_type::Action>;

// ---- pure wire types --------------------------------------------------

#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct ExpressionProof {
    pub signature: String,
    pub key: String,
}

#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Triple {
    pub source: Option<String>,
    pub target: Option<String>,
    pub predicate: Option<String>,
}

#[derive(Clone, Deserialize, Serialize, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct LinkExpression {
    pub author: String,
    pub data: Triple,
    pub timestamp: String,
    pub proof: ExpressionProof,
}

#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Clone, Debug, Serialize, Deserialize, Default, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}

impl PerspectiveDiff {
    pub fn new() -> Self {
        Self {
            additions: Vec::new(),
            removals: Vec::new(),
        }
    }

    pub fn total_diff_number(&self) -> usize {
        self.additions.len() + self.removals.len()
    }

    #[cfg(feature = "hdi")]
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CommitInput {
    pub diff: PerspectiveDiff,
    pub my_did: String,
}

/// The reference that is sent to other agents, denotes the position in
/// the DAG as well as the data at that position.
#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HashBroadcast {
    pub reference_hash: Hash,
    pub reference: PerspectiveDiffEntryReference,
    pub broadcast_author: String,
}

#[cfg(feature = "hdi")]
impl HashBroadcast {
    pub fn get_sb(self) -> ExternResult<SerializedBytes> {
        self.try_into()
            .map_err(|error| wasm_error!(WasmErrorInner::Host(String::from(error))))
    }
}

#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Snapshot {
    pub diff_chunks: Vec<Hash>,
    pub included_diffs: Vec<Hash>,
}

#[cfg(feature = "hdi")]
app_entry!(Snapshot);

#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PerspectiveDiffEntryReference {
    pub diff: PerspectiveDiff,
    pub parents: Option<Vec<Hash>>,
    pub diffs_since_snapshot: usize,
    /// Optional hashes of chunked diff entries for large diffs.
    /// When this is Some and non-empty, the `diff` field should be empty/default
    /// and the actual diff data is stored in separate chunk entries.
    #[serde(default)]
    pub diff_chunks: Option<Vec<Hash>>,
}

#[cfg(feature = "hdi")]
app_entry!(PerspectiveDiffEntryReference);

impl PerspectiveDiffEntryReference {
    pub fn new(diff: PerspectiveDiff, parents: Option<Vec<Hash>>) -> Self {
        Self {
            diff,
            parents,
            diffs_since_snapshot: 0,
            diff_chunks: None,
        }
    }

    /// Create a new entry reference with chunked diffs.
    pub fn new_chunked(
        diff_chunks: Vec<Hash>,
        parents: Option<Vec<Hash>>,
        diffs_since_snapshot: usize,
    ) -> Self {
        Self {
            diff: PerspectiveDiff::new(),
            parents,
            diffs_since_snapshot,
            diff_chunks: Some(diff_chunks),
        }
    }

    pub fn is_chunked(&self) -> bool {
        self.diff_chunks
            .as_ref()
            .map_or(false, |chunks| !chunks.is_empty())
    }

    /// Backward-compatibility shim used by the workspace render path.
    pub fn to_perspective_diff(&self) -> PerspectiveDiff {
        self.diff.clone()
    }

    fn comparison_key(&self) -> (bool, &Option<Vec<Hash>>, usize, usize, &PerspectiveDiff) {
        let has_parents = self.parents.is_some();
        (
            !has_parents,
            &self.parents,
            self.diffs_since_snapshot,
            self.diff.total_diff_number(),
            &self.diff,
        )
    }
}

impl PartialOrd for PerspectiveDiffEntryReference {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PerspectiveDiffEntryReference {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.comparison_key().cmp(&other.comparison_key())
    }
}

#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HashReference {
    pub hash: Hash,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

#[cfg(feature = "hdi")]
app_entry!(HashReference);

#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LocalHashReference {
    pub hash: Hash,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

#[cfg(feature = "hdi")]
app_entry!(LocalHashReference);

#[cfg_attr(feature = "hdi", derive(SerializedBytes))]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PullResult {
    pub diff: PerspectiveDiff,
    pub current_revision: Option<Hash>,
}

// ---- algorithm-side traits ------------------------------------------

use serde::de::DeserializeOwned;
use std::fmt::{Debug as StdDebug, Display};
use std::hash::Hash as StdHash;

/// Marker trait for substrate-specific op identifiers — both Holochain
/// (`HoloHash<Action>`) and the upcoming Kitsune2 substrate will fit.
pub trait OpId:
    Clone
    + Eq
    + Ord
    + StdHash
    + StdDebug
    + Display
    + Serialize
    + DeserializeOwned
    + Send
    + Sync
    + 'static
{
}

impl<T> OpId for T where
    T: Clone
        + Eq
        + Ord
        + StdHash
        + StdDebug
        + Display
        + Serialize
        + DeserializeOwned
        + Send
        + Sync
        + 'static
{
}

/// Anything that can expose its DAG parents as a slice of `OpId`-typed
/// references — the only structural property the topo-sort and BFS
/// walks need from a node.
pub trait HasDiffParents<O: OpId> {
    fn parents(&self) -> Option<&[O]>;
}

impl HasDiffParents<Hash> for PerspectiveDiffEntryReference {
    fn parents(&self) -> Option<&[Hash]> {
        self.parents.as_deref()
    }
}

// ---- null-node sentinel ---------------------------------------------

/// Reserved sentinel used by `Workspace::collect_until_common_ancestor`
/// when one side of a BFS reaches a chainless leaf and the other side
/// also reached a leaf — i.e. the two trees never share a real common
/// ancestor. Matches the integrity-zome
/// `ActionHash::from_raw_36(vec![0xdb; 36])` byte pattern.
pub fn null_node() -> Hash {
    holo_hash::HoloHash::from_raw_36(vec![0xdb; 36])
}
