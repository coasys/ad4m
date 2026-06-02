//! Substrate-agnostic perspective-diff DAG algorithm.
//!
//! This crate is being progressively extracted from
//! `bootstrap-languages/p-diff-sync` (SPIKE.md Step 1.5). The aim is for
//! an alternative substrate (the upcoming `holograph` Kitsune2-backed link
//! language) to consume the same DAG algorithm without dragging in HDK,
//! HDI, or `holo_hash`.
//!
//! v0.1 (this commit) ships the foundational abstraction — the [`OpId`]
//! trait and the topo-sort over `(OpId, Node)` graphs — plus the
//! [`HasDiffParents`] trait that lets the algorithm read parent links out
//! of any node type without owning the concrete node struct.
//!
//! p-diff-sync continues to host the rest of the algorithm modules
//! (`workspace`, `chunked_diffs`, `revisions`, `snapshots`, `render`,
//! `pull`, `commit`) until they can be moved without forking the
//! integrity-zome data types and abstracting HDK runtime calls
//! (`create_link`, `hash_entry`, `get_links`, `emit_signal`,
//! `send_remote_signal`, `sys_time`). See SPIKE.md §2.6 ("narrow the
//! move") and `.spike-status/step-1.5-status.md` for the deferred-work
//! list.

pub mod topo_sort;

use serde::{de::DeserializeOwned, Serialize};
use std::fmt::{Debug, Display};
use std::hash::Hash;

/// Marker trait for substrate-specific op identifiers.
///
/// Concretizations in this spike:
/// - On the Holochain path: `HoloHash<holo_hash::hash_type::Action>`.
/// - On the Kitsune2 path (Step 2): `kitsune2_api::OpId`.
///
/// Both are 32–40 byte hash-shaped values with the trait bounds below.
/// Keeping this as a marker (no methods) lets each substrate choose the
/// most natural representation — the algorithm crate doesn't care, as
/// long as the identifier is cheap to clone, totally ordered, hashable,
/// and round-trippable through serde.
pub trait OpId:
    Clone + Eq + Ord + Hash + Debug + Display + Serialize + DeserializeOwned + Send + Sync + 'static
{
}

impl<T> OpId for T where
    T: Clone
        + Eq
        + Ord
        + Hash
        + Debug
        + Display
        + Serialize
        + DeserializeOwned
        + Send
        + Sync
        + 'static
{
}

/// Anything that can expose its DAG parents as a slice of `OpId`-typed
/// references — the only structural property the topo-sort and graph-walk
/// algorithms need from a node.
///
/// Implemented on the Holochain side for `PerspectiveDiffEntryReference`
/// over `HoloHash<Action>`. The Kitsune side will impl it on whatever node
/// shape `KitsuneRetreiver` returns.
pub trait HasDiffParents<O: OpId> {
    fn parents(&self) -> Option<&[O]>;
}

pub use topo_sort::{topo_sort_diff_references, TopoSortError};
