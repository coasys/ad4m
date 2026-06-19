//! Substrate-agnostic perspective-diff DAG algorithm.
//!
//! This crate is being progressively extracted from
//! `bootstrap-languages/p-diff-sync` (SPIKE.md Step 1.5). The aim is for
//! an alternative substrate (the upcoming `holograph` Kitsune2-backed link
//! language) to consume the same DAG algorithm without dragging in HDK,
//! HDI, or `holo_hash`.
//!
//! Wire types are now defined once in `perspective-diff-types`; this
//! crate consumes them and provides the topo-sort, workspace BFS,
//! chunked-diff aggregation, snapshots, and revisions logic on top.
//! Pull/commit/render move here in wake-23 Step 2.

pub mod chunked_diffs;
pub mod errors;
pub mod retriever;
pub mod revisions;
pub mod snapshots;
pub mod topo_sort;
pub mod workspace;

pub use chunked_diffs::{load_diff_aggregated, ChunkedDiffs};
pub use errors::{AlgoError, AlgoResult};
pub use perspective_diff_types::{
    null_node, ExpressionProof, HasDiffParents, Hash, HashReference, LinkExpression,
    LocalHashReference, OpId, PerspectiveDiff, PerspectiveDiffEntryReference, Snapshot, Triple,
};
pub use retriever::{RevisionsRetriever, SnapshotRetriever, WorkspaceRetriever};
pub use snapshots::generate_snapshot;
pub use topo_sort::{topo_sort_diff_references, TopoSortError};
pub use workspace::Workspace;
