//! Substrate-agnostic retriever traits for the workspace / pull / commit /
//! snapshots algorithm modules.
//!
//! p-diff-sync's `PerspectiveDiffRetreiver` still owns the HDK-flavored
//! methods (`current_revision` / `latest_revision` / `update_*` / etc.);
//! these traits carve out just the surface the in-crate algorithm
//! needs and bridge from the algorithm mirror types (`Hash`,
//! `PerspectiveDiffEntryReference`, `Snapshot`) — the HDK-side adapter
//! converts the integrity-zome types to these on the way through.

use crate::errors::AlgoResult;
use perspective_diff_types::{
    Hash, HashReference, LocalHashReference, PerspectiveDiffEntryReference, Snapshot,
};

/// The minimum read-side surface the in-crate `Workspace` builder needs
/// from any substrate.
pub trait WorkspaceRetriever {
    /// Look up a `PerspectiveDiffEntryReference` by its hash.
    fn get_p_diff_reference(hash: &Hash) -> AlgoResult<PerspectiveDiffEntryReference>;

    /// Look up the snapshot attached to the entry at `target_hash`, if any.
    /// On the HDK side this performs the `LinkQuery::try_new + get_links +
    /// get + to_app_option::<Snapshot>` chain; on the holograph side it
    /// reads the snapshot keyed by the entry's op-id.
    fn get_snapshot_by_target(target_hash: &Hash) -> AlgoResult<Option<Snapshot>>;
}

/// Adds the write capability needed by the `snapshots` module.
///
/// Step 13b-D split off as a sibling of `WorkspaceRetriever` so the
/// workspace tests and Workspace-only callers (`render`, the BFS unit
/// tests) don't have to wire a write surface they never exercise.
/// All three substrates (Holochain, Mock, Kitsune) implement both.
pub trait SnapshotRetriever: WorkspaceRetriever {
    /// Persist a `PerspectiveDiffEntryReference` to the substrate and
    /// return the hash it can later be fetched by via
    /// `get_p_diff_reference`. `snapshots::generate_snapshot` calls this
    /// to write each chunk-diff entry the snapshot points at.
    fn create_diff_entry(entry: PerspectiveDiffEntryReference) -> AlgoResult<Hash>;
}

/// Revision pointer surface for the `revisions` module.
///
/// Step 13b-E (wake-16) — sibling of `WorkspaceRetriever`. The
/// algorithm crate's `revisions::current_revision` /
/// `revisions::update_current_revision` are thin wrappers around these
/// methods so substrate-agnostic algorithm code (and downstream
/// extracted modules — pull, render, commit) can read/write the
/// per-substrate "current revision" pointer without forking into
/// HDK-specific or sled-specific code.
///
/// `latest_revision` is also surfaced so future snapshot-driving code
/// can read the network's latest pointer without an extra trait.
pub trait RevisionsRetriever: WorkspaceRetriever {
    fn current_revision() -> AlgoResult<Option<LocalHashReference>>;

    fn latest_revision() -> AlgoResult<Option<HashReference>>;

    fn update_current_revision(
        hash: Hash,
        timestamp: chrono::DateTime<chrono::Utc>,
    ) -> AlgoResult<()>;
}
