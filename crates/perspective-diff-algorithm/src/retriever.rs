//! Substrate-agnostic retriever trait for the workspace / pull / commit
//! algorithm modules.
//!
//! p-diff-sync's `PerspectiveDiffRetreiver` still owns the HDK-flavored
//! methods (`current_revision` / `latest_revision` / `update_*` / etc.);
//! this trait carves out just the read methods the in-crate algorithm
//! needs and bridges from the algorithm mirror types (`Hash`,
//! `PerspectiveDiffEntryReference`, `Snapshot`) — the HDK-side adapter
//! converts the integrity-zome types to these on the way through.

use crate::diff_types::{Hash, PerspectiveDiffEntryReference, Snapshot};
use crate::errors::AlgoResult;

/// The minimum surface the in-crate `Workspace` builder needs from any
/// substrate.
pub trait WorkspaceRetriever {
    /// Look up a `PerspectiveDiffEntryReference` by its hash.
    fn get_p_diff_reference(hash: &Hash) -> AlgoResult<PerspectiveDiffEntryReference>;

    /// Look up the snapshot attached to the entry at `target_hash`, if any.
    /// On the HDK side this performs the `LinkQuery::try_new + get_links +
    /// get + to_app_option::<Snapshot>` chain; on the holograph side it
    /// reads the snapshot keyed by the entry's op-id.
    fn get_snapshot_by_target(target_hash: &Hash) -> AlgoResult<Option<Snapshot>>;
}
