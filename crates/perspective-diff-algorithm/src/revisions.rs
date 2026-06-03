//! Revision-pointer accessors — substrate-agnostic.
//!
//! Originally lived in
//! `bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/link_adapter/revisions.rs`
//! as two thin wrappers around the HDK-side
//! `PerspectiveDiffRetreiver::current_revision` /
//! `update_current_revision` trait methods.
//!
//! Step 13b-E (wake-16): the wrappers move here, generic over the
//! [`RevisionsRetriever`] trait. They're still mostly forwarders — the
//! per-substrate read/write is unavoidable — but pulling them into the
//! algorithm crate means downstream algorithm modules (the upcoming
//! `pull` / `render` / `commit` extractions in 13b-F/G/H) can call
//! through one substrate-agnostic surface.

use chrono::{DateTime, Utc};

use crate::diff_types::{Hash, HashReference, LocalHashReference};
use crate::errors::AlgoResult;
use crate::retriever::RevisionsRetriever;

/// The agent's local view of where they are in the DAG.
pub fn current_revision<R: RevisionsRetriever>() -> AlgoResult<Option<LocalHashReference>> {
    R::current_revision()
}

/// The substrate's most recent broadcast/published revision.
pub fn latest_revision<R: RevisionsRetriever>() -> AlgoResult<Option<HashReference>> {
    R::latest_revision()
}

/// Move the local "current" pointer.
pub fn update_current_revision<R: RevisionsRetriever>(
    hash: Hash,
    timestamp: DateTime<Utc>,
) -> AlgoResult<()> {
    R::update_current_revision(hash, timestamp)
}
