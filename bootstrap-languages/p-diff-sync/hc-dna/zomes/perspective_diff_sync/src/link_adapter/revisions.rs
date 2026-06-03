//! HDK-side shim onto the algorithm-crate `revisions` module.
//!
//! Step 13b-E (wake-16): the substrate-agnostic revision-pointer
//! wrappers now live in `perspective_diff_algorithm::revisions`. This
//! module keeps the legacy import path (`link_adapter::revisions::...`)
//! working for `pull.rs`, `render.rs`, and `lib.rs`, while preserving
//! the original signatures (return `Option<integrity::LocalHashReference>`
//! so callers don't need to bridge mirror types yet).
//!
//! The HDK-flavored profiling debug logs that used to live here are
//! gone — they were noise. Functional behaviour is unchanged.

use chrono::{DateTime, Utc};
use perspective_diff_algorithm as algo;
use perspective_diff_sync_integrity::LocalHashReference;

use crate::errors::SocialContextResult;
use crate::link_adapter::conversions::{hash_to_algo, local_hash_ref_from_algo};
use crate::retriever::PerspectiveDiffRetreiver;
use crate::Hash;

pub fn update_current_revision<Retriever: PerspectiveDiffRetreiver + algo::RevisionsRetriever>(
    hash: Hash,
    timestamp: DateTime<Utc>,
) -> SocialContextResult<()> {
    algo::revisions::update_current_revision::<Retriever>(hash_to_algo(&hash), timestamp)?;
    Ok(())
}

pub fn current_revision<Retriever: PerspectiveDiffRetreiver + algo::RevisionsRetriever>(
) -> SocialContextResult<Option<LocalHashReference>> {
    let rev = algo::revisions::current_revision::<Retriever>()?;
    Ok(rev.map(local_hash_ref_from_algo))
}
