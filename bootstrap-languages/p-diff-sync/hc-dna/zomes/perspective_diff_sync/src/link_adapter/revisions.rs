//! HDK-side shim onto the algorithm-crate `revisions` module.

use chrono::{DateTime, Utc};
use perspective_diff_algorithm as algo;
use perspective_diff_sync_integrity::LocalHashReference;

use crate::errors::SocialContextResult;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::Hash;

pub fn update_current_revision<Retriever: PerspectiveDiffRetreiver + algo::RevisionsRetriever>(
    hash: Hash,
    timestamp: DateTime<Utc>,
) -> SocialContextResult<()> {
    algo::revisions::update_current_revision::<Retriever>(hash, timestamp)?;
    Ok(())
}

pub fn current_revision<Retriever: PerspectiveDiffRetreiver + algo::RevisionsRetriever>(
) -> SocialContextResult<Option<LocalHashReference>> {
    Ok(algo::revisions::current_revision::<Retriever>()?)
}
