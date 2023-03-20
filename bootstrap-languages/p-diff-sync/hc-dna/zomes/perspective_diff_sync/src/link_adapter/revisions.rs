use chrono::{DateTime, Utc};
use hdk::prelude::debug;
use perspective_diff_sync_integrity::LocalHashReference;

use crate::errors::SocialContextResult;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::utils::get_now;
use crate::Hash;

pub fn update_current_revision<Retriever: PerspectiveDiffRetreiver>(
    hash: Hash,
    timestamp: DateTime<Utc>,
) -> SocialContextResult<()> {
    debug!("===PerspectiveDiffSync.update_current_revision(): Function start");
    let now = get_now()?.time();
    let res = Retriever::update_current_revision(hash, timestamp);
    let after = get_now()?.time();
    debug!("===PerspectiveDiffSync.update_current_revision() - Profiling: Took: {} to update current_revision", (after - now).num_milliseconds());
    res
}

//Latest revision as seen from our local state
pub fn current_revision<Retriever: PerspectiveDiffRetreiver>(
) -> SocialContextResult<Option<LocalHashReference>> {
    //debug!("===PerspectiveDiffSync.current_revision(): Function start");
    //let now = get_now()?.time();
    let rev = Retriever::current_revision()?;
    // debug!("===PerspectiveDiffSync.current_revision(): rev = {:?}", rev);
    //let after = get_now()?.time();
    //debug!("===PerspectiveDiffSync.current_revision() - Profiling: Took: {} to get the current_revision", (after - now).num_milliseconds());
    Ok(rev)
}
