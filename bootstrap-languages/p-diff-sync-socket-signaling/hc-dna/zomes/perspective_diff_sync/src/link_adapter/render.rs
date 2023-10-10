use hdk::prelude::*;
use perspective_diff_sync_integrity::PerspectiveDiff;

use crate::errors::{SocialContextError, SocialContextResult};
use crate::link_adapter::revisions::current_revision;
use crate::link_adapter::workspace::Workspace;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::utils::get_now;
use crate::Perspective;

pub fn render<Retriever: PerspectiveDiffRetreiver>() -> SocialContextResult<Perspective> {
    debug!("===PerspectiveDiffSync.render(): Function start");
    let fn_start = get_now()?.time();

    let current = current_revision::<Retriever>()?.ok_or(SocialContextError::InternalError(
        "Can't render when we have no current revision",
    ))?;

    debug!("===PerspectiveDiffSync.render(): current: {:?}", current);

    let mut workspace = Workspace::new();
    workspace.collect_only_from_latest::<Retriever>(current.hash)?;

    let mut perspective = Perspective { links: vec![] };
    for diff_node in workspace.entry_map {
        let diff_entry = Retriever::get::<PerspectiveDiff>(diff_node.1.diff.clone())?;

        for addition in diff_entry.additions {
            perspective.links.push(addition);
        }
        for removal in diff_entry.removals {
            perspective.links.retain(|l| l != &removal);
        }
    }

    let fn_end = get_now()?.time();
    debug!(
        "===PerspectiveDiffSync.render() - Profiling: Took: {} to complete render() function",
        (fn_end - fn_start).num_milliseconds()
    );
    Ok(perspective)
}
