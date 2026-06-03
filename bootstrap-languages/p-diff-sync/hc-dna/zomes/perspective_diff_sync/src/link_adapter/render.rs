use hdk::prelude::*;
use perspective_diff_algorithm as algo;
use std::collections::HashSet;

use crate::errors::{SocialContextError, SocialContextResult};
use crate::link_adapter::conversions::{hash_to_algo, link_from_algo};
use crate::link_adapter::revisions::current_revision;
use crate::link_adapter::workspace::Workspace;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::utils::get_now;
use crate::Perspective;

pub fn render<
    Retriever: PerspectiveDiffRetreiver + algo::WorkspaceRetriever + algo::RevisionsRetriever,
>() -> SocialContextResult<Perspective> {
    debug!("===PerspectiveDiffSync.render(): Function start");
    let fn_start = get_now()?.time();

    let current = current_revision::<Retriever>()?.ok_or(SocialContextError::InternalError(
        "Can't render when we have no current revision",
    ))?;

    debug!("===PerspectiveDiffSync.render(): current: {:?}", current);

    let mut workspace = Workspace::new();
    workspace.collect_only_from_latest::<Retriever>(hash_to_algo(&current.hash))?;

    let mut perspective = Perspective { links: vec![] };

    // Collect all removals into a HashSet for O(1) lookup
    let mut removals_set = HashSet::new();

    for diff_node in workspace.entry_map {
        // workspace.entry_map carries algorithm-crate mirror
        // `LinkExpression`s; convert each link back to the integrity-zome
        // shape exposed by `Perspective`.
        for addition in diff_node.1.diff.additions {
            perspective.links.push(link_from_algo(addition));
        }
        for removal in diff_node.1.diff.removals {
            removals_set.insert(link_from_algo(removal));
        }
    }

    // Remove all links that are in the removals set with a single retain call - O(N)
    perspective
        .links
        .retain(|link| !removals_set.contains(link));

    let fn_end = get_now()?.time();
    debug!(
        "===PerspectiveDiffSync.render() - Profiling: Took: {} to complete render() function",
        (fn_end - fn_start).num_milliseconds()
    );
    Ok(perspective)
}
