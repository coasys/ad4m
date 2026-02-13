use hdk::prelude::*;
use std::collections::HashSet;

use crate::errors::{SocialContextError, SocialContextResult};
use crate::link_adapter::revisions::current_revision;
use crate::link_adapter::workspace::Workspace;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::utils::get_now;
use crate::Perspective;

pub fn render<Retriever: PerspectiveDiffRetreiver>() -> SocialContextResult<Perspective> {
    info!("===PerspectiveDiffSync.render(): START");
    let fn_start = get_now()?.time();

    let current = current_revision::<Retriever>()?.ok_or(SocialContextError::InternalError(
        "Can't render when we have no current revision",
    ))?;

    info!("===PerspectiveDiffSync.render(): current revision: {:?}", current.hash);

    let mut workspace = Workspace::new();
    info!("===PerspectiveDiffSync.render(): Collecting diffs from revision graph...");
    workspace.collect_only_from_latest::<Retriever>(current.hash)?;
    info!("===PerspectiveDiffSync.render(): Collected {} diff entries from revision graph", workspace.entry_map.len());

    let mut perspective = Perspective { links: vec![] };

    // Collect all removals into a HashSet for O(1) lookup
    let mut removals_set = HashSet::new();
    let mut total_additions = 0;
    let mut total_removals = 0;

    for (hash, diff_node) in workspace.entry_map {
        let additions_count = diff_node.diff.additions.len();
        let removals_count = diff_node.diff.removals.len();
        info!("===PerspectiveDiffSync.render(): Processing diff entry {:?} - additions: {}, removals: {}", hash, additions_count, removals_count);

        total_additions += additions_count;
        total_removals += removals_count;

        // Add all additions to the perspective
        for addition in diff_node.diff.additions {
            perspective.links.push(addition);
        }
        // Collect all removals into the HashSet
        for removal in diff_node.diff.removals {
            removals_set.insert(removal);
        }
    }

    info!("===PerspectiveDiffSync.render(): Aggregated totals - additions: {}, removals: {}", total_additions, total_removals);
    info!("===PerspectiveDiffSync.render(): Links before removal filtering: {}", perspective.links.len());

    // Remove all links that are in the removals set with a single retain call - O(N)
    perspective
        .links
        .retain(|link| !removals_set.contains(link));

    info!("===PerspectiveDiffSync.render(): Links after removal filtering: {}", perspective.links.len());

    let fn_end = get_now()?.time();
    info!(
        "===PerspectiveDiffSync.render(): ✓ COMPLETE - Took: {}ms, returning {} links",
        (fn_end - fn_start).num_milliseconds(),
        perspective.links.len()
    );
    Ok(perspective)
}
