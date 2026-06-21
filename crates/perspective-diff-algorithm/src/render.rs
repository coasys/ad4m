//! Substrate-agnostic `render`: walk back from current_revision to the
//! nearest snapshot/orphan, gather additions, subtract removals, and
//! return the resulting set of `LinkExpression`s.
//!
//! Originally lived in p-diff-sync's
//! `link_adapter/render.rs` (56 LOC, HDK-flavored). Wake-23 Step 2
//! collapses the HDK glue: every substrate-touching call (`get_now`,
//! `current_revision`) now goes through the algorithm-crate traits.

use std::collections::HashSet;

use crate::env::PullCommitEnv;
use crate::errors::{AlgoError, AlgoResult};
use crate::retriever::{RevisionsRetriever, WorkspaceRetriever};
use crate::workspace::Workspace;
use perspective_diff_types::LinkExpression;

/// Compute the set of `LinkExpression`s currently in the perspective
/// at our `current_revision`. The substrate impl wraps this in its
/// `Perspective { links }` shape.
pub fn render_perspective_links<R>() -> AlgoResult<Vec<LinkExpression>>
where
    R: WorkspaceRetriever + RevisionsRetriever + PullCommitEnv,
{
    let current = crate::revisions::current_revision::<R>()?.ok_or(AlgoError::Internal(
        "Can't render when we have no current revision",
    ))?;

    let mut workspace = Workspace::new();
    workspace.collect_only_from_latest::<R>(current.hash)?;

    let mut links: Vec<LinkExpression> = Vec::new();
    let mut removals_set: HashSet<LinkExpression> = HashSet::new();

    for diff_node in workspace.entry_map {
        for addition in diff_node.1.diff.additions {
            links.push(addition);
        }
        for removal in diff_node.1.diff.removals {
            removals_set.insert(removal);
        }
    }

    links.retain(|link| !removals_set.contains(link));
    Ok(links)
}
