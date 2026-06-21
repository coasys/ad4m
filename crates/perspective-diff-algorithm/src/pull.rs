//! Substrate-agnostic `pull` and `handle_broadcast`.
//!
//! Wake-23 Step 2: moved from
//! `bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/link_adapter/pull.rs`.
//! HDK runtime calls (`emit_signal`, `get_now`, `Retriever::get`,
//! `Retriever::create_entry`) now route through the algorithm-crate
//! traits (`PullCommitEnv` + `WorkspaceRetriever` +
//! `RevisionsRetriever`). The p-diff-sync side keeps only the
//! `hdk_extern` shim that translates between `AlgoError` and
//! `SocialContextError`.

use crate::chunked_diffs::load_diff_aggregated;
use crate::env::PullCommitEnv;
use crate::errors::AlgoResult;
use crate::retriever::{RevisionsRetriever, SnapshotRetriever, WorkspaceRetriever};
use crate::revisions::{current_revision, update_current_revision};
use crate::workspace::Workspace;
use perspective_diff_types::{
    Hash, HashBroadcast, PerspectiveDiff, PerspectiveDiffEntryReference, PullResult,
};

/// Produce a merge entry pointing at `latest` + `current` and update
/// `current_revision` to point at it.
fn merge<R>(latest: Hash, current: Hash) -> AlgoResult<Hash>
where
    R: WorkspaceRetriever + RevisionsRetriever + SnapshotRetriever + PullCommitEnv,
{
    let latest_diff = R::get_p_diff_reference(&latest)?;
    let current_diff = R::get_p_diff_reference(&current)?;

    let merge_diff = PerspectiveDiff {
        additions: vec![],
        removals: vec![],
    };

    let merge_entry_reference = PerspectiveDiffEntryReference {
        parents: Some(vec![latest, current]),
        diff: merge_diff,
        diffs_since_snapshot: latest_diff.diffs_since_snapshot
            + current_diff.diffs_since_snapshot
            + 1,
        diff_chunks: None,
    };
    let merge_entry_reference_hash = R::create_diff_entry(merge_entry_reference)?;

    let now = R::now()?;
    update_current_revision::<R>(merge_entry_reference_hash.clone(), now)?;

    Ok(merge_entry_reference_hash)
}

pub fn pull<R>(emit: bool, theirs: Hash, is_scribe: bool) -> AlgoResult<PullResult>
where
    R: WorkspaceRetriever + RevisionsRetriever + SnapshotRetriever + PullCommitEnv,
{
    let current = current_revision::<R>()?;
    let current_hash = current.clone().map(|val| val.hash);

    let theirs_hash = theirs.clone();

    if Some(theirs_hash) == current_hash {
        return Ok(PullResult {
            diff: PerspectiveDiff::default(),
            current_revision: current_hash,
        });
    }

    let mut workspace = Workspace::new();

    if current.is_none() {
        workspace.collect_only_from_latest::<R>(theirs.clone())?;
        let diff = workspace.squashed_diff();
        update_current_revision::<R>(theirs, R::now()?)?;
        R::emit_diff_signal(diff)?;
        return Ok(PullResult {
            diff: PerspectiveDiff::default(),
            current_revision: None,
        });
    }

    let current = current.expect("current missing handled above");
    let current_hash = current.hash.clone();

    workspace.build_diffs::<R>(theirs.clone(), current_hash.clone())?;

    // First check if we are actually ahead of them -> we don't have to do
    // anything; they will have to merge with / or fast-forward to our current.
    if workspace.all_ancestors(&current_hash)?.contains(&theirs) {
        return Ok(PullResult {
            diff: PerspectiveDiff::default(),
            current_revision: Some(current.hash),
        });
    }

    let fast_forward_possible = workspace.all_ancestors(&theirs)?.contains(&current_hash);

    // If we can't fast forward, we have to merge — but if we are not a scribe,
    // we can't merge, so we can't do anything.
    if !fast_forward_possible && !is_scribe {
        return Ok(PullResult {
            diff: PerspectiveDiff::default(),
            current_revision: Some(current.hash),
        });
    }

    let seen_diffs = workspace.all_ancestors(&current_hash)?;
    let null = crate::null_node();
    let unseen_diffs: Vec<(Hash, PerspectiveDiffEntryReference)> = if !seen_diffs.is_empty() {
        workspace
            .sorted_diffs
            .clone()
            .expect("should be unseen diffs after build_diffs() call")
            .into_iter()
            .filter(|val| {
                if val.0 == null {
                    return false;
                };
                if val.0 == current_hash {
                    return false;
                };
                if seen_diffs.contains(&val.0) {
                    return false;
                };
                true
            })
            .collect()
    } else {
        workspace
            .sorted_diffs
            .expect("should be unseen diffs after build_diffs() call")
            .into_iter()
            .filter(|val| val.0 != null && val.0 != current_hash)
            .collect()
    };

    let (diffs, current_revision) = if fast_forward_possible {
        let mut out = PerspectiveDiff {
            additions: vec![],
            removals: vec![],
        };
        for diff_entry in unseen_diffs {
            let mut loaded_diff = load_diff_aggregated::<R>(&diff_entry.1)?;
            out.additions.append(&mut loaded_diff.additions);
            out.removals.append(&mut loaded_diff.removals);
        }
        update_current_revision::<R>(theirs.clone(), R::now()?)?;
        (out, theirs)
    } else if is_scribe {
        let mut out = PerspectiveDiff {
            additions: vec![],
            removals: vec![],
        };
        for diff_entry in unseen_diffs {
            let mut loaded_diff = load_diff_aggregated::<R>(&diff_entry.1)?;
            out.additions.append(&mut loaded_diff.additions);
            out.removals.append(&mut loaded_diff.removals);
        }
        let merge_hash = merge::<R>(theirs, current.hash)?;
        (out, merge_hash)
    } else {
        (
            PerspectiveDiff {
                additions: vec![],
                removals: vec![],
            },
            current.hash,
        )
    };

    if emit && (!diffs.additions.is_empty() || !diffs.removals.is_empty()) {
        R::emit_diff_signal(diffs.clone())?;
    }

    Ok(PullResult {
        diff: diffs,
        current_revision: Some(current_revision),
    })
}

pub fn handle_broadcast<R>(broadcast: HashBroadcast) -> AlgoResult<()>
where
    R: WorkspaceRetriever + RevisionsRetriever + SnapshotRetriever + PullCommitEnv,
{
    let diff_reference = broadcast.reference.clone();
    let revision = broadcast.reference_hash.clone();

    let current_revision = current_revision::<R>()?;

    if let Some(current_revision) = current_revision {
        if diff_reference.parents == Some(vec![current_revision.hash]) {
            // CRITICAL: load the diff BEFORE updating current_revision —
            // if loading fails (e.g. chunks unavailable) we leave the
            // local revision pointer alone.
            let loaded_diff = load_diff_aggregated::<R>(&broadcast.reference)?;
            update_current_revision::<R>(revision, R::now()?)?;
            R::emit_diff_signal(loaded_diff)?;
        };
    };
    R::emit_broadcast_signal(broadcast)?;
    Ok(())
}
