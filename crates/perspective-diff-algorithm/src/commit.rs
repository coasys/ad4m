//! Substrate-agnostic `commit` and `broadcast_current`.
//!
//! Wake-23 Step 2: moved from
//! `bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/link_adapter/commit.rs`.
//! Holochain runtime calls (`emit_signal`, `hash_entry`, `create_link`,
//! `send_remote_signal`, `sys_time`) now route through the
//! `PullCommitEnv` trait; the p-diff-sync zome side keeps only the
//! `hdk_extern` shim plus the active-agent linking that's deeply
//! Holochain-specific (agent-pubkey, query, dedup).

use crate::chunked_diffs::ChunkedDiffs;
use crate::env::PullCommitEnv;
use crate::errors::{AlgoError, AlgoResult};
use crate::retriever::{RevisionsRetriever, SnapshotRetriever, WorkspaceRetriever};
use crate::revisions::{current_revision, update_current_revision};
use crate::snapshots::generate_snapshot;
use perspective_diff_types::{Hash, HashBroadcast, PerspectiveDiff, PerspectiveDiffEntryReference};

/// Knobs that p-diff-sync exposes via `lazy_static`. The algorithm
/// crate doesn't read these directly — the caller threads them in.
#[derive(Copy, Clone, Debug)]
pub struct CommitConfig {
    /// Cap on links per chunk before splitting into chunked storage.
    pub chunk_size: u16,
    /// Take a fresh snapshot after this many diff entries.
    pub snapshot_interval: usize,
    /// Switch to chunked storage when a single diff exceeds this many
    /// total additions+removals.
    pub chunking_threshold: usize,
    /// Whether `commit` should follow up by broadcasting the new
    /// revision to active agents.
    pub enable_signals: bool,
}

const MAX_CHUNK_RETRIES: u32 = 10;
const CHUNK_RETRY_DELAY_MS: i64 = 100;

pub fn commit<R>(diff: PerspectiveDiff, my_did: String, cfg: CommitConfig) -> AlgoResult<Hash>
where
    R: WorkspaceRetriever + RevisionsRetriever + SnapshotRetriever + PullCommitEnv,
{
    let initial_current_revision = current_revision::<R>()?;

    let mut entries_since_snapshot = 0usize;
    if let Some(rev) = initial_current_revision.clone() {
        let current = R::get_p_diff_reference(&rev.hash)?;
        entries_since_snapshot = current.diffs_since_snapshot;
    }
    entries_since_snapshot += 1;

    let create_snapshot_here = if entries_since_snapshot >= cfg.snapshot_interval {
        entries_since_snapshot = 0;
        true
    } else {
        false
    };

    let (diff_entry_ref_entry, diff_entry_reference) =
        if diff.total_diff_number() > cfg.chunking_threshold {
            let mut chunked = ChunkedDiffs::new(cfg.chunk_size);
            chunked.add_additions(diff.additions.clone());
            chunked.add_removals(diff.removals.clone());

            let mut chunk_hashes: Vec<Hash> = Vec::with_capacity(chunked.chunks.len());
            for chunk in chunked.chunks.into_iter() {
                let entry = PerspectiveDiffEntryReference::new(chunk, None);
                chunk_hashes.push(R::create_diff_entry(entry)?);
            }

            // Wait for every chunk to be locally retrievable before we
            // commit the parent entry that references them — otherwise
            // a peer that fetches the parent first will reject it.
            for chunk_hash in &chunk_hashes {
                let mut retry_count = 0u32;
                loop {
                    match R::get_p_diff_reference(chunk_hash) {
                        Ok(_) => break,
                        Err(_) => {
                            retry_count += 1;
                            if retry_count >= MAX_CHUNK_RETRIES {
                                return Err(AlgoError::Internal(
                                    "Failed to verify chunk availability after creation",
                                ));
                            }
                            let start = R::sys_time_ms()?;
                            loop {
                                let now = R::sys_time_ms()?;
                                if now - start >= CHUNK_RETRY_DELAY_MS {
                                    break;
                                }
                            }
                        }
                    }
                }
            }

            let entry = PerspectiveDiffEntryReference {
                diff: PerspectiveDiff::new(),
                parents: initial_current_revision.clone().map(|val| vec![val.hash]),
                diffs_since_snapshot: entries_since_snapshot,
                diff_chunks: Some(chunk_hashes),
            };
            let hash = R::create_diff_entry(entry.clone())?;
            (entry, hash)
        } else {
            let entry = PerspectiveDiffEntryReference {
                diff: diff.clone(),
                parents: initial_current_revision.clone().map(|val| vec![val.hash]),
                diffs_since_snapshot: entries_since_snapshot,
                diff_chunks: None,
            };
            let hash = R::create_diff_entry(entry.clone())?;
            (entry, hash)
        };

    if create_snapshot_here {
        let snapshot = generate_snapshot::<R>(diff_entry_reference.clone(), cfg.chunk_size)?;
        R::create_snapshot_and_link(diff_entry_reference.clone(), snapshot)?;
    }

    // Concurrency guard: bail if some other caller in this process
    // raced us and already updated `current_revision`.
    let latest_current_revision = current_revision::<R>()?;
    let initial_dbg = initial_current_revision
        .as_ref()
        .map(|r| format!("{:?}", r));
    let latest_dbg = latest_current_revision.as_ref().map(|r| format!("{:?}", r));
    if initial_dbg != latest_dbg {
        return Err(AlgoError::Internal("Concurrent update detected in commit"));
    }

    update_current_revision::<R>(diff_entry_reference.clone(), R::now()?)?;

    if cfg.enable_signals {
        broadcast_current::<R>(&my_did)?;
    }

    // Suppress unused `diff_entry_ref_entry`/`_diff` warnings: we
    // keep the binding around so future signal-shape work (currently
    // commented out below the original commit.rs) can resurrect it.
    let _ = diff_entry_ref_entry;

    Ok(diff_entry_reference)
}

pub fn broadcast_current<R>(my_did: &str) -> AlgoResult<Option<Hash>>
where
    R: WorkspaceRetriever + RevisionsRetriever + PullCommitEnv,
{
    let current = current_revision::<R>()?;
    if let Some(current_revision) = current.clone() {
        let entry_ref = R::get_p_diff_reference(&current_revision.hash)?;
        let signal_data = HashBroadcast {
            reference: entry_ref,
            reference_hash: current_revision.hash.clone(),
            broadcast_author: my_did.to_string(),
        };
        R::send_hash_broadcast_to_active_agents(signal_data)?;
    }
    Ok(current.map(|rev| rev.hash))
}
