//! HDK-side shim onto the algorithm-crate `commit` + `broadcast_current`.
//!
//! The pull/commit/render bodies live in the algorithm crate
//! (`perspective_diff_algorithm`). This module keeps:
//! - the legacy import paths (`link_adapter::commit::commit`, etc.),
//! - error mapping from `AlgoError` to `SocialContextError`,
//! - `add_active_agent_link` (heavily HDK-specific — agent_info,
//!   agent-pubkey dedup, link queries — stays out of the algorithm
//!   crate).

use hdk::prelude::*;
use perspective_diff_algorithm as algo;
use perspective_diff_sync_integrity::{EntryTypes, LinkTypes, PerspectiveDiff};

use crate::errors::SocialContextResult;
use crate::retriever::holochain::get_active_agent_anchor;
use crate::retriever::PerspectiveDiffRetreiver;
use crate::utils::get_now;
use crate::{Hash, CHUNK_SIZE, ENABLE_SIGNALS, SNAPSHOT_INTERVAL};

const CHUNKING_THRESHOLD: usize = 500;

pub fn commit<
    Retriever: PerspectiveDiffRetreiver
        + algo::WorkspaceRetriever
        + algo::RevisionsRetriever
        + algo::SnapshotRetriever
        + algo::PullCommitEnv,
>(
    diff: PerspectiveDiff,
    my_did: String,
) -> SocialContextResult<Hash> {
    let cfg = algo::CommitConfig {
        chunk_size: *CHUNK_SIZE,
        snapshot_interval: *SNAPSHOT_INTERVAL,
        chunking_threshold: CHUNKING_THRESHOLD,
        enable_signals: *ENABLE_SIGNALS,
    };
    Ok(algo::commit::<Retriever>(diff, my_did, cfg)?)
}

pub fn broadcast_current<
    Retriever: PerspectiveDiffRetreiver
        + algo::WorkspaceRetriever
        + algo::RevisionsRetriever
        + algo::PullCommitEnv,
>(
    my_did: &str,
) -> SocialContextResult<Option<Hash>> {
    Ok(algo::commit::broadcast_current::<Retriever>(my_did)?)
}

pub fn add_active_agent_link<Retriever: PerspectiveDiffRetreiver>() -> SocialContextResult<()> {
    let now_fn_start = get_now()?.time();
    let agent_root_entry = get_active_agent_anchor();
    let _agent_root_entry_action =
        Retriever::create_entry(EntryTypes::Anchor(agent_root_entry.clone()))?;

    let agent = agent_info()?.agent_initial_pubkey;
    let agent_root_hash = hash_entry(agent_root_entry)?;

    let query = LinkQuery::try_new(agent_root_hash.clone(), LinkTypes::Index)?
        .tag_prefix(LinkTag::new("active_agent"));
    let existing_links = get_links(query, GetStrategy::Local)?;

    let link_exists = existing_links
        .iter()
        .any(|link| link.target.clone().into_agent_pub_key() == Some(agent.clone()));

    if !link_exists {
        create_link(
            agent_root_hash,
            agent,
            LinkTypes::Index,
            LinkTag::new("active_agent"),
        )?;
    }

    let after_fn_end = get_now()?.time();
    debug!(
        "add_active_agent_link: {} ms",
        (after_fn_end - now_fn_start).num_milliseconds()
    );
    Ok(())
}
