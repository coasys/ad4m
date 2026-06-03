use std::str::FromStr;

use chrono::{DateTime, NaiveDateTime, Utc};
use hdk::prelude::*;
use perspective_diff_sync_integrity::{
    Anchor, EntryTypes, HashReference, LinkTypes, LocalHashReference, PerspectiveDiffEntryReference,
};

use super::PerspectiveDiffRetreiver;
use crate::errors::{SocialContextError, SocialContextResult};
use crate::link_adapter::conversions::{
    entry_ref_from_algo, entry_ref_to_algo, hash_from_algo, hash_to_algo, snapshot_to_algo,
};
use crate::utils::dedup;
use crate::Hash;
use perspective_diff_algorithm as algo;
use perspective_diff_sync_integrity::LinkTypes as IntegrityLinkTypes;

pub struct HolochainRetreiver;

impl PerspectiveDiffRetreiver for HolochainRetreiver {
    fn get(hash: Hash) -> SocialContextResult<PerspectiveDiffEntryReference> {
        get(hash, GetOptions::network())?
            .ok_or(SocialContextError::InternalError(
                "HolochainRetreiver: Could not find entry",
            ))?
            .entry()
            .to_app_option::<PerspectiveDiffEntryReference>()?
            .ok_or(SocialContextError::InternalError(
                "Expected element to contain app entry data",
            ))
    }

    fn get_with_timestamp(
        hash: Hash,
    ) -> SocialContextResult<(PerspectiveDiffEntryReference, DateTime<Utc>)> {
        let element = get(hash, GetOptions::network())?;
        let element = element.ok_or(SocialContextError::InternalError(
            "HolochainRetreiver: Could not find entry",
        ))?;
        let entry = element.entry();
        let timestamp = element.action().timestamp().0 as u64;
        let duration = std::time::Duration::from_micros(timestamp);
        let timestamp = DateTime::<Utc>::from_utc(
            NaiveDateTime::from_timestamp(duration.as_secs() as i64, duration.subsec_nanos()),
            Utc,
        );
        let entry = entry
            .to_app_option::<PerspectiveDiffEntryReference>()?
            .ok_or(SocialContextError::InternalError(
                "Expected element to contain app entry data",
            ))?;
        Ok((entry, timestamp))
    }

    fn create_entry(entry: EntryTypes) -> SocialContextResult<Hash> {
        create_entry(entry).map_err(|e| SocialContextError::Wasm(e))
    }

    fn current_revision() -> SocialContextResult<Option<LocalHashReference>> {
        let query = query(
            QueryFilter::new()
                .entry_type(EntryType::App(AppEntryDef {
                    entry_index: 3.into(),
                    zome_index: 0.into(),
                    visibility: EntryVisibility::Private,
                }))
                .include_entries(true)
                .descending(),
        );

        let revision = match query {
            Ok(records) => {
                if records.len() == 0 {
                    None
                } else {
                    let record = records[0].clone();
                    let entry = record
                        .entry
                        .to_app_option::<LocalHashReference>()
                        .unwrap()
                        .unwrap();
                    Some(entry)
                }
            }
            Err(e) => {
                debug!("PerspectiveDiffSync.current_revision(): Error when getting current revision: {:?}", e);
                None
            }
        };
        Ok(revision)
    }

    fn latest_revision() -> SocialContextResult<Option<HashReference>> {
        let latest_root_entry = get_latest_revision_anchor();
        let latest_root_entry_hash = hash_entry(latest_root_entry.clone())?;
        let query = LinkQuery::try_new(latest_root_entry_hash, LinkTypes::Index)?;
        let mut latest_revision_links = get_links(query, GetStrategy::Network)?;

        latest_revision_links.sort_by(|link_a, link_b| {
            let link_a_str = std::str::from_utf8(&link_a.tag.0).unwrap();
            let link_b_str = std::str::from_utf8(&link_b.tag.0).unwrap();
            let link_a = DateTime::<Utc>::from_str(link_a_str).unwrap();
            let link_b = DateTime::<Utc>::from_str(link_b_str).unwrap();
            link_a.cmp(&link_b)
        });

        let mut latest_hash_revisions = latest_revision_links
            .into_iter()
            .map(|link| {
                let hash =
                    link.target
                        .into_action_hash()
                        .ok_or(SocialContextError::InternalError(
                            "Could not convert link target to hash",
                        ))?;
                let timestamp = std::str::from_utf8(&link.tag.0)
                    .map_err(|_| SocialContextError::InternalError("Could not tag to string"))?;

                let timestamp = DateTime::<Utc>::from_str(timestamp).map_err(|_| {
                    SocialContextError::InternalError("Could not convert string to timestamp")
                })?;
                Ok(HashReference { hash, timestamp })
            })
            .collect::<SocialContextResult<Vec<HashReference>>>()?;

        Ok(latest_hash_revisions.pop())
    }

    fn update_current_revision(hash: Hash, timestamp: DateTime<Utc>) -> SocialContextResult<()> {
        let hash_ref = LocalHashReference { hash, timestamp };
        create_entry(EntryTypes::LocalHashReference(hash_ref.clone()))?;
        Ok(())
    }

    fn update_latest_revision(hash: Hash, timestamp: DateTime<Utc>) -> SocialContextResult<()> {
        let latest_root_entry = get_latest_revision_anchor();
        let _latest_root_entry_action =
            self::create_entry(EntryTypes::Anchor(latest_root_entry.clone()))?;

        create_link(
            hash_entry(latest_root_entry)?,
            hash,
            LinkTypes::Index,
            LinkTag::new(timestamp.to_string()),
        )?;

        Ok(())
    }
}

// Step 13b-C phase 2: bridge `HolochainRetreiver` over to the
// algorithm-crate's `WorkspaceRetriever` trait so
// `perspective_diff_algorithm::Workspace` can drive its BFS through HDK.
//
// The methods re-shape calls + types: `algo::Hash` ↔ `HoloHash<Action>`,
// `algo::PerspectiveDiffEntryReference` ← integrity, etc. Conversion
// helpers live in `crate::link_adapter::conversions`.
impl algo::WorkspaceRetriever for HolochainRetreiver {
    fn get_p_diff_reference(
        hash: &algo::Hash,
    ) -> algo::AlgoResult<algo::PerspectiveDiffEntryReference> {
        let h = hash_from_algo(hash);
        let entry = <Self as PerspectiveDiffRetreiver>::get(h)
            .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(entry_ref_to_algo(entry))
    }

    fn get_snapshot_by_target(
        target_hash: &algo::Hash,
    ) -> algo::AlgoResult<Option<algo::Snapshot>> {
        // Replicates `Workspace::get_snapshot` from the pre-13b-C HDK
        // body: fetch the entry-ref at `target_hash`, compute its
        // content hash, query for `Snapshot` links with the
        // "snapshot" tag prefix, then deref the first link's target.
        let action_hash = hash_from_algo(target_hash);
        let entry_ref = <Self as PerspectiveDiffRetreiver>::get(action_hash)
            .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        let entry_hash = hash_entry(entry_ref)
            .map_err(|e| algo::AlgoError::Retriever(format!("hash_entry: {}", e)))?;
        let query = LinkQuery::try_new(entry_hash, IntegrityLinkTypes::Snapshot)
            .map_err(|e| algo::AlgoError::Retriever(format!("LinkQuery: {}", e)))?
            .tag_prefix(LinkTag::new("snapshot"));
        let mut snapshot_links = get_links(query, GetStrategy::Local)
            .map_err(|e| algo::AlgoError::Retriever(format!("get_links: {}", e)))?;

        if snapshot_links.is_empty() {
            return Ok(None);
        }

        let target =
            snapshot_links
                .remove(0)
                .target
                .into_entry_hash()
                .ok_or(algo::AlgoError::Retriever(
                    "snapshot link target not an entry_hash".into(),
                ))?;
        let snapshot = get(target, GetOptions::network())
            .map_err(|e| algo::AlgoError::Retriever(format!("get snapshot: {}", e)))?
            .ok_or(algo::AlgoError::Retriever(
                "snapshot entry not found".into(),
            ))?
            .entry()
            .to_app_option::<perspective_diff_sync_integrity::Snapshot>()
            .map_err(|e| algo::AlgoError::Retriever(format!("snapshot decode: {}", e)))?
            .ok_or(algo::AlgoError::Retriever("snapshot entry empty".into()))?;

        Ok(Some(snapshot_to_algo(snapshot)))
    }
}

// Step 13b-D: write-side surface for `snapshots::generate_snapshot`.
// Persists a chunk-diff entry and returns its action-hash for the
// algo crate to reference from the new `Snapshot`.
impl algo::SnapshotRetriever for HolochainRetreiver {
    fn create_diff_entry(
        entry: algo::PerspectiveDiffEntryReference,
    ) -> algo::AlgoResult<algo::Hash> {
        let integrity = entry_ref_from_algo(entry);
        let hash = <Self as PerspectiveDiffRetreiver>::create_entry(
            EntryTypes::PerspectiveDiffEntryReference(integrity),
        )
        .map_err(|e| algo::AlgoError::Retriever(format!("{}", e)))?;
        Ok(hash_to_algo(&hash))
    }
}

fn get_latest_revision_anchor() -> Anchor {
    Anchor("latest_revision".to_string())
}

pub fn get_active_agent_anchor() -> Anchor {
    Anchor("active_agent".to_string())
}

pub fn get_active_agents() -> SocialContextResult<Vec<AgentPubKey>> {
    let query = LinkQuery::try_new(hash_entry(get_active_agent_anchor())?, LinkTypes::Index)?
        .tag_prefix(LinkTag::new("active_agent"));
    let recent_agents = get_links(query, GetStrategy::Local)?;

    let recent_agents = recent_agents
        .into_iter()
        .map(|val| {
            let entry: EntryHash = val.target.try_into().unwrap();
            AgentPubKey::from(entry)
        })
        .collect();

    debug!("get_active_agents(): recent_agents: {:?}", recent_agents);

    //Dedup the agents
    let mut recent_agents = dedup(&recent_agents);
    //Remove ourself from the agents
    // TODO: should be agent_latest_pubkey, but that was made unstable behind dpki feature flag
    let me = agent_info()?.agent_initial_pubkey;
    let index = recent_agents.iter().position(|x| *x == me);
    if let Some(index) = index {
        recent_agents.remove(index);
    };

    Ok(recent_agents)
}
