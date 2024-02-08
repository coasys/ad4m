use std::str::FromStr;

use chrono::{DateTime, NaiveDateTime, Utc};
use hdk::prelude::*;
use perspective_diff_sync_integrity::{
    Anchor, EntryTypes, HashReference, LinkTypes, LocalHashReference,
};

use super::PerspectiveDiffRetreiver;
use crate::errors::{SocialContextError, SocialContextResult};
use crate::utils::dedup;
use crate::Hash;

pub struct HolochainRetreiver;

impl PerspectiveDiffRetreiver for HolochainRetreiver {
    fn get<T>(hash: Hash) -> SocialContextResult<T>
    where
        T: TryFrom<SerializedBytes, Error = SerializedBytesError>,
    {
        get(hash, GetOptions::latest())?
            .ok_or(SocialContextError::InternalError(
                "HolochainRetreiver: Could not find entry",
            ))?
            .entry()
            .to_app_option::<T>()?
            .ok_or(SocialContextError::InternalError(
                "Expected element to contain app entry data",
            ))
    }

    fn get_with_timestamp<T>(hash: Hash) -> SocialContextResult<(T, DateTime<Utc>)>
    where
        T: TryFrom<SerializedBytes, Error = SerializedBytesError>,
    {
        let element = get(hash, GetOptions::latest())?;
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
            .to_app_option::<T>()?
            .ok_or(SocialContextError::InternalError(
                "Expected element to contain app entry data",
            ))?;
        Ok((entry, timestamp))
    }

    fn create_entry<I, E: std::fmt::Debug, E2>(entry: I) -> SocialContextResult<Hash>
    where
        ScopedEntryDefIndex: for<'a> TryFrom<&'a I, Error = E2>,
        EntryVisibility: for<'a> From<&'a I>,
        Entry: TryFrom<I, Error = E>,
        WasmError: From<E>,
        WasmError: From<E2>,
    {
        create_entry::<I, E, E2>(entry).map_err(|e| SocialContextError::Wasm(e))
    }

    fn current_revision() -> SocialContextResult<Option<LocalHashReference>> {
        let query = query(
            QueryFilter::new()
                .entry_type(EntryType::App(AppEntryDef {
                    entry_index: 4.into(),
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
        let input = GetLinksInputBuilder::try_new(
            latest_root_entry_hash,
            LinkTypes::Index
        )
        .unwrap()
        .build();
        let mut latest_revision_links = get_links(input)?;

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

fn get_latest_revision_anchor() -> Anchor {
    Anchor("latest_revision".to_string())
}

pub fn get_active_agent_anchor() -> Anchor {
    Anchor("active_agent".to_string())
}

pub fn get_active_agents() -> SocialContextResult<Vec<AgentPubKey>> {
    let input = GetLinksInputBuilder::try_new(
        hash_entry(get_active_agent_anchor())?,
        LinkTypes::Index
    )
    .unwrap()
    .tag_prefix(LinkTag::new("active_agent"))
    .build();
    let recent_agents = get_links(input)?;

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
    let me = agent_info()?.agent_latest_pubkey;
    let index = recent_agents.iter().position(|x| *x == me);
    if let Some(index) = index {
        recent_agents.remove(index);
    };

    Ok(recent_agents)
}
