//use chrono::Timelike;
use hdk::prelude::*;
use perspective_diff_sync_integrity::{
    EntryTypes, HashBroadcast, LinkTypes, LocalHashReference, PerspectiveDiff, PerspectiveDiffEntryReference,
};

use crate::errors::{SocialContextResult, SocialContextError};
use crate::link_adapter::revisions::{current_revision, update_current_revision};
use crate::link_adapter::snapshots::generate_snapshot;
use crate::retriever::holochain::{get_active_agent_anchor, get_active_agents};
use crate::retriever::PerspectiveDiffRetreiver;
use crate::telepresence::status::get_my_did;
use crate::utils::get_now;
use crate::{Hash, ENABLE_SIGNALS, SNAPSHOT_INTERVAL};

pub fn commit<Retriever: PerspectiveDiffRetreiver>(
    diff: PerspectiveDiff,
) -> SocialContextResult<HoloHash<holo_hash::hash_type::Action>> {
    debug!("===PerspectiveDiffSync.commit(): Function start");
    let now_fn_start = get_now()?.time();
    let initial_current_revision: Option<LocalHashReference> = current_revision::<Retriever>()?;

    let mut entries_since_snapshot = 0;
    if initial_current_revision.is_some() {
        let current = Retriever::get::<PerspectiveDiffEntryReference>(
            initial_current_revision.clone().unwrap().hash,
        )?;
        entries_since_snapshot = current.diffs_since_snapshot;
    };
    debug!(
        "===PerspectiveDiffSync.commit(): Entries since snapshot: {:#?}",
        entries_since_snapshot
    );
    //Add one since we are comitting an entry here
    entries_since_snapshot += 1;

    let create_snapshot_here = if entries_since_snapshot >= *SNAPSHOT_INTERVAL {
        entries_since_snapshot = 0;
        true
    } else {
        false
    };

    let now = get_now()?.time();
    let diff_entry_ref_entry = PerspectiveDiffEntryReference {
        diff: diff.clone(),
        parents: initial_current_revision.clone().map(|val| vec![val.hash]),
        diffs_since_snapshot: entries_since_snapshot,
    };
    let diff_entry_reference = Retriever::create_entry(EntryTypes::PerspectiveDiffEntryReference(
        diff_entry_ref_entry.clone(),
    ))?;
    let after = get_now()?.time();
    // debug!(
    //     "===PerspectiveDiffSync.commit(): Created diff entry ref: {:#?}",
    //     diff_entry_reference
    // );
    debug!(
        "===PerspectiveDiffSync.commit() - Profiling: Took {} to create a PerspectiveDiffEntryReference",
        (after - now).num_milliseconds()
    );

    if create_snapshot_here {
        //fetch all the diff's, we need a new function which will traverse graph and then return + diffs + next found snapshot
        //create new snapshot linked from above diff_entry_reference
        let snapshot = generate_snapshot(diff_entry_reference.clone())?;

        let now = get_now()?.time();
        Retriever::create_entry(EntryTypes::Snapshot(snapshot.clone()))?;
        create_link(
            hash_entry(diff_entry_ref_entry.clone())?,
            hash_entry(snapshot)?,
            LinkTypes::Snapshot,
            LinkTag::new("snapshot"),
        )?;
        let after = get_now()?.time();
        debug!("===PerspectiveDiffSync.commit() - Profiling: Took {} to create snapshot entry and link", (after - now).num_milliseconds());
    };

    let now = get_now()?;
    let now_profile = get_now()?.time();
    //update_latest_revision::<Retriever>(diff_entry_reference.clone(), now.clone())?;
    let after = get_now()?.time();
    debug!(
        "===PerspectiveDiffSync.commit() - Profiling: Took {} to update the latest revision",
        (after - now_profile).num_milliseconds()
    );

    let latest_current_revision: Option<LocalHashReference> = current_revision::<Retriever>()?;
    
    if initial_current_revision.map(|r| format!("{:?}", r)) == latest_current_revision.map(|r| format!("{:?}", r)) {
        update_current_revision::<Retriever>(diff_entry_reference.clone(), now)?;

        if *ENABLE_SIGNALS {
            // let signal_data = PerspectiveDiffReference {
            //     diff,
            //     reference: diff_entry_ref_entry,
            //     reference_hash: diff_entry_reference.clone(),
            // };
            // send_revision_signal(signal_data)?;
            broadcast_current::<Retriever>()?;
        };
    } else {
        // Concurrent update detected; decide how to handle it
        debug!("Concurrent update detected in commit. Aborting commit without updating current revision.");
        return Err(SocialContextError::InternalError("Concurrent update detected in commit"));
    }

    let after_fn_end = get_now()?.time();
    debug!(
        "===PerspectiveDiffSync.commit() - Profiling: Took {} to complete whole commit function",
        (after_fn_end - now_fn_start).num_milliseconds()
    );
    Ok(diff_entry_reference)
}

pub fn add_active_agent_link<Retriever: PerspectiveDiffRetreiver>() -> SocialContextResult<()> {
    debug!("===PerspectiveDiffSync.add_active_agent_link(): Function start");
    let now_fn_start = get_now()?.time();
    let agent_root_entry = get_active_agent_anchor();
    let _agent_root_entry_action =
        Retriever::create_entry(EntryTypes::Anchor(agent_root_entry.clone()))?;

    let agent = agent_info()?.agent_initial_pubkey;
    let agent_root_hash = hash_entry(agent_root_entry)?;
    
    // Check if the link already exists to avoid duplicates
    let query = LinkQuery::try_new(
        agent_root_hash.clone(),
        LinkTypes::Index
    )?
    .tag_prefix(LinkTag::new("active_agent"));
    let existing_links = get_links(query, GetStrategy::Local)?;
    
    // Check if this agent already has an active link
    let link_exists = existing_links.iter().any(|link| {
        link.target.clone().into_agent_pub_key() == Some(agent.clone())
    });
    
    if !link_exists {
        debug!("===PerspectiveDiffSync.add_active_agent_link(): Creating new active agent link");
        create_link(
            agent_root_hash,
            agent,
            LinkTypes::Index,
            LinkTag::new("active_agent"),
        )?;
    } else {
        debug!("===PerspectiveDiffSync.add_active_agent_link(): Link already exists, skipping");
    }
    
    let after_fn_end = get_now()?.time();
    debug!("===PerspectiveDiffSync.add_active_agent_link() - Profiling: Took {} to complete whole add_active_agent_link()", (after_fn_end - now_fn_start).num_milliseconds());
    Ok(())
}

pub fn broadcast_current<Retriever: PerspectiveDiffRetreiver>() -> SocialContextResult<Option<Hash>>
{
    //debug!("Running broadcast_current");
    let current = current_revision::<Retriever>()?;
    //debug!("Current revision: {:#?}", current);

    if current.is_some() {
        let current_revision = current.clone().unwrap();
        let entry_ref =
            Retriever::get::<PerspectiveDiffEntryReference>(current_revision.hash.clone())?;

        let signal_data = HashBroadcast {
            reference: entry_ref,
            reference_hash: current_revision.hash.clone(),
            broadcast_author: get_my_did()?.unwrap(),
        };

        let recent_agents = get_active_agents()?;
        //debug!("Recent agents: {:#?}", recent_agents);
        send_remote_signal(signal_data.get_sb()?, recent_agents.clone())?;
    };
    Ok(current.map(|rev| rev.hash))
}
