use hdk::prelude::*;

use perspective_diff_sync_integrity::{
    Anchor, EntryTypes, LinkTypes, OnlineAgent, OnlineAgentAndAction, PerspectiveExpression,
};

use crate::errors::{SocialContextError, SocialContextResult};
use crate::retriever::holochain::get_active_agents;
use crate::utils::dedup;

pub fn set_online_status(status: PerspectiveExpression) -> SocialContextResult<()> {
    let entry = EntryTypes::PrivateOnlineStatus(status);
    create_entry(&entry)?;
    Ok(())
}

pub fn get_online_status() -> SocialContextResult<OnlineAgentAndAction> {
    //Try and get my did
    let my_did = get_my_did()?.ok_or(SocialContextError::NoDidFound)?;

    //Try and get my online status
    let query = query(
        QueryFilter::new()
            .entry_type(EntryType::App(AppEntryDef {
                entry_index: 6.into(),
                zome_index: 0.into(),
                visibility: EntryVisibility::Private,
            }))
            .include_entries(true)
            .descending(),
    );

    let status = match query {
        Ok(records) => {
            if records.len() == 0 {
                None
            } else {
                let record = records[0].clone();
                let entry = record
                    .entry
                    .to_app_option::<PerspectiveExpression>()
                    .unwrap()
                    .unwrap();
                Some((entry, record.action_address().to_owned()))
            }
        }
        Err(e) => {
            debug!(
                "PerspectiveDiffSync.current_revision(): Error when getting current revision: {:?}",
                e
            );
            None
        }
    };

    let online_status = OnlineAgentAndAction {
        did: my_did,
        status: status.clone().map(|status| status.0),
        status_action: status.map(|status| status.1),
    };
    Ok(online_status)
}

pub fn create_did_pub_key_link(did: String) -> SocialContextResult<()> {
    debug!("PerspectiveDiffSync.create_did_pub_key_link({:?})", did);
    // TODO: should be agent_latest_pubkey, but that was made unstable behind dpki feature flag
    let agent_key = agent_info()?.agent_initial_pubkey;
    debug!("PerspectiveDiffSync.create_did_pub_key_link() agent_key: {:?}", agent_key);

    // For multi-user support: check if THIS SPECIFIC DID already has a link, not just any DID
    let did_entry = EntryTypes::Anchor(Anchor(did.clone()));
    let did_entry_hash = hash_entry(&did_entry)?;

    let existing_links = get_links(
        LinkQuery::try_new(did_entry_hash.clone(), LinkTypes::DidLink)?,
        GetStrategy::Local
    )?;

    debug!("PerspectiveDiffSync.create_did_pub_key_link() existing_links: {:?}", existing_links);


    // Only create the link if this specific DID doesn't already have one
    if existing_links.len() == 0 {
        debug!("PerspectiveDiffSync.create_did_pub_key_link() creating new link for DID: {:?}", did);
        let _did_entry = create_entry(&did_entry)?;
        create_link(
            agent_key.clone(),
            did_entry_hash.clone(),
            LinkTypes::DidLink,
            LinkTag::new("did_link"),
        )?;
        create_link(
            did_entry_hash,
            agent_key,
            LinkTypes::DidLink,
            LinkTag::new("did_link"),
        )?;
    } else {
        debug!("PerspectiveDiffSync.create_did_pub_key_link() link already exists for DID: {:?}", did);
    }
    Ok(())
}

pub fn get_my_did() -> SocialContextResult<Option<String>> {
    let query = LinkQuery::try_new(
        // TODO: should be agent_latest_pubkey, but that was made unstable behind dpki feature flag
        agent_info()?.agent_initial_pubkey,
        LinkTypes::DidLink
    )?;
    let mut did_links = get_links(query, GetStrategy::Local)?;
    if did_links.len() > 0 {
        let did = get(
            did_links
                .remove(0)
                .target
                .into_entry_hash()
                .expect("Could not get entry_hash"),
            GetOptions::network(),
        )?
        .ok_or(SocialContextError::InternalError(
            "Could not find did entry for given did entry reference",
        ))?
        .entry()
        .to_app_option::<Anchor>()?
        .ok_or(SocialContextError::InternalError(
            "Expected element to contain app entry data",
        ))?;
        Ok(Some(did.0))
    } else {
        Ok(None)
    }
}

pub fn get_dids_agent_key(did: String) -> SocialContextResult<Option<AgentPubKey>> {
    let did_entry = Anchor(did);
    let did_entry_hash = hash_entry(EntryTypes::Anchor(did_entry.clone()))?;
    let query = LinkQuery::try_new(
        did_entry_hash,
        LinkTypes::DidLink
    )?;
    let did_links = get_links(query, GetStrategy::Local)?;
    debug!("PerspectiveDiffSync.get_dids_agent_key() did_links: {:?}", did_links);
    if did_links.len() > 0 {
        let entry: EntryHash = did_links[0].target.clone().try_into().unwrap();
        Ok(Some(AgentPubKey::from(entry)))
    } else {
        Ok(None)
    }
}

pub fn get_agents_did_key(agent: AgentPubKey) -> SocialContextResult<Option<String>> {
    let query = LinkQuery::try_new(
        agent,
        LinkTypes::DidLink
    )?;
    let mut did_links = get_links(query, GetStrategy::Local)?;
    if did_links.len() > 0 {
        let did = get(
            did_links
                .remove(0)
                .target
                .into_entry_hash()
                .expect("Could not get entry_hash"),
            GetOptions::network(),
        )?
        .ok_or(SocialContextError::InternalError(
            "Could not find did entry for given did entry reference",
        ))?
        .entry()
        .to_app_option::<Anchor>()?
        .ok_or(SocialContextError::InternalError(
            "Expected element to contain app entry data",
        ))?;
        Ok(Some(did.0))
    } else {
        Ok(None)
    }
}

/// Get ALL DIDs associated with an agent (for multi-user support)
/// In multi-user scenarios, one Holochain agent can have multiple DIDs
pub fn get_agents_did_keys(agent: AgentPubKey) -> SocialContextResult<Vec<String>> {
    let did_links = get_links(
        LinkQuery::try_new(
            agent,
            LinkTypes::DidLink
        )?,
        GetStrategy::Local
    )?;

    let mut dids = Vec::new();
    for link in did_links {
        let entry_hash = link.target.into_entry_hash();
        if entry_hash.is_none() {
            continue;
        }

        let did_result = get(
            entry_hash.unwrap(),
            GetOptions::network(),
        )?;

        if let Some(record) = did_result {
            if let Some(anchor) = record.entry().to_app_option::<Anchor>()? {
                dids.push(anchor.0);
            }
        }
    }
    
    // Deduplicate DIDs in case multiple agent keys map to the same DID
    let deduped_dids = dedup(&dids);
    Ok(deduped_dids)
}

pub fn get_others() -> SocialContextResult<Vec<String>> {
    // Return ALL DIDs from all active agents (including current agent)
    // The JavaScript layer will filter out the calling user's DID
    let active_agents = get_active_agents()?;
    let my_agent = agent_info()?.agent_initial_pubkey;
    let mut all_dids = Vec::new();

    // Get DIDs from all active agents (remote agents)
    for active_agent in active_agents {
        let agent_dids = get_agents_did_keys(active_agent)?;
        all_dids.extend(agent_dids);
    }

    // Also get ALL DIDs from OUR agent (for multi-user on same node)
    // Don't exclude anything here - JavaScript will filter out the calling user's DID
    let my_agent_dids = get_agents_did_keys(my_agent)?;
    all_dids.extend(my_agent_dids);

    Ok(all_dids)
}

pub fn get_online_agents() -> SocialContextResult<Vec<OnlineAgent>> {
    let active_agents = get_active_agents()?;
    let mut online_agents = Vec::new();
    for active_agent in active_agents {
        let online_agent_status = get_agents_status(active_agent);
        if online_agent_status.is_some() {
            online_agents.push(online_agent_status.unwrap());
        }
    }
    Ok(online_agents)
}

pub fn get_agents_status(agent: AgentPubKey) -> Option<OnlineAgent> {
    let online_agent_status = call_remote(
        agent.clone(),
        "perspective_diff_sync",
        "get_online_status".into(),
        None,
        {},
    );
    if online_agent_status.is_ok() {
        let online_agent_response = online_agent_status.unwrap();
        match online_agent_response {
            ZomeCallResponse::Ok(online_agent) => {
                let online_agent = online_agent
                    .decode::<OnlineAgentAndAction>()
                    .expect("Could not decode online agent");
                Some(OnlineAgent {
                    did: online_agent.did,
                    status: online_agent.status,
                })
            }
            ZomeCallResponse::Unauthorized(..) => {
                debug!("Unauthorized to call agent {}", agent.clone());
                None
            }
            ZomeCallResponse::NetworkError(..) => {
                debug!("Agent {} is offline", agent.clone());
                None
            }
            ZomeCallResponse::CountersigningSession(_) => {
                debug!("Agent {} had countersigning session error", agent);
                None
            }
            ZomeCallResponse::AuthenticationFailed(_, _) => {
                debug!("Agent {} had authentication failed error", agent);
                None
            }
        }
    } else {
        None
    }
}
