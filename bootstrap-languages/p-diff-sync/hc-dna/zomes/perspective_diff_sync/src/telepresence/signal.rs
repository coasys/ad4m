use hdk::prelude::*;
use perspective_diff_sync_integrity::PerspectiveExpression;

use super::status::get_dids_agent_key;
use crate::retriever::holochain::get_active_agents;
use crate::{errors::SocialContextResult, inputs::SignalData};

pub fn send_signal(signal_data: SignalData) -> SocialContextResult<PerspectiveExpression> {
    let agent = get_dids_agent_key(signal_data.remote_agent_did.clone())?;
    // debug!(
    //     "PerspectiveDiffSync.send_signal() to DID: {:?} / HC: {:?}",
    //     signal_data.remote_agent_did, agent
    // );
    match agent {
        Some(agent) => send_remote_signal(signal_data.payload.clone().get_sb()?, vec![agent])?,
        None => {
            debug!("PerspectiveDiffSync.send_signal(): Could not send signal since we could not get the agents pub key from did");
        }
    }
    Ok(signal_data.payload)
}

pub fn send_broadcast(data: PerspectiveExpression) -> SocialContextResult<PerspectiveExpression> {
    let active_agents = get_active_agents()?;

    //debug!("PerspectiveDiffSync.send_broadcast() to: {:?}", active_agents);
    send_remote_signal(data.clone().get_sb()?, active_agents)?;

    Ok(data)
}
