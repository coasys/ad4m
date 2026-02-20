use hdk::prelude::*;
use perspective_diff_sync_integrity::{PerspectiveExpression, RoutedSignalPayload};

use super::status::get_dids_agent_key;
use crate::retriever::holochain::get_active_agents;
use crate::{
    errors::{SocialContextError, SocialContextResult},
    inputs::SignalData,
};

pub fn send_signal(signal_data: SignalData) -> SocialContextResult<PerspectiveExpression> {
    debug!(
        "send_signal: Sending to DID: {}",
        signal_data.remote_agent_did
    );
    let agent = get_dids_agent_key(signal_data.remote_agent_did.clone())?;

    match agent {
        Some(agent_pubkey) => {
            debug!(
                "send_signal: Found AgentPubKey {:?} for DID {}",
                agent_pubkey, signal_data.remote_agent_did
            );
            // Create flattened routed payload with recipient DID for multi-user routing
            let routed_payload = RoutedSignalPayload {
                recipient_did: signal_data.remote_agent_did,
                author: signal_data.payload.author.clone(),
                data: signal_data.payload.data.clone(),
                timestamp: signal_data.payload.timestamp,
                proof: signal_data.payload.proof.clone(),
            };

            debug!(
                "send_signal: Calling send_remote_signal to AgentPubKey {:?}",
                agent_pubkey
            );
            send_remote_signal(routed_payload.get_sb()?, vec![agent_pubkey])?;
            debug!("send_signal: Successfully sent signal");
            Ok(signal_data.payload)
        }
        None => {
            debug!(
                "send_signal: ERROR - No AgentPubKey found for DID {}",
                signal_data.remote_agent_did
            );
            Err(SocialContextError::InternalError(
                "Could not send signal - no AgentPubKey found for DID",
            ))
        }
    }
}

pub fn send_broadcast(data: PerspectiveExpression) -> SocialContextResult<PerspectiveExpression> {
    let active_agents = get_active_agents()?;

    //debug!("PerspectiveDiffSync.send_broadcast() to: {:?}", active_agents);
    send_remote_signal(data.clone().get_sb()?, active_agents)?;

    Ok(data)
}
