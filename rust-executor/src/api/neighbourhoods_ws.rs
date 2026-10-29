//! Neighbourhood WS-native handlers.
//!
//! Every method that names a perspective declares `Guard::Owns` on it, as the
//! `perspective.*` methods do: a user who learns another user's perspective uuid must not
//! publish it (which adds the caller to its owners), signal or broadcast through it, or read
//! who is in it.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::agent::{create_signed_expression, AgentContext};
use crate::neighbourhoods::{self, install_neighbourhood_with_context};
use crate::types::domain::Perspective as DomainPerspective;
use crate::types::*;

use super::guards::{Guard, IdParam, Owner};
use super::perspectives_ws::guarded_perspective;
use super::types::*;
use super::ws_handler::{HandlerMap, WsRpcError};

// The neighbourhood capabilities name no perspective; the guard's rule adds the owner check.
fn neighbourhood_create(_: Vec<String>) -> Capability {
    NEIGHBOURHOOD_CREATE_CAPABILITY.clone()
}
fn neighbourhood_read(_: Vec<String>) -> Capability {
    NEIGHBOURHOOD_READ_CAPABILITY.clone()
}
fn neighbourhood_update(_: Vec<String>) -> Capability {
    NEIGHBOURHOOD_UPDATE_CAPABILITY.clone()
}

/// The perspective `neighbourhood.publish` shares: `perspectiveUuid` (or the older
/// `perspectiveUUID`).
const PUBLISHED_PERSPECTIVE: IdParam = IdParam::FirstOf(&["perspectiveUuid", "perspectiveUUID"]);

async fn join_neighbourhood(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: JoinNeighbourhoodRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = ctx.acts_as();
    let handle = install_neighbourhood_with_context(body.url, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(handle)?)
}

async fn publish_neighbourhood(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    // The perspective the guard checked.
    let perspective_uuid = PUBLISHED_PERSPECTIVE.require(&params)?;
    let body: PublishNeighbourhoodRequest = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = ctx.acts_as();
    let url = neighbourhoods::neighbourhood_publish_from_perspective_with_context(
        &perspective_uuid,
        body.link_language,
        body.meta,
        &agent_context,
    )
    .await
    .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::String(url))
}

fn build_signed_perspective(
    payload: &Value,
    signed: bool,
    agent_context: &AgentContext,
) -> Result<PerspectiveExpression, WsRpcError> {
    if signed {
        let perspective_input: crate::types::PerspectiveInput =
            serde_json::from_value(payload.clone()).map_err(|e| {
                WsRpcError::bad_request(format!("Invalid perspective input: {}", e))
            })?;
        let perspective = DomainPerspective::from(perspective_input);
        let signed = create_signed_expression(perspective, agent_context)
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        Ok(PerspectiveExpression::from(signed))
    } else {
        let unsigned: PerspectiveUnsignedInput =
            serde_json::from_value(payload.clone()).map_err(|e| {
                WsRpcError::bad_request(format!("Invalid unsigned perspective input: {}", e))
            })?;
        let links: Vec<DecoratedLinkExpression> = unsigned
            .links
            .into_iter()
            .map(|l| Link::from(l).normalize())
            .map(|l| create_signed_expression(l, agent_context))
            .filter_map(Result::ok)
            .map(LinkExpression::from)
            // No `status`: the perspective signature below must cover only
            // what reaches the receiver, and a link language carries a
            // signal's links without one (p-diff-sync's zome type has no such
            // field). Signed with it, every signal from another node failed
            // verification there.
            .map(|l| DecoratedLinkExpression {
                status: None,
                ..DecoratedLinkExpression::from((l, LinkStatus::Shared))
            })
            .collect();
        let perspective = DomainPerspective { links };
        let signed = create_signed_expression(perspective, agent_context)
            .map_err(|e| WsRpcError::internal(e.to_string()))?;
        Ok(PerspectiveExpression::from(signed))
    }
}

async fn send_broadcast(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: BroadcastRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = ctx.acts_as();
    let perspective_instance = guarded_perspective(&params).await?;

    let signed_perspective =
        build_signed_perspective(&body.payload, body.signed.unwrap_or(true), &agent_context)?;

    perspective_instance
        .send_broadcast(signed_perspective, body.loopback.unwrap_or(false))
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn send_signal(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: SignalRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = ctx.acts_as();
    let perspective_instance = guarded_perspective(&params).await?;

    let signed_perspective =
        build_signed_perspective(&body.payload, body.signed.unwrap_or(true), &agent_context)?;

    perspective_instance
        .send_signal(body.remote_agent_did, signed_perspective)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

async fn set_online_status(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: SetOnlineStatusRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;

    let agent_context = ctx.acts_as();
    let perspective_instance = guarded_perspective(&params).await?;

    let signed_perspective =
        build_signed_perspective(&body.status, body.signed.unwrap_or(true), &agent_context)?;

    perspective_instance
        .set_online_status(signed_perspective, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(Value::Bool(true))
}

/// The caller's own online status in the neighbourhood, as it last set it
/// here, or `null`. Lets a client change part of its status without
/// replacing the rest.
async fn own_online_status(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let perspective = guarded_perspective(&params).await?;
    let did = crate::agent::did_for_context(&ctx.acts_as())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(
        perspective.online_status_of(&did).await,
    )?)
}

async fn has_telepresence(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let perspective = guarded_perspective(&params).await?;
    Ok(Value::Bool(perspective.has_telepresence_adapter().await))
}

async fn online_agents(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let perspective = guarded_perspective(&params).await?;

    let agents = perspective
        .online_agents()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    Ok(serde_json::to_value(agents)?)
}

/// The other agents in the neighbourhood: every member but the caller's own agent.
async fn other_agents(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let perspective = guarded_perspective(&params).await?;
    let current_user_did = crate::agent::did_for_context(&ctx.acts_as())
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let all_dids = perspective
        .others()
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;

    let others: Vec<String> = all_dids
        .into_iter()
        .filter(|did| did != &current_user_did)
        .collect();

    Ok(serde_json::to_value(others)?)
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    // The caller joins and publishes as itself (`ctx.acts_as()`).
    map.method::<JoinNeighbourhoodRequest, PerspectiveHandle>(
        "neighbourhood.join",
        join_neighbourhood,
    )
    .long()
    .guard(Guard::Cap(NEIGHBOURHOOD_READ_CAPABILITY.clone()));
    // Publishing adds the caller to the perspective's owners, so it needs the perspective.
    map.method::<PublishNeighbourhoodRequest, String>(
        "neighbourhood.publish",
        publish_neighbourhood,
    )
    .long()
    .guard(Guard::Owns {
        cap: neighbourhood_create,
        id: PUBLISHED_PERSPECTIVE,
        owner: Owner::Perspective,
    });
    // The perspective that holds the neighbourhood decides who may use it.
    map.method::<NeighbourhoodBroadcastParams, bool>("neighbourhood.sendBroadcast", send_broadcast)
        .guard(Guard::perspective(neighbourhood_update));
    map.method::<NeighbourhoodSignalParams, bool>("neighbourhood.sendSignal", send_signal)
        .guard(Guard::perspective(neighbourhood_update));
    map.method::<NeighbourhoodOnlineStatusParams, bool>(
        "neighbourhood.setOnlineStatus",
        set_online_status,
    )
    .guard(Guard::perspective(neighbourhood_update));
    map.method::<NeighbourhoodUuidParams, Option<PerspectiveExpression>>(
        "neighbourhood.ownOnlineStatus",
        own_online_status,
    )
    .read()
    .guard(Guard::perspective(neighbourhood_read));
    map.method::<NeighbourhoodUuidParams, bool>("neighbourhood.hasTelepresence", has_telepresence)
        .read()
        .guard(Guard::perspective(neighbourhood_read));
    map.method::<NeighbourhoodUuidParams, Vec<OnlineAgent>>(
        "neighbourhood.onlineAgents",
        online_agents,
    )
    .read()
    .guard(Guard::perspective(neighbourhood_read));
    map.method::<NeighbourhoodUuidParams, Vec<String>>("neighbourhood.otherAgents", other_agents)
        .read()
        .guard(Guard::perspective(neighbourhood_read));
}

// ── Contracts ──

/// A perspective to sign and send: signed links when `signed` is true or
/// absent, bare links when `signed` is false.
#[derive(Deserialize, TS)]
#[serde(untagged)]
#[ts(export)]
pub enum NeighbourhoodPerspectivePayload {
    Signed(NeighbourhoodSignedPerspective),
    Unsigned(PerspectiveUnsignedInput),
}

/// Wire shape of `crate::types::PerspectiveInput`, named apart from the
/// api `PerspectiveInput` so the two TypeScript files do not collide.
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodSignedPerspective {
    pub links: Vec<LinkExpressionInput>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodUuidParams {
    pub uuid: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodBroadcastParams {
    pub uuid: String,
    pub payload: NeighbourhoodPerspectivePayload,
    #[ts(optional)]
    pub signed: Option<bool>,
    #[ts(optional)]
    pub loopback: Option<bool>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodSignalParams {
    pub uuid: String,
    pub remote_agent_did: String,
    pub payload: NeighbourhoodPerspectivePayload,
    #[ts(optional)]
    pub signed: Option<bool>,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodOnlineStatusParams {
    pub uuid: String,
    pub status: NeighbourhoodPerspectivePayload,
    #[ts(optional)]
    pub signed: Option<bool>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// A signal as a link language delivers it on another node: p-diff-sync's
    /// zome carries each link as author, data, timestamp and a proof of key and
    /// signature, so nothing else survives the trip.
    fn through_link_language(signal: &PerspectiveExpression) -> PerspectiveExpression {
        let mut value = serde_json::to_value(signal).unwrap();
        for link in value["data"]["links"].as_array_mut().unwrap() {
            let link = link.as_object_mut().unwrap();
            link.retain(|key, _| ["author", "data", "timestamp", "proof"].contains(&key.as_str()));
            let proof = link["proof"].as_object_mut().unwrap();
            proof.retain(|key, _| ["key", "signature"].contains(&key.as_str()));
        }
        serde_json::from_value(value).unwrap()
    }

    // `sendBroadcastU` / `sendSignalU` / `setOnlineStatusU`: the executor signs
    // the links and the perspective. A remote receiver must verify both.
    #[test]
    fn an_unsigned_payload_verifies_after_crossing_a_link_language() {
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let payload = json!({ "links": [
            { "source": "{\"kind\":\"description\"}", "predicate": "ad4m://session/rtc", "target": "*" },
        ]});
        let signal = build_signed_perspective(&payload, false, &AgentContext::main_agent())
            .unwrap_or_else(|e| panic!("signing failed: {}", e.message));

        let mut received = through_link_language(&signal);
        received.verify_signatures();

        assert_eq!(
            received.data.links[0].proof.valid,
            Some(true),
            "link signature"
        );
        assert_eq!(received.proof.valid, Some(true), "perspective signature");
    }
}
