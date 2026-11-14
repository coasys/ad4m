//! Neighbourhood WS-native handlers.
//!
//! Every method that names a perspective declares `Guard::Owns` on it, as the
//! `perspective.*` methods do: a user who learns another user's perspective uuid must not
//! publish it (which adds the caller to its owners), signal or broadcast through it, or read
//! who is in it.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::agent::{create_signed_expression, AgentContext};
use crate::membership::{
    self, InviteInfo, JoinMode, JoinRefusalReason, MemberInfo, MemberRole,
    NeighbourhoodJoinRefusedEvent,
};
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
    let caller = membership::caller(&agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    let url = body.url.clone();
    let handle = install_neighbourhood_with_context(body.url, &agent_context)
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    let perspective = crate::perspectives::get_perspective(&handle.uuid)
        .ok_or_else(|| WsRpcError::internal("the joined perspective is gone"))?;

    // A space governed here admits the caller, or the join leaves nothing behind.
    let now = chrono::Utc::now().timestamp_millis().max(0) as u64;
    let decided = membership::join_link(
        perspective.sparql_store.membership(),
        &caller,
        body.invite.as_deref(),
        now,
    );
    let written = match decided {
        Ok(Some(link)) => match perspective
            .clone()
            .add_link(link, LinkStatus::Shared, None, &agent_context)
            .await
        {
            Ok(_) => Ok(()),
            Err(e) => match membership::refusal_of(&e) {
                Some(refusal) => Err(refusal),
                // Not the space refusing the caller: the executor failed, so the join leaves
                // nothing behind and says why, without a join-refused event.
                None => {
                    crate::perspectives::remove_perspective(&handle.uuid).await;
                    return Err(WsRpcError::internal(e.to_string()));
                }
            },
        },
        Ok(None) => Ok(()),
        Err(refusal) => Err(refusal),
    };
    if let Err(refusal) = written {
        crate::perspectives::remove_perspective(&handle.uuid).await;
        let reason = JoinRefusalReason::from(refusal);
        let did = crate::agent::did_for_context(&agent_context).ok();
        crate::pubsub::get_global_pubsub_sync().publish_event(crate::events::Event::new(
            crate::events::Topic::NeighbourhoodJoinRefused,
            crate::events::Audience::agent(did.as_deref()),
            NeighbourhoodJoinRefusedEvent { url, reason },
        ));
        return Err(WsRpcError::forbidden(format!(
            "the space does not admit the caller: {}",
            serde_json::to_value(reason)?.as_str().unwrap_or_default()
        )));
    }

    Ok(serde_json::to_value(handle)?)
}

/// A membership write the owner gate refused is the caller's to fix (403); any other
/// failure is the executor's.
fn membership_error(e: deno_core::error::AnyError) -> WsRpcError {
    match membership::refusal_of(&e) {
        Some(_) => WsRpcError::forbidden(e.to_string()),
        None => WsRpcError::internal(e.to_string()),
    }
}

/// Signs `links` as the caller and writes them to the space's perspective in one diff,
/// through the owner gate.
async fn write_membership(
    params: &Value,
    ctx: &RequestContext,
    links: Vec<Link>,
) -> Result<Value, WsRpcError> {
    let mut perspective = guarded_perspective(params).await?;
    perspective
        .add_links(links, LinkStatus::Shared, None, &ctx.acts_as())
        .await
        .map_err(membership_error)?;
    Ok(Value::Null)
}

/// The person the caller speaks for; a bot key or a key without an identity has none.
async fn caller_person(ctx: &RequestContext) -> Result<String, WsRpcError> {
    let caller = membership::caller(&ctx.acts_as())
        .await
        .map_err(|e| WsRpcError::internal(e.to_string()))?;
    membership::person_of(&caller)
        .map(str::to_string)
        .map_err(|refusal| WsRpcError::forbidden(refusal.to_string()))
}

async fn members(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let perspective = guarded_perspective(&params).await?;
    Ok(serde_json::to_value(
        perspective.sparql_store.membership().members(),
    )?)
}

async fn invites(params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let perspective = guarded_perspective(&params).await?;
    Ok(serde_json::to_value(
        perspective.sparql_store.membership().invites(),
    )?)
}

async fn add_member(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: NeighbourhoodAddMemberParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let role = match body.role {
        NewMemberRole::Admin => MemberRole::Admin,
        NewMemberRole::Member => MemberRole::Member,
    };
    let link =
        membership::links::add_member(&body.person, role).map_err(WsRpcError::bad_request)?;
    write_membership(&params, &ctx, vec![link]).await
}

async fn remove_member(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: NeighbourhoodPersonParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    write_membership(
        &params,
        &ctx,
        vec![membership::links::removed(&body.person)],
    )
    .await
}

async fn leave(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let person = caller_person(&ctx).await?;
    write_membership(&params, &ctx, vec![membership::links::removed(&person)]).await
}

async fn create_invite(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: NeighbourhoodCreateInviteParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let expires_at = match body.expires_at.as_deref() {
        Some(at) => membership::ms_of(at).ok_or_else(|| {
            WsRpcError::bad_request(format!("expiresAt is not an ISO 8601 time: {at}"))
        })?,
        None => membership::NO_EXPIRY,
    };
    let (invite, link) = membership::links::invite(expires_at);
    let perspective = guarded_perspective(&params).await?;
    let neighbourhood_url = perspective
        .persisted
        .lock()
        .await
        .shared_url
        .clone()
        .ok_or_else(|| WsRpcError::bad_request("the perspective is not a neighbourhood"))?;
    write_membership(&params, &ctx, vec![link]).await?;
    let url = membership::invite_url(&neighbourhood_url, &invite);
    Ok(serde_json::to_value(CreatedInvite { invite, url })?)
}

async fn set_join_mode(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: NeighbourhoodJoinModeParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    write_membership(&params, &ctx, vec![membership::links::join_mode(body.join)]).await
}

/// The caller (a person, not a bot key) proposes and approves `botDid` as its bot.
async fn add_bot(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: NeighbourhoodBotParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let owner = caller_person(&ctx).await?;
    let links = membership::links::add_bot(&body.bot_did, &owner).to_vec();
    write_membership(&params, &ctx, links).await
}

async fn remove_bot(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: NeighbourhoodBotParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    write_membership(
        &params,
        &ctx,
        vec![membership::links::removed(&body.bot_did)],
    )
    .await
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
    // Membership (`crate::membership`): the reads answer from the space's replay; each write
    // is a membership link the caller signs, which the owner gate admits or refuses (403).
    map.method::<NeighbourhoodUuidParams, Vec<MemberInfo>>("neighbourhood.members", members)
        .read()
        .guard(Guard::perspective(neighbourhood_read));
    map.method::<NeighbourhoodUuidParams, Vec<InviteInfo>>("neighbourhood.invites", invites)
        .read()
        .guard(Guard::perspective(neighbourhood_read));
    map.method::<NeighbourhoodAddMemberParams, ()>("neighbourhood.addMember", add_member)
        .guard(Guard::perspective(neighbourhood_update));
    map.method::<NeighbourhoodPersonParams, ()>("neighbourhood.removeMember", remove_member)
        .guard(Guard::perspective(neighbourhood_update));
    map.method::<NeighbourhoodUuidParams, ()>("neighbourhood.leave", leave)
        .guard(Guard::perspective(neighbourhood_update));
    map.method::<NeighbourhoodCreateInviteParams, CreatedInvite>(
        "neighbourhood.createInvite",
        create_invite,
    )
    .guard(Guard::perspective(neighbourhood_update));
    map.method::<NeighbourhoodJoinModeParams, ()>("neighbourhood.setJoinMode", set_join_mode)
        .guard(Guard::perspective(neighbourhood_update));
    map.method::<NeighbourhoodBotParams, ()>("neighbourhood.addBot", add_bot)
        .guard(Guard::perspective(neighbourhood_update));
    map.method::<NeighbourhoodBotParams, ()>("neighbourhood.removeBot", remove_bot)
        .guard(Guard::perspective(neighbourhood_update));
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

/// The role `neighbourhood.addMember` gives: a bot is added with `neighbourhood.addBot`.
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub enum NewMemberRole {
    Admin,
    Member,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodAddMemberParams {
    pub uuid: String,
    /// The person's DID (`did:scid:coasys:1:…`).
    pub person: String,
    pub role: NewMemberRole,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodPersonParams {
    pub uuid: String,
    pub person: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodCreateInviteParams {
    pub uuid: String,
    /// When the invite expires (ISO 8601); without it, never.
    #[ts(optional)]
    pub expires_at: Option<String>,
}

/// A new invite and its deep link (`coasys://join?url=…&invite=…`).
#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreatedInvite {
    pub invite: String,
    pub url: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodJoinModeParams {
    pub uuid: String,
    pub join: JoinMode,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct NeighbourhoodBotParams {
    pub uuid: String,
    /// The bot's key (`did:key:…`).
    pub bot_did: String,
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
        assert_eq!(
            received.data.links[0].verdict,
            Some(crate::identity::verdict::LinkVerdict::Valid),
            "link verdict"
        );
        assert_eq!(received.proof.valid, Some(true), "perspective signature");
    }

    // `sendSignal` / `setOnlineStatus` with a link the caller built: the executor
    // passes it on as given, and the receiver marks a bad signature invalid. The
    // SDK's telepresence payloads carry that verdict.
    #[test]
    fn a_forged_link_in_a_payload_comes_back_invalid() {
        use crate::identity::verdict::{InvalidReason, LinkVerdict};
        crate::test_utils::setup_wallet();
        crate::test_utils::setup_agent();
        let signal: PerspectiveExpression = serde_json::from_value(json!({
            "author": "did:test",
            "timestamp": "2026-10-30T00:00:00.000Z",
            "data": { "links": [{
                "author": "did:test",
                "timestamp": "2026-10-30T00:00:00.000Z",
                "data": { "source": "alice", "target": "bob", "predicate": "signal" },
                "proof": { "key": "key", "signature": "sig" },
            }]},
            "proof": { "key": "key", "signature": "sig" },
        }))
        .unwrap();

        let mut received = through_link_language(&signal);
        received.verify_signatures();

        let link = &received.data.links[0];
        assert_eq!(link.proof.valid, Some(false));
        assert_eq!(link.proof.invalid, Some(true));
        assert_eq!(
            link.verdict,
            Some(LinkVerdict::Invalid(InvalidReason::BadSignature))
        );
        assert_eq!(
            serde_json::to_value(&link.verdict).unwrap(),
            json!({ "kind": "invalid", "reason": "badSignature" })
        );
    }
}
