//! The `host.*` methods (AR-05 4.13): an executor as a host. Joining is open to anyone the
//! gate admits; usage and leave are the tenant's own; the rest is the operator's.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::hosting::live::LiveEnv;
use crate::hosting::meter::{self, TenantUsage};
use crate::hosting::policy::{self, HostPolicyPatch, HostPolicyView, SignupPolicy};
use crate::hosting::tenants::{self, HostJoinStatus, JoinRequest, JoinResult, Tenant};
use crate::hosting::{HostEnv, HostRefusal, LeaveResult};
use crate::types::RequestContext;

use super::guards::Guard;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};

/// `host.info`: what anyone may know before joining.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostInfo {
    pub multi_user: bool,
    pub signup: SignupPolicy,
    pub name: String,
    pub run_by_coasys: bool,
    /// The tenant cap is reached.
    pub full: bool,
}

/// `host.joinStatus`'s params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostJoinStatusParams {
    pub join_handle: String,
}

/// `host.leave`'s params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostLeaveParams {
    /// Keep the tenant's data for a later rejoin; false destroys its data key.
    pub keep_data: bool,
}

/// `host.createInvite`'s params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateInviteParams {
    /// 1 to 30, default 7.
    pub expires_in_days: Option<u32>,
    pub note: Option<String>,
}

/// `host.createInvite`'s answer: the secret is shown once.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct CreateInviteResult {
    pub invite_id: String,
    /// Base64url; the invite link carries it in its fragment.
    pub secret: String,
    #[ts(type = "number")]
    pub expires_at: u64,
}

/// An invite as the operator lists it (never its secret).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct InviteView {
    pub id: String,
    #[ts(type = "number")]
    pub created_at: u64,
    #[ts(type = "number")]
    pub expires_at: u64,
    /// The tenant record the invite admitted.
    pub used_by: Option<String>,
    pub revoked: bool,
    pub note: Option<String>,
}

/// `host.revokeInvite`'s params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RevokeInviteParams {
    pub invite_id: String,
}

/// A tenant as the operator lists it.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct TenantView {
    pub tenant_id: String,
    pub did_scid: String,
    /// `pending`, `active`, `held`, `detached` or `deleted`.
    pub state: String,
    #[ts(type = "number")]
    pub created_at: u64,
    #[ts(type = "number | null")]
    pub activated_at: Option<u64>,
    #[ts(type = "number | null")]
    pub detached_at: Option<u64>,
}

impl From<Tenant> for TenantView {
    fn from(t: Tenant) -> Self {
        TenantView {
            tenant_id: t.row.id,
            did_scid: t.row.did_scid,
            state: t.state.as_str().to_string(),
            created_at: t.row.created_at,
            activated_at: t.row.activated_at,
            detached_at: t.row.detached_at,
        }
    }
}

/// `host.removeTenant`'s params.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RemoveTenantParams {
    pub tenant_id: String,
}

/// `host.removeTenant`'s answer.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RemoveTenantResult {
    /// False when no record of that id was in a state to remove.
    pub removed: bool,
}

fn value<T: Serialize>(v: T) -> Result<Value, WsRpcError> {
    serde_json::to_value(v).map_err(|e| WsRpcError::internal(e.to_string()))
}

fn params<T: serde::de::DeserializeOwned>(v: Value) -> Result<T, WsRpcError> {
    serde_json::from_value(v).map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))
}

/// The live tenant record the session acts as: `not-a-tenant` for the operator, the main
/// agent and any agent no record owns.
fn session_tenant(ctx: &RequestContext) -> Result<Tenant, HostRefusal> {
    let did = ctx.user_did().ok_or(HostRefusal::NotATenant)?;
    tenants::live_tenant_of_device(did)?.ok_or(HostRefusal::NotATenant)
}

async fn info(_params: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let env = LiveEnv;
    let policy = policy::current()?;
    value(HostInfo {
        multi_user: env.multi_user(),
        signup: policy.signup,
        name: policy.display.name,
        run_by_coasys: policy.display.run_by_coasys,
        full: tenants::is_full()?,
    })
}

async fn join(raw: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let request: JoinRequest = params(raw)?;
    let peer = ctx
        .peer
        .map(|p| p.to_string())
        .unwrap_or_else(|| "local".into());
    let result: JoinResult =
        tenants::join(&LiveEnv, tenants::join_limiter(), &peer, request).await?;
    value(result)
}

async fn join_status(raw: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: HostJoinStatusParams = params(raw)?;
    let status: HostJoinStatus = tenants::join_status(&p.join_handle)?;
    value(status)
}

async fn usage(_raw: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    value(meter::usage(&session_tenant(&ctx)?).await?)
}

async fn leave(raw: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: HostLeaveParams = params(raw)?;
    let t = session_tenant(&ctx)?;
    let result: LeaveResult = tenants::leave(&LiveEnv, &t.row.id, p.keep_data).await?;
    value(result)
}

async fn get_policy(_raw: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    value(policy::view()?)
}

async fn set_policy(raw: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let patch: HostPolicyPatch = params(raw)?;
    let view: HostPolicyView = policy::set(patch)?;
    crate::pubsub::get_global_pubsub_sync().publish_event(crate::events::Event::new(
        crate::events::Topic::HostPolicyChanged,
        crate::events::Audience::Operator,
        view.clone(),
    ));
    value(view)
}

async fn create_invite(raw: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: CreateInviteParams = params(raw)?;
    let (id, secret) = tenants::create_invite(&LiveEnv, "operator", p.expires_in_days, p.note)?;
    let expires_at = tenants::list_invites()?
        .into_iter()
        .find(|i| i.id == id)
        .map(|i| i.expires_at)
        .ok_or_else(|| WsRpcError::internal("the invite was not stored"))?;
    value(CreateInviteResult {
        invite_id: id,
        secret,
        expires_at,
    })
}

async fn list_invites(_raw: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let invites: Vec<InviteView> = tenants::list_invites()?
        .into_iter()
        .map(|i| InviteView {
            id: i.id,
            created_at: i.created_at,
            expires_at: i.expires_at,
            used_by: i.used_by,
            revoked: i.revoked,
            note: i.note,
        })
        .collect();
    value(invites)
}

async fn revoke_invite(raw: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: RevokeInviteParams = params(raw)?;
    value(tenants::revoke_invite(&p.invite_id)?)
}

async fn list_tenants(_raw: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let list: Vec<TenantView> = tenants::live_tenants()?
        .into_iter()
        .map(TenantView::from)
        .collect();
    value(list)
}

async fn remove_tenant(raw: Value, _ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let p: RemoveTenantParams = params(raw)?;
    let removed = tenants::remove_tenant(&LiveEnv, &p.tenant_id).await?;
    value(RemoveTenantResult { removed })
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<NoParams, HostInfo>("host.info", info)
        .read()
        .guard(Guard::Open);
    map.method::<JoinRequest, JoinResult>("host.join", join)
        .guard(Guard::Open);
    map.method::<HostJoinStatusParams, HostJoinStatus>("host.joinStatus", join_status)
        .read()
        .guard(Guard::Open);
    map.method::<NoParams, TenantUsage>("host.usage", usage)
        .read()
        .guard(Guard::Cap(RUNTIME_HOSTING_READ_CAPABILITY.clone()));
    map.method::<HostLeaveParams, LeaveResult>("host.leave", leave)
        .long()
        .guard(Guard::Cap(RUNTIME_HOSTING_UPDATE_CAPABILITY.clone()));
    map.method::<NoParams, HostPolicyView>("host.policy", get_policy)
        .read()
        .guard(Guard::Operator);
    map.method::<HostPolicyPatch, HostPolicyView>("host.setPolicy", set_policy)
        .guard(Guard::Operator);
    map.method::<CreateInviteParams, CreateInviteResult>("host.createInvite", create_invite)
        .guard(Guard::Operator);
    map.method::<NoParams, Vec<InviteView>>("host.listInvites", list_invites)
        .read()
        .guard(Guard::Operator);
    map.method::<RevokeInviteParams, bool>("host.revokeInvite", revoke_invite)
        .guard(Guard::Operator);
    map.method::<NoParams, Vec<TenantView>>("host.tenants", list_tenants)
        .read()
        .guard(Guard::Operator);
    map.method::<RemoveTenantParams, RemoveTenantResult>("host.removeTenant", remove_tenant)
        .long()
        .guard(Guard::Operator);
}
