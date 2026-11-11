//! `agent.session*` — the agent-session calls: conversations with the executor's agent,
//! kept in the executor's own database (`agent_sessions/`).
//!
//! A session names the agent's capabilities it may use: the turn's tools run as an
//! in-process session of the session's agent, narrowed to what those and the caller's
//! both allow, so a read-only session's write tool answers an error, and a perspective
//! outside the set is not reached. `agent.send` stores the person's message and starts
//! the turn as a detached task — it returns at once; `agent-session-changed` events tell
//! the owner as each message, status change and tool step lands.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::agent_sessions::service::{self, SessionError};
use crate::agent_sessions::types::AgentSession;
use crate::types::*;

use super::guards::{Guard, IdParam, Owner};
use super::perspectives_ws::check_credits;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};

/// Where the session-id-carrying methods read their id: `sessionId` (`send`, `cancel`,
/// `messages`) or `id` (`session`, `sessionUpdate`, `sessionDelete`).
const SESSION_ID_PARAM: IdParam = IdParam::FirstOf(&["sessionId", "id"]);

/// What an [`SessionError`] is on the wire: missing and another agent's sessions are one
/// answer (404, the `owns` rule).
fn session_error(e: SessionError) -> WsRpcError {
    match e {
        SessionError::NotFound(_) => WsRpcError::not_found(e.to_string()),
        SessionError::Busy | SessionError::TooManyTurns => WsRpcError {
            code: 409,
            message: e.to_string(),
        },
        SessionError::Invalid(_) => WsRpcError::bad_request(e.to_string()),
        SessionError::Failed(e) => WsRpcError::internal(format!("{e:#}")),
    }
}

async fn session_create(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: service::AgentSessionCreate = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    service::create(&ctx, body)
        .map_err(session_error)
        .and_then(|s| serde_json::to_value(s).map_err(WsRpcError::from))
}

async fn session_list(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    service::list(&ctx)
        .map_err(session_error)
        .and_then(|s| serde_json::to_value(s).map_err(WsRpcError::from))
}

async fn session_get(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let id = SESSION_ID_PARAM.require(&params)?;
    service::get(&ctx, &id)
        .map_err(session_error)
        .and_then(|s| serde_json::to_value(s).map_err(WsRpcError::from))
}

async fn session_update(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AgentSessionUpdateRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    service::update(&ctx, &body.id, body.update)
        .map_err(session_error)
        .and_then(|s| serde_json::to_value(s).map_err(WsRpcError::from))
}

async fn session_delete(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let id = SESSION_ID_PARAM.require(&params)?;
    service::delete(&ctx, &id)
        .map_err(session_error)
        .map(Value::Bool)
}

async fn send(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AgentSendParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    check_credits(ctx.user_did())?;
    service::send(&ctx, &body.session_id, &body.text, Default::default())
        .await
        .map_err(session_error)
        .and_then(|s| serde_json::to_value(s).map_err(WsRpcError::from))
}

async fn cancel(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let session_id = SESSION_ID_PARAM.require(&params)?;
    service::cancel(&ctx, &session_id)
        .map_err(session_error)
        .map(Value::Bool)
}

async fn messages(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AgentSessionMessagesParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    service::messages(&ctx, &body.session_id, body.limit, body.before)
        .map_err(session_error)
        .and_then(|m| serde_json::to_value(m).map_err(WsRpcError::from))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<service::AgentSessionCreate, AgentSession>("agent.sessionCreate", session_create)
        .guard(Guard::Cap(AGENT_CREATE_CAPABILITY.clone()));
    map.method::<NoParams, Vec<AgentSession>>("agent.sessionList", session_list)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    map.method::<AgentSessionIdParams, AgentSession>("agent.session", session_get)
        .read()
        .guard(Guard::Owns {
            cap: |_| AGENT_READ_CAPABILITY.clone(),
            id: SESSION_ID_PARAM,
            owner: Owner::AgentSession,
        });
    map.method::<AgentSessionUpdateRequest, AgentSession>("agent.sessionUpdate", session_update)
        .guard(Guard::Owns {
            cap: |_| AGENT_UPDATE_CAPABILITY.clone(),
            id: SESSION_ID_PARAM,
            owner: Owner::AgentSession,
        });
    map.method::<AgentSessionIdParams, bool>("agent.sessionDelete", session_delete)
        .guard(Guard::Owns {
            cap: |_| AGENT_UPDATE_CAPABILITY.clone(),
            id: SESSION_ID_PARAM,
            owner: Owner::AgentSession,
        });
    map.method::<AgentSendParams, service::AgentSendStarted>("agent.send", send)
        .guard(Guard::Owns {
            cap: |_| AGENT_UPDATE_CAPABILITY.clone(),
            id: SESSION_ID_PARAM,
            owner: Owner::AgentSession,
        });
    map.method::<AgentCancelParams, bool>("agent.cancel", cancel)
        .guard(Guard::Owns {
            cap: |_| AGENT_UPDATE_CAPABILITY.clone(),
            id: SESSION_ID_PARAM,
            owner: Owner::AgentSession,
        });
    map.method::<AgentSessionMessagesParams, Vec<crate::agent_sessions::types::AgentSessionMessage>>(
        "agent.messages",
        messages,
    )
    .read()
    .guard(Guard::Owns {
        cap: |_| AGENT_READ_CAPABILITY.clone(),
        id: SESSION_ID_PARAM,
        owner: Owner::AgentSession,
    });
}

// ── Contracts ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSessionIdParams {
    /// The session's id.
    pub id: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSendParams {
    /// The session to send on.
    pub session_id: String,
    /// What the person says.
    pub text: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentCancelParams {
    /// The session whose turn ends.
    pub session_id: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSessionMessagesParams {
    /// The session to read.
    pub session_id: String,
    /// How many entries at most (200 default).
    pub limit: Option<u32>,
    /// Only entries older than this `seq`.
    #[ts(type = "number | null")]
    pub before: Option<i64>,
}

/// `agent.sessionUpdate`'s params: the session's id and the fields to change, flat.
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSessionUpdateRequest {
    /// The session's id.
    pub id: String,
    /// The fields to change.
    #[serde(flatten)]
    #[ts(flatten)]
    pub update: service::AgentSessionUpdateParams,
}
