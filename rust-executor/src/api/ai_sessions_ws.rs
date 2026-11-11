//! `ai.session*` — the AI-session calls: conversations with the executor's AI,
//! kept in the executor's own database (`ai_sessions/`).
//!
//! A session names the agent's capabilities it may use: the turn's tools run as an
//! in-process session of the session's agent, narrowed to what those and the caller's
//! both allow, so a read-only session's write tool answers an error, and a perspective
//! outside the set is not reached. `ai.sessionSend` stores the person's message and starts
//! the turn as a detached task — it returns at once; `ai-session-changed` events tell
//! the owner as each message, status change and tool step lands.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::ai_sessions::service::{self, SessionError};
use crate::ai_sessions::types::AiSession;
use crate::types::*;

use super::ai_ws::AiIdParams;
use super::guards::{Guard, IdParam, Owner};
use super::perspectives_ws::check_credits;
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};

/// Where a method reads the session it acts on: `id` for the calls on the session itself
/// (`session`, `sessionUpdate`, `sessionDelete`), `sessionId` for the calls on its turns
/// and transcript (`send`, `cancel`, `messages`). The guard and the handler of a method
/// read the same one, so the session the guard checked is the session the handler touches.
const ID_PARAM: IdParam = IdParam::Field(&["id"]);
const SESSION_ID_PARAM: IdParam = IdParam::Field(&["sessionId"]);

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
    let body: service::AiSessionCreate = serde_json::from_value(params)
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
    let id = ID_PARAM.require(&params)?;
    service::get(&ctx, &id)
        .map_err(session_error)
        .and_then(|s| serde_json::to_value(s).map_err(WsRpcError::from))
}

async fn session_update(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AiSessionUpdateRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    service::update(&ctx, &body.id, body.update)
        .map_err(session_error)
        .and_then(|s| serde_json::to_value(s).map_err(WsRpcError::from))
}

async fn session_delete(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let id = ID_PARAM.require(&params)?;
    service::delete(&ctx, &id)
        .map_err(session_error)
        .map(Value::Bool)
}

async fn send(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AiSessionSendParams = serde_json::from_value(params.clone())
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
    let body: AiSessionMessagesParams = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {e}")))?;
    service::messages(&ctx, &body.session_id, body.limit, body.before)
        .map_err(session_error)
        .and_then(|m| serde_json::to_value(m).map_err(WsRpcError::from))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<service::AiSessionCreate, AiSession>("ai.sessionCreate", session_create)
        .guard(Guard::Cap(AGENT_CREATE_CAPABILITY.clone()));
    map.method::<NoParams, Vec<AiSession>>("ai.sessionList", session_list)
        .read()
        .guard(Guard::Cap(AGENT_READ_CAPABILITY.clone()));
    map.method::<AiIdParams, AiSession>("ai.session", session_get)
        .read()
        .guard(Guard::Owns {
            cap: |_| AGENT_READ_CAPABILITY.clone(),
            id: ID_PARAM,
            owner: Owner::AiSession,
        });
    map.method::<AiSessionUpdateRequest, AiSession>("ai.sessionUpdate", session_update)
        .guard(Guard::Owns {
            cap: |_| AGENT_UPDATE_CAPABILITY.clone(),
            id: ID_PARAM,
            owner: Owner::AiSession,
        });
    map.method::<AiIdParams, bool>("ai.sessionDelete", session_delete)
        .guard(Guard::Owns {
            cap: |_| AGENT_UPDATE_CAPABILITY.clone(),
            id: ID_PARAM,
            owner: Owner::AiSession,
        });
    map.method::<AiSessionSendParams, service::AiSessionSendStarted>("ai.sessionSend", send)
        .guard(Guard::Owns {
            cap: |_| AGENT_UPDATE_CAPABILITY.clone(),
            id: SESSION_ID_PARAM,
            owner: Owner::AiSession,
        });
    map.method::<AiSessionCancelParams, bool>("ai.sessionCancel", cancel)
        .guard(Guard::Owns {
            cap: |_| AGENT_UPDATE_CAPABILITY.clone(),
            id: SESSION_ID_PARAM,
            owner: Owner::AiSession,
        });
    map.method::<AiSessionMessagesParams, Vec<crate::ai_sessions::types::AiSessionMessage>>(
        "ai.sessionMessages",
        messages,
    )
    .read()
    .guard(Guard::Owns {
        cap: |_| AGENT_READ_CAPABILITY.clone(),
        id: SESSION_ID_PARAM,
        owner: Owner::AiSession,
    });
}

// ── Contracts ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISessionSendParams")]
pub struct AiSessionSendParams {
    /// The session to send on.
    pub session_id: String,
    /// What the person says.
    pub text: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISessionCancelParams")]
pub struct AiSessionCancelParams {
    /// The session whose turn ends.
    pub session_id: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISessionMessagesParams")]
pub struct AiSessionMessagesParams {
    /// The session to read.
    pub session_id: String,
    /// How many entries at most (200 default).
    pub limit: Option<u32>,
    /// Only entries older than this `seq`.
    #[ts(type = "number | null")]
    pub before: Option<i64>,
}

/// `ai.sessionUpdate`'s params: the session's id and the fields to change, flat.
#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "AISessionUpdateRequest")]
pub struct AiSessionUpdateRequest {
    /// The session's id.
    pub id: String,
    /// The fields to change.
    #[serde(flatten)]
    #[ts(flatten)]
    pub update: service::AiSessionUpdateParams,
}
