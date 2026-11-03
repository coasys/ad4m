//! `perspective.agentReply` and `perspective.agentReplyCancel`: an agent's reply in a
//! conversation the caller keeps (`perspectives::agent_reply`).
//!
//! The perspective the call names (`uuid`) keeps the conversation: the guard checks the caller may
//! write there, since the reply is written there. The agent works in `workIn`, which may be
//! another perspective: the handler runs the same perspective guard on it, with the update
//! capability when the agent may write and the query capability when it only reads. The run is
//! `perspective.runAgent`'s, as the caller's session and with its credit gate, but it is not tied
//! to the request: the call returns as soon as the reply exists.

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::sync::Arc;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::perspectives::agent_reply::{cancel_reply, start_reply, ReplyError, Runner};
use crate::perspectives::agent_session::{
    check_settings, run_agent_session, AgentMessage, AgentSessionRequest,
};
use crate::perspectives::agent_sessions::{
    create_session, delete_session, session_settings, NewSession, SessionSettings,
};
use crate::types::*;

use super::guards::Guard;
use super::perspectives_ws::{check_credits, guarded_perspective, WsHarnessCreditGate};
use super::ws_handler::{HandlerMap, WsRpcError};

const DEFAULT_MAX_TOOL_CALLS: u32 = 20;

async fn agent_reply(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AgentReplyRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let perspective = guarded_perspective(&params).await?;
    check_credits(ctx.user_did())?;

    // A session's settings fill in what the call leaves out.
    let saved = session_settings(&perspective, body.container.trim())
        .await
        .map_err(|e| WsRpcError::internal(format!("{e:#}")))?
        .unwrap_or_default();
    let work_in = body.work_in.or(saved.work_in).ok_or_else(|| {
        WsRpcError::bad_request(
            "`workIn` is missing: name the perspective the agent works in, or reply in a session \
             that has one",
        )
    })?;
    let write_tools = body.write_tools.unwrap_or(saved.write_tools);
    let model = body.model.or(saved.model);
    let max_tool_calls = body.max_tool_calls.unwrap_or(DEFAULT_MAX_TOOL_CALLS);
    check_settings(&write_tools, max_tool_calls).map_err(WsRpcError::bad_request)?;
    admit_work_in(&ctx, &work_in, &write_tools).await?;

    let agent_context = ctx.acts_as();
    let caller_did = crate::agent::did_for_context(&agent_context)
        .map_err(|e| WsRpcError::internal(format!("could not read the caller's DID: {e:#}")))?;
    let session = ctx.session.clone();
    let credit_gate: Option<Arc<dyn crate::ai_service::harness::CreditGate>> = Some(Arc::new(
        WsHarnessCreditGate::new(ctx.user_did().map(str::to_string)),
    ));
    let run_context = agent_context.clone();
    let context = body.context;
    let run: Runner = Box::new(move |messages: Vec<AgentMessage>, steps| {
        Box::pin(async move {
            let request = AgentSessionRequest {
                perspective_uuid: &work_in,
                messages: &messages,
                context: context.as_ref(),
                write_tools: &write_tools,
                model: model.as_deref(),
                max_tool_calls,
            };
            run_agent_session(request, &run_context, session, credit_gate, Some(steps)).await
        })
    });

    let reply_id = start_reply(
        perspective,
        &body.container,
        &caller_did,
        agent_context,
        run,
    )
    .await
    .map_err(|e| match e {
        ReplyError::Invalid(message) => WsRpcError::bad_request(message),
        e @ ReplyError::Busy => WsRpcError {
            code: 409,
            message: e.to_string(),
        },
        ReplyError::Failed(e) => WsRpcError::internal(format!("{e:#}")),
    })?;
    Ok(serde_json::to_value(AgentReplyStarted { reply_id })?)
}

async fn agent_reply_cancel(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AgentReplyCancelRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let perspective = guarded_perspective(&params).await?;
    let cancelled = cancel_reply(perspective, &body.container, &ctx.acts_as())
        .await
        .map_err(|e| WsRpcError::internal(format!("{e:#}")))?;
    Ok(json!(cancelled))
}

/// The caller may let an agent work in `work_in`: query it, and update it when the agent may
/// write. The perspective guard, run on that perspective.
async fn admit_work_in(
    ctx: &RequestContext,
    work_in: &str,
    write_tools: &[String],
) -> Result<(), WsRpcError> {
    let cap = if write_tools.is_empty() {
        perspective_query_capability
    } else {
        perspective_update_capability
    };
    Guard::perspective(cap)
        .admit("perspective.agentReply", &json!({ "uuid": work_in }), ctx)
        .await
}

async fn agent_session_create(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let body: AgentSessionCreateRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let perspective = guarded_perspective(&params).await?;
    let write_tools = body.write_tools.unwrap_or_default();
    check_settings(&write_tools, DEFAULT_MAX_TOOL_CALLS).map_err(WsRpcError::bad_request)?;
    if let Some(work_in) = &body.work_in {
        admit_work_in(&ctx, work_in, &write_tools).await?;
    }
    let session_id = create_session(
        perspective,
        NewSession {
            title: body.title.unwrap_or_default(),
            about: body.about,
            settings: SessionSettings {
                work_in: body.work_in,
                write_tools,
                model: body.model,
            },
        },
        &ctx.acts_as(),
    )
    .await
    .map_err(|e| WsRpcError::internal(format!("{e:#}")))?;
    Ok(serde_json::to_value(AgentSessionCreated { session_id })?)
}

async fn agent_session_delete(
    params: Value,
    ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let body: AgentSessionDeleteRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let perspective = guarded_perspective(&params).await?;
    let deleted = delete_session(perspective, body.session.trim(), &ctx.acts_as())
        .await
        .map_err(|e| WsRpcError::internal(format!("{e:#}")))?;
    Ok(json!(deleted))
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<AgentReplyRequest, AgentReplyStarted>("perspective.agentReply", agent_reply)
        .guard(Guard::perspective(perspective_update_capability));
    map.method::<AgentReplyCancelRequest, bool>("perspective.agentReplyCancel", agent_reply_cancel)
        .guard(Guard::perspective(perspective_update_capability));
    map.method::<AgentSessionCreateRequest, AgentSessionCreated>(
        "perspective.agentSessionCreate",
        agent_session_create,
    )
    .guard(Guard::perspective(perspective_update_capability));
    map.method::<AgentSessionDeleteRequest, bool>(
        "perspective.agentSessionDelete",
        agent_session_delete,
    )
    .guard(Guard::perspective(perspective_update_capability));
}

// ── Contracts ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentReplyRequest {
    /// The perspective that keeps the conversation, where the reply is written.
    pub uuid: String,
    /// The node that holds the conversation: its children that carry text (`TextContent`) are
    /// the messages. The person's message must be written there before the call.
    pub container: String,
    /// The perspective the agent works in: what its tools read and, with `writeTools`, change.
    /// Absent: the session's (`container` is an `AgentSession` created with one).
    #[ts(optional)]
    pub work_in: Option<String>,
    /// What the person is looking at, as a hint for the model (`{ "recordId": "…" }`).
    #[ts(optional, type = "Record<string, unknown>")]
    pub context: Option<Value>,
    /// Tools that write which the model may call, as for `perspective.runAgent`. Absent: the
    /// session's, or none, an agent that only reads.
    #[ts(optional)]
    pub write_tools: Option<Vec<String>>,
    /// A model id. Absent: the session's, else the node's default language model.
    #[ts(optional)]
    pub model: Option<String>,
    /// How many tool calls the model may make before it must answer. Default 20.
    #[ts(optional)]
    pub max_tool_calls: Option<u32>,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentReplyStarted {
    /// The reply, created as `running` under the container. Follow it with a query
    /// subscription on `AgentReply`.
    pub reply_id: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentReplyCancelRequest {
    pub uuid: String,
    /// The node that holds the conversation whose running reply is stopped.
    pub container: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSessionCreateRequest {
    /// The perspective that keeps the session: its replies are written there.
    pub uuid: String,
    #[ts(optional)]
    pub title: Option<String>,
    /// The record the session is about (an id), so a client lists the sessions about it.
    #[ts(optional)]
    pub about: Option<String>,
    /// The perspective its replies' agent works in, unless a reply names another. Kept on this
    /// node only.
    #[ts(optional)]
    pub work_in: Option<String>,
    /// The tools its replies' agent may write with. Kept on this node only.
    #[ts(optional)]
    pub write_tools: Option<Vec<String>>,
    /// The model its replies use. Kept on this node only.
    #[ts(optional)]
    pub model: Option<String>,
}

#[derive(Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSessionCreated {
    /// The session: an `AgentSession`, and the `container` to reply in.
    pub session_id: String,
}

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct AgentSessionDeleteRequest {
    pub uuid: String,
    /// The session to delete, with its replies and their steps.
    pub session: String,
}
