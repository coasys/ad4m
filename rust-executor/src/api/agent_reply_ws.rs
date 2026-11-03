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

    let write_tools = body.write_tools.unwrap_or_default();
    let max_tool_calls = body.max_tool_calls.unwrap_or(DEFAULT_MAX_TOOL_CALLS);
    check_settings(&write_tools, max_tool_calls).map_err(WsRpcError::bad_request)?;
    let work_cap = if write_tools.is_empty() {
        perspective_query_capability
    } else {
        perspective_update_capability
    };
    Guard::perspective(work_cap)
        .admit(
            "perspective.agentReply",
            &json!({ "uuid": body.work_in }),
            &ctx,
        )
        .await?;

    let agent_context = ctx.acts_as();
    let caller_did = crate::agent::did_for_context(&agent_context)
        .map_err(|e| WsRpcError::internal(format!("could not read the caller's DID: {e:#}")))?;
    let session = ctx.session.clone();
    let credit_gate: Option<Arc<dyn crate::ai_service::harness::CreditGate>> = Some(Arc::new(
        WsHarnessCreditGate::new(ctx.user_did().map(str::to_string)),
    ));
    let run_context = agent_context.clone();
    let work_in = body.work_in;
    let context = body.context;
    let model = body.model;
    let run: Runner = Box::new(move |messages: Vec<AgentMessage>| {
        Box::pin(async move {
            let request = AgentSessionRequest {
                perspective_uuid: &work_in,
                messages: &messages,
                context: context.as_ref(),
                write_tools: &write_tools,
                model: model.as_deref(),
                max_tool_calls,
            };
            run_agent_session(request, &run_context, session, credit_gate).await
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

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<AgentReplyRequest, AgentReplyStarted>("perspective.agentReply", agent_reply)
        .guard(Guard::perspective(perspective_update_capability));
    map.method::<AgentReplyCancelRequest, bool>("perspective.agentReplyCancel", agent_reply_cancel)
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
    pub work_in: String,
    /// What the person is looking at, as a hint for the model (`{ "recordId": "…" }`).
    #[ts(optional, type = "Record<string, unknown>")]
    pub context: Option<Value>,
    /// Tools that write which the model may call, as for `perspective.runAgent`. None means an
    /// agent that only reads.
    #[ts(optional)]
    pub write_tools: Option<Vec<String>>,
    /// A model id; the node's default language model when absent.
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
