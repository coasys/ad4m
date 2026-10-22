//! `perspective.runAgent` — a conversation with a model in one perspective, using the executor's
//! MCP tools (`perspectives::agent_session`).
//!
//! The same guardrails as `perspective.runInterpretationWithHarness`: it needs the capability to
//! update the perspective, runs as the caller with the caller's token (so every tool call has the
//! caller's capabilities), and a credit gate meters each completion.

use serde::Deserialize;
use serde_json::Value;
use std::sync::Arc;
use std::time::Duration;
use ts_rs::TS;

use crate::agent::capabilities::*;
use crate::perspectives::agent_session::{
    run_agent_session, AgentMessage, AgentReply, AgentSessionError, AgentSessionRequest,
};
use crate::types::*;

use super::perspectives_ws::{check_credits, get_perspective_with_access, WsHarnessCreditGate};
use super::ws_handler::{HandlerMap, ParamExt, WsRpcError};

/// Long enough for an agent that reads documentation and edits a large record over many calls.
const RUN_AGENT_TIMEOUT_SECS: u64 = 900;

async fn run_agent(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;
    ctx.require(&perspective_update_capability(vec![uuid.clone()]))
        .map_err(WsRpcError::forbidden)?;
    check_credits(ctx.user_did())?;
    let body: RunAgentRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    // Answers 404 for a perspective this caller cannot reach, before any model is asked.
    get_perspective_with_access(&uuid, &ctx).await?;

    let agent_context = ctx.acts_as();
    let auth_token = (!ctx.auth_token.is_empty()).then(|| ctx.auth_token.clone());
    let credit_gate: Option<Arc<dyn crate::ai_service::harness::CreditGate>> = Some(Arc::new(
        WsHarnessCreditGate::new(ctx.user_did().map(str::to_string)),
    ));

    let request = AgentSessionRequest {
        perspective_uuid: &uuid,
        messages: &body.messages,
        context: body.context.as_ref(),
        write_tools: body.write_tools.as_deref().unwrap_or_default(),
        model: body.model.as_deref(),
        max_tool_calls: body.max_tool_calls.unwrap_or(20),
    };
    let outcome = tokio::time::timeout(
        Duration::from_secs(RUN_AGENT_TIMEOUT_SECS),
        run_agent_session(request, &agent_context, auth_token, credit_gate),
    )
    .await
    .map_err(|_| WsRpcError {
        code: 408,
        message: format!("runAgent timed out after {RUN_AGENT_TIMEOUT_SECS}s"),
    })?;

    match outcome {
        Ok(reply) => Ok(serde_json::to_value(reply)?),
        Err(AgentSessionError::Invalid(message)) => Err(WsRpcError::bad_request(message)),
        Err(e @ AgentSessionError::OutOfCredits) => Err(WsRpcError {
            code: 402,
            message: e.to_string(),
        }),
        Err(AgentSessionError::Failed(e)) => Err(WsRpcError::internal(format!("{e:#}"))),
    }
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    map.method::<RunAgentRequest, AgentReply>("perspective.runAgent", run_agent)
        .long();
}

// ── Contracts ──

#[derive(Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct RunAgentRequest {
    pub uuid: String,
    /// The conversation so far, the person's request last.
    pub messages: Vec<AgentMessage>,
    /// What the person is looking at, as a hint for the model (`{ "recordId": "…" }`).
    #[ts(optional, type = "Record<string, unknown>")]
    pub context: Option<Value>,
    /// Tools that write which the model may call — `instance_update`, `call_record_tool`; only
    /// tools that write inside the perspective (`agent_session::SESSION_WRITES`). None means a
    /// session that only reads.
    #[ts(optional)]
    pub write_tools: Option<Vec<String>>,
    /// A model id; the node's default language model when absent.
    #[ts(optional)]
    pub model: Option<String>,
    /// How many tool calls the model may make before it must answer. Default 20.
    #[ts(optional)]
    pub max_tool_calls: Option<u32>,
}
