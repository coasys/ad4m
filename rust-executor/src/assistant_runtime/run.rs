//! The per-thread assistant run loop.
//!
//! One `run_thread` call handles one user turn end-to-end:
//!
//! 1. resolve the thread → assistant → model → personalities / skills / MCP
//!    servers (parsing the JSON id arrays and loading each record),
//! 2. create (or, on resume, reuse) the assistant reply `Message` in
//!    `status: streaming`,
//! 3. loop: assemble context → `AIService::prompt_messages_stream` (streaming
//!    tokens into `Message.content`) → `extract_tool_calls` → execute each tool
//!    → fold the tool transcript back into the prompt → repeat until the model
//!    answers without calling a tool,
//! 4. finalise `Message.status` (`complete`, or `error` on failure) and the
//!    durable `RunState`.
//!
//! Streaming persists by rewriting the assistant `Message.content` link, which
//! auto-publishes on `PERSPECTIVE_LINK_ADDED_TOPIC` so WE's live query
//! re-renders — no separate token channel, matching WE.

use anyhow::{anyhow, Result};

use kalosm::language::ArcParser;

use crate::agent::AgentContext;
use crate::ai_service::AIService;
use crate::api::openai_compat::tool_grammar::{build_tool_call_parser, extract_tool_calls, ToolChoice};

use super::context::assemble_messages;
use super::entities::{
    Assistant, McpServer, Message, Personality, Skill, Thread, CLASS_ASSISTANT, CLASS_MCP_SERVER,
    CLASS_THREAD, P_CONTENT, P_STATUS, P_TOOL_CALLS,
};
use super::store;
use super::tools::{BuiltinTools, McpToolProvider, ToolProvider, ToolSet};

/// Upper bound on tool-call iterations per turn (defence against loops).
const MAX_ITERATIONS: usize = 8;
/// Rewrite the streaming `Message.content` link every N tokens.
const CONTENT_UPDATE_EVERY_TOKENS: usize = 24;

/// What a run needs to start: the conversation perspective + the thread.
#[derive(Debug, Clone)]
pub struct RunInput {
    pub perspective_uuid: String,
    pub thread_id: String,
}

/// Run one user turn for `input.thread_id`. Errors are handled internally
/// (assistant message + RunState marked `error`); the returned `Result` is for
/// the registry's logging only.
pub async fn run_thread(input: RunInput) -> Result<()> {
    let RunInput {
        perspective_uuid,
        thread_id,
    } = input;

    let mut conv = store::writable(&perspective_uuid)
        .ok_or_else(|| anyhow!("Conversation perspective not found: {perspective_uuid}"))?;

    // Snapshot the thread's messages and split off the live user turn.
    let msgs = store::thread_messages(&conv, &thread_id).await;
    let Some(last_user_idx) = msgs.iter().rposition(|m| m.role == "user") else {
        log::debug!("assistant_runtime: thread {thread_id} has no user message; skipping");
        return Ok(());
    };
    let user_turn = msgs[last_user_idx].content.clone();
    let history: Vec<Message> = msgs[..last_user_idx].to_vec();

    // Reuse a trailing half-written assistant message (resume) or create one.
    let reply_id = match msgs.get(last_user_idx + 1) {
        Some(m) if m.role == "assistant" && m.status == "streaming" => m.id.clone(),
        _ => {
            let reply = Message {
                id: format!("we://message/{}", uuid::Uuid::new_v4()),
                thread_id: thread_id.clone(),
                role: "assistant".to_string(),
                content: String::new(),
                tool_calls: String::new(),
                ts: chrono::Utc::now().to_rfc3339(),
                status: "streaming".to_string(),
            };
            let ctx = AgentContext::main_agent();
            store::write_message(&mut conv, &reply, &ctx).await?;
            reply.id
        }
    };

    // Resolve the assistant configuration. On failure, mark the reply errored.
    let config = match resolve_config(&conv, &thread_id).await {
        Ok(c) => c,
        Err(e) => {
            log::error!("assistant_runtime: config resolution failed for thread {thread_id}: {e}");
            let _ = fail(&mut conv, &reply_id, &thread_id, &e.to_string()).await;
            return Err(e);
        }
    };

    let _ = store::upsert_run_state(&mut conv, &thread_id, "running", "0", "").await;

    match drive_loop(&mut conv, &reply_id, &thread_id, &config, &user_turn, &history).await {
        Ok(()) => {
            let _ = store::upsert_run_state(&mut conv, &thread_id, "done", "final", "").await;
            Ok(())
        }
        Err(e) => {
            log::error!("assistant_runtime: run failed for thread {thread_id}: {e}");
            let _ = fail(&mut conv, &reply_id, &thread_id, &e.to_string()).await;
            Err(e)
        }
    }
}

/// Resolved config for a run.
struct RunConfig {
    assistant: Assistant,
    personalities: Vec<Personality>,
    skills: Vec<Skill>,
    mcp_servers: Vec<McpServer>,
    model_id: String,
}

async fn resolve_config(
    conv: &crate::perspectives::perspective_instance::PerspectiveInstance,
    thread_id: &str,
) -> Result<RunConfig> {
    // Thread instance: prefer the conversation perspective, else scan.
    let thread_props = {
        let local = store::load_props(conv, thread_id).await;
        if local.contains_key(super::entities::P_ASSISTANT_ID) {
            local
        } else if let Some(p) =
            store::find_perspective_with_instance(thread_id, CLASS_THREAD).await
        {
            store::load_props(&p, thread_id).await
        } else {
            local
        }
    };
    let thread = Thread::from_props(thread_id.to_string(), &thread_props);
    if thread.assistant_id.is_empty() {
        return Err(anyhow!("Thread {thread_id} has no assistantId"));
    }

    // Assistant lives in the we-root perspective — locate it by instance.
    let we_root = store::find_perspective_with_instance(&thread.assistant_id, CLASS_ASSISTANT)
        .await
        .ok_or_else(|| anyhow!("Assistant {} not found in any perspective", thread.assistant_id))?;
    let assistant = Assistant::from_props(
        thread.assistant_id.clone(),
        &store::load_props(&we_root, &thread.assistant_id).await,
    );

    let mut personalities = Vec::new();
    for id in &assistant.personality_ids {
        let props = store::load_props(&we_root, id).await;
        if !props.is_empty() {
            personalities.push(Personality::from_props(id.clone(), &props));
        }
    }
    let mut skills = Vec::new();
    for id in &assistant.skill_ids {
        let props = store::load_props(&we_root, id).await;
        if !props.is_empty() {
            skills.push(Skill::from_props(id.clone(), &props));
        }
    }
    let mut mcp_servers = Vec::new();
    for id in &assistant.mcp_server_ids {
        // MCP servers may be defined alongside the assistant or elsewhere.
        let props = {
            let local = store::load_props(&we_root, id).await;
            if local.is_empty() {
                match store::find_perspective_with_instance(id, CLASS_MCP_SERVER).await {
                    Some(p) => store::load_props(&p, id).await,
                    None => local,
                }
            } else {
                local
            }
        };
        if !props.is_empty() {
            mcp_servers.push(McpServer::from_props(id.clone(), &props));
        }
    }

    let model_id = if !thread.model_id.is_empty() {
        thread.model_id.clone()
    } else if !assistant.model_id.is_empty() {
        assistant.model_id.clone()
    } else {
        "default".to_string()
    };

    Ok(RunConfig {
        assistant,
        personalities,
        skills,
        mcp_servers,
        model_id,
    })
}

/// The context → model → tools → repeat loop for one turn.
async fn drive_loop(
    conv: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    reply_id: &str,
    thread_id: &str,
    config: &RunConfig,
    user_turn: &str,
    history: &[Message],
) -> Result<()> {
    let ctx = AgentContext::main_agent();
    let ai = AIService::global_instance()
        .await
        .map_err(|e| anyhow!("AI service unavailable: {e}"))?;

    let toolset = ToolSet::new(vec![
        ToolProvider::Builtin(BuiltinTools::new(conv.uuid.clone())),
        ToolProvider::Mcp(McpToolProvider::connect(config.mcp_servers.clone()).await),
    ]);
    let tool_defs = toolset.tool_defs();

    // The live prompt, extended with a tool transcript between iterations so
    // the continuation stays in the executor's `final_prompt` (tool roles are
    // otherwise dropped).
    let mut working_prompt = user_turn.to_string();
    // Accumulated tool call + result records, mirrored onto Message.toolCalls.
    let mut tool_record: Vec<serde_json::Value> = Vec::new();

    for iteration in 0..MAX_ITERATIONS {
        let messages = assemble_messages(
            &config.assistant,
            &config.personalities,
            &config.skills,
            &tool_defs,
            history,
            &working_prompt,
        );

        // The assistant loop lets the model decide whether to call a tool
        // (`ToolChoice::Auto`) — an unconstrained generation whose `<tool_call>`
        // blocks are recovered from the text afterwards. `build_tool_call_parser`
        // returns `None` for `Auto`; this is the seam where a policy could force
        // `Required`/`Named` to hard-constrain decoding on a given iteration.
        let constraint: Option<ArcParser<()>> =
            build_tool_call_parser(&tool_defs, &ToolChoice::Auto, true);

        // Stream the model output, rewriting Message.content at a cadence.
        let full =
            stream_completion(conv, reply_id, &ctx, &ai, &config.model_id, messages, constraint)
                .await?;

        let calls = extract_tool_calls(&full);
        if calls.is_empty() {
            // Final answer.
            let visible = strip_tool_calls(&full);
            store::set_single_target(conv, reply_id, P_CONTENT, &visible, &ctx).await?;
            store::set_single_target(conv, reply_id, P_STATUS, "complete", &ctx).await?;
            return Ok(());
        }

        let _ = store::upsert_run_state(
            conv,
            thread_id,
            "awaiting_tool",
            &iteration.to_string(),
            &full,
        )
        .await;

        // Execute each requested tool, building the continuation transcript.
        let mut transcript = format!("\n{}", full);
        for call in &calls {
            let result = match toolset.execute(&call.name, &call.arguments).await {
                Ok(r) => r,
                Err(e) => json_err(&e.to_string()),
            };
            tool_record.push(serde_json::json!({
                "name": call.name,
                "arguments": call.arguments,
                "result": result,
            }));
            // Persist a role:'tool' message for history + WE display.
            let tool_msg = Message {
                id: format!("we://message/{}", uuid::Uuid::new_v4()),
                thread_id: thread_id.to_string(),
                role: "tool".to_string(),
                content: result.clone(),
                tool_calls: String::new(),
                ts: chrono::Utc::now().to_rfc3339(),
                status: "complete".to_string(),
            };
            let _ = store::write_message(conv, &tool_msg, &ctx).await;
            transcript.push_str(&format!(
                "\n<tool_response name=\"{}\">{}</tool_response>",
                call.name, result
            ));
        }

        // Mirror the accumulated tool calls onto the assistant message.
        if let Ok(json) = serde_json::to_string(&tool_record) {
            let _ = store::set_single_target(conv, reply_id, P_TOOL_CALLS, &json, &ctx).await;
        }

        working_prompt = format!(
            "{working_prompt}{transcript}\n\nUse the tool results above to answer the user."
        );
        let _ = store::upsert_run_state(
            conv,
            thread_id,
            "running",
            &iteration.to_string(),
            "",
        )
        .await;
    }

    // Iteration budget exhausted — finalise with whatever we have.
    log::warn!("assistant_runtime: thread {thread_id} hit MAX_ITERATIONS ({MAX_ITERATIONS})");
    store::set_single_target(conv, reply_id, P_STATUS, "complete", &ctx).await?;
    Ok(())
}

/// Run one streaming completion, rewriting `Message.content` as tokens arrive,
/// and return the full generated text.
async fn stream_completion(
    conv: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    reply_id: &str,
    ctx: &AgentContext,
    ai: &AIService,
    model_id: &str,
    messages: Vec<(String, String)>,
    constraint: Option<ArcParser<()>>,
) -> Result<String> {
    let (mut token_rx, done_rx) = ai
        .prompt_messages_stream(model_id.to_string(), messages, constraint)
        .await
        .map_err(|e| anyhow!("model stream failed: {e}"))?;

    let mut buffer = String::new();
    let mut since_update = 0usize;
    while let Some(token) = token_rx.recv().await {
        buffer.push_str(&token);
        since_update += 1;
        if since_update >= CONTENT_UPDATE_EVERY_TOKENS {
            since_update = 0;
            let visible = strip_tool_calls(&buffer);
            let _ = store::set_single_target(conv, reply_id, P_CONTENT, &visible, ctx).await;
        }
    }
    // Drain the completion signal (token counts unused here).
    let _ = done_rx.await;

    Ok(buffer)
}

/// Mark the assistant reply + RunState as errored.
async fn fail(
    conv: &mut crate::perspectives::perspective_instance::PerspectiveInstance,
    reply_id: &str,
    thread_id: &str,
    message: &str,
) -> Result<()> {
    let ctx = AgentContext::main_agent();
    let _ = store::set_single_target(conv, reply_id, P_CONTENT, &format!("Error: {message}"), &ctx)
        .await;
    store::set_single_target(conv, reply_id, P_STATUS, "error", &ctx).await?;
    let _ = store::upsert_run_state(conv, thread_id, "error", "error", message).await;
    Ok(())
}

/// Remove `<tool_call>…</tool_call>` blocks for display; keep surrounding prose.
fn strip_tool_calls(text: &str) -> String {
    const OPEN: &str = "<tool_call>";
    const CLOSE: &str = "</tool_call>";
    let mut out = String::new();
    let mut rest = text;
    while let Some(start) = rest.find(OPEN) {
        out.push_str(&rest[..start]);
        let after = &rest[start + OPEN.len()..];
        match after.find(CLOSE) {
            Some(end) => rest = &after[end + CLOSE.len()..],
            None => {
                rest = "";
                break;
            }
        }
    }
    out.push_str(rest);
    out.trim().to_string()
}

fn json_err(message: &str) -> String {
    serde_json::json!({ "error": message }).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strip_removes_tool_blocks_keeps_prose() {
        let text = "Sure!\n<tool_call>\n{\"name\":\"f\",\"arguments\":{}}\n</tool_call>\nDone.";
        assert_eq!(strip_tool_calls(text), "Sure!\n\nDone.");
    }

    #[test]
    fn strip_handles_unterminated_block() {
        let text = "Answer<tool_call>\n{\"name\":\"f\"";
        assert_eq!(strip_tool_calls(text), "Answer");
    }

    #[test]
    fn strip_noop_on_plain_text() {
        assert_eq!(strip_tool_calls("just prose"), "just prose");
    }
}
