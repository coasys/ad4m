//! Interpretation-pass LLM harness — the loop that lets the model reason
//! against tools while writing an extraction.
//!
//! Design: [[planning/llm-harness-design-2026-08-21-v3.md]] §6 + v2 §2.
//!
//! ## Shape
//!
//! ```text
//! run_with_tools(model_id, initial_messages, provider, cap) -> String
//!   messages = initial_messages
//!   loop up to cap:
//!     response = ai_service.prompt_messages_with_tools(model_id, messages, provider.tools())
//!     if response.tool_calls.is_empty(): return response.content
//!     for tc in response.tool_calls:
//!       result = provider.call(tc.name, tc.args)
//!       messages += [{role:assistant, tool_calls:[tc]}, {role:tool, tool_call_id:tc.id, content:result}]
//!   // cap hit
//!   messages += {role:system, content:"Tool budget exhausted. Answer now."}
//!   return ai_service.prompt_messages(model_id, messages).await
//! ```
//!
//! This module owns only the loop + the message-append shape. The wire
//! plumbing (OpenAI tools[] / tool_calls[] on request/response) lives in
//! `ai_service::prompt_messages_with_tools`; the tool surface lives behind
//! `ToolProvider`. Both are seams a test can double.

pub mod propose;
pub mod provider;

use anyhow::Result;
use provider::ToolProvider;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::sync::Arc;

use crate::perspectives::auto_processor::events;

/// Max characters we ship on a `ToolResult` event's `tool_result` field.
/// A `_query` tool can return a many-KB JSON payload — inflating every
/// event with the whole thing would flood the pubsub topic. Consumers
/// wanting the full text can re-run the tool or read it off the
/// InterpretationRun's overlay.
const TOOL_RESULT_EVENT_MAX_CHARS: usize = 2048;

fn truncate_for_event(text: &str) -> String {
    if text.chars().count() <= TOOL_RESULT_EVENT_MAX_CHARS {
        return text.to_string();
    }
    let head: String = text.chars().take(TOOL_RESULT_EVENT_MAX_CHARS).collect();
    format!("{head}…[truncated for event]")
}

/// Emit one `ToolCall` / `ToolResult` event on the auto-processor topic
/// when `ctx` is set. Fire-and-forget; no-op when `ctx` is `None`.
async fn emit_tool_event(
    ctx: Option<&events::InterpretationEmitContext>,
    step: events::AutoProcessorStep,
    tool_name: &str,
    tool_args_json: Option<String>,
    tool_result: Option<String>,
) {
    let Some(ctx) = ctx else {
        return;
    };
    let mut ev = events::AutoProcessorEvent::new(&ctx.perspective_uuid, &ctx.processor_id, step)
        .with_agent_did(&ctx.agent_did)
        .with_items(&ctx.item_ids)
        .with_batch_key(&ctx.batch_key);
    match (tool_args_json, tool_result) {
        (Some(args), None) => ev = ev.with_tool_call(tool_name, args),
        (None, Some(result)) => ev = ev.with_tool_result(tool_name, result),
        _ => {
            ev.tool_name = Some(tool_name.to_string());
        }
    }
    events::emit(ev).await;
}

/// A single tool_call emitted by the LLM, in the shape the harness loop
/// works with. Matches OpenAI's `tool_calls[]` element (id / type=function /
/// function.name / function.arguments) but with `arguments` already parsed
/// out of the string-encoded JSON the wire format uses.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct HarnessToolCall {
    /// Stable id the LLM assigned this call — echoed back on the paired
    /// `role: "tool"` result message so the LLM can correlate multi-call
    /// turns.
    pub id: String,
    /// The tool name the LLM asked to invoke (matches a `ToolSchema::name`).
    pub name: String,
    /// Arguments the LLM emitted as a JSON object. On the wire OpenAI ships
    /// this as `arguments: "<json-string>"`; the harness parses it into a
    /// Value at the wire boundary so downstream code doesn't re-parse.
    pub arguments: Value,
}

/// One LLM completion in the shape the harness loop consumes. Mirror-image
/// of the OpenAI `choices[0].message` fields the harness cares about.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct HarnessCompletion {
    /// Assistant text. May be empty when the model chose to only emit tool
    /// calls (`finish_reason: "tool_calls"`).
    pub content: String,
    /// Zero or more tool calls the model wants dispatched before its final
    /// answer. Empty = model is done; the harness terminates and returns
    /// `content` to the caller.
    pub tool_calls: Vec<HarnessToolCall>,
}

/// The AI-service seam the harness loop calls into. Kept as a trait so the
/// loop can be exercised end-to-end with a fake completion source (no real
/// LLM, no network) — the fake is used by every unit test in this module.
///
/// The single real implementation delegates to `AIService::prompt_messages_with_tools`
/// (see `ai_service/harness/openai_bridge.rs` — TODO next commit).
#[async_trait::async_trait]
pub trait CompletionSource: Send + Sync {
    /// Send `messages` + advertise `tools` (empty = plain completion), get
    /// back either an assistant answer or a tool-call turn.
    async fn complete(
        &self,
        model_id: &str,
        messages: &[Value],
        tools: Vec<provider::ToolSchema>,
    ) -> Result<HarnessCompletion>;
}

/// Configuration knob wired to `AutoProcessorConfig.max_tool_calls` (v3 §6
/// default 20). The cap prevents a stuck / adversarial LLM from making
/// unbounded tool calls and DOS'ing the extraction pass.
///
/// When the cap is hit, the harness appends a final system message asking
/// the model to answer without further tools, and returns the resulting
/// completion text. This is deterministic — never fails silently or spins.
#[derive(Debug, Clone, Copy)]
pub struct HarnessConfig {
    pub max_tool_calls: u32,
}

impl Default for HarnessConfig {
    fn default() -> Self {
        Self { max_tool_calls: 20 }
    }
}

/// The interpretation-pass tool-calling loop.
///
/// `initial_messages` is the caller-built prompt — for interpretation, this
/// is the extraction system prompt + few-shots + transcript-as-user-message.
/// The harness does NOT prepend any tool-use guidance here; that lives in
/// the caller's system prompt (design v2 §Q6: "the harness passes tools
/// verbatim; per-task guidance lives in the caller").
///
/// `emit_ctx` opts into the auto-processor event stream for observability:
/// when set (typically by an auto-processor pass whose config has
/// `emit_debug_events: true`), every `ToolCall` + `ToolResult` fires a
/// `AutoProcessorEvent` on the global topic so a subscribed UI can render
/// the loop live (which tool was called, with what args, what came back).
/// `None` skips all telemetry — the fast path stays fast for headless runs.
pub async fn run_with_tools(
    model_id: &str,
    initial_messages: Vec<Value>,
    provider: Arc<dyn ToolProvider>,
    completions: Arc<dyn CompletionSource>,
    config: HarnessConfig,
    emit_ctx: Option<&crate::perspectives::auto_processor::events::InterpretationEmitContext>,
) -> Result<String> {
    let mut messages = initial_messages;
    // Budget is enforced per *dispatched tool call*, not per round: a single
    // completion may return multiple tool_calls, and we don't want one round
    // to blow through the caller's per-pass budget (which downstream drives
    // how many propose_* ops the interpretation overlay accepts).
    let mut calls_used: usize = 0;
    let mut round: usize = 0;

    while calls_used < config.max_tool_calls as usize {
        round += 1;
        let tools = provider.tools().await;
        let tool_count = tools.len();
        let completion = completions.complete(model_id, &messages, tools).await?;

        // CI-visible diagnostic for silent-empty passes: shows which tools were
        // on offer, whether the LLM chose to call any, and what it said
        // otherwise. Priced at warn! because harness runs are expensive and
        // diagnostic reproduction cost from an info-level flood is nil at 1-2
        // rounds per pass.
        let names: Vec<&str> = completion
            .tool_calls
            .iter()
            .map(|c| c.name.as_str())
            .collect();
        let preview: String = completion.content.chars().take(240).collect();
        log::warn!(
            "harness: round={round} calls_used={calls_used}/{cap} tools_offered={tool_count} tool_calls={:?} content_preview={:?}",
            names,
            preview,
            cap = config.max_tool_calls,
        );

        if completion.tool_calls.is_empty() {
            // Model returned a plain answer — done.
            return Ok(completion.content);
        }

        // Append the assistant tool_calls turn AND one tool-result message
        // per call, in the OpenAI-mandated shape. The tool_calls entry must
        // precede its results so the model sees the correlation on the next
        // turn. OpenAI requires one tool-result per tool_call from the same
        // assistant turn; when the budget runs out mid-round we still emit
        // matching results (with a truthful "budget exhausted" body) so the
        // message shape stays valid.
        messages.push(assistant_tool_calls_message(&completion));
        for tc in &completion.tool_calls {
            // Emit `ToolCall` before dispatch — a UI subscribed to the
            // auto-processor event topic renders "LLM asked for <tool>"
            // live, without waiting for the tool to return. Gated on
            // `emit_ctx`: the fast headless path pays no telemetry cost.
            emit_tool_event(
                emit_ctx,
                events::AutoProcessorStep::ToolCall,
                &tc.name,
                Some(tc.arguments.to_string()),
                None,
            )
            .await;
            let result = if calls_used < config.max_tool_calls as usize {
                calls_used += 1;
                match provider.call(&tc.name, tc.arguments.clone()).await {
                    Ok(text) => text,
                    Err(e) => format!("error: {e}"),
                }
            } else {
                format!(
                    "error: tool call budget of {} exhausted mid-round",
                    config.max_tool_calls
                )
            };
            // Emit `ToolResult` after dispatch. Result is truncated to a
            // bounded prefix so a `_query` returning MBs doesn't inflate
            // every event — the UI can request the full text separately
            // if it needs it.
            emit_tool_event(
                emit_ctx,
                events::AutoProcessorStep::ToolResult,
                &tc.name,
                None,
                Some(truncate_for_event(&result)),
            )
            .await;
            messages.push(tool_result_message(&tc.id, &result));
        }
    }

    // Budget exhausted — force a final answer with no tools advertised.
    // The system nudge tells the LLM why it can't call another tool; the
    // empty `tools` on the next call makes it structurally impossible.
    messages.push(json!({
        "role": "system",
        "content": format!(
            "Tool budget of {} calls exhausted. Answer now using only what has already been gathered.",
            config.max_tool_calls
        ),
    }));
    let final_completion = completions
        .complete(model_id, &messages, Vec::new())
        .await?;
    Ok(final_completion.content)
}

fn assistant_tool_calls_message(c: &HarnessCompletion) -> Value {
    // Content is optional on the OpenAI shape when tool_calls is present —
    // most models emit "" here. Keep whatever content the model sent (some
    // emit a short pre-thought before calling); a null content silently
    // trips a subset of provider validators.
    let tool_calls: Vec<Value> = c
        .tool_calls
        .iter()
        .map(|tc| {
            json!({
                "id": tc.id,
                "type": "function",
                "function": {
                    "name": tc.name,
                    // OpenAI wire wants `arguments` as a *string* — the
                    // provider parsed it back to a Value for dispatch, but
                    // on the return trip it goes back as a string so
                    // downstream JSON validators are happy.
                    "arguments": tc.arguments.to_string(),
                },
            })
        })
        .collect();
    json!({
        "role": "assistant",
        "content": c.content,
        "tool_calls": tool_calls,
    })
}

fn tool_result_message(tool_call_id: &str, content: &str) -> Value {
    json!({
        "role": "tool",
        "tool_call_id": tool_call_id,
        "content": content,
    })
}

// ── tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use provider::{ToolProvider, ToolSchema};
    use std::sync::Mutex;

    /// Scripted CompletionSource: pop the next canned HarnessCompletion from
    /// a queue each time the loop asks. Records the (messages, tools) it
    /// received on every call so tests can assert the loop-shaping is right.
    struct ScriptedLLM {
        script: Mutex<Vec<HarnessCompletion>>,
        calls: Mutex<Vec<(Vec<Value>, Vec<ToolSchema>)>>,
    }

    impl ScriptedLLM {
        fn new(script: Vec<HarnessCompletion>) -> Self {
            Self {
                script: Mutex::new(script),
                calls: Mutex::new(Vec::new()),
            }
        }

        fn call_count(&self) -> usize {
            self.calls.lock().unwrap().len()
        }

        fn nth_messages(&self, n: usize) -> Vec<Value> {
            self.calls.lock().unwrap()[n].0.clone()
        }

        fn nth_tools(&self, n: usize) -> Vec<ToolSchema> {
            self.calls.lock().unwrap()[n].1.clone()
        }
    }

    #[async_trait::async_trait]
    impl CompletionSource for ScriptedLLM {
        async fn complete(
            &self,
            _model_id: &str,
            messages: &[Value],
            tools: Vec<ToolSchema>,
        ) -> Result<HarnessCompletion> {
            self.calls
                .lock()
                .unwrap()
                .push((messages.to_vec(), tools.clone()));
            let next = self.script.lock().unwrap().remove(0);
            Ok(next)
        }
    }

    /// Tiny provider that returns a fixed tool list and hands back a
    /// synthetic string result on any call.
    struct EchoProvider {
        tools: Vec<ToolSchema>,
    }

    #[async_trait::async_trait]
    impl ToolProvider for EchoProvider {
        async fn tools(&self) -> Vec<ToolSchema> {
            self.tools.clone()
        }
        async fn call(&self, name: &str, args: Value) -> Result<String> {
            Ok(format!("{name}({args})"))
        }
    }

    fn user_message(text: &str) -> Value {
        json!({"role": "user", "content": text})
    }

    fn plain_answer(content: &str) -> HarnessCompletion {
        HarnessCompletion {
            content: content.into(),
            tool_calls: Vec::new(),
        }
    }

    fn tool_call_turn(id: &str, name: &str, args: Value) -> HarnessCompletion {
        HarnessCompletion {
            content: String::new(),
            tool_calls: vec![HarnessToolCall {
                id: id.into(),
                name: name.into(),
                arguments: args,
            }],
        }
    }

    #[tokio::test]
    async fn terminates_when_first_completion_has_no_tool_calls() {
        let script = vec![plain_answer("hello world")];
        let llm = Arc::new(ScriptedLLM::new(script));
        let provider = Arc::new(EchoProvider { tools: vec![] });

        let out = run_with_tools(
            "test-model",
            vec![user_message("hi")],
            provider,
            llm.clone(),
            HarnessConfig::default(),
            None,
        )
        .await
        .unwrap();

        assert_eq!(out, "hello world");
        assert_eq!(llm.call_count(), 1);
    }

    #[tokio::test]
    async fn one_round_of_tool_calls_appends_assistant_then_tool_messages_in_order() {
        let script = vec![
            tool_call_turn("c1", "query_links", json!({"source": "ns://a"})),
            plain_answer("done"),
        ];
        let llm = Arc::new(ScriptedLLM::new(script));
        let provider = Arc::new(EchoProvider {
            tools: vec![ToolSchema::zero_arg("query_links", "Query")],
        });

        let out = run_with_tools(
            "test-model",
            vec![user_message("go")],
            provider,
            llm.clone(),
            HarnessConfig::default(),
            None,
        )
        .await
        .unwrap();

        assert_eq!(out, "done");
        assert_eq!(llm.call_count(), 2);

        // Second call must have seen: [initial user, assistant with tool_calls, tool result].
        let msgs_on_second = llm.nth_messages(1);
        assert_eq!(msgs_on_second.len(), 3);
        assert_eq!(msgs_on_second[0]["role"], "user");
        assert_eq!(msgs_on_second[1]["role"], "assistant");
        assert_eq!(msgs_on_second[1]["tool_calls"][0]["id"], "c1");
        assert_eq!(
            msgs_on_second[1]["tool_calls"][0]["function"]["name"],
            "query_links"
        );
        assert_eq!(msgs_on_second[2]["role"], "tool");
        assert_eq!(msgs_on_second[2]["tool_call_id"], "c1");
        assert_eq!(
            msgs_on_second[2]["content"],
            "query_links({\"source\":\"ns://a\"})"
        );
    }

    #[tokio::test]
    async fn arguments_reach_provider_as_parsed_json_not_string() {
        // OpenAI ships `arguments` as a JSON-encoded string on the wire; the
        // harness parses it at the boundary so provider.call() sees a real
        // Value. Regression against a would-be "just pass the string
        // through" simplification.
        let script = vec![
            tool_call_turn(
                "c1",
                "echo",
                json!({"nested": {"x": 1, "arr": [true, false]}}),
            ),
            plain_answer("done"),
        ];
        let llm = Arc::new(ScriptedLLM::new(script));
        let provider = Arc::new(EchoProvider {
            tools: vec![ToolSchema::zero_arg("echo", "")],
        });

        let _ = run_with_tools(
            "m",
            vec![user_message("go")],
            provider,
            llm.clone(),
            HarnessConfig::default(),
            None,
        )
        .await
        .unwrap();

        let msgs = llm.nth_messages(1);
        // The tool RESULT message includes the args verbatim (EchoProvider
        // echoed them back). Confirms provider saw structured JSON, not a
        // string literal.
        let content = msgs[2]["content"].as_str().unwrap();
        assert!(
            content.contains("\"nested\":{"),
            "provider must have received a JSON object, got: {content}"
        );
    }

    #[tokio::test]
    async fn hits_max_calls_and_forces_final_answer_with_empty_tools() {
        // Feed an infinite stream of tool calls, then a final plain answer
        // once the harness switches to the tool-less final completion.
        let script = vec![
            tool_call_turn("1", "t", json!({})),
            tool_call_turn("2", "t", json!({})),
            tool_call_turn("3", "t", json!({})),
            // The 4th call is the tool-less budget-exhausted final:
            plain_answer("forced answer"),
        ];
        let llm = Arc::new(ScriptedLLM::new(script));
        let provider = Arc::new(EchoProvider {
            tools: vec![ToolSchema::zero_arg("t", "")],
        });

        let out = run_with_tools(
            "m",
            vec![user_message("go")],
            provider,
            llm.clone(),
            HarnessConfig { max_tool_calls: 3 },
            None,
        )
        .await
        .unwrap();

        assert_eq!(out, "forced answer");
        // 3 tool iterations + 1 final = 4 calls to the completion source.
        assert_eq!(llm.call_count(), 4);

        // The last call's tools list must be empty (structurally prevents
        // another tool round) and the last message must be the system nudge.
        assert!(
            llm.nth_tools(3).is_empty(),
            "budget-exhausted final call must advertise NO tools"
        );
        let final_msgs = llm.nth_messages(3);
        let last = final_msgs.last().unwrap();
        assert_eq!(last["role"], "system");
        assert!(last["content"]
            .as_str()
            .unwrap()
            .contains("Tool budget of 3 calls exhausted"));
    }

    #[tokio::test]
    async fn max_tool_calls_is_enforced_per_dispatched_call_not_per_round() {
        // A single completion returning 5 tool_calls with a budget of 3
        // MUST dispatch only 3 of them and reject the last 2 with a budget
        // marker (matching results still emitted so the OpenAI message
        // shape stays valid). Regression guard for CodeRabbit finding on
        // this branch — pre-fix the loop counted rounds, so a single
        // burst-round could exceed the budget.
        use std::sync::atomic::{AtomicUsize, Ordering};
        struct CountingProvider {
            calls: AtomicUsize,
            tools: Vec<ToolSchema>,
        }
        #[async_trait::async_trait]
        impl ToolProvider for CountingProvider {
            async fn tools(&self) -> Vec<ToolSchema> {
                self.tools.clone()
            }
            async fn call(&self, _name: &str, _args: Value) -> Result<String> {
                self.calls.fetch_add(1, Ordering::SeqCst);
                Ok("ok".into())
            }
        }

        let burst = HarnessCompletion {
            content: String::new(),
            tool_calls: (0..5)
                .map(|i| HarnessToolCall {
                    id: format!("c{i}"),
                    name: "t".into(),
                    arguments: json!({}),
                })
                .collect(),
        };
        let script = vec![burst, plain_answer("done")];
        let llm = Arc::new(ScriptedLLM::new(script));
        let provider = Arc::new(CountingProvider {
            calls: AtomicUsize::new(0),
            tools: vec![ToolSchema::zero_arg("t", "")],
        });

        let out = run_with_tools(
            "m",
            vec![user_message("go")],
            provider.clone(),
            llm.clone(),
            HarnessConfig { max_tool_calls: 3 },
            None,
        )
        .await
        .unwrap();
        assert_eq!(out, "done");
        // Provider called 3× (budget), NOT 5×.
        assert_eq!(provider.calls.load(Ordering::SeqCst), 3);
        // 5 tool-result messages still queued (one per tool_call in the
        // burst) so the OpenAI shape is preserved.
        let msgs = llm.nth_messages(1);
        let tool_results: Vec<&Value> = msgs.iter().filter(|m| m["role"] == "tool").collect();
        assert_eq!(tool_results.len(), 5);
        // The last 2 must carry the budget-exhausted marker.
        for r in &tool_results[3..] {
            let c = r["content"].as_str().unwrap();
            assert!(
                c.contains("budget of 3 exhausted"),
                "expected budget-exhausted marker, got: {c}"
            );
        }
    }

    #[tokio::test]
    async fn tool_error_becomes_tool_result_content_not_pass_failure() {
        // A failing tool call must NOT bubble up as a pass failure — it
        // becomes an `error: ...` tool_result the LLM can read and recover
        // from. This is what lets a wrong-name / wrong-args call teach the
        // LLM to retry rather than aborting the interpretation pass.
        struct BrokenProvider;
        #[async_trait::async_trait]
        impl ToolProvider for BrokenProvider {
            async fn tools(&self) -> Vec<ToolSchema> {
                vec![ToolSchema::zero_arg("broken", "always errors")]
            }
            async fn call(&self, _name: &str, _args: Value) -> Result<String> {
                anyhow::bail!("something went wrong")
            }
        }

        let script = vec![
            tool_call_turn("c1", "broken", json!({})),
            plain_answer("recovered"),
        ];
        let llm = Arc::new(ScriptedLLM::new(script));
        let provider = Arc::new(BrokenProvider);

        let out = run_with_tools(
            "m",
            vec![user_message("go")],
            provider,
            llm.clone(),
            HarnessConfig::default(),
            None,
        )
        .await
        .unwrap();

        assert_eq!(out, "recovered");
        let msgs = llm.nth_messages(1);
        // The tool result carries the error text, prefixed with "error: ".
        assert_eq!(msgs[2]["role"], "tool");
        assert_eq!(msgs[2]["content"], "error: something went wrong");
    }

    #[test]
    fn truncate_for_event_leaves_short_text_unchanged() {
        let s = "hi";
        assert_eq!(truncate_for_event(s), "hi");
    }

    #[test]
    fn truncate_for_event_caps_long_text_with_marker() {
        let long = "x".repeat(TOOL_RESULT_EVENT_MAX_CHARS + 100);
        let out = truncate_for_event(&long);
        assert!(out.ends_with("…[truncated for event]"));
        assert!(
            out.chars().count() <= TOOL_RESULT_EVENT_MAX_CHARS + 40,
            "truncated output must be bounded (got {} chars)",
            out.chars().count()
        );
    }

    #[tokio::test]
    async fn run_with_tools_emits_tool_call_and_tool_result_events_when_ctx_present() {
        // Subscribe to the auto-processor topic BEFORE running the pass, so
        // the fire-and-forget events aren't dropped. Then script a
        // single-tool-call → plain-answer trace and assert both events
        // land with the expected step / tool_name / args / result.
        use crate::perspectives::auto_processor::events::{
            self, AutoProcessorEvent, AutoProcessorStep, InterpretationEmitContext,
        };

        let script = vec![
            tool_call_turn("c1", "query_links", json!({"source": "ns://a"})),
            plain_answer("done"),
        ];
        let llm = Arc::new(ScriptedLLM::new(script));
        let provider = Arc::new(EchoProvider {
            tools: vec![ToolSchema::zero_arg("query_links", "")],
        });
        let ctx = InterpretationEmitContext {
            perspective_uuid: "u".into(),
            processor_id: "p".into(),
            agent_did: "did:test".into(),
            item_ids: vec!["turn1".into()],
            batch_key: "bk".into(),
        };

        // Subscribe first — pubsub is broadcast-based; late subscribers miss events.
        let mut rx = events::subscribe().await;
        let out = run_with_tools(
            "m",
            vec![user_message("go")],
            provider,
            llm.clone(),
            HarnessConfig::default(),
            Some(&ctx),
        )
        .await
        .unwrap();
        assert_eq!(out, "done");

        // Drain events until we see both ToolCall + ToolResult for our tool,
        // with a short timeout so a broken emitter fails fast rather than
        // hanging CI.
        let mut saw_call = false;
        let mut saw_result = false;
        let mut saw_call_args = false;
        for _ in 0..8 {
            let evt = tokio::time::timeout(std::time::Duration::from_millis(500), rx.recv()).await;
            let Ok(Ok(raw)) = evt else { break };
            let Ok(parsed) = serde_json::from_str::<AutoProcessorEvent>(&raw) else {
                continue;
            };
            if parsed.perspective_uuid != "u" || parsed.processor_id != "p" {
                continue;
            }
            match parsed.step {
                AutoProcessorStep::ToolCall
                    if parsed.tool_name.as_deref() == Some("query_links") =>
                {
                    saw_call = true;
                    saw_call_args = parsed
                        .tool_args_json
                        .as_deref()
                        .map(|s| s.contains("\"source\":\"ns://a\""))
                        .unwrap_or(false);
                }
                AutoProcessorStep::ToolResult
                    if parsed.tool_name.as_deref() == Some("query_links") =>
                {
                    saw_result = parsed
                        .tool_result
                        .as_deref()
                        .map(|s| s.contains("query_links"))
                        .unwrap_or(false);
                }
                _ => {}
            }
            if saw_call && saw_result {
                break;
            }
        }
        assert!(
            saw_call,
            "expected a ToolCall event with tool_name=query_links"
        );
        assert!(saw_call_args, "ToolCall event must carry tool_args_json");
        assert!(
            saw_result,
            "expected a ToolResult event with tool_name=query_links + tool_result"
        );
    }

    #[tokio::test]
    async fn run_with_tools_emits_no_tool_events_when_ctx_absent() {
        // Regression: the fast headless path (no emit_ctx) must not
        // publish anything. Subscribe, run a pass with `None`, and confirm
        // no ToolCall/ToolResult events land in a short window.
        use crate::perspectives::auto_processor::events::{
            self, AutoProcessorEvent, AutoProcessorStep,
        };
        let script = vec![
            tool_call_turn("c1", "silent", json!({})),
            plain_answer("done"),
        ];
        let llm = Arc::new(ScriptedLLM::new(script));
        let provider = Arc::new(EchoProvider {
            tools: vec![ToolSchema::zero_arg("silent", "")],
        });

        let mut rx = events::subscribe().await;
        let _ = run_with_tools(
            "m",
            vec![user_message("go")],
            provider,
            llm.clone(),
            HarnessConfig::default(),
            None,
        )
        .await
        .unwrap();

        let mut saw_tool_event = false;
        for _ in 0..4 {
            let evt = tokio::time::timeout(std::time::Duration::from_millis(150), rx.recv()).await;
            let Ok(Ok(raw)) = evt else { break };
            if let Ok(parsed) = serde_json::from_str::<AutoProcessorEvent>(&raw) {
                if matches!(
                    parsed.step,
                    AutoProcessorStep::ToolCall | AutoProcessorStep::ToolResult
                ) {
                    saw_tool_event = true;
                    break;
                }
            }
        }
        assert!(
            !saw_tool_event,
            "no ToolCall/ToolResult must land when emit_ctx is None"
        );
    }
}
