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

pub mod provider;

use anyhow::Result;
use provider::ToolProvider;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::sync::Arc;

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
pub async fn run_with_tools(
    model_id: &str,
    initial_messages: Vec<Value>,
    provider: Arc<dyn ToolProvider>,
    completions: Arc<dyn CompletionSource>,
    config: HarnessConfig,
) -> Result<String> {
    let mut messages = initial_messages;

    for _ in 0..config.max_tool_calls {
        let tools = provider.tools().await;
        let completion = completions.complete(model_id, &messages, tools).await?;

        if completion.tool_calls.is_empty() {
            // Model returned a plain answer — done.
            return Ok(completion.content);
        }

        // Append the assistant tool_calls turn AND one tool-result message
        // per call, in the OpenAI-mandated shape. The tool_calls entry must
        // precede its results so the model sees the correlation on the next
        // turn.
        messages.push(assistant_tool_calls_message(&completion));
        for tc in &completion.tool_calls {
            let result = match provider.call(&tc.name, tc.arguments.clone()).await {
                Ok(text) => text,
                Err(e) => format!("error: {e}"),
            };
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
        )
        .await
        .unwrap();

        assert_eq!(out, "recovered");
        let msgs = llm.nth_messages(1);
        // The tool result carries the error text, prefixed with "error: ".
        assert_eq!(msgs[2]["role"], "tool");
        assert_eq!(msgs[2]["content"], "error: something went wrong");
    }
}
