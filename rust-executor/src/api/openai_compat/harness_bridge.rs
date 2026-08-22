//! `OpenAiCompatBridge` — the real `CompletionSource` that wires the
//! interpretation-pass harness loop to `AIService::prompt_messages` +
//! the tool-grammar constrained-decoding path Josh cherry-picked into
//! `/v1/chat/completions`.
//!
//! Lives here (not under `ai_service::harness`) because it depends on
//! [`super::tool_grammar`] and [`super::types::ToolDef`] which are already
//! declared under `api::openai_compat`. Putting the bridge here avoids an
//! `ai_service → openai_compat` cycle: `openai_compat` already depends on
//! `ai_service`, so ownership flows one way only.
//!
//! Design: [[planning/llm-harness-design-2026-08-21-v3.md]] §6 — "reuse the
//! `/v1` tool-calling wire code (system-prompt injection + grammar-
//! constrained decoding + text-side extraction) so local AND remote models
//! see tools through one uniform path."

use super::tool_grammar::{self, ToolChoice};
use super::types::{FunctionDef, ToolDef};
use crate::ai_service::harness::provider::ToolSchema;
use crate::ai_service::harness::{CompletionSource, HarnessCompletion, HarnessToolCall};
use crate::ai_service::AIService;
use anyhow::{anyhow, Result};
use serde_json::Value;
use std::sync::Arc;
use uuid::Uuid;

/// Bridge that lets the harness loop talk to a real `AIService`.
///
/// Held as a plain field, not an `Arc<AIService>`, because `AIService` is
/// a global singleton reached via `AIService::global_instance()` — the
/// bridge just remembers a handle for the duration of the interpretation
/// pass. `auth_token` is per-pass (billing context follows the pass owner).
pub struct OpenAiCompatBridge {
    service: Arc<AIService>,
    auth_token: Option<String>,
}

impl OpenAiCompatBridge {
    pub fn new(service: Arc<AIService>, auth_token: Option<String>) -> Self {
        Self {
            service,
            auth_token,
        }
    }
}

#[async_trait::async_trait]
impl CompletionSource for OpenAiCompatBridge {
    async fn complete(
        &self,
        model_id: &str,
        messages: &[Value],
        tools: Vec<ToolSchema>,
    ) -> Result<HarnessCompletion> {
        // Convert the harness's ToolSchema list into openai-compat ToolDef —
        // same fields, different owning module. A schema-side rename would
        // let us drop this conversion; keep it explicit for now so the
        // harness type stays independent of the wire layer.
        let tool_defs: Vec<ToolDef> = tools.iter().map(harness_schema_to_tool_def).collect();
        let tools_active = !tool_defs.is_empty();

        // Assemble (role, content) message pairs. When tools are advertised
        // we prepend the Hermes/Qwen `<tools>` system prompt so the model
        // knows the surface. Empty `tools` = plain completion (used by the
        // budget-exhausted final call in the loop).
        let mut flat: Vec<(String, String)> =
            Vec::with_capacity(messages.len() + if tools_active { 1 } else { 0 });
        if tools_active {
            flat.push((
                "system".to_string(),
                tool_grammar::render_tools_system_prompt(&tool_defs),
            ));
        }
        for m in messages {
            flat.push(flatten_json_message(m)?);
        }

        // Grammar-constrained decoding only kicks in when tools are active.
        // Choice is `auto` here — the harness never forces a specific tool;
        // it lets the model decide per turn (empty tool_calls terminates).
        let constraint = if tools_active {
            tool_grammar::build_tool_call_parser(
                &tool_defs,
                &ToolChoice::Auto,
                /* parallel_tool_calls */ true,
            )
        } else {
            None
        };

        let result = self
            .service
            .prompt_messages(
                model_id.to_string(),
                flat,
                self.auth_token.clone(),
                constraint,
            )
            .await?;

        // The model's text is either a plain answer or a tool-call block
        // (or both — some models emit a short pre-thought before the call).
        // `extract_tool_calls` returns [] for pure-text answers, which the
        // harness reads as "we're done".
        let extracted = if tools_active {
            tool_grammar::extract_tool_calls(&result.text)
        } else {
            Vec::new()
        };

        let tool_calls: Vec<HarnessToolCall> = extracted
            .into_iter()
            .map(|c| {
                // ExtractedToolCall.arguments is a JSON-encoded string (matches
                // OpenAI's `function.arguments` wire shape). Parse it back into
                // a Value here so the provider receives a real object; a bad
                // parse degrades to an empty object rather than aborting the
                // whole pass — the tool will get `{}` and produce its own
                // wrong-shape error, which the LLM can read and retry.
                let arguments = serde_json::from_str::<Value>(&c.arguments)
                    .unwrap_or_else(|_| Value::Object(Default::default()));
                // Stable `call_…` id so the assistant→tool→assistant triple
                // the harness appends can be correlated by the LLM on the next
                // turn. Matches the id shape /v1 mints.
                HarnessToolCall {
                    id: format!("call_{}", Uuid::new_v4()),
                    name: c.name,
                    arguments,
                }
            })
            .collect();

        // When the model emitted tool calls, wire content is `""` — the
        // model's tool-call block already went into the extracted vec, so
        // leaving `content` as the raw text would double it into the next
        // prompt turn. When it emitted plain text, content is that text.
        let content = if tool_calls.is_empty() {
            result.text
        } else {
            String::new()
        };

        Ok(HarnessCompletion {
            content,
            tool_calls,
        })
    }
}

fn harness_schema_to_tool_def(s: &ToolSchema) -> ToolDef {
    ToolDef {
        kind: "function".to_string(),
        function: FunctionDef {
            name: s.name.clone(),
            description: if s.description.is_empty() {
                None
            } else {
                Some(s.description.clone())
            },
            parameters: Some(s.parameters.clone()),
        },
    }
}

/// Fold one harness-shape JSON message into `(role, text)` for the
/// `prompt_messages` API. Mirrors `chat::flatten_message` but consumes raw
/// `serde_json::Value` (that's what the harness loop constructs) instead
/// of the typed `ChatMessage`.
///
/// Only `role: "tool"` and `role: "assistant"+tool_calls` need special
/// handling; all others pass through with `content` as-is.
fn flatten_json_message(m: &Value) -> Result<(String, String)> {
    let role = m
        .get("role")
        .and_then(|r| r.as_str())
        .ok_or_else(|| anyhow!("harness message missing `role`"))?;

    let content = m
        .get("content")
        .and_then(|c| c.as_str())
        .unwrap_or("")
        .to_string();

    match role {
        // Tool result → a `<tool_response>` block folded into a user turn,
        // since the underlying chat template has no tool role.
        "tool" => Ok((
            "user".to_string(),
            format!("<tool_response>\n{content}\n</tool_response>"),
        )),
        // Assistant turn that carried tool_calls → re-render them in the
        // Qwen `<tool_call>` convention so the model sees its own prior
        // invocations on this turn's prompt.
        "assistant" => {
            let calls = m.get("tool_calls").and_then(|v| v.as_array());
            let has_calls = calls.map_or(false, |a| !a.is_empty());
            if !has_calls {
                return Ok(("assistant".to_string(), content));
            }
            let mut text = content;
            for call in calls.unwrap() {
                let name = call
                    .get("function")
                    .and_then(|f| f.get("name"))
                    .and_then(|n| n.as_str())
                    .unwrap_or("");
                let args_str = call
                    .get("function")
                    .and_then(|f| f.get("arguments"))
                    .and_then(|a| a.as_str())
                    .unwrap_or("{}");
                let args = args_str.trim();
                let args = if args.is_empty() { "{}" } else { args };
                if !text.is_empty() {
                    text.push('\n');
                }
                text.push_str(&format!(
                    "<tool_call>\n{{\"name\": \"{name}\", \"arguments\": {args}}}\n</tool_call>"
                ));
            }
            Ok(("assistant".to_string(), text))
        }
        _ => Ok((role.to_string(), content)),
    }
}
