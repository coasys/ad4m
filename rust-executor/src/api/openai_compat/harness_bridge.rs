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

use super::tool_grammar;
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

        // The harness always runs in `auto` mode: the model decides per turn
        // whether to call a tool, and an empty `tool_calls` set terminates the
        // loop. `tool_grammar::build_tool_call_parser` returns `None` for
        // `Auto` (and `None`), so this path is deliberately unconstrained;
        // tool calls are recovered from free-form text by `extract_tool_calls`
        // (XML `<tool_call>`, fenced JSON, bare-JSON, and wrapped array
        // shapes). Switching to `Required` would preclude the model from
        // ever emitting a plain-text terminating turn.
        let constraint: Option<kalosm::language::ArcParser<()>> = None;

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

        // Preserve residual assistant text alongside tool_calls.
        //
        // Small local models often emit a short pre-thought before the call
        // block — e.g. `I need to look up the task first.\n<tool_call>{...}
        // </tool_call>`. The earlier "blank content on tool-call turn"
        // dropped that scratchpad, so the next iteration's prompt lost the
        // model's own reasoning about WHY it made this call (Lal's PR #911
        // review, harness_bridge.rs:130). Strip only the `<tool_call>...
        // </tool_call>` blocks; keep everything else as content on the
        // assistant message the harness loop appends.
        //
        // When no tool_calls were emitted, the entire text is the answer —
        // pass it through verbatim.
        let content = if tool_calls.is_empty() {
            result.text
        } else {
            strip_tool_call_blocks(&result.text)
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

/// Return `text` with every `<tool_call>…</tool_call>` block removed,
/// preserving the residual assistant scratchpad. Used to keep the model's
/// pre-thought (e.g. `"I need to look up the task first."`) in
/// `HarnessCompletion.content` on a tool-call turn without also re-emitting
/// the call block into the next prompt (which would look like the model was
/// re-issuing the same call).
///
/// Whitespace collapse: consecutive blank lines left behind by removed
/// blocks are folded to a single newline and the whole string is trimmed,
/// so an all-block reply returns `""` (existing behaviour).
fn strip_tool_call_blocks(text: &str) -> String {
    const OPEN: &str = "<tool_call>";
    const CLOSE: &str = "</tool_call>";

    let mut out = String::with_capacity(text.len());
    let mut rest = text;
    while let Some(start) = rest.find(OPEN) {
        out.push_str(&rest[..start]);
        let after = &rest[start + OPEN.len()..];
        match after.find(CLOSE) {
            Some(end) => rest = &after[end + CLOSE.len()..],
            // Unterminated block — drop the rest to avoid re-emitting a
            // half-parsed call. Matches `extract_tool_calls`'s tolerance.
            None => {
                rest = "";
                break;
            }
        }
    }
    out.push_str(rest);

    // Fold ≥2 consecutive newlines to a single newline (residual whitespace
    // from removed blocks); then trim edges so a pure-block reply is `""`.
    let mut collapsed = String::with_capacity(out.len());
    let mut prev_was_newline = false;
    for ch in out.chars() {
        if ch == '\n' {
            if !prev_was_newline {
                collapsed.push(ch);
            }
            prev_was_newline = true;
        } else {
            collapsed.push(ch);
            prev_was_newline = false;
        }
    }
    collapsed.trim().to_string()
}

/// Coerce a message-content `Value` into a plain string.
///
/// The harness constructs string-content messages today, but the OpenAI
/// wire spec allows `content` to be an array of typed content-parts
/// (`[{type:"text",text:"..."}, {type:"image_url",...}]`) — Lal's PR #911
/// review, harness_bridge.rs:232, flagged the earlier
/// `.as_str().unwrap_or("")` as silently dropping the whole message when
/// such a value ever arrived. Robust flattening:
///   * `String` → the string
///   * `Array` → concatenate `text` fields of any text parts, newline-
///     separated; non-text parts render as their JSON so nothing is silently
///     lost from the model's view
///   * `Object` (single content-part) → same handling as an array of one
///   * anything else → the value's JSON dump (defensive; better a visible
///     placeholder than a blank message)
fn flatten_content_value(v: &Value) -> String {
    match v {
        Value::Null => String::new(),
        Value::String(s) => s.clone(),
        Value::Array(parts) => {
            let mut out = String::new();
            for part in parts {
                let piece = flatten_content_part(part);
                if piece.is_empty() {
                    continue;
                }
                if !out.is_empty() {
                    out.push('\n');
                }
                out.push_str(&piece);
            }
            out
        }
        Value::Object(_) => flatten_content_part(v),
        other => other.to_string(),
    }
}

/// One content-part → text. Understands OpenAI's `{type:"text", text: "…"}`
/// shape; renders anything else as JSON so unknown modalities don't vanish
/// silently.
fn flatten_content_part(part: &Value) -> String {
    if let Some(text) = part.get("text").and_then(Value::as_str) {
        return text.to_string();
    }
    // Non-text part (image_url, input_audio, etc.): render as JSON. The LLM
    // sees it as raw shape, which is better than an empty message.
    part.to_string()
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
        .map(flatten_content_value)
        .unwrap_or_default();

    match role {
        // Tool result → a `<tool_response>` block folded into a user turn,
        // since the underlying chat template has no tool role. Includes the
        // `tool_call_id` when present so parallel tool_calls in one round
        // can be correlated back to their invocations on the next prompt
        // (Lal's PR #911 review, harness_bridge.rs:186). Absent id → bare
        // `<tool_response>` for backwards-compat with earlier turns and
        // hand-written test messages.
        "tool" => {
            // Tool result → `<tool_response>` block folded into a user
            // turn (chat template has no tool role). Round-trips
            // `tool_call_id` when present so parallel calls can be
            // correlated back to their invocations on the next prompt.
            // Shared renderer with `chat::flatten_message` — fix once,
            // fix both.
            let id = m.get("tool_call_id").and_then(|v| v.as_str());
            Ok((
                "user".to_string(),
                tool_grammar::render_tool_response_block(&content, id),
            ))
        }
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
                if !text.is_empty() {
                    text.push('\n');
                }
                text.push_str(&tool_grammar::render_tool_call_block(name, args_str));
            }
            Ok(("assistant".to_string(), text))
        }
        _ => Ok((role.to_string(), content)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn strip_tool_call_blocks_preserves_pre_and_post_thought() {
        let text = "I need to look up the task first.\n\
                    <tool_call>\n{\"name\": \"query\", \"arguments\": {}}\n</tool_call>\n\
                    Then I will link.";
        let residual = strip_tool_call_blocks(text);
        assert!(
            residual.contains("I need to look up the task first."),
            "pre-thought lost: {residual}"
        );
        assert!(
            residual.contains("Then I will link."),
            "post-thought lost: {residual}"
        );
        assert!(
            !residual.contains("<tool_call>"),
            "call block leaked into content: {residual}"
        );
    }

    #[test]
    fn strip_tool_call_blocks_multiple_calls_returns_only_scratchpad() {
        let text = "First, query beliefs.\n\
                    <tool_call>\n{\"name\":\"q1\",\"arguments\":{}}\n</tool_call>\n\
                    Second, query intentions.\n\
                    <tool_call>\n{\"name\":\"q2\",\"arguments\":{}}\n</tool_call>";
        let residual = strip_tool_call_blocks(text);
        assert!(residual.contains("First, query beliefs."), "{residual}");
        assert!(residual.contains("Second, query intentions."), "{residual}");
        assert!(!residual.contains("q1"), "{residual}");
        assert!(!residual.contains("q2"), "{residual}");
    }

    #[test]
    fn strip_tool_call_blocks_all_block_reply_returns_empty() {
        let text = "<tool_call>\n{\"name\":\"q\",\"arguments\":{}}\n</tool_call>";
        assert_eq!(strip_tool_call_blocks(text), "");
    }

    #[test]
    fn strip_tool_call_blocks_plain_text_pass_through() {
        assert_eq!(strip_tool_call_blocks("Hello world"), "Hello world");
    }

    #[test]
    fn strip_tool_call_blocks_unterminated_block_is_dropped_from_the_open_tag() {
        // Truncated stream — keep prefix, drop the malformed remainder so it
        // can't re-appear in the next prompt as a broken call.
        let text = "before\n<tool_call>\n{\"name\": \"q\", \"arguments\":";
        let residual = strip_tool_call_blocks(text);
        assert!(residual.contains("before"), "{residual}");
        assert!(!residual.contains("<tool_call>"), "{residual}");
        assert!(!residual.contains("\"name\": \"q\""), "{residual}");
    }

    #[test]
    fn flatten_tool_message_carries_tool_call_id_when_present() {
        let msg = json!({
            "role": "tool",
            "tool_call_id": "call_abc123",
            "content": "42",
        });
        let (role, body) = flatten_json_message(&msg).unwrap();
        assert_eq!(role, "user");
        assert!(
            body.contains("id=\"call_abc123\""),
            "tool_call_id must round-trip into <tool_response id=...>; got: {body}"
        );
        assert!(body.contains("\n42\n"), "content lost: {body}");
    }

    #[test]
    fn flatten_tool_message_without_id_falls_back_to_bare_tag() {
        let msg = json!({
            "role": "tool",
            "content": "42",
        });
        let (role, body) = flatten_json_message(&msg).unwrap();
        assert_eq!(role, "user");
        assert!(
            !body.contains("id="),
            "no id key should mean no id attribute; got: {body}"
        );
        assert!(body.starts_with("<tool_response>\n"), "{body}");
    }

    #[test]
    fn flatten_content_value_preserves_string() {
        assert_eq!(flatten_content_value(&json!("hello")), "hello");
    }

    #[test]
    fn flatten_content_value_folds_openai_text_parts_array() {
        // OpenAI wire spec: content can be an array of typed parts. The
        // earlier `.as_str().unwrap_or("")` silently dropped every message
        // in that shape (Lal's PR #911 review). Text parts must survive.
        let v = json!([
            {"type": "text", "text": "first paragraph"},
            {"type": "text", "text": "second paragraph"},
        ]);
        assert_eq!(
            flatten_content_value(&v),
            "first paragraph\nsecond paragraph"
        );
    }

    #[test]
    fn flatten_content_value_renders_non_text_parts_as_json_not_empty() {
        // An image_url or input_audio part shouldn't vanish silently — the
        // LLM sees it as raw JSON, which is at least a visible signal that
        // the message carried something the harness didn't yet decode.
        let v = json!([
            {"type": "text", "text": "look at this:"},
            {"type": "image_url", "image_url": {"url": "https://example.test/a.png"}},
        ]);
        let out = flatten_content_value(&v);
        assert!(out.starts_with("look at this:\n"), "{out}");
        assert!(
            out.contains("image_url"),
            "non-text part must render, got: {out}"
        );
    }

    #[test]
    fn flatten_content_value_handles_single_object_part() {
        // Some callers ship a bare object instead of an array of one.
        let v = json!({"type": "text", "text": "solo"});
        assert_eq!(flatten_content_value(&v), "solo");
    }

    #[test]
    fn flatten_content_value_null_and_missing_are_empty() {
        assert_eq!(flatten_content_value(&Value::Null), "");
    }

    #[test]
    fn flatten_tool_message_id_is_json_encoded_to_survive_quotes() {
        let msg = json!({
            "role": "tool",
            "tool_call_id": "call_\"weird\"_id",
            "content": "x",
        });
        let (_, body) = flatten_json_message(&msg).unwrap();
        // The encoded id has escaped quotes inside a quoted string — the
        // block must stay parseable if the model reads it back.
        assert!(
            body.contains("id=\"call_\\\"weird\\\"_id\""),
            "malformed encoding: {body}"
        );
    }
}
