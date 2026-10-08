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
use crate::ai_service::providers::{ChatTurn, ToolCall, ToolSpec};
use crate::ai_service::AIService;
use anyhow::{anyhow, Result};
use serde_json::Value;
use std::collections::VecDeque;
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
        // A provider whose wire format carries tools gets them handed over as
        // definitions, and answers with structured calls. Everything else
        // takes the prompt-injection path below, which works against any model
        // at all — including every local one, which is why it stays.
        if !tools.is_empty() && AIService::model_supports_native_tools(model_id) {
            return self.complete_natively(model_id, messages, tools).await;
        }

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

impl OpenAiCompatBridge {
    /// The tools-as-data path, for a provider whose wire format has them.
    ///
    /// Structurally simpler than the injected path it replaces: no `<tools>`
    /// system prompt, no grammar, no recovering calls out of prose and no
    /// stripping the blocks back out afterwards. The model is handed schemas
    /// and answers with calls.
    async fn complete_natively(
        &self,
        model_id: &str,
        messages: &[Value],
        tools: Vec<ToolSchema>,
    ) -> Result<HarnessCompletion> {
        let turns = structured_turns(messages)?;

        let specs = tools
            .iter()
            .map(|schema| ToolSpec {
                name: schema.name.clone(),
                description: schema.description.clone(),
                parameters: schema.parameters.clone(),
            })
            .collect();

        let reply = self
            .service
            .prompt_with_tools(model_id.to_string(), turns, specs, self.auth_token.clone())
            .await?;

        Ok(HarnessCompletion {
            content: reply.text,
            tool_calls: reply
                .tool_calls
                .into_iter()
                .map(|call| HarnessToolCall {
                    id: call.id,
                    name: call.name,
                    arguments: call.arguments,
                })
                .collect(),
        })
    }
}

/// The whole conversation as structured turns, with every call and result
/// paired.
///
/// Anthropic accepts a `tool_use` only if the turn after it answers it, and a
/// `tool_result` only if the turn before it made the call; either one unpaired
/// and the whole request is refused. Mapping each message on its own cannot
/// hold that, because pairing is a fact about neighbours. So the calls of the
/// latest assistant turn stay open while its results arrive:
///
/// - A result naming an open call answers it.
/// - A result with no id answers the oldest open call. This is what makes an
///   id minted by [`to_provider_call`] reachable: a client that omits the id on
///   a call omits it on the result too, and order is the only pairing left.
/// - A result that answers nothing open travels as an ordinary user turn, so
///   what the tool said still reaches the model. While calls are open it waits
///   until their results are in. The API reads consecutive user turns as one
///   message, and text ahead of a `tool_result` in that message breaks the
///   pairing as surely as a missing result does.
/// - A call still open when the next non-tool message arrives is removed from
///   the turn that made it. Its text stays. If it had none, the turn goes too:
///   an empty assistant message is refused like an unpaired call.
///
/// The harness gives every call an id and appends a result for every call,
/// budget-exhausted ones included, so on its own conversations none of the
/// last three cases fires.
pub(super) fn structured_turns(messages: &[Value]) -> Result<Vec<ChatTurn>> {
    let mut turns: Vec<ChatTurn> = Vec::with_capacity(messages.len());
    let mut open: Option<OpenCalls> = None;

    for m in messages {
        let turn = structured_turn(m)?;

        if m.get("role").and_then(|r| r.as_str()) == Some("tool") {
            match open.as_mut() {
                Some(calls) => match calls.answer(&turn) {
                    Some(id) => turns.push(ChatTurn::tool_result(id, turn.content)),
                    None => calls.unpaired.push(ChatTurn::user(turn.content)),
                },
                None => turns.push(ChatTurn::user(turn.content)),
            }
            continue;
        }

        if let Some(calls) = open.take() {
            calls.close(&mut turns);
        }
        if !turn.tool_calls.is_empty() {
            open = Some(OpenCalls {
                turn: turns.len(),
                waiting: turn.tool_calls.iter().map(|c| c.id.clone()).collect(),
                unpaired: Vec::new(),
            });
        }
        turns.push(turn);
    }

    if let Some(calls) = open.take() {
        calls.close(&mut turns);
    }
    Ok(turns)
}

/// The latest assistant turn that made calls, while its results arrive.
struct OpenCalls {
    /// Its index in the turns built so far.
    turn: usize,
    /// Ids of its calls no result has answered yet, in order.
    waiting: VecDeque<String>,
    /// Results that answered none of them, held until the run of results ends.
    unpaired: Vec<ChatTurn>,
}

impl OpenCalls {
    /// The call this result answers, if any. See [`structured_turns`].
    fn answer(&mut self, result: &ChatTurn) -> Option<String> {
        match &result.tool_result_for {
            Some(id) => {
                let i = self.waiting.iter().position(|w| w == id)?;
                self.waiting.remove(i)
            }
            None => self.waiting.pop_front(),
        }
    }

    /// Remove the calls nothing answered, then append the held results.
    fn close(self, turns: &mut Vec<ChatTurn>) {
        if !self.waiting.is_empty() {
            let calling = &mut turns[self.turn];
            calling.tool_calls.retain(|c| !self.waiting.contains(&c.id));
            // Only a turn left with no calls can be removed, and a turn with no
            // calls has no results after it, so no other index moves.
            if calling.tool_calls.is_empty() && calling.content.trim().is_empty() {
                turns.remove(self.turn);
            }
        }
        turns.extend(self.unpaired);
    }
}

/// One harness message as a structured turn, keeping the tool information the
/// text path folds into prose.
///
/// The harness appends an assistant turn carrying `tool_calls` and a
/// `role:"tool"` result after every dispatch, so these are the shapes that
/// actually arrive — a turn with neither is ordinary text.
fn structured_turn(m: &Value) -> Result<ChatTurn> {
    let role = m
        .get("role")
        .and_then(|r| r.as_str())
        .ok_or_else(|| anyhow!("harness message missing `role`"))?;

    let content = m
        .get("content")
        .map(flatten_content_value)
        .unwrap_or_default();

    match role {
        "system" => Ok(ChatTurn::system(content)),
        "tool" => {
            // Anthropic rejects a `tool_result` whose `tool_use_id` is empty,
            // so an unidentified result starts out as an ordinary user turn,
            // which keeps what the tool said in front of the model.
            // `structured_turns` pairs it with an open call where one exists.
            match m.get("tool_call_id").and_then(|v| v.as_str()) {
                Some(call_id) if !call_id.is_empty() => Ok(ChatTurn::tool_result(call_id, content)),
                _ => Ok(ChatTurn::user(content)),
            }
        }
        "assistant" => {
            let calls: Vec<ToolCall> = m
                .get("tool_calls")
                .and_then(|v| v.as_array())
                .map(|calls| calls.iter().filter_map(to_provider_call).collect())
                .unwrap_or_default();

            if calls.is_empty() {
                Ok(ChatTurn::assistant(content))
            } else {
                Ok(ChatTurn::assistant_calling(content, calls))
            }
        }
        // "user", and anything unrecognised. A role we do not know is safer
        // read as the user speaking than dropped: the text still reaches the
        // model, which is what the injected path would also have done.
        _ => Ok(ChatTurn::user(content)),
    }
}

/// One entry of an assistant turn's `tool_calls`, as the providers want it.
///
/// `arguments` arrives as a JSON *string* on the OpenAI wire and as an object
/// once it has been through the harness. Both are accepted.
///
/// Arguments that are not an object — a string that will not parse, and one
/// that parses to an array, a number or `null` — are kept rather than dropped,
/// under `_raw`. Dropping
/// the call looked safer and is not: the harness has already dispatched it and
/// appends the matching `tool_result` on the next turn, so a missing `tool_use`
/// leaves a result whose `tool_use_id` refers to nothing, and Anthropic rejects
/// the whole request. A call the model can see went wrong is recoverable; an
/// unbalanced conversation is not.
///
/// A call with no id gets one minted, for the same reason. An empty `tool_use`
/// id is refused by the wire, but dropping the call produces exactly the
/// unbalanced conversation the paragraph above is about, so the missing id
/// argues for replacing it rather than for discarding the call. `complete`
/// already mints `call_{uuid}` for every call it sees, so this is the same
/// shape by the time anything downstream reads it. No result can name a minted
/// id, so [`structured_turns`] pairs by order, and removes the call if nothing
/// answers it.
fn to_provider_call(raw: &Value) -> Option<ToolCall> {
    let function = raw.get("function").unwrap_or(raw);
    let name = function.get("name")?.as_str()?.to_string();

    let id = raw.get("id").and_then(|v| v.as_str()).unwrap_or_default();
    let id = if id.is_empty() {
        format!("call_{}", Uuid::new_v4())
    } else {
        id.to_string()
    };

    let arguments = match function.get("arguments") {
        Some(Value::String(text)) => match serde_json::from_str::<Value>(text) {
            Ok(value) if value.is_object() => value,
            // `"[]"`, `"1"` and `"null"` are valid JSON and invalid arguments:
            // a `tool_use` input must be an object. They go under `_raw` with
            // the strings that did not parse at all, for the same reason — the
            // call has to stay, or its result has nothing to name.
            _ => serde_json::json!({ "_raw": text }),
        },
        Some(value) if value.is_object() => value.clone(),
        Some(value) => serde_json::json!({ "_raw": value.to_string() }),
        None => Value::Object(Default::default()),
    };

    Some(ToolCall {
        id,
        name,
        arguments,
    })
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

// ---------------------------------------------------------------------------
// structured_turn — the native path's message mapping
// ---------------------------------------------------------------------------

#[cfg(test)]
mod native_mapping_tests {
    use super::*;
    use crate::ai_service::providers::ChatRole;
    use serde_json::json;

    #[test]
    fn a_tool_message_becomes_a_tool_result_naming_its_call() {
        let turn = structured_turn(&json!({
            "role": "tool",
            "tool_call_id": "call_7",
            "content": "42",
        }))
        .expect("maps");

        assert_eq!(turn.role, ChatRole::User);
        assert_eq!(turn.tool_result_for.as_deref(), Some("call_7"));
        assert_eq!(turn.content, "42");
    }

    #[test]
    fn an_assistant_turn_keeps_its_calls_and_its_text() {
        let turn = structured_turn(&json!({
            "role": "assistant",
            "content": "Looking that up.",
            "tool_calls": [{
                "id": "call_7",
                "function": { "name": "search", "arguments": "{\"q\":\"x\"}" },
            }],
        }))
        .expect("maps");

        assert_eq!(turn.role, ChatRole::Assistant);
        assert_eq!(turn.content, "Looking that up.");
        assert_eq!(turn.tool_calls.len(), 1);
        assert_eq!(turn.tool_calls[0].id, "call_7");
        assert_eq!(turn.tool_calls[0].name, "search");
        // The wire carries arguments as a JSON string; providers want the
        // object, so it is parsed exactly once, here.
        assert_eq!(turn.tool_calls[0].arguments["q"], "x");
    }

    #[test]
    fn already_parsed_arguments_are_taken_as_they_are() {
        // The harness appends its own assistant turns with arguments already
        // an object, so both shapes reach this function in one conversation.
        let turn = structured_turn(&json!({
            "role": "assistant",
            "content": "",
            "tool_calls": [{
                "id": "call_8",
                "function": { "name": "count", "arguments": { "n": 3 } },
            }],
        }))
        .expect("maps");

        assert_eq!(turn.tool_calls[0].arguments["n"], 3);
    }

    #[test]
    fn a_call_with_unparseable_arguments_is_kept_under_raw() {
        let turn = structured_turn(&json!({
            "role": "assistant",
            "content": "hmm",
            "tool_calls": [{
                "id": "call_9",
                "function": { "name": "search", "arguments": "{not json" },
            }],
        }))
        .expect("maps");

        // Dropping the call would leave the tool_result the harness appends
        // next turn pointing at nothing, and Anthropic rejects a request whose
        // tool_use_id matches no tool_use.
        assert_eq!(turn.tool_calls.len(), 1);
        assert_eq!(turn.tool_calls[0].arguments["_raw"], "{not json");
    }

    #[test]
    fn arguments_that_parse_to_something_other_than_an_object_go_under_raw_too() {
        // `"[]"` is valid JSON and an invalid `tool_use` input: the provider
        // wants an object. Passing it through refuses the whole conversation,
        // and dropping the call unbalances it, so it joins the `_raw` case.
        for arguments in [json!("[1,2]"), json!("7"), json!("null"), json!([1, 2])] {
            let turn = structured_turn(&json!({
                "role": "assistant",
                "content": "",
                "tool_calls": [{
                    "id": "call_10",
                    "function": { "name": "search", "arguments": arguments },
                }],
            }))
            .expect("maps");

            assert_eq!(turn.tool_calls.len(), 1);
            assert!(
                turn.tool_calls[0].arguments.is_object(),
                "{arguments} reached the provider as a non-object"
            );
            assert!(turn.tool_calls[0].arguments["_raw"].is_string());
        }
    }

    #[test]
    fn a_call_without_an_id_is_given_one_rather_than_dropped() {
        // An empty tool_use id is refused by the API — which argues for
        // replacing the id, not for discarding the call. Dropping it produces
        // the same unbalanced conversation that keeping unparseable arguments
        // under `_raw` exists to avoid.
        let turn = structured_turn(&json!({
            "role": "assistant",
            "content": "hmm",
            "tool_calls": [{ "function": { "name": "search", "arguments": "{}" } }],
        }))
        .expect("maps");

        assert_eq!(turn.tool_calls.len(), 1);
        assert_eq!(turn.tool_calls[0].name, "search");
        assert!(
            turn.tool_calls[0].id.starts_with("call_"),
            "a minted id matches the shape `complete` produces: {}",
            turn.tool_calls[0].id
        );
        assert_eq!(turn.content, "hmm");
    }

    #[test]
    fn a_call_with_an_empty_id_is_given_one_too() {
        // An id present but empty is the same failure as an absent one: the
        // wire refuses it.
        let turn = structured_turn(&json!({
            "role": "assistant",
            "content": "hmm",
            "tool_calls": [{ "id": "", "function": { "name": "search", "arguments": "{}" } }],
        }))
        .expect("maps");

        assert_eq!(turn.tool_calls.len(), 1);
        assert!(!turn.tool_calls[0].id.is_empty());
    }

    #[test]
    fn a_tool_result_without_an_id_becomes_an_ordinary_user_turn() {
        // Anthropic rejects an empty tool_use_id. The text still has to reach
        // the model, so it travels as a user turn.
        let turn = structured_turn(&json!({ "role": "tool", "content": "42" })).expect("maps");

        assert_eq!(turn.role, ChatRole::User);
        assert!(turn.tool_result_for.is_none());
        assert_eq!(turn.content, "42");
    }

    #[test]
    fn a_tool_result_with_an_empty_id_becomes_an_ordinary_user_turn() {
        let turn = structured_turn(&json!({
            "role": "tool", "tool_call_id": "", "content": "42",
        }))
        .expect("maps");

        assert!(turn.tool_result_for.is_none());
    }

    #[test]
    fn an_id_less_call_and_an_id_less_result_pair_up() {
        let turns = structured_turns(&[
            json!({ "role": "user", "content": "find x" }),
            json!({
                "role": "assistant", "content": "",
                "tool_calls": [{ "function": { "name": "search", "arguments": "{}" } }],
            }),
            json!({ "role": "tool", "content": "42" }),
        ])
        .expect("maps");

        let minted = &turns[1].tool_calls[0].id;
        assert!(minted.starts_with("call_"), "{minted}");
        assert_eq!(turns[2].tool_result_for.as_deref(), Some(minted.as_str()));
        assert_eq!(turns[2].content, "42");
    }

    #[test]
    fn id_less_results_answer_parallel_calls_in_order() {
        let turns = structured_turns(&[
            json!({
                "role": "assistant", "content": "",
                "tool_calls": [
                    { "function": { "name": "first", "arguments": "{}" } },
                    { "function": { "name": "second", "arguments": "{}" } },
                ],
            }),
            json!({ "role": "tool", "content": "one" }),
            json!({ "role": "tool", "content": "two" }),
        ])
        .expect("maps");

        let calls = &turns[0].tool_calls;
        assert_eq!(turns[1].tool_result_for.as_ref(), Some(&calls[0].id));
        assert_eq!(turns[2].tool_result_for.as_ref(), Some(&calls[1].id));
    }

    #[test]
    fn a_call_nothing_answers_does_not_go_out() {
        // An unanswered tool_use is refused just as an orphaned tool_result
        // is. The call is removed; the text the model wrote with it is not.
        let turns = structured_turns(&[
            json!({
                "role": "assistant", "content": "Looking that up.",
                "tool_calls": [{ "function": { "name": "search", "arguments": "{}" } }],
            }),
            json!({ "role": "user", "content": "never mind" }),
        ])
        .expect("maps");

        assert!(turns[0].tool_calls.is_empty());
        assert_eq!(turns[0].content, "Looking that up.");
        assert_eq!(turns[1].content, "never mind");
    }

    #[test]
    fn a_trailing_call_nothing_answers_does_not_go_out_either() {
        let turns = structured_turns(&[json!({
            "role": "assistant", "content": "",
            "tool_calls": [{ "id": "call_1", "function": { "name": "search", "arguments": "{}" } }],
        })])
        .expect("maps");

        // No calls left and no text, so nothing of the turn remains.
        assert!(turns.is_empty());
    }

    #[test]
    fn a_result_naming_a_call_that_is_not_open_travels_as_text() {
        // The mixed case: the call's id was minted, so the result's own id
        // points at nothing. Sent as a tool_result it would be refused.
        let turns = structured_turns(&[
            json!({
                "role": "assistant", "content": "",
                "tool_calls": [{ "function": { "name": "search", "arguments": "{}" } }],
            }),
            json!({ "role": "tool", "tool_call_id": "call_elsewhere", "content": "42" }),
        ])
        .expect("maps");

        // The call it did not answer is removed, and with no text the turn
        // that made it goes too.
        assert_eq!(turns.len(), 1);
        assert!(turns[0].tool_result_for.is_none());
        assert_eq!(turns[0].content, "42");
    }

    #[test]
    fn a_stray_result_does_not_come_between_a_call_and_its_result() {
        // Sent where it arrived, the stray text would sit ahead of call_a's
        // tool_result in the same user message, and the API refuses that.
        let turns = structured_turns(&[
            json!({
                "role": "assistant", "content": "",
                "tool_calls": [{ "id": "call_a", "function": { "name": "search", "arguments": "{}" } }],
            }),
            json!({ "role": "tool", "tool_call_id": "call_elsewhere", "content": "stray" }),
            json!({ "role": "tool", "tool_call_id": "call_a", "content": "42" }),
        ])
        .expect("maps");

        assert_eq!(turns.len(), 3);
        assert_eq!(turns[0].tool_calls[0].id, "call_a");
        assert_eq!(turns[1].tool_result_for.as_deref(), Some("call_a"));
        assert_eq!(turns[1].content, "42");
        assert!(turns[2].tool_result_for.is_none());
        assert_eq!(turns[2].content, "stray");
    }

    #[test]
    fn a_turn_left_with_no_calls_and_no_text_is_removed() {
        let turns = structured_turns(&[
            json!({ "role": "user", "content": "find x" }),
            json!({
                "role": "assistant", "content": "",
                "tool_calls": [{ "function": { "name": "search", "arguments": "{}" } }],
            }),
            json!({ "role": "user", "content": "never mind" }),
        ])
        .expect("maps");

        assert_eq!(turns.len(), 2);
        assert_eq!(turns[0].content, "find x");
        assert_eq!(turns[1].content, "never mind");
    }

    #[test]
    fn the_harness_shape_passes_through_unchanged() {
        // Ids on both sides, a result for every call: the only shape the
        // harness produces, and none of the repairs above may touch it.
        let turns = structured_turns(&[
            json!({ "role": "user", "content": "go" }),
            json!({
                "role": "assistant", "content": "",
                "tool_calls": [
                    { "id": "call_a", "function": { "name": "first", "arguments": "{}" } },
                    { "id": "call_b", "function": { "name": "second", "arguments": "{}" } },
                ],
            }),
            json!({ "role": "tool", "tool_call_id": "call_b", "content": "two" }),
            json!({ "role": "tool", "tool_call_id": "call_a", "content": "one" }),
            json!({ "role": "assistant", "content": "done" }),
        ])
        .expect("maps");

        assert_eq!(turns[1].tool_calls.len(), 2);
        assert_eq!(turns[2].tool_result_for.as_deref(), Some("call_b"));
        assert_eq!(turns[3].tool_result_for.as_deref(), Some("call_a"));
        assert_eq!(turns[4].content, "done");
    }

    #[test]
    fn an_assistant_turn_without_calls_stays_ordinary() {
        let turn =
            structured_turn(&json!({ "role": "assistant", "content": "done" })).expect("maps");

        assert_eq!(turn.role, ChatRole::Assistant);
        assert!(turn.tool_calls.is_empty());
    }

    #[test]
    fn a_system_turn_maps_to_system() {
        let turn =
            structured_turn(&json!({ "role": "system", "content": "be terse" })).expect("maps");
        assert_eq!(turn.role, ChatRole::System);
    }

    #[test]
    fn an_unknown_role_is_read_as_the_user_speaking() {
        // Dropping it would lose text the model should see; the injected path
        // would have passed it through too.
        let turn =
            structured_turn(&json!({ "role": "developer", "content": "note" })).expect("maps");
        assert_eq!(turn.role, ChatRole::User);
        assert_eq!(turn.content, "note");
    }

    #[test]
    fn a_message_without_a_role_is_an_error_rather_than_a_guess() {
        assert!(structured_turn(&json!({ "content": "orphan" })).is_err());
    }

    #[test]
    fn array_content_is_flattened_the_same_way_as_on_the_injected_path() {
        let turn = structured_turn(&json!({
            "role": "user",
            "content": [{ "type": "text", "text": "one" }, { "type": "text", "text": "two" }],
        }))
        .expect("maps");

        assert!(turn.content.contains("one"));
        assert!(turn.content.contains("two"));
    }
}
