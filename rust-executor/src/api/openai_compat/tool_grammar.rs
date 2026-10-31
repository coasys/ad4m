//! Tool calls as text: the fallback for a model that does not take tools as definitions
//! (an OpenAI-shaped endpoint without `nativeTools`, or a server that ignores `tools`).
//!
//! The tools are rendered into the system prompt in the Hermes / Qwen2.5-Instruct
//! convention, and the calls are read back out of the text the model writes:
//!
//! ```text
//! <tool_call>
//! {"name": "<fn>", "arguments": <args-object-matching-the-schema>}
//! </tool_call>
//! ```
//!
//! Parallel calls repeat the block separated by newlines. Nothing constrains the model's
//! decoding: a model that must call a tool (`tool_choice: "required"` or a named
//! function) is asked to, and [`extract_tool_calls`] recovers what it wrote, also from
//! the fenced and bare-JSON forms models fall back to.

use serde_json::Value;

use super::types::ToolDef;

// ---------------------------------------------------------------------------
// tool_choice
// ---------------------------------------------------------------------------

/// Resolved form of the request's `tool_choice` field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ToolChoice {
    /// Model decides whether to call a tool (OpenAI default when tools are
    /// present).
    Auto,
    /// Tools must not be called.
    None,
    /// The model must call at least one of the supplied tools.
    Required,
    /// The model must call exactly this function.
    Named(String),
}

/// Resolve the raw `tool_choice` JSON into a [`ToolChoice`].  Absent choice
/// defaults to `Auto` when tools are present, else `None`.  Unknown values
/// degrade to `Auto` rather than rejecting the request.
pub fn parse_tool_choice(value: &Option<Value>, has_tools: bool) -> ToolChoice {
    match value {
        None => {
            if has_tools {
                ToolChoice::Auto
            } else {
                ToolChoice::None
            }
        }
        Some(Value::String(s)) => match s.as_str() {
            "none" => ToolChoice::None,
            "required" => ToolChoice::Required,
            // "auto" and anything unrecognised
            _ => ToolChoice::Auto,
        },
        Some(Value::Object(obj)) => obj
            .get("function")
            .and_then(|f| f.get("name"))
            .and_then(Value::as_str)
            .map(|name| ToolChoice::Named(name.to_string()))
            .unwrap_or(ToolChoice::Auto),
        Some(_) => ToolChoice::Auto,
    }
}

// ---------------------------------------------------------------------------
// System-prompt rendering (Hermes / Qwen convention)
// ---------------------------------------------------------------------------

/// Render the `<tools>…</tools>` system-prompt block that tells the model
/// which functions it may call and in what format.  Injected as a system
/// message; the local chat template renders it verbatim.
pub fn render_tools_system_prompt(tools: &[ToolDef]) -> String {
    let mut out = String::from(
        "# Tools\n\nYou may call one or more functions to assist with the user query.\n\n\
         You are provided with function signatures within <tools></tools> XML tags. \
         You MUST only call functions whose names appear exactly in the <tools> \
         block below (spelling and case). Do NOT invent function names, and do \
         NOT rely on your prior knowledge of any other API. If none of the listed \
         functions fits, answer in plain text instead of guessing a call.\n<tools>\n",
    );
    for tool in tools {
        if let Ok(json) = serde_json::to_string(tool) {
            out.push_str(&json);
            out.push('\n');
        }
    }
    out.push_str(
        "</tools>\n\nFor each function call, return a json object with function name and \
         arguments within <tool_call></tool_call> XML tags:\n<tool_call>\n\
         {\"name\": <function-name>, \"arguments\": <args-json-object>}\n</tool_call>",
    );
    out
}

// ---------------------------------------------------------------------------
// Wire-format render helpers (shared by chat::flatten_message +
// harness_bridge::flatten_json_message — Lal's PR #911 review flagged the
// two flatteners as byte-identical duplicates of these building blocks).
// ---------------------------------------------------------------------------

/// Render one `<tool_call>` block for a prior assistant turn's re-play.
///
/// The block matches the Hermes/Qwen convention exactly — same shape
/// `extract_tool_calls` parses back and `single_tool_call_parser` writes
/// on the constrained-decoding side, so a round-trip through this helper
/// stays consistent with the grammar the model was trained on.
///
/// * `name` is JSON-encoded so a name containing `"` / `\` / newlines
///   can't produce a malformed block.
/// * `args_json_string` is validated as parseable JSON before it goes in
///   verbatim. Empty or unparseable input degrades to `{}` — the LLM sees
///   a syntactically valid call rather than a broken one.
pub fn render_tool_call_block(name: &str, args_json_string: &str) -> String {
    let args_trimmed = args_json_string.trim();
    let args = if args_trimmed.is_empty() || serde_json::from_str::<Value>(args_trimmed).is_err() {
        "{}".to_string()
    } else {
        args_trimmed.to_string()
    };
    let name_encoded = serde_json::to_string(name).unwrap_or_else(|_| "\"\"".to_string());
    format!("<tool_call>\n{{\"name\": {name_encoded}, \"arguments\": {args}}}\n</tool_call>")
}

/// Wrap a tool result string as a `<tool_response>` block. When `id`
/// is present (from OpenAI's `tool_call_id` on `role:"tool"`
/// messages), it's included as an `id=` attribute so the model can
/// correlate parallel tool_calls back to their invocations on the next
/// prompt. Absent id → bare `<tool_response>`. `id` is JSON-encoded so
/// quotes/backslashes can't produce a malformed attribute.
pub fn render_tool_response_block(content: &str, id: Option<&str>) -> String {
    match id.filter(|s| !s.is_empty()) {
        Some(id) => {
            let id_encoded = serde_json::to_string(id).unwrap_or_else(|_| "\"\"".to_string());
            format!("<tool_response id={id_encoded}>\n{content}\n</tool_response>")
        }
        None => format!("<tool_response>\n{content}\n</tool_response>"),
    }
}

// ---------------------------------------------------------------------------
// Extraction (model text → tool calls)
// ---------------------------------------------------------------------------

/// A tool call recovered from model output.  `arguments` is a JSON string
/// (object re-serialised), matching the OpenAI `function.arguments` shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtractedToolCall {
    pub name: String,
    pub arguments: String,
}

/// Pull every tool-call block out of `text` and parse the inner JSON.
///
/// Handles three shapes, in order:
///   1. `<tool_call>…</tool_call>` XML tags (Hermes/Qwen convention — what
///      our system prompt asks for).
///   2. ` ``` `-fenced code blocks (` ```json `, ` ```xml `, or bare
///      ` ``` `) whose body parses as a single `{name,arguments}` object —
///      observed in the wild from Gemma-3, which tends to reach for a
///      code-fence even when the prompt asks for XML tags.
///   3. Fallback: treat the whole trimmed text as a single bare JSON call
///      (the constrained-decoding path emits exactly this).
///
/// Anything not matching any of the above returns an empty vec — the
/// harness reads that as "the model is done".
pub fn extract_tool_calls(text: &str) -> Vec<ExtractedToolCall> {
    let mut calls = Vec::new();
    extract_delimited(text, "<tool_call>", "</tool_call>", &mut calls);
    if calls.is_empty() {
        extract_fenced(text, &mut calls);
    }
    if calls.is_empty() {
        let trimmed = text.trim();
        if let Some(arr) = parse_tool_call_json_array(trimmed) {
            calls.extend(arr);
        } else if let Some(wrapped) = parse_tool_calls_wrapper(trimmed) {
            calls.extend(wrapped);
        } else if let Some(call) = parse_tool_call_json(trimmed) {
            calls.push(call);
        }
    }
    calls
}

fn extract_delimited(text: &str, open: &str, close: &str, out: &mut Vec<ExtractedToolCall>) {
    let mut rest = text;
    while let Some(start) = rest.find(open) {
        let after = &rest[start + open.len()..];
        let (block, next) = match after.find(close) {
            Some(end) => (&after[..end], &after[end + close.len()..]),
            None => (after, ""),
        };
        if let Some(call) = parse_tool_call_json(block.trim()) {
            out.push(call);
        }
        rest = next;
    }
}

/// Scan for `\`\`\`[lang]\n…\n\`\`\`` blocks and try each one as a tool call.
/// Recognises `json`, `xml`, and bare fences (no language tag). Anything
/// inside a fence that doesn't parse as `{name, arguments}` is skipped
/// silently — model may fence-quote unrelated snippets.
fn extract_fenced(text: &str, out: &mut Vec<ExtractedToolCall>) {
    const FENCE: &str = "```";
    let mut rest = text;
    while let Some(start) = rest.find(FENCE) {
        let after = &rest[start + FENCE.len()..];
        // Skip optional language tag up to the first newline.
        let body_start = after.find('\n').map(|i| i + 1).unwrap_or(0);
        let body_rest = &after[body_start..];
        let Some(end) = body_rest.find(FENCE) else {
            break;
        };
        let block = body_rest[..end].trim();
        if let Some(arr) = parse_tool_call_json_array(block) {
            out.extend(arr);
        } else if let Some(wrapped) = parse_tool_calls_wrapper(block) {
            out.extend(wrapped);
        } else if let Some(call) = parse_tool_call_json(block) {
            out.push(call);
        }
        rest = &body_rest[end + FENCE.len()..];
    }
}

/// Parse `candidate` as an OpenAI-style response object `{"tool_calls":[…]}`.
/// Small models (Gemma-3) sometimes echo back the response wrapper shape
/// even though the prompt asks for the singleton `<tool_call>` form —
/// observed 1/8 attempts on CI job 22282. Returns None when the input
/// isn't an object with a `tool_calls` array or any element fails to parse.
fn parse_tool_calls_wrapper(candidate: &str) -> Option<Vec<ExtractedToolCall>> {
    let value: Value = serde_json::from_str(candidate).ok()?;
    let arr = value.get("tool_calls")?.as_array()?;
    let mut out = Vec::with_capacity(arr.len());
    for v in arr {
        // Support both `{name, arguments}` (Ollama/Hermes) and
        // `{function: {name, arguments}}` (OpenAI Chat Completions).
        let call = value_to_call(v).or_else(|| v.get("function").and_then(value_to_call))?;
        out.push(call);
    }
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

fn parse_tool_call_json(candidate: &str) -> Option<ExtractedToolCall> {
    let value: Value = serde_json::from_str(candidate).ok()?;
    value_to_call(&value)
}

/// Parse `candidate` as a JSON array; if every element is a valid tool-call
/// object, return them. Returns `None` when the input is not a JSON array or
/// any element fails to parse — the caller can then fall through to the
/// singleton form. Small-model bailout: Gemma-3 sometimes fences an array
/// of calls (`[{name,arguments}, …]`) even when the prompt asks for the
/// singleton XML tag form.
fn parse_tool_call_json_array(candidate: &str) -> Option<Vec<ExtractedToolCall>> {
    let value: Value = serde_json::from_str(candidate).ok()?;
    let arr = value.as_array()?;
    let mut out = Vec::with_capacity(arr.len());
    for v in arr {
        out.push(value_to_call(v)?);
    }
    if out.is_empty() {
        None
    } else {
        Some(out)
    }
}

fn value_to_call(value: &Value) -> Option<ExtractedToolCall> {
    let name = value.get("name")?.as_str()?.to_string();
    let arguments = match value.get("arguments") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => other.to_string(),
        None => "{}".to_string(),
    };
    Some(ExtractedToolCall { name, arguments })
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::openai_compat::types::FunctionDef;
    use serde_json::json;

    fn tool(name: &str, parameters: Value) -> ToolDef {
        ToolDef {
            kind: "function".to_string(),
            function: FunctionDef {
                name: name.to_string(),
                description: None,
                parameters: Some(parameters),
            },
        }
    }

    #[test]
    fn parse_tool_choice_variants() {
        assert_eq!(parse_tool_choice(&None, true), ToolChoice::Auto);
        assert_eq!(parse_tool_choice(&None, false), ToolChoice::None);
        assert_eq!(
            parse_tool_choice(&Some(json!("none")), true),
            ToolChoice::None
        );
        assert_eq!(
            parse_tool_choice(&Some(json!("required")), true),
            ToolChoice::Required
        );
        assert_eq!(
            parse_tool_choice(&Some(json!("auto")), true),
            ToolChoice::Auto
        );
        assert_eq!(
            parse_tool_choice(
                &Some(json!({ "type": "function", "function": { "name": "foo" } })),
                true
            ),
            ToolChoice::Named("foo".to_string())
        );
    }

    #[test]
    fn extract_tool_calls_tagged_and_bare() {
        // single tagged block
        let one = extract_tool_calls(
            "<tool_call>\n{\"name\": \"f\", \"arguments\": {\"a\": 1}}\n</tool_call>",
        );
        assert_eq!(one.len(), 1);
        assert_eq!(one[0].name, "f");
        assert_eq!(one[0].arguments, r#"{"a":1}"#);

        // two blocks (parallel)
        let two = extract_tool_calls(
            "<tool_call>\n{\"name\": \"a\", \"arguments\": {}}\n</tool_call>\n\
             <tool_call>\n{\"name\": \"b\", \"arguments\": {}}\n</tool_call>",
        );
        assert_eq!(two.len(), 2);
        assert_eq!(two[0].name, "a");
        assert_eq!(two[1].name, "b");

        // bare JSON (no tags)
        let bare = extract_tool_calls(r#"{"name": "g", "arguments": {"x": true}}"#);
        assert_eq!(bare.len(), 1);
        assert_eq!(bare[0].name, "g");
        assert_eq!(bare[0].arguments, r#"{"x":true}"#);

        // plain prose ⇒ nothing
        assert!(extract_tool_calls("I cannot help with that.").is_empty());
    }

    #[test]
    fn extract_tool_calls_recovers_json_fence() {
        // Model ignored the XML-tag protocol and fenced the call as ```json.
        // Observed live from Gemma-3 12B via Ollama.
        let one = extract_tool_calls(
            "Sure, here's the call:\n\
             ```json\n\
             {\"name\": \"f\", \"arguments\": {\"a\": 1}}\n\
             ```\n\
             Let me know if you need anything else.",
        );
        assert_eq!(one.len(), 1);
        assert_eq!(one[0].name, "f");
        assert_eq!(one[0].arguments, r#"{"a":1}"#);
    }

    #[test]
    fn extract_tool_calls_recovers_xml_fence() {
        // Same failure mode but the model picks ```xml as its language tag.
        let one = extract_tool_calls(
            "```xml\n<tool_call>\n{\"name\": \"g\", \"arguments\": {}}\n</tool_call>\n```",
        );
        assert_eq!(one.len(), 1);
        assert_eq!(one[0].name, "g");
    }

    #[test]
    fn extract_tool_calls_recovers_bare_fence() {
        // Model dropped the language tag on the fence.
        let one = extract_tool_calls("```\n{\"name\": \"h\", \"arguments\": {\"n\": 2}}\n```");
        assert_eq!(one.len(), 1);
        assert_eq!(one[0].name, "h");
    }

    #[test]
    fn extract_tool_calls_recovers_fenced_json_array() {
        // Gemma-3 sometimes fences an ARRAY of call objects instead of one.
        let calls = extract_tool_calls(
            "Here are the calls:\n\
             ```json\n\
             [\n\
               {\"name\": \"a\", \"arguments\": {\"x\": 1}},\n\
               {\"name\": \"b\", \"arguments\": {\"y\": 2}}\n\
             ]\n\
             ```",
        );
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].name, "a");
        assert_eq!(calls[1].name, "b");
    }

    #[test]
    fn extract_tool_calls_recovers_bare_json_array() {
        // Same array form but without any fence at all.
        let calls =
            extract_tool_calls(r#"[{"name":"a","arguments":{}},{"name":"b","arguments":{}}]"#);
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].name, "a");
        assert_eq!(calls[1].name, "b");
    }

    #[test]
    fn extract_tool_calls_ignores_fence_without_valid_call() {
        // A fenced snippet that isn't a tool-call shape → skipped, no panic.
        assert!(extract_tool_calls("```json\n{\"note\": \"nothing\"}\n```").is_empty());
    }

    #[test]
    fn extract_tool_calls_recovers_fenced_response_wrapper() {
        // Gemma-3 sometimes echoes the OpenAI Chat Completions response
        // shape: `{"tool_calls": [{"name":..,"arguments":..}, ..]}`.
        // Observed on CI job 22282 attempt 6 (integration-tests-model on
        // `64465fe17`) — the pass produced 0 ops because the extractor
        // couldn't recognise this shape.
        let calls = extract_tool_calls(
            "```json\n\
             {\n\
               \"tool_calls\": [\n\
                 {\"name\": \"a\", \"arguments\": {\"x\": 1}},\n\
                 {\"name\": \"b\", \"arguments\": {\"y\": 2}}\n\
               ]\n\
             }\n\
             ```",
        );
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].name, "a");
        assert_eq!(calls[1].name, "b");
    }

    #[test]
    fn extract_tool_calls_recovers_bare_response_wrapper() {
        // Same wrapper shape but without any fence.
        let calls = extract_tool_calls(
            r#"{"tool_calls":[{"name":"a","arguments":{}},{"name":"b","arguments":{}}]}"#,
        );
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].name, "a");
        assert_eq!(calls[1].name, "b");
    }

    #[test]
    fn extract_tool_calls_recovers_openai_function_wrapper() {
        // Full OpenAI Chat Completions shape:
        // `{"tool_calls": [{"id":..,"type":"function","function":{"name":..,"arguments":..}}]}`.
        // Argument value on `function.arguments` is typically a JSON string per
        // OpenAI's wire — handle both shapes for robustness.
        let calls = extract_tool_calls(
            r#"{"tool_calls":[{"id":"c1","type":"function","function":{"name":"f","arguments":"{\"x\":1}"}}]}"#,
        );
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].name, "f");
        // OpenAI's `function.arguments` is a JSON *string*; `value_to_call`
        // pulls the inner value out (unquoted), matching how the harness
        // downstream JSON-parses `arguments` back to a value.
        assert_eq!(calls[0].arguments, r#"{"x":1}"#);
    }

    #[test]
    fn render_tools_prompt_contains_signatures() {
        let weather = tool(
            "get_weather",
            json!({ "type": "object", "properties": { "location": { "type": "string" } } }),
        );
        let prompt = render_tools_system_prompt(&[weather]);
        assert!(prompt.contains("<tools>"));
        assert!(prompt.contains("</tools>"));
        assert!(prompt.contains("get_weather"));
        assert!(prompt.contains("<tool_call>"));
    }
}
