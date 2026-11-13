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
//! Parallel calls repeat the block separated by newlines. [`extract_tool_calls`] recovers
//! what the model wrote, also from the fenced and bare-JSON forms models fall back to.
//!
//! A model that must call a tool (`tool_choice: "required"` or a named function) is held
//! to it by its server: [`ToolCallConstraint::schema`] is a JSON schema of the call(s),
//! sent as a decoding constraint (`ChatRequest::json_schema`), which llama-server, vLLM,
//! Ollama and OpenAI decode against. What comes back is checked against the same rules
//! ([`ToolCallConstraint::check`]) whatever answered, since a server may not constrain.

use serde_json::{json, Value};

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
// Decoding constraint (`tool_choice: "required"` or a named function)
// ---------------------------------------------------------------------------

/// Most calls one constrained turn may make (`required` with parallel calls).
pub const MAX_PARALLEL_CALLS: usize = 8;

/// What `tool_choice: "required"` or a named function demands of a reply on the text path:
/// at least one call and at most [`Self::max_calls`], each to one of the allowed tools,
/// with arguments of that tool's schema.
///
/// The model's server gets it as a JSON schema ([`Self::schema`]), so a server that decodes
/// against a schema can only produce `{"tool_calls": [{"name": …, "arguments": {…}}, …]}`,
/// a shape [`extract_tool_calls`] reads. Only the keywords that shape a value are kept from
/// a tool's parameters (`type`, `properties`, `required`, `items`, `enum`, `const`,
/// `anyOf`/`oneOf`): every server's schema support covers them, and the rest (`format`,
/// `pattern`, bounds, `$ref`) is what one server or another refuses.
#[derive(Debug, Clone, PartialEq)]
pub struct ToolCallConstraint {
    /// Each allowed tool's name and the shape of its arguments.
    tools: Vec<(String, Value)>,
    max_calls: usize,
}

impl ToolCallConstraint {
    /// The constraint of `choice` over `tools`. `None` for `auto` and `none`, which generate
    /// freely, and for a named function that is not among `tools`. A named function, or
    /// `parallel_tool_calls: false`, allows exactly one call; `required` with parallel calls
    /// allows 1 to [`MAX_PARALLEL_CALLS`].
    pub fn for_choice(tools: &[ToolDef], choice: &ToolChoice, parallel: bool) -> Option<Self> {
        let selected: Vec<&ToolDef> = match choice {
            ToolChoice::Required => tools.iter().collect(),
            ToolChoice::Named(name) => tools.iter().filter(|t| &t.function.name == name).collect(),
            ToolChoice::Auto | ToolChoice::None => return None,
        };
        if selected.is_empty() {
            return None;
        }
        let max_calls = if parallel && *choice == ToolChoice::Required {
            MAX_PARALLEL_CALLS
        } else {
            1
        };
        Some(Self {
            tools: selected
                .iter()
                .map(|tool| {
                    let name = tool.function.name.clone();
                    (name, arguments_shape(tool.function.parameters.as_ref()))
                })
                .collect(),
            max_calls,
        })
    }

    /// How many calls one reply may make.
    pub fn max_calls(&self) -> usize {
        self.max_calls
    }

    /// The JSON schema of a reply that keeps to this constraint. Its root is an object,
    /// which every server takes (OpenAI refuses another root).
    pub fn schema(&self) -> Value {
        let mut calls: Vec<Value> = self
            .tools
            .iter()
            .map(|(name, arguments)| {
                json!({
                    "type": "object",
                    "properties": {
                        "name": { "type": "string", "enum": [name] },
                        "arguments": arguments,
                    },
                    "required": ["name", "arguments"],
                })
            })
            .collect();
        let call = if calls.len() == 1 {
            calls.remove(0)
        } else {
            json!({ "anyOf": calls })
        };
        json!({
            "type": "object",
            "properties": {
                "tool_calls": {
                    "type": "array",
                    "items": call,
                    "minItems": 1,
                    "maxItems": self.max_calls,
                },
            },
            "required": ["tool_calls"],
        })
    }

    /// The calls of a reply, if it keeps to this constraint: at least one, each to an
    /// allowed tool with arguments of its schema. Beyond [`Self::max_calls`] only the first
    /// are kept (a `parallel_tool_calls: false` turn keeps its first call). `Err` says why
    /// the reply does not keep to it; it quotes no argument value the model wrote.
    pub fn check(
        &self,
        mut calls: Vec<ExtractedToolCall>,
    ) -> Result<Vec<ExtractedToolCall>, String> {
        if calls.is_empty() {
            return Err("the reply makes no tool call".to_string());
        }
        calls.truncate(self.max_calls);
        for call in &calls {
            let Some((_, shape)) = self.tools.iter().find(|(name, _)| *name == call.name) else {
                return Err(format!(
                    "the reply calls {:?}, which is not an allowed tool",
                    call.name
                ));
            };
            let arguments: Value = serde_json::from_str(&call.arguments)
                .map_err(|_| format!("the arguments of {:?} are not JSON", call.name))?;
            conforms(&arguments, shape, "arguments")
                .map_err(|reason| format!("{}: {reason}", call.name))?;
        }
        Ok(calls)
    }
}

/// The shape of a tool's arguments: its parameters' shape keywords, an object at the root.
fn arguments_shape(parameters: Option<&Value>) -> Value {
    let mut shape = parameters.map(shape_of).unwrap_or_else(|| json!({}));
    if let Some(object) = shape.as_object_mut() {
        object
            .entry("type")
            .or_insert_with(|| Value::String("object".to_string()));
    }
    shape
}

/// `schema` with only the keywords that shape a value, recursively. `oneOf` becomes
/// `anyOf`: a value of tool arguments rarely matches two alternatives, and `anyOf` is what
/// every server's schema support has.
fn shape_of(schema: &Value) -> Value {
    let Some(node) = schema.as_object() else {
        return json!({});
    };
    let mut out = serde_json::Map::new();
    for key in ["type", "enum", "const"] {
        if let Some(value) = node.get(key) {
            out.insert(key.to_string(), value.clone());
        }
    }
    if let Some(required) = node.get("required").filter(|r| r.is_array()) {
        out.insert("required".to_string(), required.clone());
    }
    if let Some(properties) = node.get("properties").and_then(Value::as_object) {
        let properties = properties
            .iter()
            .map(|(key, property)| (key.clone(), shape_of(property)))
            .collect();
        out.insert("properties".to_string(), Value::Object(properties));
    }
    if let Some(items) = node.get("items").filter(|items| items.is_object()) {
        out.insert("items".to_string(), shape_of(items));
    }
    let alternatives = node
        .get("anyOf")
        .or_else(|| node.get("oneOf"))
        .and_then(Value::as_array);
    if let Some(alternatives) = alternatives {
        out.insert(
            "anyOf".to_string(),
            Value::Array(alternatives.iter().map(shape_of).collect()),
        );
    }
    Value::Object(out)
}

/// Whether `value` has the shape `shape` describes ([`shape_of`]'s keywords). `Err` names
/// the first place it does not, by its path from `path`.
fn conforms(value: &Value, shape: &Value, path: &str) -> Result<(), String> {
    if let Some(allowed) = shape.get("enum").and_then(Value::as_array) {
        if !allowed.contains(value) {
            return Err(format!("{path} is not one of its allowed values"));
        }
    }
    if let Some(expected) = shape.get("const") {
        if value != expected {
            return Err(format!("{path} is not its one allowed value"));
        }
    }
    let type_matches = match shape.get("type") {
        Some(Value::String(kind)) => has_type(value, kind),
        Some(Value::Array(kinds)) => kinds
            .iter()
            .filter_map(Value::as_str)
            .any(|kind| has_type(value, kind)),
        _ => true,
    };
    if !type_matches {
        return Err(format!("{path} is not of type {}", shape["type"]));
    }
    if let Some(alternatives) = shape.get("anyOf").and_then(Value::as_array) {
        let any = alternatives
            .iter()
            .any(|alternative| conforms(value, alternative, path).is_ok());
        if !alternatives.is_empty() && !any {
            return Err(format!("{path} matches none of its alternatives"));
        }
    }
    if let Some(object) = value.as_object() {
        if let Some(required) = shape.get("required").and_then(Value::as_array) {
            if let Some(key) = required
                .iter()
                .filter_map(Value::as_str)
                .find(|key| !object.contains_key(*key))
            {
                return Err(format!("{path}.{key} is missing"));
            }
        }
        if let Some(properties) = shape.get("properties").and_then(Value::as_object) {
            for (key, property) in properties {
                if let Some(field) = object.get(key) {
                    conforms(field, property, &format!("{path}.{key}"))?;
                }
            }
        }
    }
    if let (Some(items), Some(array)) = (shape.get("items"), value.as_array()) {
        for (index, item) in array.iter().enumerate() {
            conforms(item, items, &format!("{path}[{index}]"))?;
        }
    }
    Ok(())
}

fn has_type(value: &Value, kind: &str) -> bool {
    match kind {
        "string" => value.is_string(),
        "integer" => {
            value.is_i64() || value.is_u64() || value.as_f64().is_some_and(|f| f.fract() == 0.0)
        }
        "number" => value.is_number(),
        "boolean" => value.is_boolean(),
        "null" => value.is_null(),
        "array" => value.is_array(),
        "object" => value.is_object(),
        // A type this check does not know: the server's decoding is the only judge.
        _ => true,
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
/// In every shape a call object may also come wrapped as
/// `{"tool_call": {name, arguments}}` — see [`value_to_call`].
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

/// One call object: `{name, arguments}`, or that same object under a singular
/// `tool_call` key. Gemma-3 emits `{"tool_call": {…}}` most of the time —
/// plausibly echoing the `<tool_call>` wording of our own prompt — so the
/// wrapper is accepted wherever a call object is: inside tags, in a fence,
/// bare, and as each element of an array or of a `tool_calls` wrapper
/// (#1069). One level only; a wrapped wrapper is not a call.
///
/// This widens nothing an attacker can reach: any `{"tool_call": {X}}` they
/// could get a model to echo, `{X}` was already accepted in the same places.
fn value_to_call(value: &Value) -> Option<ExtractedToolCall> {
    let value = match value.get("tool_call") {
        Some(inner) if value.get("name").is_none() => inner,
        _ => value,
    };
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
    fn extract_tool_calls_recovers_singular_wrapper_in_every_shape() {
        // #1069: Gemma-3's usual shape. Each of these returned [] before.
        let wrapped = r#"{"tool_call": {"name": "f", "arguments": {"a": 1}}}"#;
        for text in [
            wrapped.to_string(),
            format!("<tool_call>\n{wrapped}\n</tool_call>"),
            format!("```json\n{wrapped}\n```"),
        ] {
            let calls = extract_tool_calls(&text);
            assert_eq!(calls.len(), 1, "{text}");
            assert_eq!(calls[0].name, "f");
            assert_eq!(calls[0].arguments, r#"{"a":1}"#);
        }
    }

    #[test]
    fn extract_tool_calls_recovers_every_element_of_a_singular_wrapper_array() {
        // n > 1 is the case #1057's content-channel fallback declines; a
        // relation pass wants several calls in one reply.
        let calls = extract_tool_calls(
            r#"[{"tool_call": {"name": "a", "arguments": {}}}, {"tool_call": {"name": "b", "arguments": {}}}]"#,
        );
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].name, "a");
        assert_eq!(calls[1].name, "b");

        // Mixed with plain call objects, and inside the plural wrapper.
        let calls = extract_tool_calls(
            r#"{"tool_calls": [{"tool_call": {"name": "a", "arguments": {}}}, {"name": "b", "arguments": {}}]}"#,
        );
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].name, "a");
        assert_eq!(calls[1].name, "b");
    }

    #[test]
    fn extract_tool_calls_unwraps_the_singular_key_one_level_only() {
        assert!(extract_tool_calls(
            r#"{"tool_call": {"tool_call": {"name": "f", "arguments": {}}}}"#
        )
        .is_empty());
        assert!(extract_tool_calls(r#"{"tool_call": "f"}"#).is_empty());
        // An object that is already a call keeps its own name.
        let calls = extract_tool_calls(
            r#"{"name": "outer", "arguments": {}, "tool_call": {"name": "inner"}}"#,
        );
        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].name, "outer");
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

    fn weather_and_time() -> Vec<ToolDef> {
        vec![
            tool(
                "get_weather",
                json!({
                    "type": "object",
                    "properties": {
                        "city": { "type": "string", "description": "a city", "minLength": 1 },
                        "unit": { "type": "string", "enum": ["C", "F"] },
                        "days": { "type": "integer", "minimum": 1 },
                        "tags": { "type": "array", "items": { "type": "string" } },
                    },
                    "required": ["city"],
                    "additionalProperties": false,
                }),
            ),
            tool("get_time", json!({ "type": "object", "properties": {} })),
        ]
    }

    fn call(name: &str, arguments: Value) -> ExtractedToolCall {
        ExtractedToolCall {
            name: name.to_string(),
            arguments: arguments.to_string(),
        }
    }

    #[test]
    fn auto_none_and_an_unknown_named_function_are_unconstrained() {
        let tools = weather_and_time();
        for choice in [
            ToolChoice::Auto,
            ToolChoice::None,
            ToolChoice::Named("nope".into()),
        ] {
            assert_eq!(ToolCallConstraint::for_choice(&tools, &choice, true), None);
        }
    }

    #[test]
    fn required_allows_any_tool_and_parallel_calls_up_to_the_cap() {
        let tools = weather_and_time();
        let parallel = ToolCallConstraint::for_choice(&tools, &ToolChoice::Required, true).unwrap();
        assert_eq!(parallel.max_calls(), MAX_PARALLEL_CALLS);
        let schema = parallel.schema();
        assert_eq!(schema["type"], "object");
        assert_eq!(schema["required"], json!(["tool_calls"]));
        let calls = &schema["properties"]["tool_calls"];
        assert_eq!(calls["minItems"], 1);
        assert_eq!(calls["maxItems"], MAX_PARALLEL_CALLS);
        let alternatives = calls["items"]["anyOf"].as_array().unwrap();
        let names: Vec<&Value> = alternatives
            .iter()
            .map(|alt| &alt["properties"]["name"]["enum"][0])
            .collect();
        assert_eq!(names, vec!["get_weather", "get_time"]);

        let single = ToolCallConstraint::for_choice(&tools, &ToolChoice::Required, false).unwrap();
        assert_eq!(single.max_calls(), 1);
        assert_eq!(single.schema()["properties"]["tool_calls"]["maxItems"], 1);
    }

    #[test]
    fn a_named_function_allows_that_one_call_even_with_parallel_calls() {
        let constraint = ToolCallConstraint::for_choice(
            &weather_and_time(),
            &ToolChoice::Named("get_weather".into()),
            true,
        )
        .unwrap();
        assert_eq!(constraint.max_calls(), 1);
        let item = &constraint.schema()["properties"]["tool_calls"]["items"];
        assert!(item.get("anyOf").is_none(), "{item}");
        assert_eq!(item["properties"]["name"]["enum"], json!(["get_weather"]));
        assert_eq!(item["required"], json!(["name", "arguments"]));
    }

    #[test]
    fn the_arguments_keep_only_the_keywords_that_shape_a_value() {
        let constraint = ToolCallConstraint::for_choice(
            &weather_and_time(),
            &ToolChoice::Named("get_weather".into()),
            false,
        )
        .unwrap();
        let arguments =
            &constraint.schema()["properties"]["tool_calls"]["items"]["properties"]["arguments"];
        assert_eq!(
            arguments,
            &json!({
                "type": "object",
                "properties": {
                    "city": { "type": "string" },
                    "unit": { "type": "string", "enum": ["C", "F"] },
                    "days": { "type": "integer" },
                    "tags": { "type": "array", "items": { "type": "string" } },
                },
                "required": ["city"],
            })
        );

        let no_parameters = ToolDef {
            kind: "function".into(),
            function: FunctionDef {
                name: "ping".into(),
                description: None,
                parameters: None,
            },
        };
        let ping =
            ToolCallConstraint::for_choice(&[no_parameters], &ToolChoice::Required, false).unwrap();
        assert_eq!(
            ping.schema()["properties"]["tool_calls"]["items"]["properties"]["arguments"],
            json!({ "type": "object" })
        );
    }

    #[test]
    fn a_reply_keeping_to_the_constraint_passes_with_its_calls() {
        let constraint =
            ToolCallConstraint::for_choice(&weather_and_time(), &ToolChoice::Required, true)
                .unwrap();
        let calls = vec![
            call(
                "get_weather",
                json!({ "city": "Oslo", "unit": "C", "days": 2.0 }),
            ),
            call("get_time", json!({})),
        ];
        assert_eq!(constraint.check(calls.clone()), Ok(calls));

        // What a constrained server writes reads back through `extract_tool_calls`.
        let reply = r#"{"tool_calls":[{"name":"get_time","arguments":{}}]}"#;
        assert_eq!(
            constraint.check(extract_tool_calls(reply)),
            Ok(vec![call("get_time", json!({}))])
        );
    }

    #[test]
    fn a_reply_breaking_the_constraint_says_how() {
        let constraint = ToolCallConstraint::for_choice(
            &weather_and_time(),
            &ToolChoice::Named("get_weather".into()),
            true,
        )
        .unwrap();
        let refused = |calls: Vec<ExtractedToolCall>| constraint.check(calls).unwrap_err();

        assert!(refused(vec![]).contains("no tool call"));
        assert!(refused(vec![call("get_time", json!({}))]).contains("not an allowed tool"));
        assert!(refused(vec![call("get_weather", json!({}))]).contains("arguments.city is missing"));
        assert!(refused(vec![call("get_weather", json!({ "city": 3 }))])
            .contains("arguments.city is not of type \"string\""));
        assert!(refused(vec![call(
            "get_weather",
            json!({ "city": "Oslo", "unit": "K" })
        )])
        .contains("arguments.unit is not one of its allowed values"));
        assert!(refused(vec![call(
            "get_weather",
            json!({ "city": "Oslo", "days": 1.5 })
        )])
        .contains("arguments.days is not of type \"integer\""));
        assert!(refused(vec![call(
            "get_weather",
            json!({ "city": "Oslo", "tags": ["a", 1] })
        )])
        .contains("arguments.tags[1]"));
        let not_json = ExtractedToolCall {
            name: "get_weather".into(),
            arguments: "{city".into(),
        };
        assert!(refused(vec![not_json]).contains("not JSON"));
    }

    #[test]
    fn one_call_allowed_keeps_the_first_of_several() {
        let constraint =
            ToolCallConstraint::for_choice(&weather_and_time(), &ToolChoice::Required, false)
                .unwrap();
        let first = call("get_time", json!({}));
        let checked = constraint
            .check(vec![
                first.clone(),
                call("get_weather", json!({ "city": "Oslo" })),
            ])
            .unwrap();
        assert_eq!(checked, vec![first]);
    }

    #[test]
    fn any_of_and_one_of_alternatives_are_checked() {
        let tools = vec![tool(
            "set",
            json!({
                "type": "object",
                "properties": {
                    "value": { "oneOf": [{ "type": "string" }, { "type": "integer" }] },
                },
            }),
        )];
        let constraint =
            ToolCallConstraint::for_choice(&tools, &ToolChoice::Required, false).unwrap();
        assert!(constraint
            .check(vec![call("set", json!({ "value": 4 }))])
            .is_ok());
        assert!(constraint
            .check(vec![call("set", json!({ "value": true }))])
            .unwrap_err()
            .contains("none of its alternatives"));
    }
}
