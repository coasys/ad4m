//! Runtime compiler from OpenAI tool definitions (JSON-Schema `parameters`)
//! to a kalosm [`ArcParser`] that *constrains local-model decoding* to a
//! well-formed tool call.
//!
//! The target convention is Hermes / Qwen2.5-Instruct style:
//!
//! ```text
//! <tool_call>
//! {"name": "<fn>", "arguments": <args-object-matching-the-schema>}
//! </tool_call>
//! ```
//!
//! Parallel calls repeat the block separated by newlines.
//!
//! ## What is and isn't constrained
//!
//! * `tool_choice: "required"` / a named function → we return a parser that
//!   forces the model to emit one (or, for `required` + `parallel`, several)
//!   syntactically valid tool call(s).  This is the guarantee: the emitted
//!   `arguments` are always JSON that matches the declared schema shape.
//! * `tool_choice: "auto"` / `"none"` → we return `None`.  The model
//!   generates freely and any `<tool_call>` blocks it chooses to emit are
//!   recovered afterwards with [`extract_tool_calls`].  Constraining
//!   arbitrary prose is not expressible with the available parser
//!   primitives (there is no "any free text" parser — `StringParser` only
//!   matches a quoted JSON string), and forcing prose through one would
//!   corrupt normal answers.
//!
//! ## Grammar construction
//!
//! Every schema node compiles to a uniform `ArcParser<()>` (output type
//! erased to `()` via `map_output`, then boxed).  Uniformity is what lets
//! the compiler recurse over arbitrary nested schemas and fold N
//! alternatives with `.or(..)` without the combinator tuple types
//! exploding.  We only care that the *text* is on-grammar; the structured
//! value is recovered separately by `serde_json` in [`extract_tool_calls`].
//!
//! Only the JSON-Schema keywords that shape the value are read
//! (`type`, `properties`, `items`, `enum`); everything else (`minLength`,
//! `description`, `additionalProperties`, …) is ignored, which is the
//! "strip unsupported keywords" behaviour by construction.

use std::borrow::Cow;

use kalosm::language::{
    ArcParser, FloatParser, IntegerParser, LiteralParser, ParserExt, SeparatedParser, StringParser,
};
use serde_json::Value;

use super::types::ToolDef;

/// Upper bound on a constrained JSON string value (characters).
const MAX_STRING_LEN: usize = 8192;
/// Upper bound on items in a constrained JSON array.
const MAX_ARRAY_ITEMS: usize = 64;
/// Upper bound on parallel tool calls in one turn.
const MAX_PARALLEL_CALLS: usize = 8;

// ---------------------------------------------------------------------------
// tool_choice
// ---------------------------------------------------------------------------

/// Resolved form of the request's `tool_choice` field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ToolChoice {
    /// Model decides whether to call a tool (OpenAI default when tools are
    /// present).  Not hard-constrained.
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
         You are provided with function signatures within <tools></tools> XML tags:\n<tools>\n",
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
// Grammar compiler
// ---------------------------------------------------------------------------

/// Build a decoding constraint for the given tools + choice, or `None` when
/// the mode should generate freely (`auto` / `none`).
pub fn build_tool_call_parser(
    tools: &[ToolDef],
    choice: &ToolChoice,
    parallel: bool,
) -> Option<ArcParser<()>> {
    let selected: Vec<&ToolDef> = match choice {
        ToolChoice::Required => tools.iter().collect(),
        ToolChoice::Named(name) => tools.iter().filter(|t| &t.function.name == name).collect(),
        ToolChoice::Auto | ToolChoice::None => return None,
    };
    if selected.is_empty() {
        return None;
    }

    let one = or_all(
        selected
            .iter()
            .map(|tool| single_tool_call_parser(tool))
            .collect(),
    );

    let parser = if parallel && matches!(choice, ToolChoice::Required) {
        // one ("\n" one){0,}  — 1..=MAX blocks separated by newlines.
        SeparatedParser::new(one, LiteralParser::new("\n"), 1..=MAX_PARALLEL_CALLS)
            .map_output(|_| ())
            .boxed()
    } else {
        one
    };

    Some(parser)
}

/// `<tool_call>\n{"name": "<fn>", "arguments": <args>}\n</tool_call>`
fn single_tool_call_parser(tool: &ToolDef) -> ArcParser<()> {
    let empty = Value::Object(serde_json::Map::new());
    let params = tool.function.parameters.as_ref().unwrap_or(&empty);
    let args = object_parser(params);

    let prefix = lit(format!(
        "<tool_call>\n{{\"name\": {}, \"arguments\": ",
        json_string_literal(&tool.function.name)
    ));
    let suffix = lit("}\n</tool_call>");

    seq2(seq2(prefix, args), suffix)
}

/// Compile a JSON-Schema object node into an object parser.  Emits every
/// declared property, in the schema's property order, joined by `", "`.
/// Objects with no `properties` compile to the empty object `{}`.
fn object_parser(schema: &Value) -> ArcParser<()> {
    match schema.get("properties").and_then(Value::as_object) {
        Some(props) if !props.is_empty() => {
            let mut parts = props.iter().map(|(key, prop)| {
                seq2(
                    lit(format!("{}: ", json_string_literal(key))),
                    value_parser(prop),
                )
            });
            let mut body = parts.next().expect("properties non-empty");
            for part in parts {
                body = seq2(seq2(body, lit(", ")), part);
            }
            seq2(seq2(lit("{"), body), lit("}"))
        }
        _ => lit("{}"),
    }
}

/// Compile a JSON-Schema value node into a value parser.
fn value_parser(schema: &Value) -> ArcParser<()> {
    // Enumerations may omit `type`; a value must be one of the literals.
    if let Some(values) = schema.get("enum").and_then(Value::as_array) {
        let literals: Vec<ArcParser<()>> = values
            .iter()
            .map(|v| lit(serde_json::to_string(v).unwrap_or_else(|_| "null".to_string())))
            .collect();
        if !literals.is_empty() {
            return or_all(literals);
        }
    }

    // `type` may be a string or an array of strings (union); take the first.
    let ty = match schema.get("type") {
        Some(Value::String(s)) => Some(s.as_str()),
        Some(Value::Array(a)) => a.iter().find_map(Value::as_str),
        _ => None,
    };

    match ty {
        Some("string") => string_value_parser(),
        Some("integer") => IntegerParser::new(i128::MIN..=i128::MAX)
            .map_output(|_| ())
            .boxed(),
        Some("number") => FloatParser::new(f64::MIN..=f64::MAX)
            .map_output(|_| ())
            .boxed(),
        Some("boolean") => bool_parser(),
        Some("null") => lit("null"),
        Some("array") => array_parser(schema),
        Some("object") => object_parser(schema),
        // Unknown / missing type → permissive scalar.
        _ => any_value_parser(),
    }
}

/// `[]`  OR  `[` item (", " item)* `]`
fn array_parser(schema: &Value) -> ArcParser<()> {
    let item = match schema.get("items") {
        Some(items) => value_parser(items),
        None => any_value_parser(),
    };
    let non_empty = seq2(
        seq2(
            lit("["),
            SeparatedParser::new(item, LiteralParser::new(", "), 1..=MAX_ARRAY_ITEMS)
                .map_output(|_| ())
                .boxed(),
        ),
        lit("]"),
    );
    lit("[]").or(non_empty).boxed()
}

/// A permissive scalar (string | number | boolean | null) for schema nodes
/// with no usable `type`.
fn any_value_parser() -> ArcParser<()> {
    or_all(vec![
        string_value_parser(),
        FloatParser::new(f64::MIN..=f64::MAX)
            .map_output(|_| ())
            .boxed(),
        bool_parser(),
        lit("null"),
    ])
}

fn string_value_parser() -> ArcParser<()> {
    StringParser::new(0..=MAX_STRING_LEN)
        .map_output(|_| ())
        .boxed()
}

fn bool_parser() -> ArcParser<()> {
    lit("true").or(lit("false")).boxed()
}

// -- uniform ArcParser<()> combinator helpers ------------------------------

/// A literal, boxed to the uniform `ArcParser<()>` type.
fn lit(text: impl Into<Cow<'static, str>>) -> ArcParser<()> {
    LiteralParser::new(text).boxed()
}

/// `a` then `b`, output erased to `()`.
fn seq2(a: ArcParser<()>, b: ArcParser<()>) -> ArcParser<()> {
    a.then(b).map_output(|_| ()).boxed()
}

/// Fold a non-empty list of alternatives into a single `either` parser.
/// (An empty list degrades to a parser that matches the empty string, but
/// callers never pass one.)
fn or_all(parsers: Vec<ArcParser<()>>) -> ArcParser<()> {
    let mut iter = parsers.into_iter();
    let mut acc = match iter.next() {
        Some(first) => first,
        None => return lit(""),
    };
    for parser in iter {
        acc = acc.or(parser).boxed();
    }
    acc
}

/// JSON-encode `s` as a quoted string literal (including the surrounding
/// quotes and any necessary escaping).
fn json_string_literal(s: &str) -> String {
    serde_json::to_string(s).unwrap_or_else(|_| format!("\"{}\"", s.replace('"', "\\\"")))
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

/// Pull every `<tool_call>…</tool_call>` block out of `text` and parse the
/// inner JSON.  Works for both constrained output (the text *is* the
/// block(s)) and auto-mode output (blocks embedded in prose).  Falls back
/// to treating the whole trimmed text as a single bare JSON call.
pub fn extract_tool_calls(text: &str) -> Vec<ExtractedToolCall> {
    const OPEN: &str = "<tool_call>";
    const CLOSE: &str = "</tool_call>";

    let mut calls = Vec::new();
    let mut rest = text;
    while let Some(start) = rest.find(OPEN) {
        let after = &rest[start + OPEN.len()..];
        let (block, next) = match after.find(CLOSE) {
            Some(end) => (&after[..end], &after[end + CLOSE.len()..]),
            None => (after, ""),
        };
        if let Some(call) = parse_tool_call_json(block.trim()) {
            calls.push(call);
        }
        rest = next;
    }

    if calls.is_empty() {
        if let Some(call) = parse_tool_call_json(text.trim()) {
            calls.push(call);
        }
    }
    calls
}

fn parse_tool_call_json(candidate: &str) -> Option<ExtractedToolCall> {
    let value: Value = serde_json::from_str(candidate).ok()?;
    let name = value.get("name")?.as_str()?.to_string();
    let arguments = match value.get("arguments") {
        Some(Value::String(s)) => s.clone(),
        Some(other) => other.to_string(),
        None => "{}".to_string(),
    };
    Some(ExtractedToolCall { name, arguments })
}

// ---------------------------------------------------------------------------
// Tests (no model required — pure grammar / parsing)
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::api::openai_compat::types::FunctionDef;
    use kalosm::language::{CreateParserState, ParseStatus, Parser};
    use serde_json::json;

    /// Feed the whole input to the parser and report whether it reaches a
    /// `Finished` state (i.e. the string is accepted by the grammar).
    fn accepts(parser: &ArcParser<()>, input: &str) -> bool {
        let state = parser.create_parser_state();
        matches!(
            parser.parse(&state, input.as_bytes()),
            Ok(ParseStatus::Finished { .. })
        )
    }

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

    fn obj(schema: Value) -> ArcParser<()> {
        object_parser(&schema)
    }

    #[test]
    fn string_property_accepts_valid_rejects_wrong_type() {
        let p = obj(json!({
            "type": "object",
            "properties": { "location": { "type": "string" } }
        }));
        assert!(accepts(&p, r#"{"location": "NYC"}"#));
        // number where a string is required
        assert!(!accepts(&p, r#"{"location": 5}"#));
        // undeclared extra property
        assert!(!accepts(&p, r#"{"location": "NYC", "x": "y"}"#));
    }

    #[test]
    fn integer_property() {
        let p = obj(json!({
            "type": "object",
            "properties": { "n": { "type": "integer" } }
        }));
        assert!(accepts(&p, r#"{"n": 42}"#));
        assert!(accepts(&p, r#"{"n": -7}"#));
        assert!(!accepts(&p, r#"{"n": "x"}"#));
    }

    #[test]
    fn enum_property() {
        let p = obj(json!({
            "type": "object",
            "properties": { "unit": { "type": "string", "enum": ["celsius", "fahrenheit"] } }
        }));
        assert!(accepts(&p, r#"{"unit": "celsius"}"#));
        assert!(accepts(&p, r#"{"unit": "fahrenheit"}"#));
        assert!(!accepts(&p, r#"{"unit": "kelvin"}"#));
    }

    #[test]
    fn boolean_property() {
        let p = obj(json!({
            "type": "object",
            "properties": { "b": { "type": "boolean" } }
        }));
        assert!(accepts(&p, r#"{"b": true}"#));
        assert!(accepts(&p, r#"{"b": false}"#));
        assert!(!accepts(&p, r#"{"b": 1}"#));
    }

    #[test]
    fn array_property_including_empty() {
        let p = obj(json!({
            "type": "object",
            "properties": { "xs": { "type": "array", "items": { "type": "integer" } } }
        }));
        assert!(accepts(&p, r#"{"xs": [1, 2, 3]}"#));
        assert!(accepts(&p, r#"{"xs": []}"#));
        assert!(accepts(&p, r#"{"xs": [7]}"#));
        assert!(!accepts(&p, r#"{"xs": [1, "a"]}"#));
    }

    #[test]
    fn nested_object_property() {
        let p = obj(json!({
            "type": "object",
            "properties": {
                "loc": {
                    "type": "object",
                    "properties": { "city": { "type": "string" } }
                }
            }
        }));
        assert!(accepts(&p, r#"{"loc": {"city": "NYC"}}"#));
        assert!(!accepts(&p, r#"{"loc": {"city": 5}}"#));
    }

    #[test]
    fn empty_parameters_compiles_to_empty_object() {
        let p = obj(json!({ "type": "object" }));
        assert!(accepts(&p, "{}"));
        assert!(!accepts(&p, r#"{"x": 1}"#));
    }

    #[test]
    fn required_tool_call_parser_matches_hermes_block() {
        let weather = tool(
            "get_weather",
            json!({
                "type": "object",
                "properties": { "location": { "type": "string" } }
            }),
        );
        let parser = build_tool_call_parser(&[weather], &ToolChoice::Required, false)
            .expect("required ⇒ Some(parser)");

        assert!(accepts(
            &parser,
            "<tool_call>\n{\"name\": \"get_weather\", \"arguments\": {\"location\": \"NYC\"}}\n</tool_call>"
        ));
        // wrong function name
        assert!(!accepts(
            &parser,
            "<tool_call>\n{\"name\": \"other\", \"arguments\": {\"location\": \"NYC\"}}\n</tool_call>"
        ));
        // malformed arguments (number for a string field)
        assert!(!accepts(
            &parser,
            "<tool_call>\n{\"name\": \"get_weather\", \"arguments\": {\"location\": 5}}\n</tool_call>"
        ));
    }

    #[test]
    fn named_choice_restricts_to_one_tool() {
        let a = tool("alpha", json!({ "type": "object", "properties": {} }));
        let b = tool("beta", json!({ "type": "object", "properties": {} }));
        let parser = build_tool_call_parser(&[a, b], &ToolChoice::Named("alpha".to_string()), true)
            .expect("named ⇒ Some(parser)");
        assert!(accepts(
            &parser,
            "<tool_call>\n{\"name\": \"alpha\", \"arguments\": {}}\n</tool_call>"
        ));
        assert!(!accepts(
            &parser,
            "<tool_call>\n{\"name\": \"beta\", \"arguments\": {}}\n</tool_call>"
        ));
    }

    #[test]
    fn auto_and_none_are_unconstrained() {
        let weather = tool("get_weather", json!({ "type": "object", "properties": {} }));
        assert!(build_tool_call_parser(&[weather.clone()], &ToolChoice::Auto, true).is_none());
        assert!(build_tool_call_parser(&[weather], &ToolChoice::None, true).is_none());
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
