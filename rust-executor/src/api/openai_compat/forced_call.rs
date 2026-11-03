//! A tool call the request forces (`tool_choice: "required"` or a named function), made by a
//! grammar the server enforces, for a llama-server endpoint.
//!
//! llama-server b11669 does not enforce either choice for Qwen3.5 (the managed chat model):
//! with `"required"` the model may answer in text and call nothing; the named form
//! (`{"type":"function","function":{"name":…}}`) it rejects as the wrong type and treats
//! as `"auto"`. It does honour `parallel_tool_calls: false`, and it does enforce
//! `response_format: json_schema` by a grammar (but not together with `tools`: "failed to
//! parse grammar"). So such a request goes up without `tools`, with the tools described in
//! the system prompt and a schema that admits only calls of the allowed tools: one call for a
//! named function or `parallel_tool_calls: false`, else one or more. The JSON the server
//! answers comes back to the client as `tool_calls`, as a JSON answer or as SSE.
//!
//! The executor's own decoding enforced these choices before it stopped running models
//! (Kalosm); this keeps them for the models it runs now.

use serde_json::{json, Value};
use uuid::Uuid;

use super::errors::OpenAIError;

/// What a request forces: the tools a call may name, and whether several calls may come.
#[derive(Debug, Clone, PartialEq)]
pub struct ForcedCall {
    /// The `function` objects of the tools a call may name.
    functions: Vec<Value>,
    parallel: bool,
    named: Option<String>,
}

/// The call `body` forces, if it forces one. `None`: no tools, `tool_choice` absent,
/// `"auto"` or `"none"`, or a `response_format` of the client's own, which stays as written.
/// A named function the request has no tool for is the client's error, as at OpenAI.
pub fn forced_call(body: &Value) -> Result<Option<ForcedCall>, OpenAIError> {
    let functions: Vec<Value> = body
        .get("tools")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|tool| tool.get("function").cloned())
        .collect();
    if functions.is_empty() || body.get("response_format").is_some() {
        return Ok(None);
    }
    let parallel = body.get("parallel_tool_calls").and_then(Value::as_bool) != Some(false);
    match body.get("tool_choice") {
        Some(Value::String(choice)) if choice == "required" => Ok(Some(ForcedCall {
            functions,
            parallel,
            named: None,
        })),
        Some(choice @ Value::Object(_)) => {
            let Some(name) = choice.pointer("/function/name").and_then(Value::as_str) else {
                return Ok(None);
            };
            let function = functions
                .into_iter()
                .find(|f| f.get("name").and_then(Value::as_str) == Some(name))
                .ok_or_else(|| {
                    OpenAIError::invalid_request(format!(
                        "`tool_choice` names `{name}`, which is not one of `tools`"
                    ))
                })?;
            Ok(Some(ForcedCall {
                functions: vec![function],
                parallel: false,
                named: Some(name.to_string()),
            }))
        }
        _ => Ok(None),
    }
}

impl ForcedCall {
    /// The schema of one call of `function`: its name and arguments by its `parameters`.
    fn call_schema(function: &Value) -> Value {
        let parameters = function
            .get("parameters")
            .filter(|p| p.is_object())
            .cloned()
            .unwrap_or_else(|| json!({ "type": "object" }));
        json!({
            "type": "object",
            "properties": {
                "name": { "const": function.get("name").cloned().unwrap_or(Value::Null) },
                "arguments": parameters,
            },
            "required": ["name", "arguments"],
            "additionalProperties": false,
        })
    }

    /// The schema the answer must match.
    fn schema(&self) -> Value {
        let one = match self.functions.as_slice() {
            [only] => Self::call_schema(only),
            all => json!({ "anyOf": all.iter().map(Self::call_schema).collect::<Vec<_>>() }),
        };
        if self.parallel {
            json!({ "type": "array", "minItems": 1, "items": one })
        } else {
            one
        }
    }

    /// The system prompt that describes the tools and what to answer.
    fn instructions(&self) -> String {
        let mut text = String::from("You can call these functions:\n");
        for function in &self.functions {
            text.push_str(&function.to_string());
            text.push('\n');
        }
        let what = match &self.named {
            Some(name) => format!("Call the function `{name}` now."),
            None if self.parallel => "Call one or more of these functions now.".to_string(),
            None => "Call one of these functions now.".to_string(),
        };
        let shape = if self.parallel {
            "Answer with only the calls you make: a JSON array of {\"name\": <function name>, \"arguments\": <its arguments>}."
        } else {
            "Answer with only the call: {\"name\": <function name>, \"arguments\": <its arguments>}."
        };
        format!("{text}{what} {shape}")
    }

    /// The request the server gets: no `tools`, `tool_choice` or `parallel_tool_calls`; the
    /// tools in the first system message (prepended to the client's own, as chat templates
    /// take one system message, first); the schema as `response_format`; no stream (the
    /// client's stream is framed from the whole answer).
    pub fn rewrite(&self, body: &mut Value) {
        if let Some(object) = body.as_object_mut() {
            for key in [
                "tools",
                "tool_choice",
                "parallel_tool_calls",
                "stream_options",
            ] {
                object.remove(key);
            }
        }
        body["stream"] = Value::Bool(false);
        body["response_format"] = json!({
            "type": "json_schema",
            "json_schema": { "name": "tool_calls", "strict": true, "schema": self.schema() },
        });
        let instructions = self.instructions();
        let messages = body
            .get_mut("messages")
            .and_then(Value::as_array_mut)
            .map(std::mem::take)
            .unwrap_or_default();
        let mut messages = messages.into_iter().peekable();
        let first_system = messages
            .next_if(|m| m.get("role").and_then(Value::as_str) == Some("system"))
            .map(|m| system_text(&m));
        let system = match first_system {
            Some(own) if !own.is_empty() => format!("{own}\n\n{instructions}"),
            _ => instructions,
        };
        let mut rebuilt = vec![json!({ "role": "system", "content": system })];
        rebuilt.extend(messages);
        body["messages"] = Value::Array(rebuilt);
    }

    /// The server's answer as the client's: the JSON in the first choice's content as
    /// `tool_calls`, `finish_reason: "tool_calls"`. An answer that does not parse as calls
    /// (cut off at `max_tokens`) stays as it came, so the client sees what happened.
    pub fn answer(&self, answer: &mut Value) {
        let Some(choice) = answer.pointer_mut("/choices/0") else {
            return;
        };
        let content = choice
            .pointer("/message/content")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let Ok(parsed) = serde_json::from_str::<Value>(content) else {
            return;
        };
        let calls = match parsed {
            Value::Array(calls) => calls,
            call @ Value::Object(_) => vec![call],
            _ => return,
        };
        let mut tool_calls = Vec::new();
        for call in calls {
            let (Some(name), Some(arguments)) = (
                call.get("name").and_then(Value::as_str),
                call.get("arguments"),
            ) else {
                return;
            };
            tool_calls.push(json!({
                "id": format!("call_{}", Uuid::new_v4().simple()),
                "type": "function",
                "function": { "name": name, "arguments": arguments.to_string() },
            }));
        }
        choice["message"]["content"] = Value::Null;
        choice["message"]["tool_calls"] = Value::Array(tool_calls);
        choice["finish_reason"] = json!("tool_calls");
    }
}

/// The text of a system message, whether its content is a string or text parts.
fn system_text(message: &Value) -> String {
    match message.get("content") {
        Some(Value::String(text)) => text.clone(),
        Some(Value::Array(parts)) => parts
            .iter()
            .filter_map(|p| p.get("text").and_then(Value::as_str))
            .collect::<Vec<_>>()
            .join("\n"),
        _ => String::new(),
    }
}

/// A whole chat answer as the SSE events of a stream: the message (its role, content and
/// tool calls) in one chunk, the finish reason in the next, the usage when the client asked
/// for it, then `[DONE]`.
pub fn as_sse(answer: &Value, include_usage: bool) -> String {
    let base = |choices: Value| {
        json!({
            "id": answer.get("id").cloned().unwrap_or_else(|| json!(format!("chatcmpl-{}", Uuid::new_v4().simple()))),
            "object": "chat.completion.chunk",
            "created": answer.get("created").cloned().unwrap_or(json!(0)),
            "model": answer.get("model").cloned().unwrap_or(Value::Null),
            "choices": choices,
        })
    };
    let message = answer
        .pointer("/choices/0/message")
        .cloned()
        .unwrap_or_default();
    let mut delta = json!({ "role": "assistant", "content": message.get("content").cloned().unwrap_or(Value::Null) });
    if let Some(calls) = message.get("tool_calls").and_then(Value::as_array) {
        delta["tool_calls"] = Value::Array(
            calls
                .iter()
                .enumerate()
                .map(|(index, call)| {
                    let mut call = call.clone();
                    call["index"] = json!(index);
                    call
                })
                .collect(),
        );
    }
    let finish = answer
        .pointer("/choices/0/finish_reason")
        .cloned()
        .unwrap_or(json!("stop"));
    let mut events = vec![
        base(json!([{ "index": 0, "delta": delta, "finish_reason": null }])),
        base(json!([{ "index": 0, "delta": {}, "finish_reason": finish }])),
    ];
    if include_usage {
        if let Some(usage) = answer.get("usage") {
            let mut last = base(json!([]));
            last["usage"] = usage.clone();
            events.push(last);
        }
    }
    let mut text: String = events
        .iter()
        .map(|event| format!("data: {event}\n\n"))
        .collect();
    text.push_str("data: [DONE]\n\n");
    text
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tools() -> Value {
        json!([
            { "type": "function", "function": { "name": "get_weather", "parameters": { "type": "object", "properties": { "city": { "type": "string" } }, "required": ["city"] } } },
            { "type": "function", "function": { "name": "add_note", "description": "Store a note", "parameters": { "type": "object", "properties": { "text": { "type": "string" } } } } }
        ])
    }

    #[test]
    fn only_required_or_a_named_function_forces_a_call() {
        for choice in [json!("auto"), json!("none"), Value::Null] {
            let body = json!({ "tools": tools(), "tool_choice": choice });
            assert_eq!(forced_call(&body).unwrap(), None, "{choice}");
        }
        assert_eq!(
            forced_call(&json!({ "tool_choice": "required" })).unwrap(),
            None
        );
        // The client's own response_format stays as written.
        let own = json!({ "tools": tools(), "tool_choice": "required", "response_format": { "type": "json_object" } });
        assert_eq!(forced_call(&own).unwrap(), None);

        let required = forced_call(&json!({ "tools": tools(), "tool_choice": "required" }))
            .unwrap()
            .unwrap();
        assert!(required.parallel);
        assert_eq!(required.functions.len(), 2);
        let single = forced_call(
            &json!({ "tools": tools(), "tool_choice": "required", "parallel_tool_calls": false }),
        )
        .unwrap()
        .unwrap();
        assert!(!single.parallel);
        let named = forced_call(&json!({ "tools": tools(), "tool_choice": { "type": "function", "function": { "name": "add_note" } } }))
            .unwrap()
            .unwrap();
        assert_eq!((named.functions.len(), named.parallel), (1, false));
        assert!(forced_call(
            &json!({ "tools": tools(), "tool_choice": { "type": "function", "function": { "name": "nope" } } })
        )
        .is_err());
    }

    #[test]
    fn the_request_goes_up_without_tools_with_the_calls_schema_and_one_system_message() {
        let mut body = json!({
            "model": "m", "stream": true, "stream_options": { "include_usage": true },
            "tools": tools(), "tool_choice": { "type": "function", "function": { "name": "add_note" } },
            "messages": [{ "role": "system", "content": "Be brief." }, { "role": "user", "content": "Remember tea." }]
        });
        let forced = forced_call(&body).unwrap().unwrap();
        forced.rewrite(&mut body);
        for gone in [
            "tools",
            "tool_choice",
            "parallel_tool_calls",
            "stream_options",
        ] {
            assert!(body.get(gone).is_none(), "{gone}");
        }
        assert_eq!(body["stream"], false);
        let schema = &body["response_format"]["json_schema"]["schema"];
        assert_eq!(schema["properties"]["name"]["const"], "add_note");
        assert_eq!(
            schema["properties"]["arguments"]["properties"]["text"]["type"],
            "string"
        );
        let messages = body["messages"].as_array().unwrap();
        assert_eq!(messages.len(), 2);
        let system = messages[0]["content"].as_str().unwrap();
        assert!(
            system.starts_with("Be brief.\n\nYou can call these functions:"),
            "{system}"
        );
        assert!(
            system.contains("Call the function `add_note` now."),
            "{system}"
        );
        assert!(
            !system.contains("get_weather"),
            "only the named tool: {system}"
        );
        assert_eq!(messages[1]["role"], "user");

        let mut required = json!({ "tools": tools(), "tool_choice": "required", "messages": [{ "role": "user", "content": "x" }] });
        forced_call(&required)
            .unwrap()
            .unwrap()
            .rewrite(&mut required);
        let schema = &required["response_format"]["json_schema"]["schema"];
        assert_eq!(
            (schema["type"].as_str(), schema["minItems"].as_u64()),
            (Some("array"), Some(1))
        );
        assert_eq!(schema["items"]["anyOf"].as_array().unwrap().len(), 2);
        assert_eq!(required["messages"][0]["role"], "system");
    }

    #[test]
    fn the_servers_json_comes_back_as_tool_calls_and_a_cut_off_answer_as_it_came() {
        let forced = forced_call(&json!({ "tools": tools(), "tool_choice": "required" }))
            .unwrap()
            .unwrap();
        let mut answer = json!({ "choices": [{ "index": 0, "finish_reason": "stop", "message": { "role": "assistant",
            "content": "[{\"name\": \"get_weather\", \"arguments\": {\"city\": \"Paris\"}}, {\"name\": \"get_weather\", \"arguments\": {\"city\": \"Tokyo\"}}]" } }] });
        forced.answer(&mut answer);
        let message = &answer["choices"][0]["message"];
        assert_eq!(message["content"], Value::Null);
        assert_eq!(answer["choices"][0]["finish_reason"], "tool_calls");
        let calls = message["tool_calls"].as_array().unwrap();
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[1]["function"]["name"], "get_weather");
        assert_eq!(calls[1]["function"]["arguments"], "{\"city\":\"Tokyo\"}");
        assert!(calls[0]["id"].as_str().unwrap().starts_with("call_"));
        assert_ne!(calls[0]["id"], calls[1]["id"]);

        let mut cut = json!({ "choices": [{ "index": 0, "finish_reason": "length", "message": { "content": "[{\"name\": \"get_we" } }] });
        let before = cut.clone();
        forced.answer(&mut cut);
        assert_eq!(cut, before);

        let sse = as_sse(&answer, true);
        let events: Vec<&str> = sse.split("\n\n").filter(|e| !e.is_empty()).collect();
        assert_eq!(events.last(), Some(&"data: [DONE]"));
        let first: Value = serde_json::from_str(events[0].strip_prefix("data: ").unwrap()).unwrap();
        assert_eq!(first["choices"][0]["delta"]["tool_calls"][1]["index"], 1);
        let second: Value =
            serde_json::from_str(events[1].strip_prefix("data: ").unwrap()).unwrap();
        assert_eq!(second["choices"][0]["finish_reason"], "tool_calls");
    }
}
