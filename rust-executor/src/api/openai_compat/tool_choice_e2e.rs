//! `tool_choice` on the text path against a real model server (llama-server, vLLM, ...):
//! the server decodes against the calls' schema, and `/v1/chat/completions` answers the
//! call the request requires.
//!
//! Skipped unless `TOOL_CHOICE_E2E_BASE_URL` (e.g. `http://127.0.0.1:8080`) and
//! `TOOL_CHOICE_E2E_MODEL` (the id the server knows) are set. `TOOL_CHOICE_E2E_KIND` names
//! the endpoint kind (`LLAMA_SERVER`, the default, `VLLM`, `OLLAMA`, `OPEN_AI_COMPATIBLE`).
//! A small model should run without thinking (llama-server `--reasoning-budget 0`). Run:
//!
//! ```sh
//! TOOL_CHOICE_E2E_BASE_URL=http://127.0.0.1:8080 TOOL_CHOICE_E2E_MODEL=qwen \
//! cargo test --release --lib api::openai_compat::tool_choice_e2e -- --test-threads=1 --nocapture
//! ```

use axum::response::IntoResponse;
use http_body_util::BodyExt;
use serde_json::{json, Value};

use super::chat;
use super::errors::OpenAIJson;
use crate::ai_service::AIService;
use crate::db::Ad4mDb;
use crate::types::{Endpoint, EndpointKind, ModelInput, ModelType, RequestContext};

/// The model registered on the configured server, with `nativeTools` as `native_tools`
/// says; `None` (the test is skipped) when no server is configured.
async fn text_tools_model(native_tools: bool) -> Option<()> {
    let base_url = std::env::var("TOOL_CHOICE_E2E_BASE_URL").ok()?;
    let remote_model = std::env::var("TOOL_CHOICE_E2E_MODEL").ok()?;
    let kind: EndpointKind = serde_json::from_value(json!(
        std::env::var("TOOL_CHOICE_E2E_KIND").unwrap_or_else(|_| "LLAMA_SERVER".to_string())
    ))
    .expect("TOOL_CHOICE_E2E_KIND is an endpoint kind");

    Ad4mDb::init_global_instance(":memory:").unwrap();
    let endpoint = Endpoint::for_test("e2e", kind, &base_url, "");
    Ad4mDb::with_global_instance(|db| db.insert_endpoint(&endpoint)).unwrap();
    let id = Ad4mDb::with_global_instance(|db| {
        db.add_model(&ModelInput {
            name: "chat".into(),
            model_type: ModelType::Llm,
            endpoint_id: endpoint.id.clone(),
            remote_model,
            native_tools: Some(native_tools),
        })
    })
    .unwrap();
    AIService::init_global_instance().await.unwrap();
    AIService::global_instance()
        .await
        .unwrap()
        .reload_model(id)
        .await
        .unwrap();
    Some(())
}

fn tools() -> Value {
    json!([
        { "type": "function", "function": {
            "name": "get_weather",
            "description": "The weather in a city",
            "parameters": {
                "type": "object",
                "properties": {
                    "city": { "type": "string" },
                    "unit": { "type": "string", "enum": ["C", "F"] },
                },
                "required": ["city"],
            },
        } },
        { "type": "function", "function": {
            "name": "get_time",
            "description": "The time now",
            "parameters": { "type": "object", "properties": {} },
        } },
    ])
}

async fn ask(body: Value) -> (u16, String) {
    let response = chat::chat_completions(RequestContext::operator_for_test(), OpenAIJson(body))
        .await
        .into_response();
    let status = response.status().as_u16();
    let bytes = response.into_body().collect().await.unwrap().to_bytes();
    (status, String::from_utf8(bytes.to_vec()).unwrap())
}

#[tokio::test]
async fn a_required_call_comes_back_as_a_valid_call() {
    for native_tools in [false, true] {
        if text_tools_model(native_tools).await.is_none() {
            eprintln!("skipped: TOOL_CHOICE_E2E_BASE_URL / TOOL_CHOICE_E2E_MODEL not set");
            return;
        }
        let (status, body) = ask(json!({
            "model": "chat",
            "messages": [{ "role": "user", "content": "What is the weather in Oslo?" }],
            "tools": tools(),
            "tool_choice": "required",
            "parallel_tool_calls": false,
        }))
        .await;
        eprintln!("required, nativeTools {native_tools}: {status} {body}");
        assert_eq!(status, 200, "{body}");
        let body: Value = serde_json::from_str(&body).unwrap();
        let calls = body["choices"][0]["message"]["tool_calls"]
            .as_array()
            .unwrap();
        assert_eq!(calls.len(), 1, "{body}");
        let name = calls[0]["function"]["name"].as_str().unwrap();
        assert!(["get_weather", "get_time"].contains(&name), "{body}");
        let arguments: Value =
            serde_json::from_str(calls[0]["function"]["arguments"].as_str().unwrap()).unwrap();
        if name == "get_weather" {
            assert!(arguments["city"].is_string(), "{body}");
        }
    }
}

#[tokio::test]
async fn a_named_function_is_the_one_called_also_when_streamed() {
    for native_tools in [false, true] {
        if text_tools_model(native_tools).await.is_none() {
            eprintln!("skipped: TOOL_CHOICE_E2E_BASE_URL / TOOL_CHOICE_E2E_MODEL not set");
            return;
        }
        // The question asks for the weather; the request requires `get_time`.
        let (status, body) = ask(json!({
            "model": "chat",
            "messages": [{ "role": "user", "content": "What is the weather in Oslo?" }],
            "tools": tools(),
            "tool_choice": { "type": "function", "function": { "name": "get_time" } },
            "stream": true,
        }))
        .await;
        eprintln!("named, streamed, nativeTools {native_tools}: {status} {body}");
        assert_eq!(status, 200, "{body}");
        let call = body
            .split("\n\n")
            .filter_map(|event| event.strip_prefix("data: "))
            .filter_map(|data| serde_json::from_str::<Value>(data).ok())
            .find_map(|chunk| chunk["choices"][0]["delta"]["tool_calls"].get(0).cloned())
            .unwrap_or_else(|| panic!("no tool call in {body}"));
        assert_eq!(call["function"]["name"], "get_time", "{body}");
        assert!(body.contains("\"finish_reason\":\"tool_calls\""), "{body}");
    }
}
