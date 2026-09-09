//! Real-API tests for the Anthropic provider.
//!
//! # What these prove that the mock tests cannot
//!
//! The tests in `anthropic.rs` prove we send what Anthropic's documentation
//! describes. They cannot prove the documentation matches the API. This file
//! closes that gap: every test here talks to `api.anthropic.com`.
//!
//! # Gating
//!
//! Two gates, matching `perspectives/flow_context/real_llm_e2e.rs`.
//!
//! - `#[ignore = "llm-e2e"]`, so regular CI skips them.
//! - Each test returns early when `ANTHROPIC_API_KEY` is unset, so a developer
//!   without a key sees a skip and not a failure.
//!
//! These cost real money, unlike the nightly `llm-e2e` suite, which runs
//! against a local Ollama. That is why they are not wired into the nightly.
//! Keep the prompts tiny and the `max_tokens` low.
//!
//! ```sh
//! ANTHROPIC_API_KEY=$(cat ~/.anthropic_key) cargo test --release --lib \
//!   ai_service::providers::anthropic_e2e -- --ignored --test-threads=1 --nocapture
//! ```

#![cfg(test)]

use super::anthropic::AnthropicChat;
use super::{ChatRequest, ChatTurn, RemoteChat, ToolSpec};
use url::Url;

const MODEL: &str = "claude-opus-5";

/// The key, or `None` when the test should skip.
///
/// Trimmed, because a key read from a file usually carries a trailing newline
/// and a newline inside an HTTP header produces a 401 that names nothing
/// useful.
fn key() -> Option<String> {
    match std::env::var("ANTHROPIC_API_KEY") {
        Ok(k) if !k.trim().is_empty() => Some(k.trim().to_string()),
        _ => {
            println!("SKIP: ANTHROPIC_API_KEY is not set");
            None
        }
    }
}

fn client(api_key: &str) -> AnthropicChat {
    AnthropicChat::new(
        api_key,
        Url::parse("https://api.anthropic.com").expect("base URL parses"),
    )
}

/// A system prompt long enough to pass Anthropic's minimum cacheable prefix.
///
/// Below that minimum the API declines to cache and reports nothing, so a
/// short prompt cannot tell a working breakpoint from a broken one.
///
/// This is real prose from this repository rather than generated filler, and
/// that matters. Three synthetic prefixes were refused outright with three
/// different categories — a repeated paragraph, 900 numbered behaviour rules
/// (`reasoning_extraction`), and a parts catalogue (`cyber`). Bulk
/// machine-generated text appears to trip classifiers whatever it says, and a
/// refusal looks exactly like a caching failure from the outside. Real
/// documentation is also closer to the case this feature exists for, where the
/// cached prefix is a large generated reference document.
fn long_system_prompt() -> String {
    let mut prompt = String::from(
        "The following is internal documentation. Answer questions about it briefly.\n\n",
    );
    prompt.push_str(include_str!("../../../AGENTS.md"));
    prompt.push_str(include_str!("../../../src/ai_service/AGENTS.md"));
    prompt.push_str(include_str!(
        "../../../../planning/llm-harness-design-2026-08-21-v3.md"
    ));
    prompt
}

/// The whole point of the provider: send turns, get text back.
///
/// For this to pass, the request shape, the auth headers and the response
/// parsing must all be right at once.
#[tokio::test]
#[ignore = "llm-e2e"]
async fn a_completion_returns_text() {
    let Some(api_key) = key() else { return };

    let reply = client(&api_key)
        .chat(ChatRequest::new(
            MODEL,
            vec![
                ChatTurn::system("Answer with one word."),
                ChatTurn::user("What colour is a clear midday sky? One word."),
            ],
        ))
        .await
        .expect("Anthropic answers");

    println!("text: {:?}  usage: {:?}", reply.text, reply.usage);
    assert!(!reply.text.trim().is_empty(), "expected some text");
    assert!(reply.tool_calls.is_empty(), "no tools were offered");
    assert!(
        reply.usage.input_tokens.unwrap_or(0) > 0,
        "usage should report input tokens"
    );
}

/// Streaming must deliver more than one delta, or it is not streaming.
///
/// A provider that answered in one chunk would pass a weaker assertion while
/// giving a caller no benefit at all.
#[tokio::test]
#[ignore = "llm-e2e"]
async fn a_streamed_completion_arrives_in_pieces() {
    let Some(api_key) = key() else { return };

    let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
    let reply = client(&api_key)
        .chat_stream(
            ChatRequest::new(
                MODEL,
                vec![ChatTurn::user(
                    "List the numbers 1 to 60 in words, comma separated, nothing else.",
                )],
            ),
            tx,
        )
        .await
        .expect("Anthropic streams");

    let mut deltas = Vec::new();
    while let Ok(delta) = rx.try_recv() {
        deltas.push(delta);
    }

    println!("{} deltas, {} chars", deltas.len(), reply.text.len());

    // The trait's default implementation answers in exactly one chunk. Asking
    // for a few hundred characters is what makes a single delta evidence that
    // the Anthropic override was not used, rather than evidence of a short
    // reply. An earlier version asked for ten numbers and saw 4, 2 and then 1
    // delta across three runs, which made it flaky rather than wrong.
    assert!(
        reply.text.len() > 200,
        "the prompt should produce a long answer, got {} chars",
        reply.text.len()
    );
    assert!(
        deltas.len() > 1,
        "a {}-char answer arriving in one delta means the one-chunk default ran",
        reply.text.len()
    );
    assert_eq!(
        deltas.concat(),
        reply.text,
        "the deltas must reassemble into the returned text"
    );
}

/// Native tool calling: the model is handed a schema and answers with a call.
///
/// This is the path that replaces prompt injection for Anthropic, so it has to
/// return a structured call with arguments already parsed into an object.
#[tokio::test]
#[ignore = "llm-e2e"]
async fn a_tool_is_called_with_parsed_arguments() {
    let Some(api_key) = key() else { return };

    let weather = ToolSpec {
        name: "get_weather".to_string(),
        description: "Get the current weather in a given city.".to_string(),
        parameters: serde_json::json!({
            "type": "object",
            "properties": { "city": { "type": "string" } },
            "required": ["city"],
        }),
    };

    let reply = client(&api_key)
        .chat(
            ChatRequest::new(
                MODEL,
                vec![ChatTurn::user("What is the weather in Lisbon right now?")],
            )
            .with_tools(vec![weather]),
        )
        .await
        .expect("Anthropic answers");

    println!("calls: {:?}", reply.tool_calls);
    assert_eq!(reply.tool_calls.len(), 1, "expected exactly one call");

    let call = &reply.tool_calls[0];
    assert_eq!(call.name, "get_weather");
    assert!(!call.id.is_empty(), "a call needs an id to answer");
    assert!(
        call.arguments
            .get("city")
            .and_then(|c| c.as_str())
            .is_some(),
        "arguments should be an object carrying a city, got {:?}",
        call.arguments
    );
}

/// A tool result goes back and the model reads it.
///
/// The round trip is the thing that matters. A call the model cannot be
/// answered is half a feature, and the `tool_result` block shape is the part
/// most likely to be wrong.
#[tokio::test]
#[ignore = "llm-e2e"]
async fn a_tool_result_is_read_back_by_the_model() {
    let Some(api_key) = key() else { return };

    let lookup = ToolSpec {
        name: "get_secret_number".to_string(),
        description: "Return the secret number.".to_string(),
        parameters: serde_json::json!({ "type": "object", "properties": {} }),
    };

    let first = client(&api_key)
        .chat(
            ChatRequest::new(
                MODEL,
                vec![ChatTurn::user(
                    "Call get_secret_number, then tell me the number.",
                )],
            )
            .with_tools(vec![lookup.clone()]),
        )
        .await
        .expect("Anthropic answers");

    let call = first
        .tool_calls
        .first()
        .expect("the model should call the tool");

    let second = client(&api_key)
        .chat(
            ChatRequest::new(
                MODEL,
                vec![
                    ChatTurn::user("Call get_secret_number, then tell me the number."),
                    ChatTurn::assistant_calling(first.text.clone(), first.tool_calls.clone()),
                    ChatTurn::tool_result(&call.id, "8675309"),
                ],
            )
            .with_tools(vec![lookup]),
        )
        .await
        .expect("Anthropic answers the second turn");

    println!("second turn: {:?}", second.text);
    assert!(
        second.text.contains("8675309"),
        "the model should repeat the number we returned, got: {:?}",
        second.text
    );
}

/// The cache breakpoint lands.
///
/// This is the only test that can catch a silent caching failure. If
/// `cache_control` stops being accepted, every other test still passes and the
/// only symptom is a larger bill.
///
/// Two calls share one long system prompt. The first writes the cache, the
/// second must read it.
#[tokio::test]
#[ignore = "llm-e2e"]
async fn the_second_call_reads_the_prompt_cache() {
    let Some(api_key) = key() else { return };

    let system = long_system_prompt();
    let client = client(&api_key);

    let ask = |question: &'static str| {
        ChatRequest::new(
            MODEL,
            vec![ChatTurn::system(system.clone()), ChatTurn::user(question)],
        )
    };

    let first = client.chat(ask("Say the word red.")).await.expect("first");
    println!("first usage:  {:?}", first.usage);

    let second = client
        .chat(ask("Say the word blue."))
        .await
        .expect("second");
    println!("second usage: {:?}", second.usage);

    // The first call engages the cache. Whether it writes or reads depends on
    // whether an earlier run left the prefix warm, and the five-minute window
    // means both happen in practice. Asserting a write here made this test
    // pass alone and fail in a suite. What matters is that the breakpoint was
    // accepted at all, which either number shows.
    let first_touched =
        first.usage.cache_write_tokens.unwrap_or(0) + first.usage.cache_read_tokens.unwrap_or(0);
    assert!(
        first_touched > 0,
        "the first call should write or read the cache, got {:?}",
        first.usage
    );

    // This is the claim. A second call over an identical prefix must read it
    // back. If `cache_control` stops being accepted, this is the only test
    // that notices, because nothing else about the response changes.
    assert!(
        second.usage.cache_read_tokens.unwrap_or(0) > 0,
        "the second call should read the cache, got {:?}",
        second.usage
    );
}
