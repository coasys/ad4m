# ai_service/providers/ — agent guide

Remote LLM endpoints behind one trait. Local (candle/kalosm) models do not come
through here — `LlmModel::Local` is a different thing entirely.

## Files

| File | Content |
|---|---|
| `mod.rs` | `RemoteChat` trait, the neutral `ChatRole`/`ChatTurn`/`ChatRequest`/`ChatReply`/`ChatUsage`/`ToolSpec`/`ToolCall` types, and the per-`ModelApiType` dispatch (`list_models`, `api_type_supports_native_tools`). No wire format. |
| `http.rs` | Sending a credential over HTTP: `credentialed_http` (no redirects), `is_transport_safe`, and the endpoint helpers both providers share. |
| `openai.rs` | OpenAI-shaped HTTP (`chat_gpt_lib_rs`): OpenAI, Groq, OpenRouter, Google compat, Ollama `/v1`, vLLM. Tools are not passed natively. |
| `anthropic.rs` | Messages API on `reqwest`: prompt caching, SSE streaming, native `tool_use`/`tool_result`. |
| `anthropic_e2e.rs` | `#[ignore]`d tests against the real API; need `ANTHROPIC_API_KEY` and cost money. |

## Entry points

- `OpenAiChat::new` / `AnthropicChat::new`: built once per model by
  `AIService::build_remote_client`, owned by that model's worker thread.
- `RemoteChat::chat` / `chat_stream`: called from that thread's loop.
  `chat_stream` defaults to one chunk; tools are not promised on it.
- `list_models(api_type, api_key, base_url)`: a free function, because
  discovery happens before a model exists. Reached as `ai.discoverModels`.

## Invariants

- One instance per configured model: a connection pool is fine, per-request state is not.
- Every client that sends a key is built from `http::credentialed_http`.
- A client answering `supports_native_tools() == false` refuses a request
  carrying tools rather than dropping them.
- Billing is the caller's. `ChatUsage` reports the provider's counts;
  `ai_service` estimates and bills around the call. Do not add a billing hook here.

## Do not

- Do not add an `LlmModel` variant per provider. That enum answers "local
  weights or remote endpoint"; which remote protocol is this module's question.
- Do not reach for `AIService::global_instance()` from a provider — everything
  a call needs is on `ChatRequest`.
