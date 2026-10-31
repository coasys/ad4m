# ai_service/providers/ — agent guide

Remote model servers behind three traits: `RemoteChat` (one client per model),
`embed::RemoteEmbed` (one client per embedding model) and `discovery::Discover` (one
listing per endpoint). Local (candle/kalosm) models do not come
through here — `LlmModel::Local` is a different thing entirely.

## Files

| File | Content |
|---|---|
| `mod.rs` | `RemoteChat` trait, the neutral `ChatRole`/`ChatTurn`/`ChatRequest`/`ChatReply`/`ChatUsage`/`ToolSpec`/`ToolCall` types, and `api_type_supports_native_tools`. No wire format. |
| `embed.rs` | `RemoteEmbed`, `embed_client(endpoint)`: OpenAI-shaped `POST /v1/embeddings`, Ollama native `POST /api/embed`; one request per batch, order kept, the upstream's token count when reported. None for Anthropic. |
| `discovery.rs` | `Discover` (list models; health = the listing, classified), `discover_for(kind, …)`, capability guesses per model id, `PROBE_TIMEOUT`. |
| `http.rs` | Sending a credential over HTTP: `credentialed_http` (no redirects, a timeout), `is_transport_safe`, `fetch_json` + `UpstreamStatus` (error status with the key scrubbed), and the endpoint helpers the providers share. |
| `openai.rs` | OpenAI-shaped HTTP on `reqwest`: OpenAI, Groq, OpenRouter, Google compat, Ollama `/v1`, vLLM, llama-server. `tools`, `tool_choice`, `response_format`, SSE with tool-call assembly and `usage`. `OpenAiDiscover` for every OpenAI-shaped kind. |
| `anthropic.rs` | Messages API on `reqwest`: prompt caching, SSE streaming, native `tool_use`/`tool_result`. `AnthropicDiscover`. |
| `ollama.rs` | Ollama native `/api/chat`: native tools, `num_ctx`, ndjson streaming. `OllamaDiscover` (`/api/tags` + `/api/show` capabilities), `pull_model` (ndjson progress), `delete_model`. |
| `anthropic_e2e.rs` | `#[ignore]`d tests against the real API; need `ANTHROPIC_API_KEY` and cost money. |

## Entry points

- `OpenAiChat::new` / `AnthropicChat::new` / `OllamaChat::new`: built once per
  model by `AIService::build_remote_client`, owned by that model's worker thread.
- `RemoteChat::chat` / `chat_stream`: called from that thread's loop.
  `chat_stream` defaults to one chunk; tools on a stream only on the OpenAI-shaped client.
- `discovery::discover_for(kind, base_url, key, timeout)`: listing and health of an
  endpoint, also before one is saved (`ai.discoverModels`, `ai.addEndpoint`).

## Invariants

- One instance per configured model or endpoint: a connection pool is fine, per-request state is not.
- Every client that sends a key is built from `http::credentialed_http`.
- No error carries a key: an upstream body goes through `http::scrub` (`fetch_json` does it).
- An upstream failure is typed: `http::UpstreamStatus` (it answered an error status, with
  its `Retry-After`) or `http::UpstreamUnreachable` (no connection, or no answer in time).
  `/v1` maps them to the client's status (`OpenAIError::from_upstream`).
- A client answering `supports_native_tools() == false` refuses a request
  carrying tools rather than dropping them. A provider that does not translate
  `tool_choice` / `response_format` refuses a request carrying them
  (`ChatRequest::refuse_openai_options`).
- Whether a *model* gets tools as data is `ModelApi::uses_native_tools`: the client can,
  and the model's `native_tools` flag (default on for Anthropic and Ollama, off for
  OpenAI-shaped servers) says it should.
- Billing is the caller's. `ChatUsage` reports the provider's counts;
  `ai_service` estimates and bills around the call. Do not add a billing hook here.

## Do not

- Do not add an `LlmModel` variant per provider. That enum answers "local
  weights or remote endpoint"; which remote protocol is this module's question.
- Do not reach for `AIService::global_instance()` from a provider — everything
  a call needs is on `ChatRequest` or the constructor.
