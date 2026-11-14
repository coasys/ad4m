# ai_service/providers/ — agent guide

Model servers behind four traits: `RemoteChat` (one client per LLM, `chat_client`),
`audio::RemoteTranscribe` (speech to text), `embed::RemoteEmbed` (one client per embedding
model) and `discovery::Discover` (one listing per endpoint). Every model the executor uses
comes through here: it runs none itself.

## Files

| File | Content |
|---|---|
| `mod.rs` | `RemoteChat` trait, the neutral `ChatRole`/`ChatTurn`/`ChatRequest`/`ChatReply`/`ChatUsage`/`ToolSpec`/`ToolCall` types, and `api_type_supports_native_tools`. No wire format. |
| `embed.rs` | `RemoteEmbed`, `embed_client(endpoint)`: OpenAI-shaped `POST /v1/embeddings`, Ollama native `POST /api/embed`; one request per batch, order kept, the upstream's token count when reported. None for Anthropic. |
| `discovery.rs` | `Discover` (list models; `health_of` = the listing, classified; `listing_or_registered`: an audio server's 404 listing), `discover_for(kind, …)`, capability guesses per model id, `PROBE_TIMEOUT`. |
| `http.rs` | Sending a credential over HTTP: `credentialed_http` (no redirects, a timeout), `is_transport_safe`, `fetch_json` + `UpstreamStatus` (error status with the key scrubbed, `Retry-After` kept) + `UpstreamUnreachable`, `read_json` / `read_error_text` (every non-streamed body read: 16 MiB answers, 64 KiB error reasons), and the endpoint helpers the providers share. |
| `openai.rs` | OpenAI-shaped HTTP on `reqwest`: OpenAI, Groq, OpenRouter, Google compat, Ollama `/v1`, vLLM, llama-server. `tools`, `tool_choice`, `response_format`, SSE with tool-call assembly and `usage`. `OpenAiDiscover` for every OpenAI-shaped kind. |
| `anthropic.rs` | Messages API on `reqwest`: prompt caching, SSE streaming, native `tool_use`/`tool_result`. `AnthropicDiscover`. |
| `audio.rs` | `RemoteTranscribe`, `OpenAiTranscribe`: `POST /v1/audio/transcriptions` multipart, one file per call, extra form fields passed as given; the answer's body and content type kept for a passthrough, its transcript parsed. `TranscribeFailure` classifies a failure in words with no URL or key. Built per stream or per request from the endpoint (`for_endpoint`: OpenAI-shaped kinds only, no key in the clear). |
| `ollama.rs` | Ollama native `/api/chat`: native tools, `num_ctx`, ndjson streaming. `OllamaDiscover` (`/api/tags` + `/api/show` capabilities), `pull_model` (ndjson progress), `delete_model`. |
| `anthropic_e2e.rs` | `#[ignore]`d tests against the real API; need `ANTHROPIC_API_KEY` and cost money. |

## Entry points

- `chat_client(endpoint)`: the `OpenAiChat` / `AnthropicChat` / `OllamaChat` of the
  endpoint's protocol, built once per model by `AIService::init_model` and held in its
  client map (no per-model thread).
- `RemoteChat::chat` / `chat_stream`: called concurrently through `ai_service::ChatModel`,
  which takes a slot of the endpoint's `max_concurrent` first. `chat_stream` defaults to
  one chunk; tools on a stream only on the OpenAI-shaped client.
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
- `ChatRequest::json_schema` is a decoding constraint, not a caller option: the OpenAI-shaped
  client sends it as `response_format` `json_schema` (unless the caller set its own
  `response_format`), Ollama's native client as `format`, Anthropic not at all. The caller
  validates the reply whichever answered (`/v1` text-tools path: `tool_grammar::ToolCallConstraint`).
- `ChatRequest::disable_thinking` / `max_output_tokens` are the same kind of internal
  constraint. Ollama sends `think: false` only when `/api/show` capabilities include
  `thinking` (otherwise some versions answer 400 "does not support thinking") and
  `options.num_predict` when a cap is set. llama-server gets
  `chat_template_kwargs.enable_thinking: false` plus `max_tokens`. A strict OpenAI
  endpoint (`EndpointKind::OpenAi`) gets `max_tokens` only — it rejects unknown fields.
  Anthropic gets neither. A failed completion is logged here
  (`log_upstream_error`: status + a 500-char body excerpt, key already scrubbed).
- Whether a *model* gets tools as data is `Model::uses_native_tools(endpoint)`: the client can,
  and the model's `native_tools` flag (default on for Anthropic and Ollama, off for
  OpenAI-shaped servers) says it should.
- Billing is the caller's. `ChatUsage` reports the provider's counts;
  `ai_service` estimates and bills around the call. Do not add a billing hook here.

## Do not

- Do not branch on the provider above this module: a new protocol is a `RemoteChat`
  implementation and an arm in `chat_client`.
- Do not reach for `AIService::global_instance()` from a provider — everything
  a call needs is on `ChatRequest` or the constructor.
