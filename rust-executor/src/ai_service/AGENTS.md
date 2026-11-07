# ai_service/ — agent guide

`AIService` (global singleton, tokio-mutexed: `AIService::global_instance().await`).
Split plan: spec item 9 (`mod.rs`), item 7 (`harness/` → `agentic/`).

The executor runs no model itself. Every model is an upstream model on an **endpoint** (a
model server: Ollama, llama-server, vLLM, speaches, OpenAI, Anthropic, ...): `Model {
endpoint_id, remote_model, model_type, native_tools }`. The endpoint owns URL, key,
protocol and health. A fresh executor has no model and loads none.

## `mod.rs` sections (1.8k lines, one file)

| Lines (approx) | Content |
|---|---|
| top | DTOs: `PromptResult`, `EmbedResult`, `EmbedBatch`, `TranscriptionSession`; `AIService` (two client maps), `ChatModel`, `Embedder`; token estimates; `turns_for` |
| ~260–430 | construction, `load` (a client per stored model), `init_model` / `drop_clients`, model add/default/views, global instance |
| ~430–860 | AI tasks CRUD (rows only: a prompt reads its task from the database), `prompt` / `prompt_messages` / `prompt_messages_stream` / `prompt_with_tools`, billing hooks (`bill_prompt_if_authed`) |
| ~860–950 | embeddings: `embed_batch` resolves an id or a name and sends one request per batch to the model's `Embedder` |
| ~950–1110 | transcription streams: open, feed, close, ownership |
| ~1110–1135 | model update / reload / remove |
| rest | tests |

A client is `Send + Sync` and takes concurrent requests: `llms` holds one `ChatModel`
(`providers::chat_client(endpoint)`, the remote model id, the endpoint) per LLM, `embedders`
one `Embedder` per embedding model, both keyed by model id. There is no per-model thread.
`ChatModel::chat` / `chat_stream` and `embed_batch` take a slot of the endpoint's
`max_concurrent` (none by default), so the cap holds for tasks, the harness and `/v1` alike.
A model whose client does not build is not kept by `add_model`; at `load` it logs and stays
without one. Transcription and speech build their clients per stream / request.

Persistence of models/tasks is in `db.rs` (`models`, `tasks`, `default_models` tables),
endpoints in `db/endpoints.rs` (`ai_endpoints`), the one-time move from the old model
shape in `db/model_migration.rs`. Tasks have an owner (`tasks.agent_did`, the managed
agent's DID; `NULL` for a task of the main agent; see `src/db/AGENTS.md`): `add_task` takes
it, and the WS guards enforce it. Billing goes through `crate::billing` with the caller's
agent DID.

## `endpoints/`

The registry of upstream model servers (an endpoint owns URL, key, capabilities, health):

- `mod.rs`: `ai.addEndpoint` / `updateEndpoint` / `removeEndpoint` (probe once before a
  save; an update reloads the models on it), `check_health` and the 60 s
  health loop (`ai-endpoint-status`, to the main agent's sessions only; a change also
  publishes `model-status`, to everyone, for each model on the endpoint whose status it
  changes: that is how managed users see an endpoint go down or come back;
  `check_health` probes an endpoint at most once per `PROBE_TIMEOUT`, a pull or delete
  probes at once; a check adds the capabilities of the models listed, removes none, and
  stores nothing when the URL moved meanwhile; an `OPEN_AI_AUDIO` server whose listing is
  a 404 serves its registered models), `after_failed_request` (a refused connection or
  key re-checks the endpoint at once, a 404 publishes the model `MISSING`; called by
  `/v1`, `ChatModel`, `embed_batch` and the transcription stream worker, so a client
  keeps its connection error typed as `http::UpstreamUnreachable`), Ollama `pull_model` /
  `delete_upstream_model` (`model-status`, at most 4/s, a failure's detail names no URL),
  and the per-request limits: `request_timeout` (the endpoint's, else the route's
  default) and `request_slot` (a semaphore only when the operator set `max_concurrent`;
  none by default). Which calls hold them:
  - `/v1` forwards (`passthrough::send`): the slot, and the timeout from the slot wait on
    (default 300 s for chat, 120 s for speech); a forwarded stream instead gets at most 30 s
    to its response headers (less when less of the timeout is left after the slot wait),
    then the timeout between two chunks, the first body chunk included.
  - Embeddings: the slot, and the timeout from the slot wait on (default `EMBED_TIMEOUT`,
    60 s).
  - The remote chat calls of a model worker (`within_endpoint_limits` in
    `ai_service/mod.rs`): a call that is not a stream gets the slot and the timeout from
    the slot wait on (default `CHAT_TIMEOUT`, 600 s, the provider clients' own
    `CHAT_REQUEST_TIMEOUT`). A streaming call gets the slot only: neither its wait for the
    slot nor the stream has `request_timeout`; the provider client's 600 s bounds the call
    once it holds the slot.
  - Not covered: Ollama `pull_model` (its own idle timeout, `PULL_IDLE_TIMEOUT`) and
    `delete_upstream_model` (30 s), health checks and discovery (`PROBE_TIMEOUT`). None
    takes a slot.

  A URL with a user name or password is refused.
- `detect.rs`: `ai.detectEndpoints`, loopback ports, classified by answer shape. Saves nothing.
- `config.rs`: the config file's `ai_endpoints` / `ai_models` / `ai_defaults`, applied
  before the service loads; idempotent by name (a new name takes over the endpoint the
  start-up migration made for the same protocol, URL and key), keys only from env vars.

`add_model` and `update_model` refuse (`InvalidModel`, a 400) a model whose endpoint has no
route for its type, before anything is written: transcription and speech need an
OpenAI-shaped server (not Ollama or Anthropic), embeddings a kind with an embeddings
client. So a refused model never gives its endpoint the capability.

A model's status (`ModelView::status`, `ai.modelStatus`) is computed from its endpoint's
last health check (`Endpoint::model_status`): `READY` when the endpoint lists it,
`MISSING` when it answers without it, `ENDPOINT_DOWN`, or `UNKNOWN` before a check.

## Transcription

`transcription.rs` holds the endpoint path and what streams and `/v1` share:

- A stream (`endpoint_transcriber` builds its client) runs a `RemoteStream` worker: each fed utterance becomes a 16 kHz mono PCM16 WAV and one
  `providers::audio::RemoteTranscribe` call. The worker takes utterances from a channel
  filled under the stream-map lock, so text comes out in the order fed. A feed answers
  after its transcript is published; an upstream failure is the feed's error (no URL in
  it). A streaming upstream can fail after it sent pieces: those pieces stay published and
  their words are billed; nothing more is published.
- Each call takes a slot of the endpoint's `max_concurrent` and ends at its
  `request_timeout_secs` (else 120 s). An endpoint whose capabilities leave out
  transcription is refused (`NotATranscriptionEndpoint`; `model_not_supported` on
  `/v1/audio/transcriptions`). A reload of the model closes its streams.
- A stream of OpenAI events or vLLM chunks must end with `transcript.text.done` or
  `[DONE]`; one that stops before is a bad answer. An `error` event's message stays out of
  the error and the log. Answers are read up to `MAX_BODY_BYTES`.
- The client cuts utterances; the executor's Silero gate (`transcription::SpeechGate`)
  drops one that is not speech, before the request, on a blocking thread. Without ONNX
  Runtime (loaded at run time on macOS) the gate is off and every utterance passes.
- `bill_transcription` is the only place transcription is billed (streams and
  `/v1/audio/transcriptions`); `publish_text` sends `transcription-text` to the owner's
  DID only. `transcription_is_billed_in_one_place` pins the first.
- The idle reaper (`start_stream_reaper`) holds the stream map weakly: dropping a clone of
  the service (every `global_instance()` call makes one) does not stop it. A stream is idle
  30 s after its last feed or answer, and never while an utterance is upstream
  (`TranscriptionSession::in_flight`, counted by an `InFlight` guard that a dropped feed
  or a dropped `queue_utterance` answer releases too).
- `transcription_e2e.rs`: real server, skipped unless `TRANSCRIPTION_E2E_BASE_URL` and
  `TRANSCRIPTION_E2E_MODEL` are set.

## `harness/`

Tool-calling loop used by `perspectives/interpretation` and `perspectives/agent_session` (not by the OpenAI shim):

- `provider.rs`: `ToolProvider`, `ToolSchema`, `CreditGate` traits. The clean seam;
  implemented by `mcp/tools/provider_impl.rs`.
- `mod.rs`: `run_with_tools` loop, `HarnessConfig`, step events.
- `propose.rs`, `flow_propose.rs`: propose-write / propose-flow-transition tools and
  buffers consumed by `interpretation/run.rs`.

**Known cycle**: `harness/*` imports `perspectives::{interpretation, auto_processor::events,
flow_context}` and `mcp::shacl`, while `interpretation/run.rs` imports `harness`. Don't add
edges; item 7 moves harness + interpretation + flows + auto_processor into `agentic/`.

## Rules

- Never call a model without a billing decision: use the existing `bill_*_if_authed` /
  `CreditGate` path.
- Model names are data: the onboarding recommendations live in one place
  (`core/src/ai/recommendedModels.ts`); don't scatter new ones in Rust.
- Streaming responses the executor answers itself (Anthropic, the text tool fallback) go
  through `prompt_messages_stream`. `/v1` forwards a model on an
  OpenAI-shaped endpoint itself (`api/openai_compat/passthrough.rs`).
