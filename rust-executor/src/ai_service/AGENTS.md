# ai_service/ — agent guide

`AIService` (global singleton, tokio-mutexed: `AIService::global_instance().await`).
Split plan: spec item 9 (`mod.rs`), item 7 (`harness/` → `agentic/`).

## `mod.rs` sections (2.8k lines, one file)

| Lines (approx) | Content |
|---|---|
| top | DTOs: `PromptResult`, `EmbedResult`, `LLMTask*Request`, `LlmModel`, `TranscriptionSession` / `StreamBackend` |
| ~330–510 | construction, model load/add/status, background tasks, global instance |
| ~510–1120 | LLM: candle device selection, `build_local_llama`, `build_remote_client` (a `providers::RemoteChat` per protocol), `spawn_llm_model` (per-model worker thread + channel) |
| ~1120–1670 | AI tasks CRUD, `prompt` / `prompt_messages` / `prompt_messages_stream`, billing hooks (`bill_prompt_if_authed`) |
| ~1670–1775 | embeddings: one `Embedder` per model id (the endpoint's `providers::embed` client, or a Bert thread for a model without an endpoint); `embed_batch` resolves an id or a name and sends one request per batch. `default_embedding.rs`: the Bert (`bert` / `bert-id`, not a row) of an executor without models, loaded by the first embed that resolves to it |
| ~1775–2440 | transcription streams: open (endpoint or local Whisper), feed, close, ownership |
| rest | tests |

Persistence of models/tasks is in `db.rs` (`models`, `tasks`, `model_status`,
`default_models` tables), endpoints in `db/endpoints.rs` (`ai_endpoints`). Tasks have an owner (`tasks.agent_did`, the managed agent's DID;
`NULL` for a task of the main agent; see `src/db/AGENTS.md`): `add_task` takes it, and the
WS guards enforce it. Billing goes through `crate::billing` with the caller's agent DID.

## `endpoints/`

The registry of upstream model servers (an endpoint owns URL, key, capabilities, health):

- `mod.rs`: `ai.addEndpoint` / `updateEndpoint` / `removeEndpoint` (probe once before a
  save; an update rewrites and reloads the models on it), `check_health` and the 60 s
  health loop (`ai-endpoint-status`, to the main agent's sessions only; `check_health`
  probes an endpoint at most once per `PROBE_TIMEOUT`, a pull or delete probes at once; a check adds the
  capabilities of the models listed, removes none, and stores nothing when the URL moved
  meanwhile; an `OPEN_AI_AUDIO` server whose listing is a 404 serves its registered
  models), Ollama `pull_model` / `delete_upstream_model` (`model-status`, at most 4/s, a
  failure's detail names no URL), and the per-request limits: `request_timeout` (the
  endpoint's, else the route's default) and `request_slot` (a semaphore only when the
  operator set `max_concurrent`; none by default). Which calls hold them:
  - `/v1` forwards (`passthrough::send`): the slot, and the timeout from the slot wait on
    (default 300 s for chat, 120 s for speech); a forwarded stream instead gets at most 30 s
    to its first byte (less when less of the timeout is left after the slot wait) and the
    timeout between two chunks.
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

A remote model still carries its own `api` (URL, key, protocol) and gets an `endpoint_id`
from `Ad4mDb::link_models_to_endpoints` (startup and every model write). Never set a
model's connection fields without going through the endpoint when it has one.

## Transcription

`transcription.rs` holds the endpoint path and what both paths share:

- A stream on a model with an endpoint (`endpoint_transcriber`) runs a
  `RemoteStream` worker: each fed utterance becomes a 16 kHz mono PCM16 WAV and one
  `providers::audio::RemoteTranscribe` call. The worker takes utterances from a channel
  filled under the stream-map lock, so text comes out in the order fed. A feed answers
  after its transcript is published; an upstream failure is the feed's error (no URL in
  it). A streaming upstream can fail after it sent pieces: those pieces stay published and
  their words are billed; nothing more is published. A model without `api` stays on local
  Whisper (until P4).
- Each call takes a slot of the endpoint's `max_concurrent` and ends at its
  `request_timeout_secs` (else 120 s). An endpoint whose capabilities leave out
  transcription is refused (`NotATranscriptionEndpoint`; `model_not_supported` on
  `/v1/audio/transcriptions`). A reload of the model closes its streams.
- A stream of OpenAI events or vLLM chunks must end with `transcript.text.done` or
  `[DONE]`; one that stops before is a bad answer. An `error` event's message stays out of
  the error and the log. Answers are read up to `MAX_BODY_BYTES`.
- The client cuts utterances; the executor's Silero gate (`transcription::SpeechGate`)
  drops one that is not speech, before the request and before local Whisper. Without
  ONNX Runtime (loaded at run time on macOS) the gate is off and every utterance passes.
- `bill_transcription` is the only place transcription is billed (streams and
  `/v1/audio/transcriptions`); `publish_text` sends `transcription-text` to the owner's
  DID only. `transcription_is_billed_in_one_place` pins the first.
- The idle reaper (`start_stream_reaper`) holds the stream map weakly: dropping a clone of
  the service (every `global_instance()` call makes one) does not stop it. A stream with
  an utterance still upstream (`Waiting`) is not idle; idle counts from its last answer.
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
- Model name strings (whisper table, default model ids) are data; don't scatter new ones.
- Streaming responses of models the executor runs go through `prompt_messages_stream`; WS
  `ai.*` handlers and `/v1` for those models sit on it. `/v1` forwards a model on an
  OpenAI-shaped endpoint itself (`api/openai_compat/passthrough.rs`).
