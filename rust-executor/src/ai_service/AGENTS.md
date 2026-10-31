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
| ~1670–1775 | embeddings |
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
  health loop (`ai-endpoint-status`, to the main agent's sessions only), Ollama
  `pull_model` / `delete_upstream_model` (`model-status`, at most 4/s).
- `detect.rs`: `ai.detectEndpoints`, loopback ports, classified by answer shape. Saves nothing.
- `config.rs`: the config file's `ai_endpoints` / `ai_models` / `ai_defaults`, applied
  before the service loads; idempotent by name, keys only from env vars.

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
  it) and publishes nothing. A model without `api` stays on local Whisper (until P4).
- No executor-side voice-activity detection: the client cuts utterances.
- `bill_transcription` is the only place transcription is billed (streams and
  `/v1/audio/transcriptions`); `publish_text` sends `transcription-text` to the owner's
  DID only. `transcription_is_billed_in_one_place` pins the first.
- The idle reaper (`start_stream_reaper`) holds the stream map weakly: dropping a clone of
  the service (every `global_instance()` call makes one) does not stop it.
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
- Streaming responses go through `prompt_messages_stream`; the OpenAI shim (`api/openai_compat`)
  and WS `ai.*` handlers both sit on it.
