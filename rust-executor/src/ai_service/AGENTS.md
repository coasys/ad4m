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
  changes: that is how managed users see an endpoint go down or come back),
  `after_failed_request` (a refused connection or key re-checks the endpoint at once, a
  404 publishes the model `MISSING`; called by `/v1`, `ChatModel`, `embed_batch` and the
  transcription stream worker, so a client keeps its connection error typed as
  `http::UpstreamUnreachable`), Ollama
  `pull_model` / `delete_upstream_model` (`model-status`, at most 4/s), and the
  per-request limits: `request_timeout` (the endpoint's, else the route's default) and
  `request_slot` (a semaphore only when the operator set `max_concurrent`; none by default).
- `detect.rs`: `ai.detectEndpoints`, loopback ports, classified by answer shape. Saves nothing.
- `config.rs`: the config file's `ai_endpoints` / `ai_models` / `ai_defaults`, applied
  before the service loads; idempotent by name, keys only from env vars.

`add_model` and `update_model` refuse (`InvalidModel`, a 400) a model whose endpoint has no
route for its type, before anything is written: transcription and speech need an
OpenAI-shaped server (not Ollama or Anthropic), embeddings a kind with an embeddings
client. So a refused model never gives its endpoint the capability.

A model's status (`ModelView::status`, `ai.modelStatus`) is computed from its endpoint's
last health check (`Endpoint::model_status`): `READY` when the endpoint lists it,
`MISSING` when it answers without it, `ENDPOINT_DOWN`, or `UNKNOWN` before a check.

## `local/` (one-click local AI)

The executor still runs no model in-process: `local/` installs and supervises model
*servers* as child processes and registers them as **managed** endpoints
(`Endpoint::managed`, column `ai_endpoints.managed`).

| File | Does |
|---|---|
| `data.rs` | embeds `local/manifest.json` (runtime builds and model files: URL, size, sha256, memory) and `ai-wind-tunnel/{recommendations,hardware-classes}.json` (schema v1); `classify` (passes `ai-wind-tunnel/hardware-class-fixtures.json`, as the wind tunnel's TS `classify` does) and `chain` (its `fallbackClasses`: the family's smaller tiers, then the cpu family); `Recommendations::get` skips rows the executor cannot run (`completion`, speech on speaches). The wind tunnel owns these files |
| `hardware.rs` | `ai.localHardware`: sysinfo (RAM, CPU, disk), std CPU features, NVML and the Vulkan loader opened with `libloading` when present (no `wgpu`), amdgpu sysfs, macOS sysctl. GPU probes run once per process. `memory_budget` = min(GPU − 1 GiB, RAM − 4 GiB) |
| `plan.rs` | pure `plan()`: per service the recommendation of the class or the nearest smaller measured one (no evidence = unmeasured), memory sum vs budget with step-down, build per provider (recommended, else CUDA > Metal > Vulkan > ROCm > CPU; a recommended ROCm build becomes Vulkan where Vulkan runs: the ROCm build silently runs on the CPU without the system ROCm libraries), each model's `serverFlags` (measured, else `default_flags`: `--reasoning off` for chat), disk check |
| `install.rs` | the store (`downloads/`, `runtime/<id>/<version>-<build>/`, `models/`), `download` (`.part` + `Range` resume, sha256 and size before rename), `unpack` (tar.gz, zip; no path escapes), macOS quarantine strip |
| `supervisor.rs` | `Supervised` child: loopback port (`choose_port`, never the `detect.rs` ports), readiness (llama `/health`, whisper TCP), `RestartBudget` (3, reset after 5 healthy min), SIGTERM then SIGKILL, Linux `PR_SET_PDEATHSIG` from one long-lived spawner thread, `reap_stale` by pid + exe path |
| `mod.rs` | `install` / `start` / `stop` / `remove` / `status`, `register` (endpoints `This device (llama.cpp)` / `This device (whisper.cpp)`, models named by manifest id, defaults), `managed_health` (what `check_health` uses for a managed endpoint: whisper-server lists no models), `autostart` (`local_ai_autostart`), `stop_on_exit` (the shutdown handler) |

- Storage: the store is per user, shared by agents (`local_ai_dir` / `AD4M_LOCAL_AI_DIR`,
  else `dirs::data_local_dir()/ad4m/local-ai`); per-executor state (`selection.json`,
  `pids.json`, `llama-models.ini`, `<provider>.log`) is `<app_data_path>/local-ai/`. Tests set
  the directories explicitly; never let one write to `~/.local/share/ad4m`.
- llama-server runs in router mode: `--models-preset` names the chat and embedding models
  (`embeddings = true` on the latter, `load-on-startup`), each model's server flags become its preset keys (`-c 8192` is `c = 8192`), chat runs
  with `reasoning = off` (Qwen3.5 otherwise thinks until the context is full and `ai.prompt`
  answers ""), the key (`LLAMA_API_KEY`, in the environment so `ps` does not show it) is new
  per start, and after a start on a GPU build `spawn_gpu_check` reads the DRM fdinfo of its
  processes: under half the chat model's size in GPU memory is `onGpu: false` (Linux, AMD
  or Intel; NVIDIA is not checked),
  `--offline`. whisper-server: `--request-path /v1/audio --inference-path /transcriptions`,
  no key (loopback only).
- `ai.updateEndpoint` / `ai.removeEndpoint` refuse a managed endpoint (`refuse_managed`);
  `ai.detectEndpoints` skips the managed ports.
- RPCs (`api/ai_ws.rs`): `ai.localHardware` / `localPlan` / `localStatus` are
  `Executor(AI_READ)` reads; `ai.localInstall` (long) / `localStart` (long) / `localStop` /
  `localRemove` are `Guard::Operator`. Progress: the `ai-local-progress` event (operator).
- Model names live in `manifest.json` and the recommendations, as data (the rule below).
  Updating a pin: new URL, `sizeBytes` and `sha256` (GitHub `digest`, HF `lfs.oid`), and an
  HF URL with a commit, never `main` (a unit test checks).

## Transcription

`transcription.rs` holds the endpoint path and what streams and `/v1` share:

- A stream (`endpoint_transcriber` builds its client) runs a `RemoteStream` worker: each fed utterance becomes a 16 kHz mono PCM16 WAV and one
  `providers::audio::RemoteTranscribe` call. The worker takes utterances from a channel
  filled under the stream-map lock, so text comes out in the order fed. A feed answers
  after its transcript is published; an upstream failure is the feed's error (no URL in
  it) and publishes nothing.
- No executor-side voice-activity detection: the client cuts utterances.
- `bill_transcription` is the only place transcription is billed (streams and
  `/v1/audio/transcriptions`); `publish_text` sends `transcription-text` to the owner's
  DID only. `transcription_is_billed_in_one_place` pins the first.
- The idle reaper (`start_stream_reaper`) holds the stream map weakly: dropping a clone of
  the service (every `global_instance()` call makes one) does not stop it. A stream is idle
  30 s after its last feed or answer, and never while an utterance is upstream
  (`TranscriptionSession::in_flight`, counted by an `InFlight` guard that a dropped feed
  releases too).
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
- Model names are data: the onboarding recommendations for model servers live in
  `core/src/ai/recommendedModels.ts`, the local AI models in `local/manifest.json` and
  `ai-wind-tunnel/recommendations.json`; don't scatter new ones in Rust code.
- Streaming responses the executor answers itself (Anthropic, the text tool fallback) go
  through `prompt_messages_stream`. `/v1` forwards a model on an
  OpenAI-shaped endpoint itself (`api/openai_compat/passthrough.rs`).
