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

## `local/` (one-click local AI)

The executor still runs no model in-process: `local/` installs and supervises model
*servers* as child processes and registers them as **managed** endpoints
(`Endpoint::managed`, column `ai_endpoints.managed`).

| File | Does |
|---|---|
| `data.rs` | embeds `local/manifest.json` (runtime builds and model files: URL, size, sha256, memory) and `ai-wind-tunnel/{recommendations,hardware-classes}.json` (schema v1); `classify` (passes `ai-wind-tunnel/hardware-class-fixtures.json`, as the wind tunnel's TS `classify` does) and `chain` (its `fallbackClasses`: the family's smaller tiers, then the cpu family); `Recommendations::get` skips rows the executor cannot run (`completion`, speech on speaches); the manifest keeps only runtimes and models of a `Provider` (llama-server, nemo-speech): whisper-server and sherpa-onnx are the wind tunnel's baselines. The wind tunnel owns these files |
| `hardware.rs` | `ai.localHardware`: sysinfo (RAM, CPU, disk), std CPU features, NVML and the Vulkan loader opened with `libloading` when present (no `wgpu`), amdgpu sysfs, macOS sysctl. GPU probes run once per process. `memory_budget` = min(GPU − 1 GiB, RAM − 4 GiB); the GPU is `primary_gpu`: the largest discrete GPU of the vendor the preferred GPU backend runs on (CUDA: NVIDIA, ROCm: AMD, Metal: Apple; Vulkan: any), so with two vendors the budget is the GPU's the build uses |
| `plan.rs` | pure `plan()`: per service the recommendation of the class or the nearest smaller measured one (no evidence = unmeasured), else the manifest's smallest model of the service whose runtime runs here (`basis: default`, also when the measured provider has no build here), memory sum vs budget with step-down, build per provider (recommended, else CUDA > Metal > Vulkan > ROCm > CPU; a recommended ROCm build becomes Vulkan where Vulkan runs: the ROCm build silently runs on the CPU without the system ROCm libraries), each model's `serverFlags` (measured, else `default_flags`: `--reasoning off` for chat), disk check |
| `install.rs` | the store (`downloads/`, `runtime/<id>/<version>-<build>/`, `models/`), `download` (`.part` + `Range` resume, sha256 and size before rename; a redirect only to https), `unpack` (tar.gz, zip; no path escapes) into `<version>-<build>.partial` (the whole name: `with_extension` would cut at the version's last dot), macOS quarantine strip |
| `supervisor.rs` | `Supervised` child (owns it: dropping it stops the child, so a start cancelled before `RUNNING` has it leaves nothing running): loopback port (`choose_port`, never the `detect.rs` ports), readiness (llama `/health`, nemo-speech `/ready`), `RestartBudget` (3, reset after 5 healthy min), SIGTERM then SIGKILL, Linux `PR_SET_PDEATHSIG` from one long-lived spawner thread, `reap_stale` by pid + exe path |
| `mod.rs` | `LocalPaths` / `store_root`, the API types, `Selection` (and the lock on its file), the shared statics (`RUNNING`, `MANAGED_PORTS`, `GPU_CHECKS`, `INSTALLING`), `hardware` / `current_plan`; re-exports the rest so callers use `local::…` |
| `setup.rs` | `install` (one at a time, progress events; on macOS with transcription also ONNX Runtime), `install_into` (`local-ai install`, `INCLUDE_MODELS`), `remove`. The only paths that download |
| `autostart.rs` | `autostart` (`local_ai_autostart`: starts what is installed, never installs) |
| `runtimes.rs` | `start` / `stop` / `status`, child specs and flags, pid files, `stop_on_exit`, `stop_children_blocking` |
| `health.rs` | `managed_health`, the health and pid followers, the GPU check |
| `registration.rs` | `register` / `unregister`, managed endpoint names |

| (the six above, in detail) | `install` / `start` / `stop` / `remove` / `status`, `register` (endpoints `This device (llama.cpp)` / `This device (NeMo Speech)`, models named by manifest id; a nemo-speech model's `remote_model` is the id its `/v1/models` lists, `parakeet-tdt-0.6b-v3.oss-align.q8_0`, since it refuses any other; the chat model `native_tools: true`; defaults), `managed_health` (what `check_health` uses for a managed endpoint: the supervisor's phase, then llama-server's liveness or nemo-speech's listing), `autostart` (`local_ai_autostart`), `stop_on_exit` (run by the executor's shutdown, `clean_up_before_exit` in `lib.rs`, and by `runtime.quit`: SIGTERM to all at once, SIGKILL after 2 s, inside the 4 s a signal leaves the shutdown), `stop_children_blocking` (the same from a thread that cannot await: the launcher's Quit, which exits without the executor's shutdown) |

- Storage: the store is per user, shared by agents (`local_ai_dir` / `AD4M_LOCAL_AI_DIR`,
  else `dirs::data_local_dir()/ad4m/local-ai`; never the temporary directory: without a
  per-user data directory `store_root` is an error that asks for `local_ai_dir`), made
  absolute (a runtime starts in its own directory); per-executor state (`selection.json`,
  `pids.json`, `llama-models.ini`, `<provider>.log`) is `<app_data_path>/local-ai/`. Tests set
  the directories explicitly; never let one write to `~/.local/share/ad4m`. Docker: an image
  without local AI sets `AD4M_LOCAL_AI_DIR=/data/local-ai/store` (`docker/ai-env.sh`
  `seed_local_ai`), so the store stays in the volume beside the selection. `start_at` checks
  that the selection's binaries and models exist; when one is gone it starts nothing and
  refuses "run ai.localInstall again".
- Lifecycle: `LIFECYCLE` (one `tokio::sync::Mutex`) is held by `start_at`, `stop_all`
  (`ai.localStop`, the install paths) and all of `remove`, so they run one at a time: a
  second start waits and finds the runtime in `RUNNING`. `stop_on_exit` does not take it
  (an exit must not wait for a start); every `ManagedPort` holds the supervisor's
  `StopHandle` (weak: a dropped `Supervised` still stops its child), so
  `stop_children_blocking` stops a runtime that is still starting too. `start_at` starts
  again a runtime whose entry is `Failed` or `Stopped`, registers again a running one whose
  endpoint or model rows are missing (`registration_missing`: a registration that failed
  or was cancelled; `Running` keeps the runtime's key for it), and removes the managed
  endpoints whose runtime the selection does not have. `spawn_pid_follower` starts at the
  spawn and writes `pids.json` (the runtimes in `RUNNING` and the one loading) when the
  runtime gets a pid and when a restart gives it a new one; the start writes it again
  after each `RUNNING.insert`. `register` first removes the model rows on its endpoint
  that the selection does not name (and so the defaults that named them), then registers
  the selection's models, so a type whose default went gets the new model as its default
  also without `make_default`; it asks a running nemo-speech for its listing up to 5
  times (1 s apart) before it falls back to the manifest id. `autostart` (`autostart.rs`)
  only starts what an install put in place and never downloads; it waits for `AIService::wait_loaded` (the start's load of the stored models) first: a
  load after the registration would put back the clients of the old endpoint settings.
  The health follower probes the endpoint at its start and at each phase change
  (`probe_health`, not the debounced `check_health`); the GPU check of a runtime that is
  gone (its status sender closed) publishes nothing. Download progress is published at
  once (`publish_progress_now`), in order. `install::download` waits at most
  `IDLE_TIMEOUT` for the response headers as for each chunk. The plan of an install with
  `services` covers only those (`plan_services`), so it fits by them; when a measured
  model exceeds the budget, the manifest's smaller default is the last step down.
- llama-server runs in router mode: `--models-preset` names the chat and embedding models
  (`embeddings = true` on the latter, `load-on-startup`), each model's server flags become its preset keys (`-c 8192` is `c = 8192`), chat runs
  with `reasoning = off` (Qwen3.5 otherwise thinks until the context is full and `ai.prompt`
  answers ""), the key (`LLAMA_API_KEY`, in the environment so `ps` does not show it) is new
  per start, and after a start on a GPU build `spawn_gpu_check` reads the DRM fdinfo of its
  processes: under half the chat model's size in GPU memory is `onGpu: false` (Linux, AMD
  or Intel; NVIDIA is not checked; `drm-resident-*` counts, `drm-memory-*` only where no
  resident key is printed: amdgpu from 6.13 prints both for the same memory),
  `--offline`, `jinja = true` on the chat model (tool calls through its chat template).
  nemo-speech: `serve --config <state>/nemo-speech.yaml --asr-model <gguf> --host 127.0.0.1
  --port P --device <the build's backend> --no-ui`; the key is `http.api-key` in that YAML
  (mode 0600, so `ps` does not show it); a measured `--device` is replaced by the build's,
  so a CPU fallback never asks for Vulkan.
- `ai.updateEndpoint` / `ai.removeEndpoint` refuse a managed endpoint (`refuse_managed`);
  `ai.detectEndpoints` skips the managed ports. A config-file `ai_endpoints` entry that
  names a managed endpoint is skipped (an update would clear `managed`). A backup keeps
  `managed`; the import restores it only for local AI's endpoint names on loopback.
- `hardware.rs` adds CUDA only when NVML lists a device (`cuda_usable`); the disk reading
  takes the store path absolute, so a relative store that does not exist yet has one.
- RPCs (`api/ai_ws.rs`): `ai.localHardware` / `localPlan` / `localStatus` are
  `Executor(AI_READ)` reads; `ai.localInstall` (long) / `localStart` (long) / `localStop` /
  `localRemove` are `Guard::Operator`. Progress: the `ai-local-progress` event (operator).
- Tests of `local/*.rs` that touch `RUNNING`, `MANAGED_PORTS`, `GPU_CHECKS`, the install
  flag or the global service take `TEST_LOCK` first: a plain `cargo test` runs them in
  parallel.
- **Nothing downloads unless the user or operator asks.** Local AI installs only through
  `ai.localInstall` (an app's call: the launcher's setup screen), `ad4m-executor local-ai
  install` and the Docker image's `INCLUDE_MODELS` (`install_into`). Never add a download
  to a request path or to executor start. `AIService::default_model` (`ai.embed` or
  `ai.transcriptionOpen` on `"default"` with no model of the type) answers `LocalAiRefused`:
  the operator gets "No default <TYPE> model on this executor: set up local AI
  (ai.localInstall) or register a model on an endpoint"; any other caller, and the
  executor's own embeds (`None`), "… ask the operator to set up local AI". `/v1` has no
  default (404). On macOS the install that sets up transcription fetches ONNX Runtime for
  the speech gate (`onnx_runtime::fetch_pinned`; `plan_for` counts its size); a stream
  never fetches it and runs ungated without it.
- Model names live in `manifest.json` and the recommendations, as data (the rule below).
  Updating a pin: new URL, `sizeBytes` and `sha256` (GitHub `digest`, HF `lfs.oid`), and an
  HF URL with a commit, never `main` (a unit test checks).

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
- Model names are data: the onboarding recommendations for model servers live in
  `core/src/ai/recommendedModels.ts`, the local AI models in `local/manifest.json` and
  `ai-wind-tunnel/recommendations.json`; don't scatter new ones in Rust code.
- Streaming responses the executor answers itself (Anthropic, the text tool fallback) go
  through `prompt_messages_stream`. `/v1` forwards a model on an
  OpenAI-shaped endpoint itself (`api/openai_compat/passthrough.rs`).
