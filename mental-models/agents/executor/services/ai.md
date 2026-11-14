# AI service

The **AI service** (`AIService`) sends LLM, embedding, transcription and speech requests to models on **endpoints**. The executor runs no model itself. A model is a reference: one upstream model on one endpoint (`Model { endpoint_id, remote_model, model_type }`). An endpoint is a model server, such as Ollama, llama-server, vLLM, OpenAI, Anthropic or a speech server. **One-click local AI** installs model servers on the device, supervises them and registers them as managed endpoints. An **AI task** is a stored system prompt with examples, bound to one LLM. An **AI session** is a conversation with the executor's AI, stored in the executor's database.

**Why:** Model servers (llama.cpp, vLLM, Ollama) already run models well on each kind of hardware. The executor only routes, bills and checks access, so one code path serves a local model and a hosted model. Local AI keeps inference on the operator's device without model code in the executor. The executor downloads nothing unless the operator asks (`ai.localInstall`, `ad4m-executor local-ai install`).

**Parent:** [Executor services](README.md)

<details><summary>How it works</summary>

```
 RPC ai.*  ·  /v1/*  ·  harness  ·  POST /api/v1/ai/transcription/feed
        │
        ▼
 AIService ── model id or name ──► Model { endpoint_id, remote_model, model_type }
   llms: ChatModel per LLM                │
   embedders: Embedder per embedding      ▼
        └──────────── HTTP ────────► Endpoint (URL, key, kind, health, max_concurrent)
                                          ▲
                local/: install, supervise, register (managed = true)
```

1. The operator registers endpoints (`ai.addEndpoint`, `ai.detectEndpoints` for loopback ports, the config file's `ai_endpoints`) and models on them (`ai.discoverModels`, `ai.addModel`, Ollama `ai.pullModel`). Endpoints live in `ai_endpoints`. Models, tasks and defaults live in `models`, `tasks` and `default_models`. `ai.setDefaultModel` picks one model per `ModelType` (`LLM`, `EMBEDDING`, `TRANSCRIPTION`, `SPEECH`).
2. At start, `AIService::load` builds one client per stored model: a `ChatModel` (`providers::chat_client`) per LLM and an `Embedder` per embedding model. A client takes concurrent requests. There is no thread per model. Transcription and speech build a client for each stream or request.
3. Each call takes a slot of its endpoint's `max_concurrent` (no limit by default). The limit holds for RPC, `/v1` and the harness. A call that is not a chat stream also ends at the endpoint's `request_timeout`, else at the default of its route.
4. The health loop checks each endpoint every 60 s. A model's status (`READY`, `MISSING`, `ENDPOINT_DOWN`, `UNKNOWN`) comes from the last check of its endpoint. A change sends `ai-endpoint-status` (main agent) and `model-status` (all agents).
5. Five kinds of caller reach the models:

   | Caller | Entry |
   |---|---|
   | WebSocket RPC | `ai.prompt` (through an AI task), `ai.embed`, `ai.transcriptionOpen` / `ai.transcriptionClose`. Audio goes in through `POST /api/v1/ai/transcription/feed`. Text comes back as `transcription-text`. |
   | `/v1` aggregator (OpenAI-shaped) | `/models`, `/chat/completions`, `/completions`, `/embeddings`, `/audio/transcriptions`, `/audio/speech`, `/realtime`. A chat for a model on an OpenAI-shaped endpoint goes to that endpoint as the client wrote it (`passthrough.rs`). The executor answers the other chats itself. |
   | the [harness](../../../perspectives/social-dna/interpretation/README.md), the tool-calling loop | `run_with_tools`, for interpretation, agent sessions ([MCP server](../../interfaces/mcp-server.md)) and AI sessions |
   | AI sessions | `ai.sessionCreate`, `ai.sessionSend`, `ai.sessionCancel`, `ai.sessionMessages`. `ai.sessionSend` starts a turn in a detached task and returns at once. The turn writes its answer and each tool call to `ai_messages` and sends `ai-session-changed`. One turn runs per session, at most 8 per caller, for at most 15 minutes. At start, the executor marks each `running` row `interrupted`. |
   | the executor | interpretation and its embeddings for duplicates (`interpretation/dedup.rs`), started by a caller or by the [auto-processor](../../../perspectives/social-dna/interpretation/auto-processor.md) |

6. One-click local AI (`ai_service/local/`):
   1. `ai.localHardware` reads RAM, disk and GPUs. `ai.localPlan` picks models and builds from the wind tunnel's measured recommendations (`ai-wind-tunnel/recommendations.json`), else from the smallest model in `local/manifest.json`, within the memory budget.
   2. `ai.localInstall` downloads the runtime builds (llama-server, nemo-speech) and the model files, and checks each sha256. It sends `ai-local-progress`.
   3. The supervisor starts each runtime as a child process on a loopback port, restarts it on a crash, and stops it when the executor stops.
   4. `registration.rs` registers each runtime as a managed endpoint (`This device (llama.cpp)`, `This device (NeMo Speech)`) with its models. A model becomes the default only of a type that has no default.

   At start, `local_ai_autostart` starts what an install put in place and never downloads.

Who may do what:

| What | Who | Guard in `ai_ws.rs` |
|---|---|---|
| endpoints, models, defaults | the operator, or a grant on the main agent | `Guard::Executor` |
| local install, start, stop, remove | the operator | `Guard::Operator` |
| a stored task or AI session, and a prompt through a task | the agent that owns it (`tasks.agent_did`, `ai_sessions.owner_did`, `NULL` for the main agent) | `Guard::Owns` |
| embed, transcription | any caller with the capability | `Guard::Cap` |

An AI session's tools act only within the session's `capabilities` intersected with the caller's own. Every AI call goes through billing with the caller's agent DID ([Billing and hosting](billing-and-hosting.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/ai_service/AGENTS.md](../../../../rust-executor/src/ai_service/AGENTS.md): section map, limits and rules.
- [rust-executor/src/ai_service/mod.rs](../../../../rust-executor/src/ai_service/mod.rs): `AIService`, `ChatModel`, `Embedder`, `load`, tasks, `prompt`, `prompt_messages_stream`, `embed_batch`, `bill_prompt_if_authed`.
- [rust-executor/src/ai_service/endpoints/](../../../../rust-executor/src/ai_service/endpoints): the endpoint registry, `check_health`, `spawn_health_loop`, `request_slot`, `detect.rs`, `config.rs`.
- [rust-executor/src/ai_service/providers/](../../../../rust-executor/src/ai_service/providers): `RemoteChat`, `RemoteEmbed`, `RemoteTranscribe`, `Discover`, and the clients `openai.rs`, `anthropic.rs`, `ollama.rs`, `http.rs` (`credentialed_http`).
- [rust-executor/src/ai_service/transcription.rs](../../../../rust-executor/src/ai_service/transcription.rs): transcription streams, `SpeechGate`, `bill_transcription`.
- [rust-executor/src/ai_service/local/](../../../../rust-executor/src/ai_service/local): `hardware.rs`, `plan.rs`, `install.rs`, `setup.rs`, `supervisor.rs`, `runtimes.rs`, `registration.rs`, `autostart.rs`, `manifest.json`.
- [rust-executor/src/ai_service/harness/](../../../../rust-executor/src/ai_service/harness): `run_with_tools`, `ToolProvider`, `CreditGate`.
- [rust-executor/src/ai_sessions/](../../../../rust-executor/src/ai_sessions): `service.rs` (turn registry), `run.rs` (one turn), and [db/ai_sessions.rs](../../../../rust-executor/src/db/ai_sessions.rs) (`ai_sessions`, `ai_messages`).
- [rust-executor/src/api/ai_ws.rs](../../../../rust-executor/src/api/ai_ws.rs), [ai_sessions_ws.rs](../../../../rust-executor/src/api/ai_sessions_ws.rs), [openai_compat/](../../../../rust-executor/src/api/openai_compat): access paths.
- [rust-executor/src/db/endpoints.rs](../../../../rust-executor/src/db/endpoints.rs), [model_migration.rs](../../../../rust-executor/src/db/model_migration.rs): endpoint rows, and the one-time move from the old model rows.
- [rust-executor/src/types/core.rs](../../../../rust-executor/src/types/core.rs): `Model`, `ModelType`, `EndpointKind`, `ModelApiType`.
- [core/src/ai/AIClient.ts](../../../../core/src/ai/AIClient.ts): the SDK side.
- [ai-wind-tunnel/](../../../../ai-wind-tunnel): measures each provider and service through a real executor, and writes the recommendations that local AI reads.

</details>
