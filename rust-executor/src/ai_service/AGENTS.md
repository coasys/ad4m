# ai_service/ — agent guide

`AIService` (global singleton, tokio-mutexed: `AIService::global_instance().await`).
Split plan: spec item 9 (`mod.rs`), item 7 (`harness/` → `agentic/`).

## `mod.rs` sections (2.8k lines, one file)

| Lines (approx) | Content |
|---|---|
| top | DTOs: `PromptResult`, `EmbedResult`, `LLMTask*Request`, `LlmModel`, transcription params |
| ~330–510 | construction, model load/add/status, background tasks, global instance |
| ~510–1120 | LLM: candle device selection, `build_local_llama`, `build_remote_client` (OpenAI-compatible HTTP via `chat_gpt_lib_rs`), `spawn_llm_model` (per-model worker thread + channel) |
| ~1120–1670 | AI tasks CRUD, `prompt` / `prompt_messages` / `prompt_messages_stream`, billing hooks (`bill_prompt_if_authed`) |
| ~1670–1775 | embeddings |
| ~1775–2440 | whisper transcription sessions + VAD |
| rest | tests |

Persistence of models/tasks is in `db.rs` (`models`, `tasks`, `model_status`,
`default_models` tables). Billing goes through `crate::billing` with the caller's
`AgentContext`/email.

## `harness/`

Tool-calling loop used by `perspectives/interpretation` (not by the OpenAI shim):

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
