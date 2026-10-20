# AI service

The **AI service** (`AIService`) runs models for the executor: LLMs (large language models), embedding models and Whisper transcription. A model is **local** or **remote**. A local model's weights run in the executor with candle / kalosm, on CPU or GPU. A remote model is an HTTP endpoint behind the `RemoteChat` trait: OpenAI-shaped, Anthropic or Ollama. An **AI task** is a stored system prompt with examples, bound to one model.

**Why:** Apps and [interpretation](../../../perspectives/social-dna/interpretation/README.md) need inference that can stay on the operator's machine, behind one API for every backend. One worker thread per model keeps a slow local model from blocking the others. Local inference needs native code and GPU access, which a language isolate cannot have.

**Parent:** [Executor services](README.md)

<details><summary>How it works</summary>

1. Models and tasks live in SQLite (`models`, `tasks` with an owner column, `default_models`, `model_status`); a task's prompt fields are sealed under its owner's key. The operator adds models (`ai.addModel`). `ai.setDefaultModel` picks one model per `ModelType` (`LLM`, `EMBEDDING`, `TRANSCRIPTION`).
2. `spawn_llm_model` starts one worker thread per model with a request channel. `LlmModel` is `Local(Llama)` or `Remote(Box<dyn RemoteChat>, model)`.
3. Three kinds of caller reach the models:

   | Caller | Entry |
   |---|---|
   | WebSocket RPC | `ai.prompt`, `ai.embed`, `ai.transcriptionOpen` |
   | OpenAI-compatible HTTP API under `/v1` | `/chat/completions`, `/completions`, `/embeddings`, `/audio/*`, `/realtime`, `/models` |
   | the **harness** (`harness/`), the tool-calling loop that interpretation uses | `run_with_tools` ([Interpretation](../../../perspectives/social-dna/interpretation/README.md)) |

4. Streaming goes through `prompt_messages_stream`.
5. Each AI call resolves the session's agent once and bills that agent through `bill_ai_operation` or `CreditGate`, the transcription rate included. Under free access the call is priced and logged at amount 0 ([Billing and hosting](billing-and-hosting.md)).
6. For a tenant, each admitted call (a prompt or an embedding over RPC, a `/v1` call, an opened transcription stream) adds one to `tenant_ai_runs` for the day; the tenant view reads it, and no limit does ([Tenants](../managed-agents/tenants.md)).
7. A provider API key is a `Secret`: results show `hasApiKey`, never the key. At rest it sits in the encrypted SQLite file ([Encrypted stores](../data/encrypted-stores.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/ai_service/AGENTS.md](../../../../rust-executor/src/ai_service/AGENTS.md): section map of `mod.rs`.
- [rust-executor/src/ai_service/mod.rs](../../../../rust-executor/src/ai_service/mod.rs): `AIService`, `LlmModel`, `spawn_llm_model`, `build_remote_client`, tasks, prompt, embeddings, whisper.
- [rust-executor/src/ai_service/providers/](../../../../rust-executor/src/ai_service/providers): `RemoteChat`, `openai.rs`, `anthropic.rs`, `ollama.rs`, `http.rs` (`credentialed_http`).
- [rust-executor/src/ai_service/harness/](../../../../rust-executor/src/ai_service/harness): `run_with_tools`, `ToolProvider`, `CreditGate`. `harness/` imports `perspectives::interpretation` and `mcp::shacl` ([propose.rs](../../../../rust-executor/src/ai_service/harness/propose.rs)), and `interpretation` imports `harness` ([run.rs](../../../../rust-executor/src/perspectives/interpretation/run.rs)).
- [rust-executor/src/api/ai_ws.rs](../../../../rust-executor/src/api/ai_ws.rs), [rust-executor/src/api/openai_compat/](../../../../rust-executor/src/api/openai_compat): access paths.
- [rust-executor/src/types/core.rs](../../../../rust-executor/src/types/core.rs): `ModelType`, `ModelApiType`.
- [rust-executor/src/hosting/meter.rs](../../../../rust-executor/src/hosting/meter.rs) (new): the per-tenant AI-run count at the same admission points as billing.
- [docs-src/pages/developer-guides/ai.mdx](../../../../docs-src/pages/developer-guides/ai.mdx): user docs.

</details>
