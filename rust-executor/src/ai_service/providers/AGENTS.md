# ai_service/providers/ — agent guide

Remote LLM endpoints behind one trait. Local (candle/kalosm) models do not come
through here — `LlmModel::Local` is a different thing entirely.

## Files

| File | Content |
|---|---|
| `mod.rs` | `RemoteChat` trait + the neutral `ChatRole`/`ChatTurn`/`ChatRequest`/`ChatReply` DTOs. Declarations only. |
| `anthropic.rs` | Anthropic Messages API. Native tool calling, prompt caching, SSE streaming. |
| `ollama.rs` | Ollama native `/api/chat`. Native tool calling, `num_ctx`, ndjson streaming. |
| `openai.rs` | OpenAI-shaped HTTP (`chat_gpt_lib_rs`): OpenAI, Groq, OpenRouter, Google compat, vLLM. |

## Entry points

- `OpenAiChat::new(api_key, base_url)` / `AnthropicChat::new(..)` /
  `OllamaChat::new(..)` — built once per model by
  `ai_service::AIService::build_remote_client`, then owned by that model's
  worker thread for its lifetime.
- `RemoteChat::chat` / `chat_stream` — called from the `LLMTaskRequest::Prompt`
  and `PromptStream` arms of that thread's loop. `chat_stream` has a default
  that answers in one chunk; override it only where the upstream really streams.
- `list_models(api_type, api_key, base_url)` — free function, not a trait
  method, because discovery happens *before* a model is configured: there is no
  instance and no worker thread. Reached over WS as `ai.discoverModels`.

## Invariants

- One instance per configured model, reused across prompts: hold a connection
  pool if you like, never per-request state.
- `ChatRole` has no tool role on purpose. Tool calls and tool results reach us
  already rendered into text by `api::openai_compat` (`flatten_message`,
  `harness_bridge`), so a provider that cannot express tools structurally never
  has to invent a representation. A provider that can, reconstructs them.
- Billing is the caller's: these types carry no token counts and no
  `AgentContext`. `ai_service` estimates and bills around the call
  (`bill_prompt_if_authed`). Do not add a billing hook here.

## Do not

- Do not add an `LlmModel` variant per provider. That enum answers "local
  weights or remote endpoint"; which remote protocol is this module's question.
- Do not reach for `AIService::global_instance()` from a provider — everything
  a call needs is on `ChatRequest`.
