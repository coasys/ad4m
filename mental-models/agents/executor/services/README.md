# Executor services

An **executor service** is a Rust module inside the executor that is not a language. Each one has a global instance (or free functions) and its own RPC namespace, and every method declares a guard. Some also expose ops to language isolates.

**Why:** These services need native code (GPU inference, WebRTC media, the Holochain conductor), executor-wide state (credits, trust lists), or a key that only the executor holds (the index client signs with the executor's device key). A sandboxed language isolate cannot have any of these. The cost is a larger core.

**Parent:** [Executor](../README.md)

## Parts
- [AI service](ai.md): local and remote LLMs, embeddings, transcription, AI tasks, the OpenAI-compatible `/v1` API.
- [SFU](sfu.md): the WebRTC media server in the executor for neighbourhood calls, and cascades of SFUs.
- [Holochain service](holochain.md): the embedded conductor that Holochain languages and the Unyt DNA run on; optional for a host.
- [Billing and hosting](billing-and-hosting.md): the charge mode, host rates, the compute log, credits and earnings through the Unyt DNA.
- [Hosting-index client](hosting-index.md): the executor's sign-in to the hosting index by device key, its listing, and Unyt membrane proofs.

Email and the **runtime service** (bootstrap seed, trust and friend lists, notifications, `runtime.*` calls) have no page of their own; see the table below. The identity module and the keyring are not services: they are on [Identity](../../identity/README.md) and [Executor data](../data/README.md).

<details><summary>How it works</summary>

| Service | Global | RPC handlers | Reached by languages |
|---|---|---|---|
| AI | `AIService::global_instance()` | `api/ai_ws.rs` (`ai.*`), `api/openai_compat/` (`/v1`) | no |
| SFU | `get_sfu_service()` | `api/sfu_ws.rs` (`sfu.*`) | no |
| Holochain | `get_holochain_service()` and two softer accessors | none of its own | yes, 14 ops |
| Billing / Unyt | free functions in `billing.rs`, `unyt_service.rs` | `api/hosting_ws.rs` (`hosting.*`), `runtime.unyt*`, `runtime.hostRates`, `runtime.setHostRates`, `runtime.setFreeHostingEnabled` | not directly; the Unyt app through the Holochain ops |
| Hosting-index client | `hosting/index_client.rs` | `index.*` | no |
| Email | `EmailService` | through password sign-up and login | no |
| Runtime | `RuntimeService` (seed, trusted agents, known link languages, friends, outbox) | `api/runtime_ws.rs` (`runtime.*`: info, quit, restart, config, friends, notifications, export and import, link-language templates) | yes: `friends`, `add_message_outbox`, `get_trusted_agents` ops |

Direct messages are not implemented: `runtime.sendFriendMessage` answers 501, and `runtime.inbox` answers an empty list ([runtime_ws.rs](../../../../rust-executor/src/api/runtime_ws.rs)). The device inbox of [Devices](../../identity/devices/README.md) carries messages between one person's devices only; `runtime.inbox` is not it.

The order in which `run()` starts the services is on [Executor startup](../startup.md). The Holochain conductor starts later, when the wallet opens. A host sends no email for `host.join`: the email check runs on the platform.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/lib.rs](../../../../rust-executor/src/lib.rs): `run()`, `holochain_signal_receiver`, `build_sfu_gossip`.
- [rust-executor/src/ai_service/](../../../../rust-executor/src/ai_service), [rust-executor/src/sfu/](../../../../rust-executor/src/sfu), [rust-executor/src/holochain_service/](../../../../rust-executor/src/holochain_service).
- [rust-executor/src/billing.rs](../../../../rust-executor/src/billing.rs), [rust-executor/src/unyt_service.rs](../../../../rust-executor/src/unyt_service.rs), [rust-executor/src/hosting/index_client.rs](../../../../rust-executor/src/hosting/index_client.rs) (new), [rust-executor/src/email_service.rs](../../../../rust-executor/src/email_service.rs).
- [rust-executor/src/runtime_service/mod.rs](../../../../rust-executor/src/runtime_service/mod.rs): `BootstrapSeed`, `RuntimeService`; [runtime_service_extension.rs](../../../../rust-executor/src/runtime_service/runtime_service_extension.rs): its ops; [api/runtime_ws.rs](../../../../rust-executor/src/api/runtime_ws.rs): `runtime.*`.
- [rust-executor/src/api/guards.rs](../../../../rust-executor/src/api/guards.rs): the guard each method declares.
- [rust-executor/AGENTS.md](../../../../rust-executor/AGENTS.md): module map.

</details>
