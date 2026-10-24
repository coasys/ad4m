# Language isolates

A **language isolate** is one Deno `MainWorker`, built from the executor's V8 snapshot, that runs one language bundle on its own OS thread. The bundle reaches the executor through the `ad4m:host` module, which wraps Rust `#[op2]` ops. The executor runs one isolate per language, and every agent on the executor shares it: each request names the agent it runs as.

**Why:** Third-party code must not see the agent's keys. The isolate gets file access only to its own storage directory. Signing is an op that returns a signature, never a key. One isolate per language, not one per agent, means an executor runs one instance of a language even when several of its agents share a neighbourhood. [`docs-src/host-contract.md`](../../../docs-src/host-contract.md) describes the `host.js` surface for other hosts (browser, Node.js).

**Parent:** [Language runtime](README.md)

<details><summary>How it works</summary>

Sandbox (`JsCore::new_for_language`):

| Permission | Normal language | System language |
|---|---|---|
| read / write | own storage directory | own storage directory + executor working directory |
| net | any host | any host |
| import | only `ad4m.language` (the bundle's own URL) | same |
| env, run, ffi, sys | denied | denied |

`ad4m:host` exports (from `host.js`):

| Area | Exports |
|---|---|
| agent | `agentDid`, `agentSigningKeyId`, `agentSign`, `agentSignStringHex`, `agentCreateSignedExpression`, `agentGetAllLocalUserDids`, `agentCreateSignedExpressionForUser`, `agentDidForUser` |
| Holochain | `holochainRegisterDnas`, `holochainCall`, `holochainCallAsync` |
| HTTP, hash | `httpFetch` (10 s timeout, URL secrets redacted in errors), `hash` |
| language context | `languageStorageDirectory`, `languageAddress`, `languageSettings` |
| emit | `emitPerspectiveDiff`, `emitSyncStateChange`, `emitTelepresenceSignal`, `emitSignal` |
| storage | `storageGet`, `storagePut`, `storageDelete`, `storageListKeys`; optional `readStorageFile`, `writeStorageFile` |

`host.js` is a wrapper. The ops behind it are also on globals that bundle code can call directly: `AGENT`, `HOLOCHAIN_SERVICE`, `LANGUAGE_CONTROLLER`.

Threading:

1. `process_requests` selects between the request channel and the V8 event loop, so timers and signals run between requests.
2. Requests run one at a time. For `Execute(js, ctx)` the thread-local `AgentContext` is set to `ctx`, the script runs, and the context is reset to the main agent.
3. Holochain signals arrive through `lib.rs::holochain_signal_receiver`. It looks up the cell in `HOLOCHAIN_SIGNAL_HANDLERS` and calls `__handleHolochainSignal__` in that language's isolate.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/js_core/mod.rs](../../../rust-executor/src/js_core/mod.rs): `JsCore`, `new_for_language` (permissions), `execute`.
- [rust-executor/src/js_core/options.rs](../../../rust-executor/src/js_core/options.rs): `ad4m_language_extensions()` (the op extensions every isolate gets), worker options, module loader.
- [rust-executor/src/js_core/host.js](../../../rust-executor/src/js_core/host.js): the `ad4m:host` module.
- [rust-executor/src/js_core/language_bootstrap.js](../../../rust-executor/src/js_core/language_bootstrap.js): main module of every isolate; `initLanguage`, the export table, DOM and `Buffer` shims.
- [rust-executor/src/js_core/agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs), [languages_extension.rs](../../../rust-executor/src/js_core/languages_extension.rs), [signature_extension.rs](../../../rust-executor/src/js_core/signature_extension.rs), [utils_extension.rs](../../../rust-executor/src/js_core/utils_extension.rs): the ops.
- [rust-executor/src/holochain_service/holochain_service_extension.rs](../../../rust-executor/src/holochain_service/holochain_service_extension.rs), [rust-executor/src/runtime_service/runtime_service_extension.rs](../../../rust-executor/src/runtime_service/runtime_service_extension.rs): Holochain and runtime ops.

</details>