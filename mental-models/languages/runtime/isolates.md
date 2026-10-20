# Language isolates

A **language isolate** is one Deno `MainWorker`, built from the executor's V8 snapshot, that runs one language bundle on its own OS thread. The bundle reaches the executor only through the `ad4m:host` module, which wraps Rust `#[op2]` ops. The executor runs one isolate per language, and every agent on the executor shares it: each request names the agent it runs as. A non-system language has no file access of its own: its storage goes through the host's storage calls, which **seal** every file under the key of the store the language serves.

**Why:** Third-party code must not see the agent's keys or another language's data. Signing is an op that returns a signature, never a key, and the one signature that seeds a secret (the room-key seed) has its own call that every other signing op refuses. Each op acts only for the isolate's own language address. Storage is sealed by the host, not by the language, so encryption at rest holds for every language a host installs, with no allowlist. One isolate per language, not one per agent, means an executor runs one instance of a language even when several of its agents share a neighbourhood. [`docs-src/host-contract.md`](../../../docs-src/host-contract.md) describes the `host.js` surface for other hosts (browser, Node.js).

**Parent:** [Language runtime](README.md)

<details><summary>How it works</summary>

Sandbox (`JsCore::new_for_language`):

| Permission | Normal language | System language |
|---|---|---|
| read / write | none: only the host's storage calls | own storage directory + executor working directory |
| net | any host | any host |
| import | only `ad4m.language` (the bundle's own URL) | same |
| env, run, ffi, sys | denied | denied |

`ad4m:host` exports (from `host.js`):

| Area | Exports |
|---|---|
| agent | `agentDid`, `agentSigningKeyId`, `agentSign`, `agentSignStringHex`, `agentCreateSignedExpression`, `agentGetAllLocalUserDids`, `agentCreateSignedExpressionForUser`, `agentDidForUser`, `agentRoomKeySeed` |
| membership | `membershipRules`, `membershipIsMember`, `membershipMembers`, `membershipPersonOf`: answered from the executor's replay cache, never from the network |
| Holochain | `holochainRegisterDnas`, `holochainCall`, `holochainCallAsync` |
| HTTP, hash | `httpFetch` (10 s timeout, URL secrets redacted in errors), `hash` |
| language context | `languageStorageDirectory`, `languageAddress`, `languageSettings` |
| emit | `emitPerspectiveDiff`, `emitSyncStateChange`, `emitTelepresenceSignal`, `emitSignal`, `roomKeyEvent` |
| storage | `storageGet`, `storagePut`, `storageDelete`, `storageListKeys`; optional `readStorageFile`, `writeStorageFile` |

1. **Sealed storage.** `storagePut`, `storageDelete` and `writeStorageFile` write through `op_language_storage_write`; reads through `op_language_storage_read`. The op finds the store from the isolate's address: the perspective's store key for a neighbourhood's link language, the executor store key for every other language. A file is `AD4MSEAL` | format | key version | nonce | XChaCha20-Poly1305 ciphertext, bound to its path and store. A system language's files inside its storage directory are sealed; files it writes elsewhere through its working-directory permission stay plain, and the op logs a warning. Where the keys come from: [Keyring and storage secret](../../agents/executor/data/keyring.md).
2. **Signing.** Every signing op passes `sign_for_context`, which refuses the room-key seed message; `agentRoomKeySeed()` returns that seed for the calling agent context ([Room keys](../link-languages/server-link-language/room-keys.md)).
3. **Own language only.** `perspectiveDiffReceived`, `syncStateChanged`, `telepresenceSignalReceived`, `ad4mSignalEmitted` and `registerHolochainSignalHandler` refuse another language's address. `PUBSUB.publish` always refuses: language code publishes no executor event.
4. **Threading.** `process_requests` selects between the request channel and the V8 event loop. Requests run one at a time; for `Execute(js, ctx)` the thread-local `AgentContext` is set to `ctx`, then reset to the main agent. Holochain signals arrive through `lib.rs::holochain_signal_receiver` and `__handleHolochainSignal__`.

`host.js` is a wrapper. The ops behind it are also on globals that bundle code can call directly: `AGENT`, `HOLOCHAIN_SERVICE`, `LANGUAGE_CONTROLLER`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/js_core/mod.rs](../../../rust-executor/src/js_core/mod.rs): `JsCore`, `new_for_language` (permissions), `execute`.
- [rust-executor/src/js_core/options.rs](../../../rust-executor/src/js_core/options.rs): `ad4m_language_extensions()` (the op extensions every isolate gets), worker options, module loader.
- [rust-executor/src/js_core/host.js](../../../rust-executor/src/js_core/host.js): the `ad4m:host` module; [ad4m-ldk/js/src/host.d.ts](../../../ad4m-ldk/js/src/host.d.ts): its types.
- [rust-executor/src/js_core/language_bootstrap.js](../../../rust-executor/src/js_core/language_bootstrap.js): main module of every isolate; `initLanguage`, the export table, the storage-file calls, DOM and `Buffer` shims.
- [rust-executor/src/js_core/languages_extension.rs](../../../rust-executor/src/js_core/languages_extension.rs): `op_language_storage_read`, `op_language_storage_write`, the own-language checks, the membership and `roomKeyEvent` ops.
- [rust-executor/src/js_core/agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs), [signature_extension.rs](../../../rust-executor/src/js_core/signature_extension.rs), [utils_extension.rs](../../../rust-executor/src/js_core/utils_extension.rs), [pubsub_extension.rs](../../../rust-executor/src/js_core/pubsub_extension.rs): the other ops.
- [rust-executor/src/holochain_service/holochain_service_extension.rs](../../../rust-executor/src/holochain_service/holochain_service_extension.rs), [rust-executor/src/runtime_service/runtime_service_extension.rs](../../../rust-executor/src/runtime_service/runtime_service_extension.rs): Holochain and runtime ops.

</details>
