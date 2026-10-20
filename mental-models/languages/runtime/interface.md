# Language interface

A language bundle is an ES module. It exports `init`, `teardown`, `name` and a flat set of functions, grouped into **language capabilities**. The executor reads the exports once at load time and stores the detected `Capability` set. An export that exists is a language capability. Two groups, **identity** and **token**, are system-only: the executor calls them on the agent language and on no other language.

**Why:** A language implements only what it can do: a static store exports `expressionGet`, a write-only drop box exports `perspectiveCommit`, the agent language adds `identity*`. Callers ask `Language::has(Capability::…)` instead of guarding every call. Routing the identity and token groups only to the agent language keeps one route to identities, whatever other languages export. The older shape, a `create(context)` factory that returned one large object, is gone.

**Parent:** [Language runtime](README.md)

<details><summary>How it works</summary>

| `Capability` | Export(s) the runtime looks for |
|---|---|
| `ExpressionGet`, `ExpressionCreate` | `expressionGet`; `expressionCreate`, `expressionAddressOf` or `addressOf` |
| `PerspectiveCommit` | `perspectiveCommit` |
| `PerspectiveSync`, `PerspectiveRender`, `PerspectiveCurrentRevision` | `perspectiveSyncSync`, `perspectiveSyncRender`, `perspectiveSyncCurrentRevision` |
| `PerspectiveQuery` | `perspectiveQueryRun` |
| `PeersLocal`, `PeersRemote` | `peersSetLocal`, `peersRemote` |
| `Telepresence*` (4) | `telepresenceSetOnlineStatus`, `…GetOnlineAgents`, `…SendSignal`, `…SendBroadcast` |
| `LanguageGetSource` | `languageGetSource` |
| `HolochainSignal` | `handleHolochainSignal` |
| `Identity*` (agent language only) | `identityLog`, `identityHead`, `identityAppend`, `identityKeyOwner`, `identityPowParams`, `identityAttachNote`, `identityPutAgent`, and the offer, inbox, record, request and pause calls ([Identity interface](../other-kinds/agent-language/identity-interface.md)) |
| `TokenAssets`, `TokenBalance`, `TokenSend`, `TokenReceived` (agent language only) | the `token` section of `defineLanguage` ([Token interface](../other-kinds/agent-language/token-interface.md)) |

There is no direct-message language capability: direct messages are not implemented.

1. `initLanguage` copies the known export names from the module namespace onto `globalThis.__ad4m_language_instance__` (also `globalThis.language`). Controller scripts use both names.
2. `handleMembershipChange` is an optional export with no capability of its own; the executor calls it once for each membership change when the link language exports it.
3. `register_callbacks` passes sinks into the callback setters `linkSyncAddCallback`, `linkSyncAddSyncStateChangeCallback` and `telepresenceRegisterSignalCallback` if they exist, then returns the language capability list. A language pushes diffs either through the `linkSyncAddCallback` setter (p-diff-sync) or with the `emitPerspectiveDiff` host import (the server link language).
4. Authors write languages with the **ALDK**: `defineLanguage({…})` in TypeScript, with optional `identity` and `token` sections, or the `ad4m_language!` macro in Rust compiled to WASM with wasm-bindgen (`IdentityCapability`, `TokenCapability` beside `ExpressionCapability`). Both import from `ad4m:host`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/capability.rs](../../../rust-executor/src/languages/capability.rs): `Capability`, `register_capabilities`, `get_capabilities`, the agent-language-only routing.
- [rust-executor/src/languages/language_runtime.rs](../../../rust-executor/src/languages/language_runtime.rs): `register_callbacks` (detection script).
- [rust-executor/src/js_core/language_bootstrap.js](../../../rust-executor/src/js_core/language_bootstrap.js): `initLanguage`, `flatExportNames`.
- [ad4m-ldk/js/src/defineLanguage.ts](../../../ad4m-ldk/js/src/defineLanguage.ts), [ad4m-ldk/js/src/types.ts](../../../ad4m-ldk/js/src/types.ts), [ad4m-ldk/js/src/imports.ts](../../../ad4m-ldk/js/src/imports.ts): JS ALDK.
- [ad4m-ldk/rust/src/traits.rs](../../../ad4m-ldk/rust/src/traits.rs), [ad4m-ldk/rust/src/macros.rs](../../../ad4m-ldk/rust/src/macros.rs): Rust ALDK traits and `ad4m_language!`.
- [docs-src/ad4m-lang.wit](../../../docs-src/ad4m-lang.wit) (normative; interface version 1.0), [docs-src/language-interface-spec.md](../../../docs-src/language-interface-spec.md) (prose, with the membership section), [docs-src/pages/languages.mdx](../../../docs-src/pages/languages.mdx) (user docs).

</details>
