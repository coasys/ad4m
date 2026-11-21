# Language interface

A language bundle is an ES module. It exports `init`, `teardown`, `name` and a flat set of functions, grouped into **language capabilities**. The executor reads the exports once at load time and stores the detected `Capability` set. An export that exists is a language capability.

**Why:** A language implements only what it can do: a static store exports `expressionGet`, a write-only drop box exports `perspectiveCommit`. Callers ask `Language::has(Capability::…)` instead of guarding every call. The older shape, a `create(context)` factory that returned one large object, is gone.

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

There is no direct-message language capability: direct messages are not implemented.

1. `initLanguage` copies the known export names from the module namespace onto `globalThis.__ad4m_language_instance__` (also `globalThis.language`). Controller scripts use both names.
2. `register_callbacks` passes sinks into the callback setters `linkSyncAddCallback`, `linkSyncAddSyncStateChangeCallback` and `telepresenceRegisterSignalCallback` if they exist, then returns the language capability list. A language pushes diffs either through the `linkSyncAddCallback` setter (p-diff-sync) or with the `emitPerspectiveDiff` host import (the server link language).
3. Authors write languages with the **ALDK**: `defineLanguage({…})` in TypeScript, or the `ad4m_language!` macro in Rust compiled to WASM with wasm-bindgen. Both import from `ad4m:host`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/capability.rs](../../../rust-executor/src/languages/capability.rs): `Capability`, `register_capabilities`, `get_capabilities`.
- [rust-executor/src/languages/language_runtime.rs](../../../rust-executor/src/languages/language_runtime.rs): `register_callbacks` (detection script).
- [rust-executor/src/js_core/language_bootstrap.js](../../../rust-executor/src/js_core/language_bootstrap.js): `initLanguage`, `flatExportNames`.
- [ad4m-ldk/js/src/defineLanguage.ts](../../../ad4m-ldk/js/src/defineLanguage.ts), [ad4m-ldk/js/src/imports.ts](../../../ad4m-ldk/js/src/imports.ts): JS ALDK.
- [ad4m-ldk/rust/src/traits.rs](../../../ad4m-ldk/rust/src/traits.rs), [ad4m-ldk/rust/src/macros.rs](../../../ad4m-ldk/rust/src/macros.rs): Rust ALDK traits and `ad4m_language!`.
- [docs-src/ad4m-lang.wit](../../../docs-src/ad4m-lang.wit) (normative; interface version 1.0), [docs-src/language-interface-spec.md](../../../docs-src/language-interface-spec.md) (prose), [docs-src/pages/languages.mdx](../../../docs-src/pages/languages.mdx) (user docs).

</details>
