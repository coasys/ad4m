# languages/ and js_core/ — agent guide

AD4M Languages are JS bundles (built with ALDK, see `../../ad4m-ldk/`) that
implement expression/link-language adapters. The executor runs each loaded
Language in its own Deno isolate on its own OS thread. Split plan: spec item 9.

## languages/

| File | Role |
|---|---|
| `mod.rs` (2.9k, no tests) | `LanguageController` global (`LanguageController::global_instance()`; lazily self-initialises). Load/unload runtimes, `execute_on_language[_with_context]`, install from address / bundle, system languages, settings, `language_by_ref` (306 lines), expression get/create/interact, **and** neighbourhood create/get + Holochain DNA templating (both moving out) |
| `language_runtime.rs` | One runtime = thread + `JsCore::new_for_language` + request channel; thread-local `AgentContext` so ops sign as the right user |
| `language_runtime_handle.rs` | Cloneable handle: send JS source string, await result |
| `language.rs` | `Language` struct: executor-side view (name, address, adapters present) |
| `language_context.rs`, `capability.rs`, `literal.rs`, `byte_array.rs`, `error.rs`, `wasm_delegate.rs` | context passed at load, per-language capability grants, `Literal` codec, `LanguageError` |

Calls into a Language are built as JS source via `format!` and evaluated (≈20
sites in `mod.rs`). A malformed template used to deadlock that language forever
(`js_core/mod.rs` comments). Keep templates small and escape inputs with
`serde_json::to_string`.

## js_core/

| File | Role |
|---|---|
| `mod.rs` | `JsCore`: Deno `MainWorker` construction from the custom snapshot, extension registration, `new_for_language` |
| `language_bootstrap.js` | Main module of every language isolate: loads the bundle, wires `ad4m:host` |
| `host.js` | `ad4m:host` shim: ~28 functions (agent, holochain, http, hashing, language ctx, emit, storage) backed by ops |
| `*_extension.rs` + `.js` | `#[op2]` ops per area: agent (19), languages (8), wallet (9), utils (5), signature (2), pubsub (1). Holochain (15), runtime (3), entanglement (5) ops live in their service dirs |
| `string_module_loader.rs`, `options.rs`, `futures.rs`, `error.rs` | module loading from strings, worker options, future bridging |
| `residual_lazy.rs`, `../../CUSTOM_DENO_SNAPSHOT.bin` | snapshot artifacts; rebuild with `pnpm build` after any `.js` change |

Rules: JS files here must be pure ASCII (`ascii_str_include!`). Changing any op or
`.js` requires `pnpm build` (snapshot), not just `cargo build`. `main.js` is
unreferenced (deleted in spec item 1).

Obsolete: the "Phase 1" hybrid where JS owned the language controller. There is no
`core.languageController` any more; Rust owns everything, JS only runs bundles.
