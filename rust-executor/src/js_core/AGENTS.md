# js_core/ — agent guide

Deno isolates that run AD4M Language bundles. Controller, install, and
expressions live in `languages/`; read [`../languages/AGENTS.md`](../languages/AGENTS.md)
as well. Split plan: spec item 9 (languages), item 1 (delete dead `main.js`).

## Files

| File | Role |
|---|---|
| `mod.rs` | `JsCore`: Deno `MainWorker` from the custom snapshot, extension registration, `new_for_language` |
| `language_bootstrap.js` | Main module of every language isolate: loads the bundle, wires `ad4m:host` |
| `host.js` | `ad4m:host` shim: ~28 functions (agent, holochain, http, hashing, language ctx, emit, storage) backed by ops |
| `*_extension.rs` + `.js` | `#[op2]` ops per area: agent, languages, wallet, utils, signature, pubsub. Holochain, runtime, entanglement ops live in their service dirs |
| `string_module_loader.rs`, `options.rs`, `futures.rs`, `error.rs` | module loading from strings, worker options, future bridging |
| `residual_lazy.rs`, `../../CUSTOM_DENO_SNAPSHOT.bin` | snapshot artifacts; rebuild with `pnpm build` after any `.js` change |

## Rules

- JS files here must be pure ASCII (`ascii_str_include!`). Non-ASCII fails const-eval.
- Changing any op or `.js` requires `pnpm build` in `rust-executor/` (snapshot),
  not just `cargo build`.
- `main.js` is unreferenced (deleted in spec item 1). Do not add to it.
- There is no `core.languageController` any more; Rust owns the controller, JS
  only runs bundles.
