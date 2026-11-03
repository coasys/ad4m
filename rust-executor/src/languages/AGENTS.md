# languages/ — agent guide

AD4M Languages are JS bundles (built with ALDK, see `../../ad4m-ldk/`) that
implement expression/link-language adapters. The executor runs each loaded
Language in its own Deno isolate on its own OS thread. Split plan: spec item 9.

## languages/

| File | Role |
|---|---|
| `mod.rs` (2.9k, no tests) | `LanguageController` global (`LanguageController::global_instance()`; lazily self-initialises). Load/unload runtimes, `execute_on_language[_with_context]`, install from address / bundle, system languages, settings, `language_by_ref` (306 lines), expression get/create/interact, **and** neighbourhood create/get + Holochain DNA templating (both moving out) |
| `language_runtime.rs` | One runtime = thread + `JsCore::new_for_language` + request channel; thread-local `AgentContext` so ops sign as the right user |
| `language_runtime_handle.rs` | Cloneable handle: send JS source string, await result |
| `call_deadline_tests.rs` | Calls into one runtime through a real handle: the call deadline, send order, the agent a call signs as; `#[ignore]` benchmarks |
| `language.rs` | `Language` struct: executor-side view (name, address, adapters present) |
| `conductor_languages.rs` | `load_link_and_installed_languages`: the system-language steps whose constructors wait for the Holochain conductor (known link languages, installed languages). Run in the background after generate/unlock by `agent::conductor_startup` |
| `language_context.rs`, `capability.rs`, `literal.rs`, `byte_array.rs`, `error.rs`, `wasm_delegate.rs` | context passed at load, per-language capability grants, `Literal` codec, `LanguageError` |

A runtime serves its calls one at a time, in send order, and never abandons or
interrupts a call it has started: commits must reach the link language in the
order they were made, and the `AgentContext` thread-local holds one agent at a
time, so a call that resumed beside a later one would sign as the later call's
agent. The caller of a running call waits until it ends: a call that keeps
making progress (a file upload with one zome call per chunk) has no total
bound. Once the running call has run for `LANGUAGE_CALL_DEADLINE` (60 s), a
script call (`Execute`) still queued behind it gets an error, and a new one
fails at once without being queued. The runtime never starts a call whose
caller has gone, so a deadline error means the call did not run. A call that never
returns (a hung `await` or a JS CPU loop) still blocks its language and its
own caller, but the callers behind it get errors instead of waiting for ever.
Loading and teardown wait as before.

Calls into a Language are built as JS source via `format!` and evaluated (≈20
sites in `mod.rs`). A malformed template used to deadlock that language forever
(`js_core/mod.rs` comments). Keep templates small and escape inputs with
`serde_json::to_string`.

The Deno isolate machinery (worker construction, `ad4m:host` shim, `#[op2]`
extensions, snapshot) lives in `js_core/` — read
[`../js_core/AGENTS.md`](../js_core/AGENTS.md) as well; do not duplicate its
file table here.

Obsolete: the "Phase 1" hybrid where JS owned the language controller. There is no
`core.languageController` any more; Rust owns everything, JS only runs bundles.
