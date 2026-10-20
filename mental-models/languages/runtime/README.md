# Language runtime

The **language runtime** is the part of the executor that loads and runs languages. The **language controller** holds one **language isolate** (a Deno worker) per language address. It calls a language by sending JavaScript source to the isolate and reading back a string.

**Why:** Language code comes from third parties. A separate isolate per language keeps one language's state, file access and crashes away from the executor and from other languages. Rust owns the controller; JavaScript only runs bundles. The option that lost: a JavaScript executor that owned the language controller.

**Parent:** [Languages](../README.md)

## Parts
- [Language controller](controller.md): the registry of loaded languages, and every call into them.
- [Language isolates](isolates.md): the sandbox of one bundle, and the `ad4m:host` ops it may call.
- [Language interface](interface.md): what a bundle exports, language capability detection, and the ALDK.
- [Installing languages](installing/README.md): fetch by address, hash check, trust check, publishing.
- [Bootstrap languages](bootstrap-languages.md): the bootstrap seed, and which languages load at start.

<details><summary>How it works</summary>

`LanguageController::load_language(bundle_path, is_system_language)`:

1. Read `bundle.js` and compute its [address](../expressions/addresses.md). If a runtime for this address exists, call its `teardown()` and drop it.
2. Build a `LanguageContext` (main agent DID, signing key id, `settings.json`, storage directory).
3. `LanguageRuntimeHandle::spawn` starts a thread with a current-thread Tokio runtime and a `JsCore` from `new_for_language`.
4. `load_module` loads the bundle as `https://ad4m.language/<address>/bundle.js`. `load_language` runs `initLanguage(context)`, which calls the module's `init()`.
5. `register_callbacks` wires the diff, sync-state and telepresence sinks and returns the language capability set. `capability::register_capabilities` caches it.
6. The handle goes into the `runtimes` map, keyed by address.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/AGENTS.md](../../../rust-executor/src/languages/AGENTS.md), [rust-executor/src/js_core/AGENTS.md](../../../rust-executor/src/js_core/AGENTS.md): file maps and rules.
- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `LanguageController::load_language`. The same file also holds neighbourhood expression storage (`create_neighbourhood_with_context`) and Holochain DNA templating.
- [rust-executor/src/languages/language_runtime_handle.rs](../../../rust-executor/src/languages/language_runtime_handle.rs): `spawn`, `execute`, `execute_with_context`.
- [rust-executor/src/languages/language_runtime.rs](../../../rust-executor/src/languages/language_runtime.rs): `LanguageRuntime`, `LanguageOperation`, `process_requests`.
- [rust-executor/src/languages/language_context.rs](../../../rust-executor/src/languages/language_context.rs): `LanguageContext`.
- [rust-executor/src/api/languages_ws.rs](../../../rust-executor/src/api/languages_ws.rs): RPC `language.all`, `get`, `meta`, `source`, `publish`, `applyTemplate`, `remove`, `writeSettings`.

</details>