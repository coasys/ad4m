# Language controller

The **language controller** (`LanguageController`) is the executor's one registry of loaded languages. It maps a language address to a `LanguageRuntimeHandle` and resolves aliases. It is the only way Rust code calls into a language.

**Why:** One owner for all runtimes gives one place to resolve aliases, cache names and capability sets, and tear runtimes down. It is a `lazy_static` global singleton (`LanguageController::global_instance` in [languages/mod.rs](../../../rust-executor/src/languages/mod.rs)). It is not an [executor service](../../agents/executor/services/README.md), but it is held the same way as several of them, for example the AI service (`AI_SERVICE` in [ai_service/mod.rs](../../../rust-executor/src/ai_service/mod.rs)).

**Parent:** [Language runtime](README.md)

<details><summary>How it works</summary>

State (each field is an `Arc<tokio::Mutex<…>>`, so a clone of `LanguageController` is cheap):

| Field | Holds |
|---|---|
| `runtimes` | address → `LanguageRuntimeHandle` |
| `system_addresses` | `SystemLanguageAddresses`: language language, agent, neighbourhood, perspective, and the set of system addresses |
| `language_aliases` | `did`, `lang`, `neighbourhood`, `perspective` → address |
| `language_names` | address → name, for log lines |

Calls:

1. `execute_on_language(addr, js)` resolves the alias and sends `Execute(js, AgentContext::main_agent())`.
2. `execute_on_language_with_context(addr, js, ctx)` sends the caller's `AgentContext` instead.
3. `Language::new(addr)` (in `language.rs`) is the typed wrapper for link-language calls: `commit`, `sync`, `current_revision`, `render`, `set_local_agents`, `others`, telepresence. Each method checks the cached language capability set first and returns a default when the language lacks that language capability.
4. `Language::commit`, `sync`, `render`, `set_local_agents` and the telepresence calls use `execute_on_language`, so link-language calls run as the main agent. Inside the language, `agentDid()` returns the main agent's DID, also for a neighbourhood that a managed agent owns. A link language learns the local owners of its neighbourhood through `peersSetLocal` and `agentGetAllLocalUserDids` ([Managed agents](../../agents/executor/managed-agents.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/mod.rs](../../../rust-executor/src/languages/mod.rs): `LanguageController`, `global_instance`, `execute_on_language[_with_context]`, `unload_language`, `reload_language`, `language_remove`, `get_installed_languages`, `write_settings`, `shutdown`.
- [rust-executor/src/languages/language.rs](../../../rust-executor/src/languages/language.rs): `Language`, the per-capability call wrappers.
- [rust-executor/src/languages/language_runtime_handle.rs](../../../rust-executor/src/languages/language_runtime_handle.rs): `LanguageRuntimeHandle::execute` (sends `AgentContext::main_agent()`), `execute_with_context`.
- [rust-executor/src/languages/error.rs](../../../rust-executor/src/languages/error.rs): `LanguageError`.
- [rust-executor/src/lib.rs](../../../rust-executor/src/lib.rs): boot order (`LanguageController` after V8 init).

</details>