# Executor

The **executor** is the ADAM process (crate `ad4m-executor`, lib `rust_executor`). It hosts the agents, runs the perspectives, the language runtimes and the executor services, and serves the APIs. It starts **sealed** and opens its data only after it receives its **storage secret**. The operator starts it with `ad4m-executor run`, or the **launcher** embeds it.

**Why:** One process holds everything an agent needs, so an agent's data stays on the machine that runs it. Multi-user mode runs many agents on the same code paths, so a host can serve users who run no executor. Who may act on the executor is on [Access](../access/README.md).

**Parent:** [Agents](../README.md)

## Parts
- [Executor startup](startup.md): the sealed state, the boot order in `run()`, the first `agent.generate`, unlock, shutdown.
- [Executor config](config.md): `Ad4mConfig` and where it comes from: config file, environment, flags, secrets, defaults.
- [Executor data](data/README.md): the files under the app data path, the keyring, encryption at rest, export and import, and the shared backends of hosted mode.
- [Managed agents](managed-agents/README.md): multi-user mode: tenants, user accounts, sign-up and login, leaving a host, how a managed agent acts.
- [Executor services](services/README.md): the Rust modules that are not languages: AI, SFU, Holochain, billing, the hosting-index client, email, runtime service.

<details><summary>How it works</summary>

```
ad4m-executor run ──┐                    ┌── launcher (ui/src-tauri) embeds rust_executor::run
                    ▼                    ▼
              Ad4mConfig ──► rust_executor::run()
                                 │ sealed until storage.unlock(secret)
     ┌───────────────┬───────────┼──────────────────┬───────────────┬────────────────┐
     ▼               ▼           ▼                  ▼               ▼                ▼
 wallet + db     AgentService  LanguageController  perspectives   executor        API threads
 backends        (main agent)  (Deno runtimes)     (Oxigraph)     services        RPC :12000, MCP :3001, dapp :8080
```

| Executor | Main agent | Managed agents | Operator |
|---|---|---|---|
| one process | one, created by `agent.generate` | zero or more, one per `users` row | the identity in the operator record, and whoever holds the admin credential ([Operator and main agent](../access/operator-and-main-agent.md)) |

`AgentService` holds the main agent. Managed agents are rows of the `users` table, reached through static functions in `agent/mod.rs` ([Identity](../identity/README.md)).

Every service is a global singleton (`lazy_static` or `OnceLock`): `Ad4mDb`, `AgentService`, `LanguageController`, `AIService`, the pub/sub bus, the grants store, the perspective registry. One process therefore runs one executor, and unit tests run serially.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/](../../../rust-executor) and its [AGENTS.md](../../../rust-executor/AGENTS.md): the module map.
- [rust-executor/src/lib.rs](../../../rust-executor/src/lib.rs): `run()`, the boot order.
- [rust-executor/src/config.rs](../../../rust-executor/src/config.rs): `Ad4mConfig`.
- `rust-executor/src/at_rest/` (new): the keyring, the storage secret, the sealed state.
- [cli/src/ad4m_executor.rs](../../../cli/src/ad4m_executor.rs): the `ad4m-executor` binary.
- [ui/src-tauri/src/lib.rs](../../../ui/src-tauri/src/lib.rs): the launcher, which embeds the executor, holds the admin credential and shows the operator's UI.

</details>
