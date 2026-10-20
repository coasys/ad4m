# Executor

The **executor** is the ADAM process (crate `ad4m-executor`, lib `rust_executor`). It hosts the agents, runs the perspectives, the language runtimes and the executor services, and serves the APIs. Each executor is a **device** of its operator: its main agent's wallet key sits in the operator's key log, and its **operator record** names that identity. With multi-user mode on it is also a **host**: it serves **tenants**, people who use it and do not run it. It starts **sealed** and opens its data only after it receives its **storage secret**. The desktop app's shell runs it as a supervised child process; a server runs `ad4m-executor run`.

**Why:** One process holds everything an agent needs, so an agent's data stays on the machine that the person chose. A desktop, a server, a community host and Coasys cloud run the same code and differ only in settings. Each person's database rows and language storage are encrypted under that person's own key, with the keys in memory only, so other OS users, other processes and a copied disk do not read them. The executor leaves the shell's process on the desktop, because it runs language code from the network and must not share memory with the custody service. Who may act on the executor is on [Access](../access/README.md).

**Parent:** [Agents](../README.md)

## Parts
- [Executor startup](startup.md): the sealed state, `storage.unlock`, the boot order in `run()`, the supervised run mode, shutdown.
- [Executor config](config.md): `Ad4mConfig` and its sources; ADAM's config calls and key classes; the identity, host, index and billing keys.
- [Executor data](data/README.md): the files under the app data path, the keyring and storage secret, which stores are encrypted, export and import.
- [Managed agents](managed-agents/README.md): multi-user mode: tenants and the join gate, leaving a host, password users.
- [Executor services](services/README.md): the Rust modules that are not languages: AI, SFU, Holochain, billing and Unyt, the hosting-index client.

<details><summary>How it works</summary>

```
 desktop shell supervisor ──spawn, --custody-channel──┐        ┌── ad4m-executor run [--operator did:scid]
                                                      ▼        ▼
                                          Ad4mConfig ──► rust_executor::run()
                                                          │ sealed: only status, storage.unlock, health
                                                          ▼ storage.unlock(secret) → keyring → SQLCipher DB → wallet
     ┌───────────────┬──────────────┬─────────────┬───────┴──────┬───────────────┬────────────────────┐
     ▼               ▼              ▼             ▼              ▼               ▼                    ▼
 DataKeys +      AgentService   identity      LanguageController perspectives  executor services    API listeners
 keyring.json    (main agent)   (KelCache,    (Deno runtimes)  (Oxigraph,     (AI, SFU, Holochain, RPC :12000, MCP :3001,
                                 inbox, bots)                   plain files)   billing, index)      home-network listener (desktop)
```

| Executor | Main agent | Managed agents | Operator |
|---|---|---|---|
| one process per data folder | one, created by `agent.generate`; a device key of the operator | zero or more: tenants (`tenants` row), password users, guests | the identity in the operator record; the admin credential on the machine ([Operator and main agent](../access/operator-and-main-agent.md)) |

`AgentService` holds the main agent. Managed agents are rows of the `users` table (a tenant also has a `tenants` row), reached through static functions in `agent/mod.rs` ([Identity](../identity/README.md)).

Every service is a global singleton (`lazy_static` or `OnceLock`): `Ad4mDb`, `AgentService`, `DataKeys`, `LanguageController`, `AIService`, the pub/sub bus, the grants store, the perspective registry. One process therefore runs one executor, and unit tests run serially.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/](../../../rust-executor) and its [AGENTS.md](../../../rust-executor/AGENTS.md): the module map.
- [rust-executor/src/lib.rs](../../../rust-executor/src/lib.rs): `run()`, the boot order.
- [rust-executor/src/config.rs](../../../rust-executor/src/config.rs): `Ad4mConfig`.
- [rust-executor/src/at_rest/](../../../rust-executor/src/at_rest) (new): the keyring, the storage secret, the sealed state.
- [rust-executor/src/identity/](../../../rust-executor/src/identity) (new) and [rust-executor/src/hosting/](../../../rust-executor/src/hosting) (new): identity and hosting.
- [rust-executor/src/exit.rs](../../../rust-executor/src/exit.rs) (new): `ExitReason`, the exit codes a supervisor reads.
- [cli/src/ad4m_executor.rs](../../../cli/src/ad4m_executor.rs): the `ad4m-executor` binary.
- [ui/src-tauri/src/lib.rs](../../../ui/src-tauri/src/lib.rs): the launcher, which embeds the executor in its own process and passes the storage secret in memory (`Ad4mConfig.storage_secret`). The desktop app of the apps repository runs it as a child process instead.

</details>
