# Executor startup

Startup has two phases. First, `rust_executor::run(config)` starts the services in a fixed order and returns the API server's thread; a local wallet stays **locked**. Second, `agent.generate` (first run) or `agent.unlock` (later runs) opens the wallet. Only this second phase loads the system languages, starts the Holochain conductor and publishes the main agent's profile. A locked executor therefore has no system languages.

**Why:** The executor does not store the wallet's passphrase. Nothing that signs can run before a person or a secret file unlocks the wallet. Headless executors use `AD4M_UNLOCK_PASSPHRASE_FILE` to unlock after a restart without a person. `run()` does not exit the process on an API failure, because the launcher embeds it and must stay up to show the error.

**Parent:** [Executor](README.md)

<details><summary>How it works</summary>

**Phase 1: `run(config)`**, in the order of the statements in `lib.rs`. This page owns the boot order.

1. Shutdown channel and SIGINT/SIGTERM handler, logging, `config.prepare()` (defaults), `check_admin_credential()`. The check panics without a credential unless `insecure_no_admin_credential` is set.
2. Wallet backend and DB backend: local or shared ([Executor data](data.md)). Hosted mode restores perspective snapshots, then refuses an `internal_api_token` equal to the admin credential.
3. Data directories under `<app_data>/ad4m/`; `Ad4mDb` (`ad4m_db.sqlite`); the multi-user flag, turned on if `enable_multi_user` is set.
4. `AIService`, then `AgentService` (loads `agent.json` if it exists), notification owner migration, `RuntimeService`, apps registry path.
5. Prolog service (`init_prolog_service`; the engine is disabled), SFU service, V8 platform, `LanguageController`, perspectives.
6. Background tasks: dapp server (fixed port 8080), Holochain signal receiver and Unyt, credit flush, MCP server thread (if `enable_mcp`), RPC server thread.

**Phase 2: the first `agent.generate { passphrase }`, or each `agent.unlock { passphrase }`.**

1. Create keys (generate) or decrypt the keystore (unlock). See [Wallet and keys](../identity/wallet-and-keys.md).
2. With `run_holochain`, `spawn_conductor_startup(passphrase)` starts Holochain and `load_core_languages` loads the system languages ([Bootstrap languages](../../languages/runtime/bootstrap-languages.md)). Without it, only `load_core_system_languages` runs.
3. `spawn_main_agent_publish()` publishes the profile to the agent language in the background.
4. Emit the `agent-status-changed` event with `AgentStatus`. Its `error` field reports a service or language load that failed.

**Shutdown:**

1. SIGINT/SIGTERM → `SHUTDOWN_TX` → stop the conductor, remove the PID file, exit.
2. In hosted mode, `POST /internal/shutdown` (with `INTERNAL_API_TOKEN`) takes a final perspective snapshot first.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/lib.rs](../../../rust-executor/src/lib.rs): `run`, `unlock_agent_at_startup`, `exit_when_api_fails`, `holochain_signal_receiver`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `generate_agent`, `unlock_main_agent`, `spawn_main_agent_publish`.
- [rust-executor/src/holochain_service/conductor_startup.rs](../../../rust-executor/src/holochain_service/conductor_startup.rs): `spawn_conductor_startup`, `ConductorStartup::load_core_languages`.
- [rust-executor/src/api/mod.rs](../../../rust-executor/src/api/mod.rs): `start_server`, `bind_api_listeners`.
- [rust-executor/src/api/internal.rs](../../../rust-executor/src/api/internal.rs): `/internal/shutdown`.
- [cli/src/ad4m_executor.rs](../../../cli/src/ad4m_executor.rs): `init` and `run` subcommands.
- [docs-src/headless-executor.md](../../../docs-src/headless-executor.md), [docs-src/running-an-executor.md](../../../docs-src/running-an-executor.md).

</details>
