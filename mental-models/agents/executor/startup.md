# Executor startup

Startup has two phases. First, `rust_executor::run(config)` starts the services in a fixed order and returns the API server's thread; a local wallet stays **locked**. Second, `agent.generate` (first run) or `agent.unlock` (later runs) opens the wallet. Only this second phase loads the system languages, starts the Holochain conductor and publishes the main agent's profile. A locked executor therefore has no system languages.

**Why:** The executor does not store the wallet's passphrase. Nothing that signs can run before a person or a secret file unlocks the wallet. Headless executors use `AD4M_UNLOCK_PASSPHRASE_FILE` to unlock after a restart without a person. `run()` does not exit the process on an API failure, because the launcher embeds it and must stay up to show the error.

**Parent:** [Executor](README.md)

<details><summary>How it works</summary>

**Phase 1: `run(config)`**, in the order of the statements in `lib.rs`. This page owns the boot order.

1. Graceful-stop channel and SIGINT/SIGTERM/SIGHUP handler; the lock on the data folder (`exit::hold_data_folder`, file `executor.lock`: a second executor on the same folder exits 79); logging; the parent watch (if `exit_with_parent`); the soft open-file limit raised towards 65 536, at most the hard limit (`fd_limit::raise_fd_limit_at_startup`); `config.prepare()` (defaults); `check_admin_credential()`. The check panics without a credential unless `insecure_no_admin_credential` is set, and always with `enable_multi_user`. Then core dumps off (Linux), the PID file, the stdin watch (if `watch_stdin`).
2. Wallet backend and DB backend: local or shared ([Executor data](data.md)). Hosted mode restores perspective snapshots, then refuses an `internal_api_token` equal to the admin credential.
3. Data directories under `<app_data>/ad4m/`; `Ad4mDb` (`ad4m_db.sqlite`); AI turns left `running` become `interrupted`; `RuntimeService` (bootstrap seed); the key-log cache (`identity::init`); the multi-user flag, turned on if `enable_multi_user` is set. A start with multi-user mode on and no admin credential, or with `custody_channel` and multi-user mode, is refused (exit 78).
4. AI settings from the config file, `AIService`, then `AgentService` (loads `agent.json` if it exists), the AI health loop and local AI autostart, notification owner migration, then the grants (`grants::load_at_start`; the executor does not start without them).
5. Prolog service (`init_prolog_service`; the engine is disabled), SFU service, free Holochain admin and app ports when unset, V8 platform, `LanguageController`, perspectives (`perspectives::open_at_start`), periodic snapshots in hosted mode.
6. Background tasks: dapp server (fixed port 8080), Holochain signal receiver and Unyt (if `run_holochain`), credit flush, MCP server thread (if `enable_mcp`), RPC server thread.

A refused start returns a thread that ends at once with the reason; `exit_when_api_fails` exits with its code.

**Phase 2: the first `agent.generate { passphrase }`, or each `agent.unlock { passphrase }`.**

1. Create keys (generate) or decrypt the keystore (unlock). See [Wallet and keys](../identity/wallet-and-keys.md).
2. With `run_holochain`, `spawn_conductor_startup(passphrase)` starts Holochain and `load_core_languages` loads the system languages ([Bootstrap languages](../../languages/runtime/bootstrap-languages.md)). Without it, only `load_core_system_languages` runs.
3. `spawn_main_agent_publish()` publishes the profile to the agent language in the background.
4. Emit the `agent-status-changed` event with `AgentStatus`. Its `error` field reports a service or language load that failed.

**Shutdown:**

1. `runtime.quit`, `runtime.restart`, SIGINT/SIGTERM/SIGHUP, a closed stdin (`watch_stdin`) or a parent that is gone (`exit_with_parent`) → `exit::request_shutdown` → `SHUTDOWN_TX` → `clean_up_before_exit` stops the conductor and the local AI runtimes, removes the PID file, and exits with the `ExitReason` code: 0, or 75 for `runtime.restart`. After a signal, a second signal or 4 s (`SHUTDOWN_DEADLINE`) exits 1 without the cleanup.
2. In hosted mode, `POST /internal/shutdown` (with `INTERNAL_API_TOKEN`) takes a final perspective snapshot, stops the local AI runtimes and exits 0.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/lib.rs](../../../rust-executor/src/lib.rs): `run`, `unlock_agent_at_startup`, `exit_when_api_fails`, `clean_up_before_exit`, `holochain_signal_receiver`.
- [rust-executor/src/exit.rs](../../../rust-executor/src/exit.rs): `ExitReason` (exit codes 0, 75, 78, 79, 80), `request_shutdown`, `SHUTDOWN_TX`, `DataFolderLock`, `stop_when_closed`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `generate_agent`, `unlock_main_agent`, `spawn_main_agent_publish`.
- [rust-executor/src/holochain_service/conductor_startup.rs](../../../rust-executor/src/holochain_service/conductor_startup.rs): `spawn_conductor_startup`, `ConductorStartup::load_core_languages`.
- [rust-executor/src/api/mod.rs](../../../rust-executor/src/api/mod.rs): `start_server`, `bind_api_listeners`.
- [rust-executor/src/api/internal.rs](../../../rust-executor/src/api/internal.rs): `/internal/shutdown`.
- [cli/src/ad4m_executor.rs](../../../cli/src/ad4m_executor.rs): `init` and `run` subcommands.
- [docs-src/headless-executor.md](../../../docs-src/headless-executor.md), [docs-src/running-an-executor.md](../../../docs-src/running-an-executor.md).

</details>
