# Executor startup

Startup has three phases. First, `rust_executor::run(config)` reads config and secrets and starts the RPC server **sealed**: it answers only `runtime.storageStatus`, `storage.unlock` and the health check. Second, the storage secret arrives (from the desktop shell over loopback, a credential file, or a person typing it); `DataKeys::unlock` opens the keyring, a plaintext executor migrates once, `Ad4mDb` opens with its SQLCipher key, and the rest of the boot runs. Third, `agent.generate` (first start) or `agent.unlock` opens the wallet with the passphrase derived from the same secret; this loads the system languages, starts the Holochain conductor and publishes the main agent's profile. An executor whose wallet is locked therefore has no system languages.

**Why:** Nothing that opens a person's data may run before the secret that protects it arrives, and the secret never sits on the data disk, so the executor cannot open its own files alone. A person types nothing on the desktop: the OS keystore holds the secret behind the login. On Coasys cloud a person types it after each restart, so a copy of the machine's disks never opens the executor. Exit codes and `--watch-stdin` give the desktop supervisor and systemd one contract. `run()` does not exit the process on an API failure, because the launcher embeds it and must stay up to show the error.

**Parent:** [Executor](README.md)

<details><summary>How it works</summary>

**Phase 1: `run(config)`, sealed**, in the order of the statements in `lib.rs`. This page owns the boot order.

1. Shutdown channel, SIGINT/SIGTERM handler, stdin watch (`--watch-stdin`), logging (`--log-dir`), `config.prepare()`, `check_admin_credential()`. The data-folder lock refuses a second executor (exit 79). A different `--operator` than the operator record stops the start (exit 78).
2. The RPC server with the sealed-state methods only. Every other call and every socket with a token gets close code 1013 or HTTP 503 `storage-sealed`.

**Phase 2: `storage.unlock { secret }`, or `AD4M_STORAGE_SECRET_FILE` at start.**

1. Refuse a secret file on the data disk (unless `allow_secret_on_data_disk`). Derive the root key and the agent passphrase; zero the secret. Unwrap the executor store key and every person and store key from `keyring.json` ([Keyring and storage secret](data/keyring.md)).
2. If a store is still plain: state `migrating`, encrypt it, emit `storage-migration-progress` ([Encrypted stores](data/encrypted-stores.md)).
3. `Ad4mDb` (`ad4m_db.sqlite`) with `PRAGMA key`; the multi-user flag; the operator record; the grants store.
4. `AIService`, `AgentService` (loads `agent.json`), `RuntimeService`, the identity module (`KelCache`), SFU service, V8 platform, `LanguageController`, perspectives (each Oxigraph store opens only now).
5. Background tasks: dapp server (fixed port 8080, unless `--run-dapp-server false`), Holochain signal receiver and Unyt, credit flush, the inbox pull (every 15 min), the key-log refresh (15 min), the tenant revocation sweep (10 min), the key-bound grants re-check (5 min), the usage meter (15 min), the bot-log and compute-log sweeps, the hosting-index client, MCP server thread, the home-network listener (desktop, if `lan.enabled`), RPC listeners in full.

**Phase 3: `agent.generate` or `agent.unlock`, with no passphrase.**

1. Create `main` or decrypt the keystore under the derived passphrase ([Wallet and keys](../identity/wallet-and-keys.md)).
2. With `run_holochain`, `spawn_conductor_startup(passphrase)` starts Holochain and `load_core_languages` loads the system languages ([Bootstrap languages](../../languages/runtime/bootstrap-languages.md)). Without it, only `load_core_system_languages` runs.
3. `spawn_main_agent_publish()` publishes the profile to the agent language in the background.
4. Emit the `agent-status-changed` event with `AgentStatus`. Its `error` field reports a service or language load that failed.

**Shutdown and exit codes:** `ExitReason::Stopped` (0) on SIGTERM, SIGINT, stdin closed, `runtime.quit`; `RestartRequested` (75) on `runtime.restart`; `ConfigRefused` (78); `DataFolderBusy` (79); `BindFailed` (80). In hosted mode, `POST /internal/shutdown` takes a final sealed perspective snapshot first.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/lib.rs](../../../rust-executor/src/lib.rs): `run`, `exit_when_api_fails`, `holochain_signal_receiver`.
- [rust-executor/src/at_rest/](../../../rust-executor/src/at_rest) (new): `secret_source.rs`, `data_keys.rs` (`DataKeysApi::unlock`), `migrate.rs`; [rust-executor/src/api/storage_ws.rs](../../../rust-executor/src/api/storage_ws.rs) (new): `runtime.storageStatus`, `storage.unlock`.
- [rust-executor/src/exit.rs](../../../rust-executor/src/exit.rs) (new): `ExitReason`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `generate_agent`, `unlock_main_agent`, `spawn_main_agent_publish`.
- [rust-executor/src/holochain_service/conductor_startup.rs](../../../rust-executor/src/holochain_service/conductor_startup.rs): `spawn_conductor_startup`, `ConductorStartup::load_core_languages`.
- [rust-executor/src/api/mod.rs](../../../rust-executor/src/api/mod.rs): `start_server`, `bind_api_listeners`; [rust-executor/src/lan_listener.rs](../../../rust-executor/src/lan_listener.rs) (new): the desktop's home-network listener.
- [rust-executor/src/api/internal.rs](../../../rust-executor/src/api/internal.rs): `/internal/shutdown`.
- [cli/src/ad4m_executor.rs](../../../cli/src/ad4m_executor.rs): `init`, `run`, and the new `storage new-secret | unlock | status | rotate-secret | reset` subcommands.
- [docs-src/headless-executor.md](../../../docs-src/headless-executor.md), [docs-src/running-an-executor.md](../../../docs-src/running-an-executor.md).

</details>
