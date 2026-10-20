# Executor config

`Ad4mConfig` is the one struct that `run()` reads. `ad4m-executor run` builds it from **ADAM's config** file (`--config` / `AD4M_CONFIG`), then applies `AD4M_*` environment variables and flags on top. Secrets never sit in the file: `ExecutorSecrets` (admin credential, SMTP password, storage secret) come from `AD4M_*_FILE` paths or environment variables. Settings that change while the executor runs live in the `settings` table, seeded from the file at start. The operator reads and edits the file through `runtime.config` and `runtime.setConfig`, from any of their devices; each key has a class that says when a change takes effect and who may make it.

**Why:** The CLI, the desktop app and the operator view read one source, so what Settings shows is what runs; the desktop shell passes no setting as a flag except its run arguments. Unknown keys and inline secrets fail the parse, so a typo or a pasted secret stops the start. Keys that open ports or name paths on the machine are `machine`-class: only the person at the machine changes them. A build with the `closed-beta` cargo feature pins some values, so no config can turn charging on.

**Parent:** [Executor](README.md)

<details><summary>How it works</summary>

```
ADAM's config (JSON) ─┐
AD4M_* env, flags ────┼─ cli/src/run_config.rs merges ─► ExecutorConfigFile ─┐
launcher state ───────┘                                                       ├─ to_ad4m_config ─► Ad4mConfig ─► prepare() ─► run()
AD4M_*_FILE, AD4M_SMTP_PASSWORD ─► ExecutorSecrets ───────────────────────────┘
runtime.setConfig ─► atomic write of the --config file (mode 0600) ─► config-changed event; restart-class keys at the next start
```

| Setting | Default | Class | Effect |
|---|---|---|---|
| `admin_credential` (`AD4M_ADMIN_CREDENTIAL[_FILE]`) | none: `run` refuses to start | secret | the machine's operator token |
| storage secret (`AD4M_STORAGE_SECRET_FILE`) | none: the executor stays sealed until `storage.unlock` | secret | opens the keyring; a file on the data disk is refused |
| `--operator <did:scid>` (`init`, `run`) | none: no link route opens | first run only | sets the empty operator record |
| `identity.storeUrl`, `identity.storeKeys` | from the bootstrap seed | restart | the shared store's URL and head keys, passed to the agent language |
| `localhost`, `port`, `tls` (file: `multi_user_config.tls_config`) | `true`, `12000`, none | machine | RPC listener address and TLS |
| `lan.enabled`, `lan.port`, `lan.scope` | off | machine | the desktop's home-network listener for the person's phone |
| `enable_multi_user` (file: `multi_user_config.enabled`) | `false` | machine | managed agents: tenants and password users |
| `host.signup`, `host.max_tenants`, `host.display`, `host.pinned` | no row: sign-up "no one" | seed for `host_policy` | who may join through `host.join` ([Tenants](managed-agents/tenants.md)) |
| `hosting_index.url`, `hosting_index.enabled` | the Coasys index, on | restart | the hosting-index client |
| `billing.charge_mode` | `"free"`; the `closed-beta` build accepts only `"free"` | restart | `ChargeMode` ([Billing and hosting](services/billing-and-hosting.md)) |
| `link_server.url` | the seed's link server | restart, with a validator | the link-server directory's operator override |
| `auto_permit_cap_requests` | `true` for `ad4m-executor run` without a config file, else `false` | restart | approve every app capability request |
| `enable_mcp` (file: `mcp_enabled`), `mcp_port` | `false`, `3001` | restart | MCP server |
| `wallet_backend`, `db_backend` | local | machine | `shared` for hosted mode |
| `internal_api_token` | none | secret | executor → hosting service; must differ from `admin_credential` |

Run arguments of a supervised executor: `--config`, `--app-data-path`, `--port`, `--run-dapp-server`, `--watch-stdin`, `--pid-file`, `--log-dir`, `--custody-channel`. `AD4M_UNLOCK_PASSPHRASE_FILE` is read once, to move an old wallet to the derived passphrase, and is then ignored.

`runtime.setConfig` refuses a key with `local-only` (a `machine` key from a remote session), `pinned-by-config` (a flag overrides it), `pinned-by-build`, `invalid` (its validator failed) or `unknown-key`. Without `--config` it answers `409 no-config-file`. `ad4m-executor config print` prints the merged result with secrets redacted.

The multi-user flag lives in a `settings` row of SQLite. `run()` sets it when `enable_multi_user` is true; `user.setMultiUserEnabled` changes it at runtime. A restart with `enable_multi_user` off leaves the row as it is ([lib.rs](../../../rust-executor/src/lib.rs)). Free access has a runtime row too: `free_hosting_enabled`, which `billing.charge_mode` seeds at start and `runtime.setFreeHostingEnabled` changes it.

Other defaults from `Ad4mConfig::prepare`: `app_data_path = ~/.ad4m`, `run_holochain = true`, `run_dapp_server = true`, Holochain bootstrap and proxy at `bootstrap.ad4m.dev`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/config.rs](../../../rust-executor/src/config.rs): `Ad4mConfig` (gains `storage_secret` for an embedding shell and `allow_secret_on_data_disk`), `prepare`, `check_admin_credential`, `signing_key_name`, `redacted_json`, `DEFAULT_PORT`, `get_global_config`.
- [rust-executor/src/config_file.rs](../../../rust-executor/src/config_file.rs): `ExecutorConfigFile` (gains `host`, `hosting_index`, `billing`, `identity`, `link_server`, `lan`), `ExecutorSecrets` (gains `storage_secret`), `INLINE_SECRETS`, `to_ad4m_config`.
- `register_config_key`, `ConfigKeySpec` and the `runtime.config`, `runtime.setConfig`, `runtime.restart`, `runtime.logTail` handlers (new, in [rust-executor/src/api/runtime_ws.rs](../../../rust-executor/src/api/runtime_ws.rs)).
- [cli/src/run_config.rs](../../../cli/src/run_config.rs): `RunArgs`, the flag/env/file merge.
- [rust-executor/src/mainnet_seed.json](../../../rust-executor/src/mainnet_seed.json): the compiled seed with the store's URL and keys and the link server's address.
- [ui/src-tauri/src/app_state.rs](../../../ui/src-tauri/src/app_state.rs): the launcher's `launcher-state.json` and its mapping to `ExecutorConfigFile`.
- [docs-src/pages/developer-guides/executor-config.mdx](../../../docs-src/pages/developer-guides/executor-config.mdx): every key, with flags and variables.

</details>
