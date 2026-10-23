# Executor config

`Ad4mConfig` is the one struct that `run()` reads. `ad4m-executor run` builds it from a JSON config file (`--config` / `AD4M_CONFIG`), then applies `AD4M_*` environment variables and flags on top. Secrets never sit in the file. `ExecutorSecrets` (admin credential, SMTP password, unlock passphrase) come from `AD4M_*_FILE` paths or environment variables. The launcher maps its own state through the same file type.

**Why:** A headless executor and the launcher must give every key the same meaning, so one mapping (`ExecutorConfigFile::to_ad4m_config`) serves both. Unknown keys and inline secrets fail the parse. A typo or a pasted secret therefore stops the start; it is not ignored.

**Parent:** [Executor](README.md)

<details><summary>How it works</summary>

```
config file (JSON) ─┐
AD4M_* env, flags ──┼─ cli/src/run_config.rs merges ─► ExecutorConfigFile ─┐
launcher state ─────┘                                                       ├─ to_ad4m_config ─► Ad4mConfig ─► config.prepare() ─► run()
AD4M_*_FILE, AD4M_SMTP_PASSWORD ─► ExecutorSecrets ─────────────────────────┘
```

Settings that decide who the operator is and who can connect:

| Setting | Default | Effect |
|---|---|---|
| `admin_credential` (`AD4M_ADMIN_CREDENTIAL[_FILE]`) | none: `run` refuses to start | the operator's token |
| `insecure_no_admin_credential` | `false` | tests only: no token on a loopback listener is the operator |
| `localhost` | `true` | RPC listener on `127.0.0.1`; `false` binds `0.0.0.0` |
| `tls` (file: `multi_user_config.tls_config`) | none | HTTPS/WSS on `0.0.0.0:tls_port`; the cleartext listener moves to loopback |
| `enable_multi_user` (file: `multi_user_config.enabled`) | `false` | managed agents: sign-up and login; see below |
| `auto_permit_cap_requests` | `true` for `ad4m-executor run` without a config file, else `false` | approve every app capability request without the operator |
| `enable_mcp` (file: `mcp_enabled`), `mcp_port` | `false`, `3001` | MCP server |
| `port` | `12000` | RPC port |
| `wallet_backend`, `db_backend` (environment `WALLET_BACKEND`, `DB_BACKEND`) | local | `shared` for hosted mode |
| `internal_api_token` (environment `INTERNAL_API_TOKEN`) | none | executor → hosting service; in hosted mode it must differ from `admin_credential` |

The multi-user flag lives in a `settings` row of SQLite. `run()` sets it when `enable_multi_user` is true; `user.setMultiUserEnabled` changes it at runtime. A restart with `enable_multi_user` off leaves the row as it is ([lib.rs](../../../rust-executor/src/lib.rs)).

Other defaults from `Ad4mConfig::prepare`: `app_data_path = ~/.ad4m`, `run_holochain = true`, `run_dapp_server = true`, Holochain bootstrap and proxy at `bootstrap.ad4m.dev`.

`ad4m-executor config print` prints the merged result with secrets redacted.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/config.rs](../../../rust-executor/src/config.rs): `Ad4mConfig`, `prepare`, `check_admin_credential`, `signing_key_name`, `redacted_json`, `DEFAULT_PORT`, `get_global_config`, `auto_permit_cap_requests`.
- [rust-executor/src/config_file.rs](../../../rust-executor/src/config_file.rs): `ExecutorConfigFile`, `ExecutorSecrets`, `MultiUserSettings`, `TlsSettings`, `to_ad4m_config`, `INLINE_SECRETS`.
- [cli/src/run_config.rs](../../../cli/src/run_config.rs): `RunArgs`, the flag/env/file merge, `AD4M_UNLOCK_PASSPHRASE_FILE`.
- [ui/src-tauri/src/app_state.rs](../../../ui/src-tauri/src/app_state.rs): the launcher's `launcher-state.json` and its mapping to `ExecutorConfigFile`.
- [docs-src/pages/developer-guides/executor-config.mdx](../../../docs-src/pages/developer-guides/executor-config.mdx): every key, with flags and variables.

</details>
