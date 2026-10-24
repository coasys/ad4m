# Rust client and CLI

`ad4m-client` (directory `rust-client/`) is the Rust client of the [RPC API](rpc-api.md). Its `Ad4mClient` has sub-clients (`agent`, `perspectives`, `neighbourhoods`, `languages`, `expressions`, `runtime`; no AI sub-client) over one `WsRpcClient`. The `cli/` crate builds two binaries on it: `ad4m`, the operator's command-line client, and `ad4m-executor`, which runs the executor itself (`init`, `run`, `config print`).

**Why:** Operators of headless executors need a shell tool. The executor itself reuses the client's types, for example `ad4m_client::literal::Literal` in MCP and the perspective code. The executor binary lives in the CLI crate, so one release ships both.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

`ad4m <domain> <command>` (domains: `agent`, `languages`, `perspectives`, `neighbourhoods`, `runtime`, `expression`, `log`) picks its token in this order:

1. With `--admin-credential`, the CLI uses it as the token (operator).
2. `--no-capability`, `log`, `agent lock`, `agent unlock` and `agent generate` send no token.
3. Otherwise `startup::get_cap_token` reads `~/.ad4m-cli/cap_token` and tests it with a call that lists perspectives.
4. If that fails, the CLI runs the capability-request flow of [App grants](../access/app-grants.md) as app "AD4M cli" with the full capability `{*, [*], [*]}`. It reads the code from the terminal and stores the JWT.

`ad4m-executor`:

| Command | Does |
|---|---|
| `init [--data-path] [--network-bootstrap-seed]` | prepares the data directory and seed |
| `run [flags]` | merges config file, `AD4M_*` and flags ([Executor config](../executor/config.md)), calls `rust_executor::run`, optionally `unlock_agent_at_startup`, then `exit_when_api_fails` |
| `config print` | prints the merged config with secrets redacted |

</details>

<details><summary>Where in the code</summary>

- [rust-client/src/lib.rs](../../../rust-client/src/lib.rs): `Ad4mClient`.
- [rust-client/src/ws_rpc.rs](../../../rust-client/src/ws_rpc.rs): `WsRpcClient` (tokio-tungstenite, `{ id, type, params }`).
- [rust-client/src/agent.rs](../../../rust-client/src/agent.rs): `request_capability`, `generate_jwt` and the agent calls.
- [cli/src/ad4m.rs](../../../cli/src/ad4m.rs): the `ad4m` binary.
- [cli/src/startup.rs](../../../cli/src/startup.rs): `get_cap_token`, `data_path`.
- [cli/src/ad4m_executor.rs](../../../cli/src/ad4m_executor.rs), [cli/src/run_config.rs](../../../cli/src/run_config.rs): the `ad4m-executor` binary.
- [docs-src/pages/developer-guides/cli.mdx](../../../docs-src/pages/developer-guides/cli.mdx).

</details>
