# Rust client and CLI

`ad4m-client` (directory `rust-client/`) is the Rust client of the [RPC API](rpc-api.md). Its `Ad4mClient` has sub-clients (`agent`, `perspectives`, `neighbourhoods`, `languages`, `expressions`, `runtime`; no AI sub-client) over one `WsRpcClient`. The `cli/` crate builds two binaries on it: `ad4m`, the operator's command-line client, and `ad4m-executor`, which runs the executor itself (`init`, `run`, `config print`) and holds the operator's commands that must run on the machine: naming the operator, the storage secret, a new device key, a device-link code.

**Why:** Operators of headless executors need a shell tool. The executor itself reuses the client's types, for example `ad4m_client::literal::Literal` in MCP and the perspective code. The executor binary lives in the CLI crate, so one release ships both. The commands that carry the storage secret or replace the executor's key run on the machine, over loopback with the admin credential, because no remote session may reach those calls ([Operator and main agent](../access/operator-and-main-agent.md)). A supervisor (the desktop shell, systemd) reads the executor's exit code, so a restart, a busy data folder and a refused config each have their own code.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

`ad4m <domain> <command>` (domains: `agent`, `languages`, `perspectives`, `neighbourhoods`, `runtime`, `expression`, `log`) picks its token in this order:

1. With `--admin-credential`, the CLI uses it as the token (operator, `OperatorSource::Credential`).
2. `--no-capability`, `log`, `agent lock`, `agent unlock` and `agent generate` send no token.
3. Otherwise `startup::get_cap_token` reads `~/.ad4m-cli/cap_token` and tests it with a call that lists perspectives.
4. If that fails, the CLI runs the capability-request flow of [App approval](../access/grants/app-approval.md) as app "AD4M cli" with the full capability `{*, [*], [*]}`. It reads the code from the terminal and stores the JWT. There is no narrower request.

The `ad4m` CLI has no key login: it reaches a remote executor with an app grant or the admin credential ([Key login](../access/key-login.md)).

`ad4m-executor`:

| Command | Does |
|---|---|
| `init [--data-path] [--network-bootstrap-seed] [--operator <did:scid>]` | prepares the data directory and seed; records the operator while the record is empty; on a server with no address, prints the device string and a terminal QR code for the first link |
| `run [flags] [--operator <did:scid>]` | merges config file, `AD4M_*` and flags ([Executor config](../executor/config.md)), calls `rust_executor::run`; an `--operator` that differs from a set record stops the start |
| `run --custody-channel`, `--watch-stdin`, `--log-dir` | the desktop shell's flags: answer `identity.takeMaster`, stop when stdin closes, write logs to a folder |
| `storage new-secret \| unlock \| status \| rotate-secret \| reset` | the storage secret: `unlock` reads it from the terminal with echo off and calls `storage.unlock` on loopback ([Executor](../executor/README.md)) |
| `rekey` | a new main wallet key (`identity.rekey`), on the machine only |
| `link-code` | prints a new device string for a server with no address |
| `config print` | prints the merged config with secrets redacted |

Exit codes (`ExitReason`): 0 stopped, 75 restart requested, 78 config refused (including another operator), 79 data folder busy, 80 bind failed; any other code or a signal is a crash.

</details>

<details><summary>Where in the code</summary>

- [rust-client/src/lib.rs](../../../rust-client/src/lib.rs): `Ad4mClient`.
- [rust-client/src/ws_rpc.rs](../../../rust-client/src/ws_rpc.rs): `WsRpcClient` (tokio-tungstenite, `{ id, type, params }`).
- [rust-client/src/agent.rs](../../../rust-client/src/agent.rs): `request_capability`, `generate_jwt` and the agent calls.
- [cli/src/ad4m.rs](../../../cli/src/ad4m.rs): the `ad4m` binary.
- [cli/src/startup.rs](../../../cli/src/startup.rs): `get_cap_token`, `data_path`.
- [cli/src/ad4m_executor.rs](../../../cli/src/ad4m_executor.rs), [cli/src/run_config.rs](../../../cli/src/run_config.rs): the `ad4m-executor` binary, its subcommands and flags.
- [rust-executor/src/exit.rs](../../../rust-executor/src/exit.rs) (new): `ExitReason`.
- [docs-src/pages/developer-guides/cli.mdx](../../../docs-src/pages/developer-guides/cli.mdx).

</details>
