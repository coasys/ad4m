# Executor interfaces

Callers reach the executor through two servers. The **RPC API** is one WebSocket at `/api/v1/ws` for calls, events and live-query updates, plus a few HTTP routes. The **MCP server** offers Model Context Protocol (MCP) tools to AI agents. Client libraries wrap the RPC API: the TypeScript SDK, ad4m-connect, the Rust client and the `ad4m` CLI.

**Why:** One socket per client carries everything, so a client has one connection to authenticate and resume. The TypeScript types are generated from the executor's method contracts, so SDK and executor of one revision cannot drift. MCP sits beside the RPC API because AI agents speak MCP.

**Parent:** [Agents](../README.md)

## Parts
- [RPC API](rpc-api.md): the message format, dispatch and checks, and the HTTP routes.
- [Events](events.md): what the executor pushes on a socket, and who receives each event.
- [MCP server](mcp-server.md): the tools for AI agents, the session and guards they share with RPC, the listeners.
- [TypeScript SDK](typescript-sdk/README.md): `@coasys/ad4m`, with ad4m-connect and model classes below it.
- [Rust client and CLI](rust-client-and-cli.md): `ad4m-client`, the `ad4m` CLI and the `ad4m-executor` binary.

<details><summary>How it works</summary>

| Caller | Library | Endpoint | Token |
|---|---|---|---|
| Web or Node app | `@coasys/ad4m-connect` → `@coasys/ad4m` | `ws(s)://host:12000/api/v1/ws?token=` | key session, app grant or user token |
| Launcher UI | `@coasys/ad4m` | same | admin credential |
| Operator script / CLI | `ad4m` → `ad4m-client` | same | admin credential or app grant |
| AI agent (Claude Code, OpenClaw, …) | any MCP client | `http(s)://host:3001` (TLS on 3002) | admin credential, app grant or user token, via tools or `Authorization` |
| OpenAI-compatible client | any | `/v1/*`, `/api/v1/openai/v1/*` | `Authorization: Bearer` |
| Hosting service | — | `/health` (open), `/internal/shutdown` | `INTERNAL_API_TOKEN` for the shutdown |

Both servers open the caller's `Session` with the same code and run the same guards ([Guards](../access/guards.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/](../../../rust-executor/src/api) and [its AGENTS.md](../../../rust-executor/src/api/AGENTS.md).
- [rust-executor/src/mcp/](../../../rust-executor/src/mcp) and [its AGENTS.md](../../../rust-executor/src/mcp/AGENTS.md).
- [core/](../../../core): `@coasys/ad4m`.
- [connect/](../../../connect): `@coasys/ad4m-connect`.
- [rust-client/](../../../rust-client) and [cli/](../../../cli).
- [ad4m-hooks/](../../../ad4m-hooks): React and Vue hooks over the SDK.
- [dapp/](../../../dapp): the bundled dapp; it builds against the published `@coasys/ad4m` package, not against `core/`.

</details>
