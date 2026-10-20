# Executor interfaces

Callers reach the executor through two servers. The **RPC API** is one WebSocket at `/api/v1/ws` for calls, events and live-query updates, plus a few HTTP routes; it listens on loopback, on an optional TLS address, and, on a desktop with the setting on, on the **home-network listener**. The **MCP server** offers Model Context Protocol (MCP) tools to AI agents and runs each tool as the RPC method it wraps. Both open the caller's `Session` the same way ([Sessions](../access/sessions.md)). Client libraries wrap the RPC API: the TypeScript SDK, ad4m-connect (with Coasys Connect for web apps), the Rust client and the `ad4m` CLI.

**Why:** One socket per client carries everything, so a client has one connection to authenticate, renew and resume. The TypeScript types and the guard table are generated from the executor's method registrations, so SDK, docs and executor of one revision cannot drift. MCP sits beside the RPC API because AI agents speak MCP, but it shares the RPC handler map, so the two surfaces cannot disagree on who may do what. No executor forwards another executor's calls: an app that works on a second device opens its own session there.

**Parent:** [Agents](../README.md)

## Parts
- [RPC API](rpc-api.md): the message format, the namespaces, dispatch and guards, the listeners, and the HTTP routes.
- [Events](events.md): typed events with a topic, an audience and a capability, and who receives each one.
- [MCP server](mcp-server.md): the tools for AI agents on the same session and guards, and their listeners.
- [TypeScript SDK](typescript-sdk/README.md): `@coasys/ad4m`, with ad4m-connect and model classes below it.
- [Rust client and CLI](rust-client-and-cli.md): `ad4m-client`, the `ad4m` CLI and the `ad4m-executor` binary.

<details><summary>How it works</summary>

| Caller | Library | Endpoint | Token |
|---|---|---|---|
| Desktop main window (WE) | `@coasys/ad4m` | loopback `ws://127.0.0.1:<port>/api/v1/ws?token=` | local session from the shell, 1 h |
| Desktop shell (WE) | its own client | loopback, the shell's channel | admin credential |
| Phone app (WE) | `@coasys/ad4m` over the phone shell's pinned transport | a host's `wss` address, or the home-network listener | key session, 1 h |
| Web app, third-party web app | `@coasys/ad4m-connect` (Coasys Connect) → `@coasys/ad4m` | the executor's `wss` address | key session through the auth domain, 1 h, memory only; a guest's token in `localStorage` |
| Local tool, old launcher | `@coasys/ad4m-connect` (six-digit code) | loopback | app grant |
| Operator script / CLI | `ad4m` → `ad4m-client` | loopback | admin credential or app grant |
| AI agent, bot harness | any MCP client | `http(s)://host:3001` (TLS on 3002); loopback only on the desktop | admin credential, app grant or user token |
| OpenAI-compatible client | any | `/v1/*`, `/api/v1/openai/v1/*` | `Authorization: Bearer` |
| Hosting service, supervisor | — | `/health` (open), `/internal/shutdown` | `INTERNAL_API_TOKEN` for the shutdown |

The chrome's session broker (in WE) holds one serving session per app, plus at most one side session on another device; Switch device opens a new key session on the target and swaps the client ([Key login](../access/key-login.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/](../../../rust-executor/src/api) and [its AGENTS.md](../../../rust-executor/src/api/AGENTS.md) (with the generated guard table).
- [rust-executor/src/mcp/](../../../rust-executor/src/mcp) and [its AGENTS.md](../../../rust-executor/src/mcp/AGENTS.md).
- [rust-executor/src/lan_listener.rs](../../../rust-executor/src/lan_listener.rs) (new): the home-network listener.
- [core/](../../../core): `@coasys/ad4m`.
- [connect/](../../../connect): `@coasys/ad4m-connect`.
- [rust-client/](../../../rust-client) and [cli/](../../../cli).
- [ad4m-hooks/](../../../ad4m-hooks): React and Vue hooks over the SDK.
- [dapp/](../../../dapp): the bundled dapp; it builds against the published `@coasys/ad4m` package, not against `core/`.

</details>
