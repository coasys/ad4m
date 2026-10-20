# MCP server

The **MCP server** exposes the executor to AI agents over the Model Context Protocol (MCP), with the crate `rmcp` over streamable HTTP. Its tools work on perspectives, instances, flows, subscriptions, neighbourhoods, profiles and languages. Each MCP session holds one `Session` (`Caller`), opened by `Session::open` exactly as on RPC, and checked again on every tool call. A tool that does what an RPC method does calls that method through the shared handler map, so the method's [guard](../access/guards.md) applies; the other tools use the perspective guard.

**Why:** AI agents speak MCP. MCP once decoded tokens and checked access with its own code, and the two surfaces drifted: a revoked token still worked, and one write needed different capabilities over MCP and RPC. With one session and one handler map, a rule changes in one place. A fixed, class-agnostic tool set (`instance_*`) keeps the tool list the same whatever social DNA is loaded; per-class tools are generated only with `dynamicClassTools`.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

```
MCP request ─► Caller (one per MCP session)
                ├─ remembered Session passes check()? ─► use it
                ├─ Authorization: Bearer <token>?      ─► Session::open(token, admin, reach), remembered
                ├─ remembered Session dead?            ─► refuse with its AuthError
                └─ no token ─► operator on loopback without a credential, else anonymous
tool ─► wraps an RPC method? ─yes─► call::<T>(method, params) ─► HandlerMap as the caller (guard, then handler)
                             ─no──► Guard::perspective(query | update)
refusals: { "error", "status" }: 401 no session, 403 missing capability, 404 missing or not yours
```

1. A remembered live session wins: an MCP session that logged in keeps that identity when a later request also sends an `Authorization` header, and the header counts only after the remembered session dies (`Caller::resolve`).
2. Login tools set the session's token: `request_capability` + `generate_jwt` ([App approval](../access/grants/app-approval.md); `request_capability` takes `agent` and auto-permits only when the caller owns that agent), `login_email`, `signup`, `request_login_verification` + `verify_email_code`. No MCP tool runs a key login: an AI agent or a bot's harness holds an app grant.
3. `auth_status` reports `principal` (`operator`, `app`, `user`, `anonymous`), `did`, `capabilities` and, for a dead token, the reason.
4. A tool call of a bot agent's session passes the tool's `reason` argument to the bot activity log ([Bot grants](../access/grants/bot-grants.md)).
5. MCP has no RPC connection, so it opens no live query.
6. An interpretation pass calls the same tools in the process with the caller's session; without a caller (the auto-processor) it runs as `Session::in_process`, limited to querying its own perspective.

Listeners: the server runs in its own thread when `enable_mcp` is set, on `mcp_port` (default 3001), with HTTPS on `mcp_port + 1` when TLS is configured. On the desktop it binds `127.0.0.1` only: it follows `localhost` and never the home-network listener.

| Admin credential | TLS | Plain listener | HTTPS listener (`mcp_port + 1`) |
|---|---|---|---|
| none | any | `127.0.0.1` | not started |
| set | not configured | `0.0.0.0` (`127.0.0.1` with `localhost: true`) | not started |
| set | configured, HTTPS bound | `127.0.0.1` | `0.0.0.0` |
| set | configured, HTTPS not bound (port clash, bind error) | as "not configured" | not started |

An explicit `--mcp-host` / `MCP_HOST` sets the plain listener's address; a tokenless call through it, or with a forwarding header, is anonymous.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/mcp/server.rs](../../../rust-executor/src/mcp/server.rs): `McpContext`, `McpServerConfig`, `start_mcp_server`, `resolve_host`, `resolve_plain_host`, the TLS listener.
- [rust-executor/src/mcp/caller.rs](../../../rust-executor/src/mcp/caller.rs) (new): `Caller::{resolve, sign_in}`.
- [rust-executor/src/mcp/tools/mod.rs](../../../rust-executor/src/mcp/tools/mod.rs): `Ad4mMcpHandler`, `call`, `call_method`, `admit_tool_call`, `method_error`, `get_readable_perspective`, `get_writable_perspective`.
- [rust-executor/src/mcp/tools/auth.rs](../../../rust-executor/src/mcp/tools/auth.rs): login and capability tools, `auth_status`.
- [rust-executor/src/mcp/tools/instances/](../../../rust-executor/src/mcp/tools/instances), [dynamic.rs](../../../rust-executor/src/mcp/tools/dynamic.rs): instance tools (see [Perspectives](../../perspectives/README.md)).
- [rust-executor/src/mcp/tools/harness_bridge.rs](../../../rust-executor/src/mcp/tools/harness_bridge.rs): in-process tool calls.
- [rust-executor/src/mcp/shacl.rs](../../../rust-executor/src/mcp/shacl.rs): class shapes for the tools, read by `load_class` and by `load_class_properties_with_uri`.
- [rust-executor/src/mcp/docs/](../../../rust-executor/src/mcp/docs): the `get_documentation` topics.
- [rust-executor/src/mcp/AGENTS.md](../../../rust-executor/src/mcp/AGENTS.md), [docs-src/pages/developer-guides/mcp.mdx](../../../docs-src/pages/developer-guides/mcp.mdx).
- [plugins/](../../../plugins): client-side plugin.

</details>
