# MCP server

The **MCP server** exposes the executor to AI agents over the Model Context Protocol (MCP), with the crate `rmcp` over streamable HTTP. Its tools work on perspectives, instances, flows, subscriptions, neighbourhoods, profiles and languages. The tools call `PerspectiveInstance` and the executor services directly, not the RPC handlers. Each MCP session keeps its own token, and MCP checks it with its own code, not with `Session`.

**Why:** AI agents speak MCP. A fixed, class-agnostic tool set (`instance_*`) keeps the tool list the same whatever social DNA is loaded. Per-class tools are generated only with `dynamicClassTools`.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

Authentication (per MCP session, `McpContext.auth_token`):

1. The client logs in through tools, and the MCP session stores the token. The tools are `request_capability` + `generate_jwt` (the capability-request flow of [App grants](../access/app-grants.md)), `login_email`, or `signup` / `request_login_verification` + `verify_email_code` (managed agent).
2. Or the client sends `Authorization: Bearer <admin credential or JWT>`. The session stores a valid one (`adopt_credential`).
3. `check_auth` runs before every tool outside `AUTH_TOOLS`. It passes in three cases: the session token is the admin credential or a valid JWT; the header credential is valid; or the executor has no admin credential and the caller sent no token at all.
4. `request_capability` always asks for `{*, [*], [*]}`. It returns the code inline only if the caller may permit (`caller_can_permit`: the admin credential, or a token with `agent/PERMIT`).
5. Tools resolve the agent with `AgentContext::from_auth_token` and the capabilities with `capabilities_from_token`. Perspective access goes through `get_readable_perspective` / `get_writable_perspective` ([Access checks](../access/checks.md)).

Listeners: the server runs in its own thread when `enable_mcp` is set, on `mcp_port` (default 3001). It adds HTTPS on `mcp_port + 1` when TLS is configured.

Binding (`resolve_host`, `resolve_plain_host`, `start_tls_listener`), with no `--mcp-host` / `MCP_HOST`:

| Admin credential | TLS | Plain listener | HTTPS listener (`mcp_port + 1`) |
|---|---|---|---|
| none | any | `127.0.0.1` | not started |
| set | not configured | `0.0.0.0` | not started |
| set | configured, HTTPS bound | `127.0.0.1` | `0.0.0.0` |
| set | configured, HTTPS not bound (port clash with the RPC HTTPS port, no port above, bind error) | `0.0.0.0` | not started |

An explicit `--mcp-host` / `MCP_HOST` sets the plain listener's address in every row; without a credential and off loopback it only logs a warning. The HTTPS listener always binds `0.0.0.0` (`resolve_host`, `start_tls_listener` in [server.rs](../../../rust-executor/src/mcp/server.rs)).

The interpretation harness calls the same tools in the process (`mcp/tools/harness_bridge.rs`), with the dynamic tools included.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/mcp/server.rs](../../../rust-executor/src/mcp/server.rs): `McpContext`, `McpServerConfig`, `start_mcp_server`, `resolve_host`, `resolve_plain_host`, the TLS listener.
- [rust-executor/src/mcp/tools/mod.rs](../../../rust-executor/src/mcp/tools/mod.rs): `Ad4mMcpHandler`, `AUTH_TOOLS`, `check_auth`, `adopt_credential`, `get_agent_context`, `get_capabilities`, `caller_can_permit`.
- [rust-executor/src/mcp/tools/auth.rs](../../../rust-executor/src/mcp/tools/auth.rs): login and capability tools, `auth_status`.
- [rust-executor/src/mcp/tools/instances/](../../../rust-executor/src/mcp/tools/instances), [dynamic.rs](../../../rust-executor/src/mcp/tools/dynamic.rs): instance tools (see [Perspectives](../../perspectives/README.md)).
- [rust-executor/src/mcp/tools/harness_bridge.rs](../../../rust-executor/src/mcp/tools/harness_bridge.rs): in-process tool calls.
- [rust-executor/src/mcp/shacl.rs](../../../rust-executor/src/mcp/shacl.rs): class shapes for the tools, read by `load_class` and by `load_class_properties_with_uri`.
- [rust-executor/src/mcp/docs/](../../../rust-executor/src/mcp/docs): the `get_documentation` topics.
- [rust-executor/src/mcp/AGENTS.md](../../../rust-executor/src/mcp/AGENTS.md), [docs-src/pages/developer-guides/mcp.mdx](../../../docs-src/pages/developer-guides/mcp.mdx).
- [plugins/](../../../plugins): client-side plugin.

</details>
