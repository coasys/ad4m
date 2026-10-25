# MCP server

The **MCP server** exposes the executor to AI agents over the Model Context Protocol (MCP), with the crate `rmcp` over streamable HTTP. Its tools work on perspectives, instances, flows, subscriptions, neighbourhoods, profiles and languages. A tool that does the job of an RPC method calls that method through the shared handler map, so the method's guard decides; the other tools call `PerspectiveInstance` directly. Each MCP session holds one `Session`, opened by `Session::open` as on RPC.

**Why:** AI agents speak MCP. A fixed, class-agnostic tool set (`instance_*`) keeps the tool list the same whatever social DNA is loaded. Per-class tools are generated only with `dynamicClassTools`.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

Authentication (per MCP session, `McpContext.caller`, a `Caller`):

1. The client logs in through tools, and the MCP session keeps the `Session` the token opens. The tools are `request_capability` + `generate_jwt` (the capability-request flow of [App grants](../access/app-grants.md)), `login_email`, or `signup` / `request_login_verification` + `verify_email_code` (managed agent).
2. Or the client sends `Authorization: Bearer <admin credential or JWT>` with its requests, `tools/list` included. A token that opens a session is remembered. A remembered session that still works wins over a later header; a header replaces one that stopped working.
3. With no token, `Session::open("")` runs again on every call: the operator on a loopback listener of an executor without an admin credential, anonymous otherwise. A request with a forwarding header reads as `Network`.
4. `call_tool` resolves the caller on every call (`Caller::resolve`, which runs `Session::check`). A tool outside `AUTH_TOOLS` needs a working session that is not anonymous.
5. `request_capability` always asks for `{*, [*], [*]}`. It returns the code inline only if `agent.permitCapability`'s guard admits the caller: the operator, or a session that holds `agent/PERMIT` and acts as the requested agent. The approval is capped at the caller's capabilities.
6. Tools read the agent from the session (`get_agent_context`). Tools without an RPC method reach perspectives through `get_readable_perspective` / `get_writable_perspective`, which run the perspective guards ([Access checks](../access/checks.md)).

Listeners: the server runs in its own thread when `enable_mcp` is set, on `mcp_port` (default 3001). It adds HTTPS on `mcp_port + 1` when TLS is configured.

Binding (`resolve_host`, `resolve_plain_host`, `start_tls_listener`), with no `MCP_HOST`:

| Admin credential | TLS | Plain listener | HTTPS listener (`mcp_port + 1`) |
|---|---|---|---|
| none | any | `127.0.0.1` | not started |
| set | not configured | `0.0.0.0` | not started |
| set | configured, HTTPS bound | `127.0.0.1` | `0.0.0.0` |
| set | configured, HTTPS not bound (port clash with the RPC HTTPS port, no port above, bind error) | `0.0.0.0` | not started |

An explicit `MCP_HOST` environment variable sets the plain listener's address in every row; without a credential and off loopback it only logs a warning. The HTTPS listener always binds `0.0.0.0` (`resolve_host`, `start_tls_listener` in [server.rs](../../../rust-executor/src/mcp/server.rs)).

The interpretation harness calls the same tools in the process (`mcp/tools/harness_bridge.rs`), with the dynamic tools included.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/mcp/server.rs](../../../rust-executor/src/mcp/server.rs): `McpContext`, `McpServerConfig`, `start_mcp_server`, `resolve_host`, `resolve_plain_host`, the TLS listener.
- [rust-executor/src/mcp/caller.rs](../../../rust-executor/src/mcp/caller.rs): `Caller` (`resolve`, `sign_in`).
- [rust-executor/src/mcp/tools/mod.rs](../../../rust-executor/src/mcp/tools/mod.rs): `Ad4mMcpHandler`, `AUTH_TOOLS`, `admit_tool_call`, `listed_tools`, `session`, `get_agent_context`, `call_method`, `admit_method`.
- [rust-executor/src/mcp/tools/auth.rs](../../../rust-executor/src/mcp/tools/auth.rs): login and capability tools, `auth_status`.
- [rust-executor/src/mcp/tools/instances/](../../../rust-executor/src/mcp/tools/instances), [dynamic.rs](../../../rust-executor/src/mcp/tools/dynamic.rs): instance tools (see [Perspectives](../../perspectives/README.md)).
- [rust-executor/src/mcp/tools/harness_bridge.rs](../../../rust-executor/src/mcp/tools/harness_bridge.rs): in-process tool calls.
- [rust-executor/src/mcp/shacl.rs](../../../rust-executor/src/mcp/shacl.rs): class shapes for the tools, read by `load_class` and by `load_class_properties_with_uri`.
- [rust-executor/src/mcp/docs/](../../../rust-executor/src/mcp/docs): the `get_documentation` topics.
- [rust-executor/src/mcp/AGENTS.md](../../../rust-executor/src/mcp/AGENTS.md), [docs-src/pages/developer-guides/mcp.mdx](../../../docs-src/pages/developer-guides/mcp.mdx).
- [plugins/](../../../plugins): client-side plugin.

</details>
