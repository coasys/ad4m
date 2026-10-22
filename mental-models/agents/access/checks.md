# Access checks

An **access check** applies a session's grant to one call: a capability check plus an ownership check. No central guard exists. Each RPC handler checks by hand, MCP tools use their own helpers, each event stream has its own filter, and language ops take the agent from the language runtime. This page lists where each check lives, so that a reader can study one surface at a time.

**Why:** The checks grew with each surface (RPC, MCP, events, language host). Each new surface added its own checks beside its handlers.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

| Surface | Capability check | Ownership check | Denied answer |
|---|---|---|---|
| RPC handler (`*_ws.rs`) | `ctx.require(&X_CAPABILITY)` in each handler | `get_perspective_with_access(uuid, ctx)` → `can_access_perspective_with_did` | 404 if no such perspective, 403 if not owned |
| RPC executor-level calls | as above | `refuse_user_session(ctx, name)`, or `ctx.is_operator()` | 403 |
| MCP read tools | none beyond a valid token | `get_readable_perspective` → `can_access_perspective` | 404-shaped JSON |
| MCP write tools | `PERSPECTIVE_CREATE_CAPABILITY` for every write | `get_writable_perspective` → `get_perspective_with_auth` | 404-shaped JSON |
| Event stream ([Events](../interfaces/events.md)) | any authenticated session; per stream only `AGENT_PERMIT` (`apps-changed`) and `NEIGHBOURHOOD_READ` (SFU events) | owner DID filters in `events_ws.rs`; `notification_access.rs` for notifications | event not sent |
| Live query updates | at subscribe | the subscribing RPC connection only | — |
| Language ops (JS) | none: language code is trusted with the ops | signs as `get_runtime_agent_context()`; `agent_create_signed_expression_for_user` checks the runtime's user | error |

MCP and RPC check different capabilities for the same write. MCP writes check `perspective/CREATE` on `*` (`get_writable_perspective`). RPC writes check `perspective/UPDATE` on the perspective's UUID (`add_link` in [perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs)). This page owns the MCP and RPC rules.

Ownership rule (`can_access_perspective_with_did`):

- A managed agent reaches only perspectives whose handle names its DID as owner.
- The main agent reaches unowned perspectives and those it owns.
- The operator skips the check.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs): `get_perspective_with_access`, `check_credits`, the representative handler shape (`add_link`).
- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs): `refuse_user_session`.
- [rust-executor/src/helpers.rs](../../../rust-executor/src/helpers.rs): `can_access_perspective`, `can_access_perspective_with_did`.
- [rust-executor/src/mcp/tools/mod.rs](../../../rust-executor/src/mcp/tools/mod.rs): `check_auth`, `get_readable_perspective`, `get_writable_perspective`, `get_perspective_with_auth`, `caller_can_permit`.
- [rust-executor/src/api/events_ws.rs](../../../rust-executor/src/api/events_ws.rs): `build_event_stream` and its filters; [event_interest.rs](../../../rust-executor/src/api/event_interest.rs): `events.watch`.
- [rust-executor/src/runtime_service/notification_access.rs](../../../rust-executor/src/runtime_service/notification_access.rs): notification grants.
- [rust-executor/src/js_core/agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs): agent ops for language code.
- [rust-executor/src/api/AGENTS.md](../../../rust-executor/src/api/AGENTS.md), [rust-executor/src/mcp/AGENTS.md](../../../rust-executor/src/mcp/AGENTS.md): the rules each surface states.

</details>
