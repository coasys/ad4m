# Access checks

An **access check** applies a session's grant to one call: a capability check plus an ownership check. On RPC, each method registers one **method guard** (`Guard`), and dispatch runs it before the handler. MCP tools run the same guards, each event names its audience and capability, and language ops take the agent from the language runtime. This page lists where each check lives, so that a reader can study one surface at a time.

**Why:** A guard declared where the method registers makes every method state who may call it, and the executor does not start with a method that has none. Missing and not yours give one answer, 404, so a caller cannot learn which ids exist.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

`HandlerMap::dispatch`: method (404) → params fit the contract (400) → the guard's capability, for `Cap`, `Executor` and `Owns` (403) → the guard's rule → the handler.

| Guard | Rule after the capability |
|---|---|
| `Open` | none, and no capability: the handler decides (`runtime.info`, `user.logout`) |
| `Operator` | the operator only, else 403; no capability check |
| `Cap(cap)` | none: any session that holds `cap`; the handler acts as `ctx.acts_as()` |
| `Executor(cap)` | the operator or a grant on the main agent, else 403 (calls on the executor itself) |
| `Owns { cap, id, owner }` | the resource that `id` names exists and the session's agent owns it (`guards::owns`), else 404; the operator reaches all |

`Owner` kinds: `Perspective` (its `owners`; none: the main agent), `Task` (`tasks.agent_did`; none: the main agent), `Grant` (`agent.revokeToken`, `agent.removeApp`), `Request` (a pending capability request, `agent.permitCapability`). A method flagged `.or_null()` (`perspective.get`, `perspective.snapshot`) answers `null` in place of 404.

| Surface | Capability check | Ownership check | Denied answer |
|---|---|---|---|
| RPC method | its `Guard` | its `Guard` (`Owns` → `get_perspective_with_access` → `can_access_perspective_with_did` for a perspective) | 403, or 404 for a resource |
| MCP tool that wraps an RPC method | the method's guard, through the shared handler map (`shared_handler_map`) | the same | the method's status, as JSON (`method_error`) |
| MCP tool without an RPC method | `Guard::perspective(perspective_query_capability)` to read, `perspective_update_capability` to write | the same guard (`get_readable_perspective`, `get_writable_perspective`) | the same |
| Event stream ([Events](../interfaces/events.md)) | any authenticated session; per event `Event::capability` (`Topic::capability`) | the event's `Audience`, which the publisher names; `Session::may_receive` | event not sent |
| Live query updates | at subscribe | the subscribing RPC connection only | — |
| Language ops (JS) | none: language code is trusted with the ops; it cannot publish executor events | signs as `get_runtime_agent_context()`; `agent_create_signed_expression_for_user` checks the runtime's user | error |

Ownership rule (`can_access_perspective_with_did`):

- A managed agent reaches only perspectives whose handle names its DID as owner.
- The main agent reaches unowned perspectives and those it owns.
- The operator skips the check.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs): `Guard`, `Owner`, `IdParam`, `owns`, `OPERATOR_ONLY`.
- [rust-executor/src/api/ws_handler.rs](../../../rust-executor/src/api/ws_handler.rs): `HandlerMap::dispatch`, `admit`, `require_guards`, `shared_handler_map`.
- [rust-executor/src/api/perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs): `get_perspective_with_access`, `guarded_perspective`, `check_credits`.
- [rust-executor/src/helpers.rs](../../../rust-executor/src/helpers.rs): `can_access_perspective`, `can_access_perspective_with_did`.
- [rust-executor/src/mcp/tools/mod.rs](../../../rust-executor/src/mcp/tools/mod.rs): `call_method`, `admit_method`, `get_readable_perspective`, `get_writable_perspective`, `method_error`.
- [rust-executor/src/events.rs](../../../rust-executor/src/events.rs): `Topic::capability`, `Audience`; [rust-executor/src/api/events_ws.rs](../../../rust-executor/src/api/events_ws.rs): `build_event_stream`; [event_interest.rs](../../../rust-executor/src/api/event_interest.rs): `events.watch`.
- [rust-executor/src/runtime_service/notification_access.rs](../../../rust-executor/src/runtime_service/notification_access.rs): notification grants.
- [rust-executor/src/js_core/agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs): agent ops for language code; [pubsub_extension.rs](../../../rust-executor/src/js_core/pubsub_extension.rs): the refused publish op.
- [rust-executor/src/api/AGENTS.md](../../../rust-executor/src/api/AGENTS.md): the generated guard table of every method; [rust-executor/src/mcp/AGENTS.md](../../../rust-executor/src/mcp/AGENTS.md): the MCP rules.
- [rust-executor/src/api/tests/method_matrix_tests.rs](../../../rust-executor/src/api/tests/method_matrix_tests.rs), [method_matrix_mcp_tests.rs](../../../rust-executor/src/api/tests/method_matrix_mcp_tests.rs): every method and wrapping tool as each kind of caller.

</details>
