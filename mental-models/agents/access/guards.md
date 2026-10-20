# Guards

A **guard** (`Guard`) is the access rule that each RPC method declares where it registers. The dispatcher runs it before the handler: first the capability, then the rule of the variant. `Open` checks nothing, `Operator` admits the operator role only, `Cap(c)` admits any session that holds `c`, `Executor(c)` admits the operator and grants on the main agent, and `Owns` admits the owner of a perspective, an AI task, a grant or a capability request. A method can also carry `.local_only()`. MCP tools call the same methods through the same handler map, so the same guards apply. Events check their own audience and capability, and language ops act as the agent of the running call.

**Why:** One question per method: does it act on the executor, or on something an agent owns? A guard on the registration answers it once, so a handler cannot forget it. A method without a guard stops the executor at start, and a test calls every method as every kind of caller. A resource that exists but is not the caller's answers 404, the same as a missing one, so a refusal does not reveal that it exists. The hand-written checks in each handler, and MCP's own helpers, lost: each surface had drifted from the others.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

```
request ─► method known? ─404─► params parse? ─400─► guard capability ─403─► guard rule ─► local_only? ─► handler
                                                                             Operator, Executor: 403
                                                                             Owns: 404 "… not found"
                                                     .or_null(): 404 → null    .local_only(): 403 local-only
start-up: HandlerMap::require_guards() ─► a method without a guard ─► the executor exits
```

| Guard | Operator | Grant on the main agent | Grant on a managed agent | Anonymous |
|---|---|---|---|---|
| `Open` | ✓ | ✓ | ✓ | ✓ |
| `Operator` | ✓ | 403 | 403 | 403 |
| `Cap(c)` | ✓ | holds `c` | holds `c` | `c` in the anonymous set |
| `Executor(c)` | ✓ | holds `c` | 403 | 403 |
| `Owns { Perspective \| Task \| Grant \| Request }` | every one | the main agent's (and unowned perspectives) | its own | 403 |
| `.local_only()` (after the guard) | credential or loopback only | 403 | 403 | 403 |

| Surface | Rule |
|---|---|
| RPC method | its guard; then, for a bot agent's session, the bot grant ([Bot grants](grants/bot-grants.md)) |
| MCP tool that does what an RPC method does | that method's guard, through the shared handler map |
| MCP tool with no RPC method (`instance_*`, `describe_perspective`, flows, per-class tools) | `Guard::perspective(query \| update)` |
| Event ([Events](../interfaces/events.md)) | `Session::may_receive`: in the event's audience and holds its capability |
| Live query | at subscribe (`Owns(Perspective)`); updates go to the subscribing connection only |
| Language ops (JS) | sign as `get_runtime_agent_context()`; a language acts only for its own address and cannot publish executor events |
| Interpretation pass | the caller's session; without a caller, `Session::in_process` limited to querying its own perspective, never the operator |

Executor-wide reads are `Cap`, so a user's session reaches them: `agent.getTrustedAgents`, `ai.models`, the language reads, link-language templates, `runtime.hcAgentInfos`, `networkMetrics`, `hostRates`. SFU rooms have no `Owner`: the `sfu.*` methods are `Cap`, and their handlers check room membership ([sfu_ws.rs](../../../rust-executor/src/api/sfu_ws.rs)).

The guard table in `api/AGENTS.md` is generated from the handler map, with a column for `.local_only()`, and a test fails when it is stale.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs): `Guard`, `Owner`, `IdParam`, `Guard::admit`, `owns`.
- [rust-executor/src/api/ws_handler.rs](../../../rust-executor/src/api/ws_handler.rs): `MethodFlags::{guard, or_null, local_only}`, `HandlerMap::{admit, require_guards, guards}`, `shared_handler_map`.
- [rust-executor/src/helpers.rs](../../../rust-executor/src/helpers.rs): `can_access_perspective_with_did`, the ownership rule `owns` applies to perspectives.
- [rust-executor/src/api/AGENTS.md](../../../rust-executor/src/api/AGENTS.md): the generated guard table.
- [rust-executor/src/api/tests/](../../../rust-executor/src/api/tests): `method_matrix_tests.rs` and `method_matrix_mcp_tests.rs` (new), every method and tool as every caller.
- [rust-executor/src/mcp/tools/mod.rs](../../../rust-executor/src/mcp/tools/mod.rs): `call_method`, `get_readable_perspective`, `get_writable_perspective`.
- [rust-executor/src/js_core/agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs), [languages_extension.rs](../../../rust-executor/src/js_core/languages_extension.rs), [pubsub_extension.rs](../../../rust-executor/src/js_core/pubsub_extension.rs): language ops.
- [rust-executor/src/runtime_service/notification_access.rs](../../../rust-executor/src/runtime_service/notification_access.rs): notification grants.

</details>
