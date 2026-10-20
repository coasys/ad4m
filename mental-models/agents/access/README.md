# Access

Access decides what each caller may do on the executor. Each connection opens one **session** from its token or a **key login**: a **principal** (operator, one agent, or anonymous) plus a **grant** (its capabilities). Each RPC method declares a **guard** that makes two **access checks** before its handler runs: a capability covers the call, and the session's agent owns the data.

**Why:** Each call raises four questions: who is this, what kind of action, whose data, and who may hear about it. `Session` answers the first in one place. The method's guard answers the next two, and each event stream the last.

**Parent:** [Agents](../README.md)

## Parts
- [Sessions](sessions.md): how a token becomes a `Session`, and when it stops working.
- [Key login](key-login.md): a login signed by one of the person's device keys, its role table, renewal, and the end of sessions when a key is revoked.
- [Operator and main agent](operator-and-main-agent.md): the operator role, its sources, and the main agent it acts as.
- [Grants](grants/README.md): what a grant may do: capabilities, app approval, bot grants.
- [Guards](guards.md): where RPC, MCP, events and language ops apply the rules.

<details><summary>How it works</summary>

```
token │ key login ──► Session::open ──► Principal  ─┐
                                       caps         │   per request: Session::check()  (revoked? expired?)
                                                    ▼
  guard:    Cap │ Owns │ Executor │ Operator    ── what kind of action, whose data, executor-level calls
  handler:  ctx.acts_as()                     ── the AgentContext to sign, bill, write as
  events:   audience + capability per event   ── who may hear (see Events)
```

| Question | Answered by |
|---|---|
| Who is this? | `Session` / `Principal` (one place) |
| What kind of action? | `Capability`, checked by the method's `Guard` |
| Whose data? | `Guard::Owns`; `Guard::Operator` and `Guard::Executor` for executor-level calls |
| Who may hear? | the audience and capability of each event ([Events](../interfaces/events.md)) |

The MCP server opens the same `Session` and runs the same guards ([Guards](guards.md)). In multi-user mode a user approves apps on their own managed agent; a login gives a fixed capability set (`get_user_default_capabilities`, [Capabilities](grants/capabilities.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Session`, `Principal`, `AuthError`.
- [rust-executor/src/agent/capabilities/](../../../rust-executor/src/agent/capabilities): `Capability`, definitions, JWTs, pending requests.
- `rust-executor/src/agent/grants.rs` (new): the grants store.
- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs), [rust-executor/src/helpers.rs](../../../rust-executor/src/helpers.rs): `Guard`, ownership checks.
- [rust-executor/src/api/events_ws.rs](../../../rust-executor/src/api/events_ws.rs): event filters.
- [rust-executor/src/types/domain.rs](../../../rust-executor/src/types/domain.rs): `RequestContext` (carries the `Session`).
- [docs-src/pages/auth.mdx](../../../docs-src/pages/auth.mdx): the user-facing description.

</details>
