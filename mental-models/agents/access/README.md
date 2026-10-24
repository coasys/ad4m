# Access

Access decides what each caller may do on the executor. Each connection opens one **session** from its token: a **principal** (operator, one agent, or anonymous) plus a **grant** (its capabilities). Each handler then makes two **access checks**: a capability covers the call, and the session's agent owns the data.

**Why:** Each call raises four questions: who is this, what kind of action, whose data, and who may hear about it. `Session` answers the first in one place. Each handler and event stream answers the other three by itself.

**Parent:** [Agents](../README.md)

## Parts
- [Sessions](sessions.md): how a token becomes a `Session`, and when it stops working.
- [Operator and main agent](operator-and-main-agent.md): the admin-credential role and the main agent it acts as.
- [Capabilities](capabilities.md): the `Capability` type, the built-in sets, the match rule.
- [App grants](app-grants.md): how an app gets a JWT through operator approval; revocation.
- [Access checks](checks.md): where RPC, MCP, events and language ops apply the rules.

<details><summary>How it works</summary>

```
token ──► Session::open ──► Principal  ─┐
                           caps         │   per request: Session::check()  (revoked? expired?)
                                        ▼
  handler:  ctx.require(&CAPABILITY)          ── what kind of action
            get_perspective_with_access(uuid) ── whose data (owner DID)
            refuse_user_session / is_operator ── executor-level calls
            ctx.acts_as()                     ── the AgentContext to sign, bill, write as
  events:   per-socket filters in events_ws   ── who may hear (see Events)
```

| Question | Answered by |
|---|---|
| Who is this? | `Session` / `Principal` (one place) |
| What kind of action? | `Capability`, checked in each handler (`ctx.require`) |
| Whose data? | `get_perspective_with_access`, `can_access_perspective_with_did`, `refuse_user_session`, `is_operator` in each handler |
| Who may hear? | filters per event stream in `events_ws.rs` ([Events](../interfaces/events.md)) |

The MCP server does not open a `Session`: it decodes tokens with its own code ([Access checks](checks.md)). In multi-user mode a user approves no apps: a login gives a fixed capability set (`get_user_default_capabilities`, [Capabilities](capabilities.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Session`, `Principal`, `AuthError`.
- [rust-executor/src/agent/capabilities/](../../../rust-executor/src/agent/capabilities): `Capability`, definitions, JWTs, apps registry.
- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs), [rust-executor/src/helpers.rs](../../../rust-executor/src/helpers.rs): ownership checks.
- [rust-executor/src/api/events_ws.rs](../../../rust-executor/src/api/events_ws.rs): event filters.
- [rust-executor/src/types/domain.rs](../../../rust-executor/src/types/domain.rs): `RequestContext` (carries the `Session`).
- [docs-src/pages/auth.mdx](../../../docs-src/pages/auth.mdx): the user-facing description.

</details>
