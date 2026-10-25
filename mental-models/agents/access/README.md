# Access

Access decides what each caller may do on the executor. Each connection opens one **session** from its token: a **principal** (operator, one agent, or anonymous) plus its capabilities. An agent's token works only while its **grant** does, and the grant holds its capabilities; the operator holds every capability without a grant, and an anonymous session has no token. Before each handler, the method's **method guard** makes its **access checks**: a capability covers the call (`Cap`, `Executor`, `Owns`), and the session's agent owns the data (`Owns`). `Open` makes neither; `Operator` admits the operator only.

**Why:** Each call raises four questions: who is this, what kind of action, whose data, and who may hear about it. `Session` answers the first in one place. The method guard that each RPC method registers answers the next two; each event names who may hear it.

**Parent:** [Agents](../README.md)

## Parts
- [Sessions](sessions.md): how a token becomes a `Session`, and when it stops working.
- [Operator and main agent](operator-and-main-agent.md): the admin-credential role and the main agent it acts as.
- [Capabilities](capabilities.md): the `Capability` type, the built-in sets, the match rule.
- [App grants](app-grants.md): grants, the allowlist of tokens; how an app gets one when an agent's owner approves; revocation.
- [Access checks](checks.md): where RPC, MCP, events and language ops apply the rules.

<details><summary>How it works</summary>

```
token ──► Session::open ──► Principal  ─┐
                           grant, caps  │   per request: Session::check()  (grant revoked? expired?)
                                        ▼
  dispatch: Guard (registered per method)     ── Open | Operator | Cap | Executor | Owns
            capability (403)                  ── what kind of action
            Owns: the owner of the resource   ── whose data (404, missing or not)
            Operator / Executor (403)         ── executor-level calls
  handler:  ctx.acts_as()                     ── the AgentContext to sign, bill, write as
  events:   Event audience + capability      ── who may hear (see Events)
```

| Question | Answered by |
|---|---|
| Who is this? | `Session` / `Principal` (one place) |
| What kind of action? | `Capability`, checked by the method's `Guard` |
| Whose data? | the `Guard`: `Owns` (`guards::owns`, `can_access_perspective_with_did`), `Executor`, `Operator` |
| Who may hear? | the audience and capability of each event, checked by `Session::may_receive` ([Events](../interfaces/events.md)) |

The MCP server opens one `Session` per MCP session with the same `Session::open`, and its tools run the same guards ([Access checks](checks.md)). In multi-user mode a login gives a fixed capability set (`get_user_default_capabilities`, [Capabilities](capabilities.md)), and a user approves apps for its own agent ([App grants](app-grants.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Session`, `Principal`, `AuthError`.
- [rust-executor/src/agent/capabilities/](../../../rust-executor/src/agent/capabilities): `Capability`, definitions, JWTs, capability requests.
- [rust-executor/src/agent/grants.rs](../../../rust-executor/src/agent/grants.rs): grants.
- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs): `Guard`, `Owner`, `owns`; [rust-executor/src/helpers.rs](../../../rust-executor/src/helpers.rs): the perspective ownership rule.
- [rust-executor/src/events.rs](../../../rust-executor/src/events.rs): event audiences and capabilities.
- [rust-executor/src/types/domain.rs](../../../rust-executor/src/types/domain.rs): `RequestContext` (carries the `Session`).
- [docs-src/pages/auth.mdx](../../../docs-src/pages/auth.mdx): the user-facing description.

</details>
