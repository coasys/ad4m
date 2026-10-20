# Access

Access decides what each caller may do on the executor. Each connection opens one **session** from its token. A token opens a session only while its **grant** is active in the **grants store**. The grant names the agent the token acts as, its capabilities, and, after a **key login**, the person's identity and the device key that signed. The operator role goes to the identity in the executor's **operator record**. Each RPC method declares a **guard**, and the dispatcher runs it before the handler.

**Why:** Each call raises four questions: who is this, what kind of action, whose data, and who may hear about it. The session answers the first once per connection. The guard answers the next two once per method, at registration. The event answers the last, because its publisher names the audience. The rights follow the person's identity, not the machine: any device key in the person's key log can log in, and a revoked key ends its sessions. The design extends the session layer that already exists (one session, one principal, one grants store, one guard per call) and adds no second system beside it.

**Parent:** [Agents](../README.md)

## Parts
- [Sessions](sessions.md): how a token becomes a `Session`, the grants store, token lifetimes, and how a session ends.
- [Key login](key-login.md): a login signed by one of the person's device keys, its role table, renewal, and the end of sessions when a key is revoked.
- [Operator and main agent](operator-and-main-agent.md): the operator record, the four sources of the operator role, the desktop's local session, and the calls that stay on the machine.
- [Grants](grants/README.md): what a grant may do: capabilities, app approval, bot grants.
- [Guards](guards.md): the guard of each RPC method and MCP tool, and the checks on events and language ops.

<details><summary>How it works</summary>

```
token ──► Session::open ──► grants store: grant active? ──► Principal + caps (+ key binding)
            │                 key-bound grants: re-checked against the key index every 5 min
            ▼
 per request:  Session::check()          grant still active, token not past exp
               guard of the method        Open │ Operator │ Cap │ Executor │ Owns  (+ .local_only())
               Session::may              a bot agent's caps ∩ its bot grant
               handler: ctx.acts_as()    the AgentContext to sign, bill, write as
 per event:    Session::may_receive(e)   in e.audience and holds e.capability (see Events)
```

| Question | Answered by |
|---|---|
| Who is this? | `Session` from its grant ([Sessions](sessions.md)); for a key login, the identity and role ([Key login](key-login.md)) |
| May this caller run this method? | the method's `Guard` and `.local_only()` flag ([Guards](guards.md)) |
| Whose data? | `Guard::Owns` (perspective, AI task, grant, capability request) |
| Who may hear? | the event's `Audience` and capability, set by its publisher ([Events](../interfaces/events.md)) |

| Caller | Principal | Grant |
|---|---|---|
| desktop shell, CLI (admin credential) | `Operator` (credential) | none |
| desktop main window | `Operator` (local) | local session, 1 h |
| the person's own app, logged in by key as the operator | `Operator` (key) | key-bound, 1 h |
| a tenant's app, logged in by key | `Agent(Managed)` | key-bound, 1 h |
| a third-party app on the operator's executor | `Agent(Main)` | approved capabilities |
| a password login, a guest | `Agent(Managed)` | user grant, guest grant |
| no token | `Anonymous` (or `Operator` on loopback with no credential) | none |

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Session`, `Principal`, `AuthError`, `OperatorSource`.
- [rust-executor/src/agent/grants.rs](../../../rust-executor/src/agent/grants.rs) (new), [rust-executor/src/db/grants.rs](../../../rust-executor/src/db/grants.rs) (new): the grants store.
- [rust-executor/src/agent/capabilities/](../../../rust-executor/src/agent/capabilities): `Capability`, definitions, tokens, pending requests.
- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs): `Guard`, `Owner`, `owns`.
- [rust-executor/src/identity/resolver.rs](../../../rust-executor/src/identity/resolver.rs) (new): `KeyResolver`, which key login and bot grants read ([Key-log cache and resolver](../identity/key-log/cache.md)).
- [rust-executor/src/events.rs](../../../rust-executor/src/events.rs) (new): `Event`, `Audience`, `Topic`.
- [docs-src/pages/auth.mdx](../../../docs-src/pages/auth.mdx): the user-facing description.

</details>
