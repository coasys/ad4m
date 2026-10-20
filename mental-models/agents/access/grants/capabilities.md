# Capabilities

A **capability** (`Capability { with: Resource { domain, pointers }, can }`) names one kind of action. It has a domain (`agent`, `perspective`, `language`, `runtime.friends`, `artificial intelligence`, …), the objects in that domain (`pointers`, such as perspective UUIDs, or `*`), and the verbs (`READ`, `CREATE`, `UPDATE`, `DELETE`, `SUBSCRIBE`, …). A grant holds a list of them. A method's guard names the capability it needs, and the call passes if one capability in the grant covers it.

**Why:** Capabilities let an owner give an app less than full access, for example one perspective only. They say what kind of action, not whose data: ownership is a separate rule of the guard ([Guards](../guards.md)). The capabilities sit in the grant, so an approver can narrow them, and the executor caps them at what the approver and the target agent hold.

**Parent:** [Grants](README.md)

<details><summary>How it works</summary>

`capability_matches(grant, expected)` is true if one capability in the grant has:

1. `domain == expected.domain` or `domain == "*"`, and
2. `pointers` contains `*` or every pointer of `expected`, and
3. `can` contains `*` or every verb of `expected`.

Built-in sets:

| Set | Who gets it | Content |
|---|---|---|
| `ALL_CAPABILITY` | the operator role | `{*, [*], [*]}` |
| `anonymous_capabilities()` | no token | `agent/AUTHENTICATE`, `runtime.user_management/READ_ENABLED` and `LOGIN` (the challenge and key login); plus `CREATE` and `VERIFY` on user management in multi-user mode |
| `get_user_default_capabilities()` | a tenant's key login, a password login | agent read/create/update/sign/subscribe and `PERMIT` on its own agent, expressions, languages, perspectives `*`, neighbourhoods, runtime social features, hosting, AI; not `user_management/READ` or `runtime/QUIT` |
| an approved request | an app | the approved subset, within the approver's and the target agent's capabilities |
| the guest set | a guest | the guest's own neighbourhood calls, and `agent:SIGN` / `agent:UPDATE` only for keeping an identity |
| a bot grant's actions | a bot agent's sessions | the action map of [Bot grants](bot-grants.md) |

- `grant_caps_for(agent, approved)` caps a grant at the agent's own capabilities; `caps_within(held, approved)` keeps the per-verb, per-pointer overlap.
- Perspective capabilities take the UUID as pointer: `perspective_query_capability(vec![uuid])`. Each perspective and neighbourhood method passes the UUID or URL it acts on, so a bot grant's pointers bind.
- A user's default set holds `perspective` query, update and delete on `*`; `Guard::Owns` keeps users apart ([Guards](../guards.md)).
- The CLI ([Rust client and CLI](../../interfaces/rust-client-and-cli.md)) and MCP's `request_capability` ([mcp/tools/auth.rs](../../../../rust-executor/src/mcp/tools/auth.rs)) ask for the full capability `{*, [*], [*]}` every time; the grant is capped as above.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/capabilities/types.rs](../../../../rust-executor/src/agent/capabilities/types.rs): `Capability`, `Resource`, `AuthInfo`, `Claims`.
- [rust-executor/src/agent/capabilities/defs.rs](../../../../rust-executor/src/agent/capabilities/defs.rs): every `*_CAPABILITY`, the domain and verb constants, `get_user_default_capabilities`.
- [rust-executor/src/agent/capabilities/mod.rs](../../../../rust-executor/src/agent/capabilities/mod.rs): `capability_matches`, `check_capability`, `anonymous_capabilities`, `grant_caps_for`, `caps_within`.
- [core/src/agent/](../../../../core/src/agent) and [core/src/generated/api/](../../../../core/src/generated/api): SDK types (`Capability`, `AuthInfo`).

</details>
