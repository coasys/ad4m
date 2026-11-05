# Capabilities

A **capability** (`Capability { with: Resource { domain, pointers }, can }`) names one kind of action. It has a domain (`agent`, `perspective`, `language`, `runtime.friends`, `artificial intelligence`, …), the objects in that domain (`pointers`, such as perspective UUIDs, or `*`), and the verbs (`READ`, `CREATE`, `UPDATE`, `DELETE`, `SUBSCRIBE`, …). A session's capabilities come from its token's **grant** ([App grants](app-grants.md)). A method's guard names one capability, and the call passes if one of the session's capabilities covers it ([Access checks](checks.md)).

**Why:** Capabilities let the owner of an agent give an app less than full access, for example one perspective only. The grant, not the JWT, holds them, so a revoked grant takes them away at once.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

`capability_matches(caps, expected)` is true if one capability in `caps` has:

1. `domain == expected.domain` or `domain == "*"`, and
2. `pointers` contains `*` or every pointer of `expected`, and
3. `can` contains `*` or every verb of `expected`.

Built-in sets:

| Set | Who gets it | Content |
|---|---|---|
| `ALL_CAPABILITY` | operator | `{*, [*], [*]}` |
| `anonymous_capabilities()` | no token | `agent/AUTHENTICATE`, `runtime.user_management/READ` on pointer `enabled`; plus `CREATE`, `LOGIN`, `VERIFY` on user management when multi-user mode is on |
| `get_user_default_capabilities()` | every managed-agent login | agent read/create/update/sign/subscribe, expressions, languages, perspectives `*`, neighbourhoods, runtime social features, hosting, AI read/create/prompt/transcribe; `agent/PERMIT`, for requests on the user's own agent; not `user_management/READ` or `runtime/QUIT` |
| the app's request | an approved app | what the approver approved, within the approver's own and, on a managed agent, within that user's |
| a grant whose app asked for none | — | `agent/AUTHENTICATE` only |

Perspective capabilities take the UUID as pointer: `perspective_query_capability(vec![uuid])`, `perspective_update_capability(vec![uuid])`.

A capability names a kind of action, not whose data. A user's default set holds `perspective` query, update and delete on `*`; the ownership rule of each method's guard keeps users apart ([Access checks](checks.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/capabilities/types.rs](../../../rust-executor/src/agent/capabilities/types.rs): `Capability`, `Resource`, `AuthInfo`.
- [rust-executor/src/agent/capabilities/defs.rs](../../../rust-executor/src/agent/capabilities/defs.rs): every `*_CAPABILITY`, the domain and verb constants, `get_user_default_capabilities`.
- [rust-executor/src/agent/capabilities/mod.rs](../../../rust-executor/src/agent/capabilities/mod.rs): `capability_matches`, `require_capability`, `check_capability`, `anonymous_capabilities`.
- [core/src/agent/](../../../core/src/agent) and [core/src/generated/api/](../../../core/src/generated/api): SDK types (`Capability`, `AuthInfo`).

</details>
