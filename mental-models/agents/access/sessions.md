# Sessions

A **session** (`Session`) is one connection's identity. `Session::open(token, admin_credential, reach)` turns the connection's token into a **principal** (`Operator`, `Agent(AgentContext)` or `Anonymous`) with its capabilities; no other code does this. `Session::check()` runs again on every request and every delivered event, so a revoked or expired token ends an open connection. Handlers read identity only from the session (`acts_as()`, `managed_did()`, `is_operator()`) and never decode the token again.

**Why:** Identity is decided once per connection. A token that opens no session is refused at the door, so a token that stops working cannot fall back to the main agent. Three principals (operator, agent, anonymous) cover every caller.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

`Session::open`:

1. `is_admin_credential_token_on` is true → `Operator`, `ALL_CAPABILITY`. The token equals the admin credential, or it is empty under the no-credential condition owned by [Operator and main agent](operator-and-main-agent.md) (no credential, so `--insecure-no-admin-credential`; loopback listener; no forwarding header).
2. Empty token → `Anonymous`, `anonymous_capabilities()`.
3. Wallet locked → `AuthError::Locked`.
4. Token in a revoked app → `AuthError::Revoked`. JWT does not decode or is past `exp` → `Invalid` / `Expired`.
5. JWT with `sub` → `managed_agent_for_subject(sub, user_email)`; failure → `AuthError::NoIdentity`. Without `sub` → `AgentContext::Main`.
6. Capabilities from the token's claims (default `agent/AUTHENTICATE` only).

Refusal at the door, by transport:

| Transport | Token from | Locked | Other refusal |
|---|---|---|---|
| RPC WebSocket `/api/v1/ws` | `?token=` | close 1013 | close 1008 |
| REST, OpenAI-compatible `/v1` | `Authorization: Bearer`, or `?token=` | HTTP 503 | HTTP 401 |

On the RPC WebSocket, the inline control messages `events.watch`, `events.unwatch` and `request.cancel` are answered before `Session::check()` runs. They read or change only the connection's own state. Every delivered event runs the check again.

`Session` fields:

| Field | Content |
|---|---|
| `principal` | operator, agent or anonymous |
| `caps` | the grant |
| `token_id` | the JWT `nonce` |
| `token` | kept for the revocation lookup; `Debug` hides it |
| `expires_at` | the token's expiry |
| `did` | set lazily for the main agent |

The request context (`RequestContext`) wraps one `Arc<Session>`, a per-request cancel token and the RPC connection id. Its `auth_token` field keeps the raw token beside the `Session`; interpretation hands it to an in-process MCP client.

Revocation: `Session::check()` looks for the raw token in the apps registry (`apps_map::is_revoked`) on every request and event.

The MCP server resolves its tokens with its own code (`capabilities_from_token`, `AgentContext::from_auth_token`), not with `Session` ([Access checks](checks.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Session::{open, check, may, require, acts_as, acts_as_did, managed_did, is_operator}`, `Principal`, `AuthError::close_code`, `TokenId`.
- [rust-executor/src/types/domain.rs](../../../rust-executor/src/types/domain.rs): `RequestContext::open`, `user_did`, test constructors.
- [rust-executor/src/api/ws_rpc.rs](../../../rust-executor/src/api/ws_rpc.rs): `connection_context`, the check on every request, close on refusal.
- [rust-executor/src/api/auth.rs](../../../rust-executor/src/api/auth.rs): the axum extractor for REST.
- [rust-executor/src/agent/capabilities/mod.rs](../../../rust-executor/src/agent/capabilities/mod.rs): `track_last_seen` (called per REST request for user sessions).

</details>
