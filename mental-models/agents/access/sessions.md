# Sessions

A **session** (`Session`) is one connection's identity. `Session::open(token, admin_credential, reach)` turns the connection's token into a **principal** (`Operator`, `Agent(AgentContext)` or `Anonymous`) with its capabilities; no other code turns a token into a principal. `Session::in_process` is a session without a token, which the executor opens for its own work as one agent. `Session::check()` runs again on every request and every delivered event, so a revoked or expired token ends an open connection. Handlers read identity only from the session (`acts_as()`, `managed_did()`, `is_operator()`) and never decode the token again.

**Why:** Identity is decided once per connection. A token that opens no session is refused at the door, so a token that stops working cannot fall back to the main agent. Three principals (operator, agent, anonymous) cover every caller.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

`Session::open`:

1. `is_admin_credential_token_on` is true → `Operator`, `ALL_CAPABILITY`. The token equals the admin credential, or it is empty under the no-credential condition owned by [Operator and main agent](operator-and-main-agent.md) (no credential, so `--insecure-no-admin-credential`; loopback listener; no forwarding header).
2. Empty token → `Anonymous`, `anonymous_capabilities()`.
3. Wallet locked → `AuthError::Locked`.
4. JWT does not decode or is past `exp` → `Invalid` / `Expired`.
5. `grant_for`: no active, unexpired grant for the token's `nonce` ([App grants](app-grants.md)), or a `sub` that does not name the grant's agent → `AuthError::Revoked`. A signed token that no grant records opens nothing.
6. The grant's agent: a managed agent by its DID (its wallet key must load, else `AuthError::NoIdentity`), or `AgentContext::Main`. The capabilities are the grant's.

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
| `caps` | the grant's capabilities; every capability for the operator |
| `grant` | the `LiveGrant` the token acts within; `None` for the operator and anonymous callers |
| `did` | set at open for a managed agent, lazily for the main agent |

The request context (`RequestContext`) wraps one `Arc<Session>`, a per-request cancel token and the RPC connection id. It carries no token. Interpretation hands the caller's `Session` to its in-process MCP client, or an in-process session for a pass without a caller.

Revocation: `Session::check()` asks the session's grant (`LiveGrant::holds`: not revoked or removed, not expired) on every request and event; it takes no lock and needs no wallet. Revoking or removing the grant also cancels its `ended` token. Each socket of the session watches `Session::ended()` and `Session::over()` (which also completes at expiry) and then closes at once with 1008, also when idle.

The MCP server opens one `Session` per MCP session with `Session::open`, and checks it again on every tool call ([MCP server](../interfaces/mcp-server.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Session::{open, in_process, check, ended, over, may, require, acts_as, acts_as_did, managed_did, is_operator}`, `grant_for`, `Principal`, `AuthError::close_code`, `TokenId`.
- [rust-executor/src/agent/grants.rs](../../../rust-executor/src/agent/grants.rs): `LiveGrant`, `Grants::get`.
- [rust-executor/src/types/domain.rs](../../../rust-executor/src/types/domain.rs): `RequestContext::open`, `user_did`, test constructors.
- [rust-executor/src/api/ws_rpc.rs](../../../rust-executor/src/api/ws_rpc.rs): `connection_context`, the check on every request, close on refusal.
- [rust-executor/src/api/auth.rs](../../../rust-executor/src/api/auth.rs): the axum extractor for REST.
- [rust-executor/src/mcp/caller.rs](../../../rust-executor/src/mcp/caller.rs): `Caller`, the `Session` of one MCP session.
- [rust-executor/src/agent/capabilities/mod.rs](../../../rust-executor/src/agent/capabilities/mod.rs): `track_last_seen` (called per REST request for user sessions).

</details>
