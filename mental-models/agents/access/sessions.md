# Sessions

A **session** (`Session`) is one connection's identity. `Session::open(token, admin_credential, reach)` turns the connection's token into a **principal** (`Operator`, `Agent(AgentContext)` or `Anonymous`) with its capabilities; no other code does this, for RPC and MCP alike. Every token has a **grant** in the **grants store**, and the grants store is the allow list: a token opens a session only while its grant exists, is active and has not expired. Revoking or removing a grant ends its token at once, closes its sockets (1008) and cancels its calls in flight (401).

**Why:** Identity is decided once per connection, and "is this token still good?" has one answer: its grant is active. The grants store replaced the revoked-token list, the per-user cut-offs, the open-socket registry and the app registry file, which were four lists of exceptions beside each other. A login is an auto-approved grant on the user's managed agent, so a user lists and revokes their own apps and logins one by one, as the operator does for the main agent. A token carries only `iss, aud, iat, exp, nonce, sub`; its capabilities come from the grant. Key logins add a binding to the signing device key to the same grant, not a second session table.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

`Session::open`:

1. Token equals the admin credential (constant time), or is empty under the loopback rule of [Operator and main agent](operator-and-main-agent.md) → `Operator`, no grant.
2. Empty token → `Anonymous`, `anonymous_capabilities()`.
3. Wallet locked, or storage sealed → `AuthError::Locked` (close 1013, HTTP 503).
4. JWT that does not decode, or past `exp` → `Invalid` / `Expired`.
5. `grant_for(claims)`: no active grant for the `nonce`, or `sub` names another agent → `Revoked`.
6. A grant with `role: Operator` (a key login or a local session) → `Operator`; any other grant → `Agent(grant.agent)`. Capabilities = the grant's.

Every request and every delivered event runs `Session::check()` (grant active). `Session::over()` fires when the grant ends or its `expires_at` passes; every socket and every call watches it. A renewal moves `expires_at`, so an open socket stays open.

| Grant | `client` | Life | Ends by |
|---|---|---|---|
| key login, native or web ([Key login](key-login.md)) | `Native`, `Web { origin, first_party }` | 1 h, renewed by a new signed statement | `user.logout`, `user.endAllSessions`, a key verdict change, an operator change, no renewal |
| desktop local session | `Local` | 1 h, renewed by the shell | `user.logout`, shell restart |
| app approved with a six-digit code ([App approval](grants/app-approval.md)) | `Native` | 180 days | `agent.revokeToken`, `agent.removeApp` |
| password or email-code login (`user.login`, `user.verifyEmail`) | `Native` | 180 days | `user.logout`, `user.revokeAllSessions` |
| guest (`guest.create`) | `Guest` | no expiry | the operator removes the guest, or the guest keeps an identity |
| in-process (`Session::in_process`) | — (no grant) | the interpretation pass | the pass ends |

Ending sessions:

| Call | Ends |
|---|---|
| `user.logout` | the caller's own grant, for every grant-backed session; 400 without a grant |
| `user.endAllSessions { keepCurrent, before? }` | every grant of the caller's identity (`scid`), and for a managed agent every grant of that agent; `before` keeps grants issued later |
| `user.revokeAllSessions { email? }` | every grant of one managed agent; the operator names the email |
| `Grants::revoke_key`, `revoke_role` | grants bound to a revoked key; operator grants after an operator change |

No event announces a session's end: the socket closes with 1008. The executor publishes nothing about other people's sessions.

Grants live in the executor's local database. Executors that share a database backend (`db_backend = "shared"`) do not share grants ([db_backend.rs](../../../rust-executor/src/db_backend.rs)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Session::{open, check, over, ended, may, may_receive, acts_as, acts_as_did, is_operator, operator_source, key_binding, grant_id, in_process}`, `grant_for`, `Principal`, `AuthError::close_code`.
- [rust-executor/src/agent/grants.rs](../../../rust-executor/src/agent/grants.rs) (new): `LiveGrant { id, request_id, agent, did, caps, auth, role, key, client, expires_at, ended }`, `GrantRole`, `KeyBinding`, `ClientKind`, `Grants::{get, find, list, revoke, revoke_agent, revoke_key, revoke_identity, revoke_role, renew, remove, load_at_start}`, `mint`.
- [rust-executor/src/db/grants.rs](../../../rust-executor/src/db/grants.rs) (new): table `grants (id, request_id, agent_did, auth_info, state, expires_at, created_at, role, scid, signing_key, key_id, client)`.
- [rust-executor/src/agent/capabilities/token.rs](../../../rust-executor/src/agent/capabilities/token.rs): `sign_token`, `decode_jwt`, `read_stored_claims`.
- [rust-executor/src/api/users_ws.rs](../../../rust-executor/src/api/users_ws.rs): `user.logout`, `user.endAllSessions`, `user.revokeAllSessions`, `user.currentSession`.
- [rust-executor/src/api/ws_rpc.rs](../../../rust-executor/src/api/ws_rpc.rs): the check on every request, `REPLY_GRACE`, close on refusal and on `Session::over`.
- [rust-executor/src/types/domain.rs](../../../rust-executor/src/types/domain.rs): `RequestContext` (one `Arc<Session>`, a cancel token, the connection id).

</details>
