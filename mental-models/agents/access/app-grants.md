# App grants

A **grant** (`LiveGrant`, table `grants`) is the executor's record of one token it issued: the agent the token acts as, its capabilities and the app that holds it. An **app grant** is the grant that an app gets after the owner of an agent approves the app's **capability request**. The flow has three calls:

1. The app asks for capabilities on one agent (`agent.requestCapability { authInfo, agent? }`, where `agent` is a user's email or an agent's DID; absent, the main agent).
2. The agent's owner approves, possibly with fewer capabilities, and receives a six-digit code (`agent.permitCapability`).
3. The app trades the request id and the code for the JWT (`agent.generateJwt`). This mints the grant.

Grants are the one allowlist of tokens: a token opens a session only while its grant exists, is active and has not expired ([Sessions](sessions.md)). A login mints an auto-approved grant on the user's managed agent the same way ([Managed agents](../executor/managed-agents.md)).

**Why:** The code shown to the person ties the token to the app that asked, even when the app runs on another device. A signed token alone opens nothing, so revoking or removing its grant ends the token at once. The app name and domain are whatever the caller declares, so a request never approves itself.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

```
app (anonymous)                executor                                approver (operator, or a session
 │                                                                       of that agent with agent/PERMIT)
 │ agent.requestCapability ──►  PendingRequest (memory only); publish
 │   { authInfo, agent? }       exception-occurred (CapabilityRequested)
 │                              to the agent's DID ──────────────────►  shows app + capabilities
 │                                                                  ◄── agent.permitCapability { auth }
 │                              approve within the approver's own ───►  code (6 digits) shown to person
 │ agent.generateJwt ─────────►  check code (max 5 wrong),
 │   { requestId, rand }        grants::mint on the request's agent
 │ ◄──────────────────────────  JWT (180 days)         apps-changed (no token) ──► the agent's approvers
```

1. `may_approve`: the operator approves any request. Another session approves only a request for the agent it acts as, and only with `agent/PERMIT`. Any other caller gets 403 without `agent/PERMIT`, else 404, as for a missing request.
2. The approver grants no more than it holds (`caps_within`). At redemption, a grant on a managed agent is capped again at that user's capabilities (`grant_caps_for`).
3. A request waits 10 minutes for the main agent and 24 hours for a managed agent, counted again from the approval. A restart forgets it. An email that no user has is taken all the same: its approval request reaches nobody, and its code mints nothing.
4. A mint that fails (locked wallet, database) leaves the request approved, and the same code redeems later.

JWT claims (`Claims`), signed HS256 with the wallet's JWT signing key (`signing_key_name()`): `iss` (the signing key's DID), `aud` (app name), `iat`, `exp`, `nonce` (a UUID: the token id, which names the grant) and `sub` (a managed agent's DID; none for the main agent). The token carries no capabilities: the grant holds them.

Revocation acts on the grants of the caller's own agent (the operator: any grant). `agent.revokeToken { token }` takes the request id (which the SDK sends) or the JWT and revokes the grant. `agent.removeApp` deletes it. `user.logout` revokes the caller's grant, `user.revokeAllSessions` every grant on that user's agent. Each revocation closes the grant's sockets at once. `agent.getApps` lists the grants on the caller's agent, without tokens. A grant has no refresh. At start, `load_at_start` deletes the rows of expired tokens and loads the rest.

With `auto_permit_cap_requests`, `agent.requestCapability` permits a request for the main agent at once. The code is printed only with `AD4M_LOG_SECRETS=1`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/grants.rs](../../../rust-executor/src/agent/grants.rs): `LiveGrant`, `Grants` (`get`, `find`, `revoke`, `revoke_agent`, `remove`, `list`), `mint`, `load_at_start`.
- [rust-executor/src/db/grants.rs](../../../rust-executor/src/db/grants.rs): the `grants` table.
- [rust-executor/src/agent/capabilities/mod.rs](../../../rust-executor/src/agent/capabilities/mod.rs): `request_capability`, `requested_agent`, `may_approve`, `permit_capability_within`, `caps_within`, `grant_caps_for`, `generate_capability_token`, `DEFAULT_TOKEN_VALID_PERIOD`.
- [rust-executor/src/agent/capabilities/pending.rs](../../../rust-executor/src/agent/capabilities/pending.rs): `PendingRequests` (memory only), `RequestTarget`, `MAX_CODE_ATTEMPTS`, the request lifetimes.
- [rust-executor/src/agent/capabilities/token.rs](../../../rust-executor/src/agent/capabilities/token.rs): `sign_token`, `decode_jwt`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `agent.requestCapability`, `agent.permitCapability`, `agent.generateJwt`, `agent.getApps`, `agent.revokeToken`, `agent.removeApp`.
- [rust-executor/src/api/users_ws.rs](../../../rust-executor/src/api/users_ws.rs): `user.logout`, `user.revokeAllSessions`.
- [rust-executor/src/mcp/tools/auth.rs](../../../rust-executor/src/mcp/tools/auth.rs): the same flow as MCP tools (`request_capability`, `generate_jwt`).
- [connect/src/core.ts](../../../connect/src/core.ts): `requestCapability`, `verifyLocalAd4mCode`.

</details>
