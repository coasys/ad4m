# App grants

An **app grant** is a JWT that the executor issues to an app after the operator approves the app's **capability request**. The flow has three calls:

1. The app asks (`agent.requestCapability`).
2. The operator approves and receives a six-digit code (`agent.permitCapability`).
3. The app trades the request id and the code for the JWT (`agent.generateJwt`).

The **apps registry** (`apps_data.json`) records each issued grant so that the operator can list and revoke it. Managed agents get a user token at login instead ([Managed agents](../executor/managed-agents.md)).

**Why:** The code shown to the person ties the token to the app that asked, even when the app runs on another device. The token is a signed JWT, so the executor checks it without a lookup. The registry exists only for listing and revocation.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

```
app (anonymous)                executor                        operator UI (launcher)
 │ agent.requestCapability ──►  request_id; publish
 │   { authInfo }               exception-occurred
 │                              (CapabilityRequested) ───────►  shows app + capabilities
 │                                                          ◄── agent.permitCapability { auth }
 │                              requests_map[id-code]=auth  ──►  code (6 digits) shown to person
 │ agent.generateJwt ─────────►  check code (max 5 wrong)
 │   { requestId, rand }        generate_jwt(...), apps_map.insert
 │ ◄──────────────────────────  JWT (180 days)      apps-changed (no token) ──► operator UI
```

The capability request reaches the operator as an `exception-occurred` event of type `CapabilityRequested`. Only sessions with `agent/PERMIT` receive it. A grant lasts 180 days (`DEFAULT_TOKEN_VALID_PERIOD`) and has no refresh.

JWT claims (`Claims`), signed HS256 with the wallet's JWT signing key (`signing_key_name()`):

| Claim | Value |
|---|---|
| `iss` | the signing key's DID |
| `aud` | app name |
| `exp`, `iat` | expiry and issue time |
| `nonce` | UUID, the token id |
| `sub` | managed agent's DID (user tokens only) |
| `capabilities` | `AuthInfo`: app info, capability list, `user_email` |

Revocation: `agent.revokeToken { token }` takes the request id (which the SDK sends) or the JWT and sets `revoked` in the registry. `agent.removeApp` deletes the entry. `Session::check` scans the registry on each request.

With `auto_permit_cap_requests`, `agent.requestCapability` permits at once. The code is printed only with `AD4M_LOG_SECRETS=1`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/capabilities/mod.rs](../../../rust-executor/src/agent/capabilities/mod.rs): `request_capability`, `permit_capability`, `generate_capability_token`, `MAX_CODE_ATTEMPTS`, `DEFAULT_TOKEN_VALID_PERIOD`.
- [rust-executor/src/agent/capabilities/token.rs](../../../rust-executor/src/agent/capabilities/token.rs): `generate_jwt`, `decode_jwt`.
- [rust-executor/src/agent/capabilities/requests_map.rs](../../../rust-executor/src/agent/capabilities/requests_map.rs): pending approvals (memory only).
- [rust-executor/src/agent/capabilities/apps_map.rs](../../../rust-executor/src/agent/capabilities/apps_map.rs): the registry file, `revoke_app`, `is_revoked`, `client_view` (tokens removed).
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `agent.requestCapability`, `agent.permitCapability`, `agent.generateJwt`, `agent.getApps`, `agent.revokeToken`, `agent.removeApp`.
- [rust-executor/src/mcp/tools/auth.rs](../../../rust-executor/src/mcp/tools/auth.rs): the same flow as MCP tools (`request_capability`, `generate_jwt`).
- [connect/src/core.ts](../../../connect/src/core.ts): `requestCapability`, `verifyLocalAd4mCode`.

</details>
