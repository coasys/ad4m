# Key login

A **key login** opens a session on any executor with a short statement signed by one of the person's **device keys**. The executor checks the signature, asks the key index (through the executor's `KeyResolver`) whether the key is valid in the person's key log and carries no bot mark, and maps the identity to a role: **operator** (the identity in the operator record), **tenant** (an active tenant of this host), or **app** (the operator, through a third-party web app). Anyone else is refused. The grant it mints is **key-bound**: it records the identity and the signing key, lives 1 hour, and ends when that key stops being valid.

**Why:** The rights follow the person, not the machine. "Run any device you operate from any of your devices" needs a proof that every executor can check without a shared secret, and the key log is that proof. A device-key signature, not the master key, signs every login, so the master key stays out of routine use and one proof works on every executor; the web app gets its own browser device key for this ([ad4m-connect](../interfaces/typescript-sdk/ad4m-connect.md)). Tokens last 1 hour and renew with a fresh signature instead of a refresh token: the device key is the long-lived credential, and a revoked key cannot renew. A third-party app never gets the operator role, because an approval card must never hand out the role that decides who may use the executor.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

```
app (phone shell, desktop shell, Connect frame)        executor                      shared store
  user.sessionChallenge ─────────────────────────────►  32 bytes, one use, 60 s
  sign KeyStatement { v, purpose: login, scid, key,
    executor: <this executor's DID>, challenge,
    client: native | web{origin}, codeChallenge?,
    issuedAt } under "coasys:session"
  user.loginWithKey { proof, app } ──────────────────►  1 challenge  2 audience + clock (±120 s)
                                                         3 signature  4 resolve_key ──────────► key index (cache ≤ 5 min)
                                                         5 role table 6 mint grant (native)
  ◄───────── native: { token, grantId, role, did, scid, expiresAt }
  ◄───────── web:    { code } → app redeems with user.redeemCode { code, codeVerifier } (PKCE, 60 s, 5 tries)
```

Role table (first match wins):

| The identity is … | Grant | Principal |
|---|---|---|
| the operator record, client native or a first-party web origin | `role: Operator`, agent `Main` | `Operator` (`OperatorSource::Key`), all capabilities |
| an active or held tenant of this host (its managed agent) | `role: Agent`, agent `Managed { did }` | `Agent(Managed)`: user defaults; a third-party origin gets its approved set |
| the operator record, client a third-party web origin | `role: Agent`, agent `Main` | `Agent(Main)`: the approved set, capped |
| anyone else | none | refused `not-known` |

- Refusals answer 403 with one reason only: `challenge`, `audience`, `signature`, `key` (not valid, or a bot key), `not-known`; the store down answers 503 `store-unavailable` (fail closed).
- `user.renewSession` takes a `renew` statement that names the caller's own grant and is signed by the grant's key, from 15 minutes before expiry. It re-resolves the key, keeps the grant id and moves `expires_at` by 1 hour. A cached valid verdict less than 1 hour old lets a renewal through while the store is down.
- Every 5 minutes the executor re-resolves the keys of its key-bound grants, and `KeyResolver::subscribe` pushes each verdict change. A key that is revoked, expired, gone or bot-marked ends every grant bound to it (`Grants::revoke_key`).
- `user.currentSession` answers the caller's own role, DID, identity, grant and `operatorSource`.
- Password and email-code logins stay beside key login, on the same managed agent; neither ends the other. A tenant made by `host.join` logs in by key ([Tenants](../executor/managed-agents/tenants.md)). A password user has no identity, so `user.endAllSessions` by identity and key revocation do not reach its grants; `user.revokeAllSessions` does ([Password users](../executor/managed-agents/password-users.md)).
- The desktop shell signs for its main window with `agent.signSessionStatement` (the wallet key of the computer, `.local_only()`); the phone shell signs with its own device key; the web app signs through Coasys Connect. The same statement, with the hosting index as audience, signs the executor into the hosting index inside the executor.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/users_ws.rs](../../../rust-executor/src/api/users_ws.rs): `user.sessionChallenge`, `user.loginWithKey`, `user.redeemCode`, `user.renewSession`, `user.currentSession`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `agent.signSessionStatement`.
- [rust-executor/src/identity/resolver.rs](../../../rust-executor/src/identity/resolver.rs) (new): `KeyResolver::{resolve_key, subscribe}`, `KeyResolution`, `KeyVerdict { Valid, Revoked, Expired, NotInLog }`, `VerdictChange` ([Key-log cache and resolver](../identity/key-log/cache.md)).
- [rust-executor/src/agent/grants.rs](../../../rust-executor/src/agent/grants.rs) (new): `KeyBinding`, `ClientKind`, `Grants::{revoke_key, renew, key_bound}`; constants `KEY_SESSION_TTL`, `LOGIN_MAX_AGE`, `KEY_RECHECK_INTERVAL`.
- `KeyStatement`, `SignedKeyStatement` and `signing_bytes("coasys:session", …)`: the identity core crate `ad4m-identity-core` ([Identity](../identity/README.md)).
- [core/src/agent/AgentClient.ts](../../../core/src/agent/AgentClient.ts): the SDK calls.

</details>
