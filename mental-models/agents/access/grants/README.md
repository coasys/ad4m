# Grants

A **grant** is the stored permission behind one token: the agent the token acts as, the capabilities it holds, the app it was issued to, its state (`active`, `revoked`) and its expiry. The executor mints one at every login and every app approval; a key login adds the person's identity and the signing key. A grant is never wider than its agent's own capabilities, and a **bot grant**, signed by the owner's master key, narrows what a bot's sessions may do to named neighbourhoods.

**Why:** An app is not a separate kind of caller: it is what a person uses to reach their agent, so every token is "one agent, within a grant". The capabilities live in the grant, not in the token, so a grant can narrow, end or be listed without reissuing a token. Whoever owns the agent approves a grant on it: the operator for the main agent, a user for their managed agent.

**Parent:** [Access](../README.md)

## Parts
- [Capabilities](capabilities.md): the `Capability` type, the built-in sets, the match rule and the caps on a grant.
- [App approval](app-approval.md): how an app asks for a grant on an agent, how that agent's owner approves it, and how grants are listed and revoked.
- [Bot grants](bot-grants.md): the signed statement that names a bot's neighbourhoods and actions, and how the executor applies it at each call.

<details><summary>How it works</summary>

| How a grant is minted | Agent | Capabilities | `client` |
|---|---|---|---|
| key login as the operator | main | all (operator role) | `Native` or `Web` |
| key login as a tenant | the tenant's managed agent | user defaults, or the approved set for a third-party origin | `Native` or `Web` |
| key login, operator via a third-party web app | main | the approved set, capped | `Web` |
| `runtime.localSession` | main | all (operator role) | `Local` |
| six-digit approval (`agent.generateJwt`) | the request's target agent | the approved subset, capped | `Native` |
| `user.login`, `user.verifyEmail` | the user's managed agent | user defaults | `Native` |
| `guest.create` | the guest's managed agent | the guest set | `Guest` |

`agent.getApps` lists the grants of the caller's own agent; `agent.revokeToken` and `agent.removeApp` act on one grant of the caller's agent (the operator: any). A grant on another agent answers 404.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/grants.rs](../../../../rust-executor/src/agent/grants.rs) (new): `LiveGrant`, `GrantRole`, `ClientKind`, `Grants`, `mint`.
- [rust-executor/src/db/grants.rs](../../../../rust-executor/src/db/grants.rs) (new): the `grants` table.
- [rust-executor/src/agent/capabilities/mod.rs](../../../../rust-executor/src/agent/capabilities/mod.rs): `grant_caps_for`, `caps_within`.
- [rust-executor/src/agent/bot_grants.rs](../../../../rust-executor/src/agent/bot_grants.rs) (new): bot grants.
- [rust-executor/src/api/agent_ws.rs](../../../../rust-executor/src/api/agent_ws.rs): `agent.getApps`, `agent.revokeToken`, `agent.removeApp`.

</details>
