# App approval

**App approval** gives an app a grant on one agent with a six-digit code. The app asks for capabilities on a **target agent** (`agent.requestCapability { authInfo, agent? }`; no `agent` means the main agent). The owner of that agent approves a subset and gets a code (`agent.permitCapability`): the operator for the main agent, the user for their own managed agent. The app trades the request id and the code for a token (`agent.generateJwt`). A pending request is a grant in its **requested** state, held in memory only, with a short life.

**Why:** Whoever owns the identity approves the grant on it, so a user consents for their own agent as the operator does for the main agent. The code shown to the person ties the token to the app that asked, even when the app runs on another device. Pending requests stay in memory because anonymous callers create them, and anonymous callers must get no write path to disk. Apps that a name or a domain pre-approves lost: the app declares its own name and domain, so a match on them would give any caller a token. The person's own Coasys apps do not use this flow: they log in by key ([Key login](../key-login.md)); this route stays for local tools, old launchers and a bot's harness.

**Parent:** [Grants](README.md)

<details><summary>How it works</summary>

```
app (anonymous)                    executor                           owner of the target agent
 │ agent.requestCapability ──────►  PendingRequest { auth, target,
 │   { authInfo, agent? }             code: None, attempts, expires }
 │                                   exception-occurred (CapabilityRequested)
 │                                     audience: target's DID, AGENT_PERMIT ──►  shows app + capabilities
 │                                                                        ◄──  agent.permitCapability { requestId, subset }
 │                                   Owns { Request }; code: Secret ───────►  six digits shown to the person
 │ agent.generateJwt ────────────►  check code (5 tries), mint grant on the target,
 │   { requestId, rand }              caps = grant_caps_for(target, subset ∩ approver's caps)
 │ ◄─────────────────────────────  JWT (sub = target's DID, none for main), once      apps-changed ──► owner
```

| Limit | Value |
|---|---|
| request life | 10 min for the main agent, 24 h for a managed agent; restarts at approval |
| wrong codes | 5, then the request is gone |
| pending requests | 1,024 per executor, 64 per target; a full store answers 429 |
| `AuthInfo` text fields | 1 KiB each; `capabilities` 64 KiB as JSON |
| token life | 180 days, no renewal |

The request reaches the owner as an `exception-occurred` event of type `CapabilityRequested`; only sessions on the target agent with `AGENT_PERMIT` receive it ([Events](../../interfaces/events.md)).

1. A user holds `AGENT_PERMIT` on their own agent by default, so they approve requests for it.
2. `auto_permit_cap_requests` (development) approves main-agent requests only.
3. A bot's harness on the desktop asks for a grant on the bot agent with a one-time pairing code from the bots screen; the person approves in the main window. It never gets the admin credential ([Bot grants](bot-grants.md)).
4. Listing and revoking: `agent.getApps`, `agent.revokeToken { token }` (the request id, which the SDK sends, or the token), `agent.removeApp` act on the caller's own agent's grants ([Grants](README.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/capabilities/mod.rs](../../../../rust-executor/src/agent/capabilities/mod.rs): `request_capability`, `requested_agent`, `permit_capability_within`, `generate_capability_token`, `DEFAULT_TOKEN_VALID_PERIOD`.
- [rust-executor/src/agent/capabilities/pending.rs](../../../../rust-executor/src/agent/capabilities/pending.rs) (new): `PendingRequests`, `PendingRequest`, `MAX_CODE_ATTEMPTS`, the TTL and size constants.
- [rust-executor/src/secret.rs](../../../../rust-executor/src/secret.rs) (new): `Secret`, which holds the code.
- [rust-executor/src/api/agent_ws.rs](../../../../rust-executor/src/api/agent_ws.rs): `agent.requestCapability`, `agent.permitCapability`, `agent.generateJwt`, `agent.getApps`, `agent.revokeToken`, `agent.removeApp`.
- [rust-executor/src/mcp/tools/auth.rs](../../../../rust-executor/src/mcp/tools/auth.rs): the same flow as MCP tools.
- [connect/src/core.ts](../../../../connect/src/core.ts): `requestCapability`, `verifyLocalAd4mCode`.

</details>
