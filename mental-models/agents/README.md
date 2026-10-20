# Agents

An **agent** is a DID with an Ed25519 key pair; the executor signs and stores data on its behalf. One **executor** hosts the **main agent**, which the **operator** owns, and in multi-user mode one **managed agent** per user account. Each caller opens a **session**: it is the operator, an agent within a **grant**, or anonymous.

**Why:** The agent signs every expression, not the app or the server. The code names an agent only by its DID (`AgentContext`). One executor can therefore host many agents on the same code paths.

**Parent:** [ADAM](../README.md)

## Parts
- [Identity](identity/README.md): an agent in code: its DID, its key in the wallet, how it signs, its profile.
- [Executor](executor/README.md): the process that hosts the agents: startup, config, data, managed agents, executor services.
- [Access](access/README.md): what each caller may do: sessions, the operator, capabilities, app grants, access checks.
- [Executor interfaces](interfaces/README.md): how callers reach the executor: WebSocket RPC, events, MCP, SDKs, CLI.

<details><summary>How it works</summary>

```
 caller (app, launcher, CLI, MCP client)
   │  token: admin credential │ JWT (grant) │ none
   ▼
 Executor interfaces ── /api/v1/ws (RPC + events), MCP, /v1 (OpenAI-compatible)
   │
   ▼
 Access: Session::open ──► Principal = Operator │ Agent(AgentContext) │ Anonymous
   │        capability check + ownership check in each handler
   ▼
 Identity: AgentContext::Main │ Managed { did } ──► wallet key ──► sign / DID
   │
   ▼
 Perspectives, Languages (act and sign as that agent)
```

| Role | Code | Has a DID | Who holds it |
|---|---|---|---|
| Operator | `Principal::Operator` | no (acts as the main agent) | the admin credential |
| Main agent | `AgentContext::Main` | yes | the operator |
| Managed agent | `AgentContext::Managed { did }` | yes | a user account (email + password) |
| App | `Principal::Agent(..)` from a JWT | no; acts as the agent of its grant | a grant on one agent |
| Anonymous | `Principal::Anonymous` | no | nobody |

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/](../../rust-executor/src/agent): `AgentContext`, `AgentService`, `Session`, capabilities.
- [rust-executor/src/agent/session.rs](../../rust-executor/src/agent/session.rs): `Session`, `Principal`, `AuthError`.
- [rust-executor/src/wallet.rs](../../rust-executor/src/wallet.rs): key storage.
- [rust-executor/src/user_management.rs](../../rust-executor/src/user_management.rs): user accounts of managed agents.
- [rust-executor/src/api/](../../rust-executor/src/api) and [rust-executor/src/mcp/](../../rust-executor/src/mcp): the two server surfaces.
- [rust-executor/AGENTS.md](../../rust-executor/AGENTS.md): "Agents and sessions" and "Keystore" sections.

</details>
