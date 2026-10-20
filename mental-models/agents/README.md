# Agents

An **agent** is a **device key**: an Ed25519 key in an executor's wallet, named by its `did:key`. The executor signs and stores data on the agent's behalf. A person is an **identity** (`did:scid:coasys:1:…`) whose public **key log** lists every device key that speaks for the person. One **executor** hosts the **main agent**, whose key is a device key of the **operator**'s identity, and in multi-user mode one **managed agent** per tenant or user account. Each caller opens a **session**: it is the operator, an agent within a **grant**, or anonymous.

**Why:** A link names the device key that signed it, not the person. Links therefore keep their format, and any executor finds the person through the key log. The person, not a machine, owns the rights: any of the person's device keys can log in, and the master key that changes the log never rests in an executor. The code still names an agent only by its `AgentContext`, so one executor hosts many agents on the same code paths.

**Parent:** [ADAM](../README.md)

## Parts
- [Identity](identity/README.md): who an agent is: the key log, devices and linking, the wallet, signing and verification, the profile.
- [Executor](executor/README.md): the process that hosts the agents: startup and the sealed state, config, data at rest, managed agents and tenants, executor services.
- [Access](access/README.md): what each caller may do: sessions, key login, the operator, grants, guards.
- [Executor interfaces](interfaces/README.md): how callers reach the executor: WebSocket RPC, events, MCP, SDKs, CLI.

<details><summary>How it works</summary>

```
 caller (desktop shell, phone shell, web app through Connect, CLI, MCP client, bot)
   │  token: admin credential │ local session │ key-bound grant │ app grant │ user token │ none
   ▼
 Executor interfaces ── /api/v1/ws (RPC + events), MCP, /v1 (OpenAI-compatible)
   │
   ▼
 Access: Session::open ──► Principal = Operator │ Agent(AgentContext) │ Anonymous
   │        guard on every method; key-bound grants re-checked against the key log
   ▼
 Identity: AgentContext::Main │ Managed { did } ──► wallet key = device key ──► sign, anchor
   │                                                   └─ resolver: device key ──► identity (did:scid)
   ▼
 Perspectives, Languages (act and sign as that agent)
```

| Role | Code | Key | Who holds it |
|---|---|---|---|
| Operator | `Principal::Operator` | none of its own; acts as the main agent | the identity in the operator record, through a key login from the person's own app; the admin credential on the machine |
| Main agent | `AgentContext::Main` | wallet key `main`, a device key in the operator's key log once linked | the operator |
| Managed agent: tenant | `AgentContext::Managed { did }` + a `tenants` row | its own wallet key, a device key in the tenant's key log | a person who uses this executor and does not run it |
| Managed agent: password user, guest | `AgentContext::Managed { did }` | its own wallet key, in no key log | an email and password, or a guest grant |
| App, bot program | `Principal::Agent(..)` from a grant | none; acts as the agent of its grant | a grant on one agent |
| Anonymous | `Principal::Anonymous` | none | nobody |

`agent.get().did` is the device `did:key`. The field `identity` carries the `did:scid`, so code that compares `did` with link authors keeps working ([Agent profiles](identity/profiles.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/](../../rust-executor/src/agent): `AgentContext`, `AgentService`, `Session`, capabilities; `grants.rs` (new) holds the grants store.
- [rust-executor/src/agent/session.rs](../../rust-executor/src/agent/session.rs): `Session`, `Principal`, `AuthError`, `OperatorSource`.
- `rust-executor/src/identity/` (new): the key-log cache, the resolver, device linking and the device inbox ([Identity](identity/README.md)).
- [rust-executor/src/wallet.rs](../../rust-executor/src/wallet.rs): key storage.
- `rust-executor/src/hosting/` (new): tenants and the join gate ([Managed agents](executor/managed-agents/README.md)).
- [rust-executor/src/user_management.rs](../../rust-executor/src/user_management.rs): password user accounts.
- [rust-executor/src/api/](../../rust-executor/src/api) and [rust-executor/src/mcp/](../../rust-executor/src/mcp): the two server surfaces.
- [rust-executor/AGENTS.md](../../rust-executor/AGENTS.md): "Agents and sessions" and "Keystore" sections.

</details>
