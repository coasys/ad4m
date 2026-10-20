# Managed agents

A **managed agent** is an agent that the executor hosts in **multi-user mode** for a **tenant** (a person with a key log), a **user account** or a **guest**. It has its own DID and its own key in the executor's wallet; `AgentContext::Managed { did }` names it. Sessions, perspective ownership, billing and notifications use the DID. A managed agent is an account on the executor: the executor holds its key, and the key has no export ([Wallet and keys](../../identity/wallet-and-keys.md)). Entanglement proofs are for the main agent only.

**Why:** A host can serve people who run no executor, with the same code that serves the main agent. A tenant joins by the same linking protocol as any device, so the host never learns an email address or a password. Everything is keyed by DID, so no email enters signed data.

**Parent:** [Executor](../README.md)

## Parts
- [Tenants](tenants.md): the tenant record and its states, the host's sign-up setting and invites, the join gate, the usage meter.
- [Leaving a host](leaving.md): export, keep or delete, the host's leave call, the revocation sweep, rejoin.
- [Password users](password-users.md): the email-and-password accounts, sign-up, login, user tokens.

<details><summary>How it works</summary>

| Kind | Made by | Logs in with |
|---|---|---|
| Tenant | `host.join` with a `node` offer from the person's app | key login by a device key of the person ([Key login](../../access/key-login.md)) |
| Password user | `user.create { email, password }` | `user.login`, or an emailed code |
| Guest | `guest.create` on a host that admits guests | its guest grant |

1. The operator turns on multi-user mode (`enable_multi_user` at start, or `user.setMultiUserEnabled`).
2. Handlers act as `ctx.acts_as()`. Perspectives that a managed agent creates get `owner = DID`, and it reaches only perspectives it owns ([Guards](../../access/guards.md)). Each language has one runtime per executor, shared by every agent. Expression writes run language code with `set_runtime_agent_context(ctx)` and sign as that agent. Link-language calls (commit, sync, render, signals) run as the main agent ([Identity](../../identity/README.md)).
3. Calls on the executor itself refuse managed agents through their guard.

</details>

<details><summary>Where in the code</summary>

- `rust-executor/src/hosting/` (new): tenants and the join gate ([Tenants](tenants.md)).
- [rust-executor/src/user_management.rs](../../../../rust-executor/src/user_management.rs): password accounts ([Password users](password-users.md)).
- [rust-executor/src/agent/mod.rs](../../../../rust-executor/src/agent/mod.rs): `AgentContext::Managed`, `create_managed_agent_key`, `managed_agent_for_subject`, `wallet_key_for_did`.
- [rust-executor/src/helpers.rs](../../../../rust-executor/src/helpers.rs): `can_access_perspective_with_did`.
- [docs-src/pages/multi-user.mdx](../../../../docs-src/pages/multi-user.mdx).

</details>
