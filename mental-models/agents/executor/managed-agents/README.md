# Managed agents

A **managed agent** is an agent that the executor hosts in **multi-user mode** for someone other than its operator. It has its own DID, its own key in the executor's wallet and its own person data key; `AgentContext::Managed { did }` names it. It comes in three kinds. A **tenant** is a person with a key log: the managed agent's key is a device key in that person's log, and a `tenants` row maps the person's `did:scid` to it. A **password user** has an email and a password and no key log. A **guest** has only a device key and a guest grant, on a host that admits guests. Sessions, perspective ownership, billing and notifications use the managed agent's DID for all three. A managed agent is an account on the executor: the executor holds its key, and the key has no export ([Wallet and keys](../../identity/wallet-and-keys.md)). Entanglement proofs are for the main agent only.

**Why:** A host serves people who run no executor, with the same code that serves the main agent. A tenant joins by the same linking protocol as any device, so the host never learns an email address or a password, and the person can revoke the host's key from any app. The email-and-password sign-up stays beside it, unchanged, so existing users and tools keep working. Each managed agent has its own data key, so leaving a host can shred that person's encrypted data.

**Parent:** [Executor](../README.md)

## Parts
- [Tenants](tenants.md): the tenant record and its states, the host's sign-up setting and invites, the join gate, the usage meter.
- [Leaving a host](leaving.md): export, keep or delete, the host's leave call, the revocation sweep, rejoin.
- [Password users](password-users.md): the email-and-password accounts, sign-up, login, user tokens.

<details><summary>How it works</summary>

| Kind | Made by | Logs in with | Record |
|---|---|---|---|
| Tenant | `host.join` with a `node` offer from the person's app; a guest's "Keep this" | key login (`user.loginWithKey`) by a device key of the person ([Key login](../../access/key-login.md)) | `tenants` row (`did_scid`, state) + `users` row with `username = t-<uuid>` and an unusable password |
| Password user | `user.create { email, password }` | `user.login`, or an emailed code | `users` row keyed by the email |
| Guest | `guest.create` on a host whose sign-up setting is "anyone" | its guest grant, whose token the web app keeps | `users` row of kind guest, no password |

1. The operator turns on multi-user mode (`enable_multi_user` at start, or `user.setMultiUserEnabled`).
2. Each connection opens a `Session` with `Principal::Agent(Managed { did })`. Handlers act as `ctx.acts_as()`; a managed agent reaches only perspectives it owns ([Access](../../access/README.md)). Each language has one runtime per executor, shared by every agent. Expression writes run language code with `set_runtime_agent_context(ctx)` and sign as that agent. Link-language calls (commit, sync, render, signals) run as the main agent ([Language controller](../../../languages/runtime/controller.md)).
3. Methods that act on the executor itself refuse managed agents through their guard.
4. A password user becomes a tenant only by claiming a `did:scid` (`JoinRoute::Claim`). Until then, the same person can be a password user and, after a `host.join`, a separate tenant on the same host.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/hosting/](../../../../rust-executor/src/hosting) (new): `policy.rs`, `tenants.rs`, `meter.rs`, `index_client.rs`; [rust-executor/src/api/host_ws.rs](../../../../rust-executor/src/api/host_ws.rs) (new): `host.*`.
- [rust-executor/src/user_management.rs](../../../../rust-executor/src/user_management.rs): password accounts, unchanged.
- [rust-executor/src/agent/mod.rs](../../../../rust-executor/src/agent/mod.rs): `AgentContext::Managed`, `create_managed_agent_key`, `managed_agent_for_subject`, `wallet_key_for_did`; `AgentData` gains `scid`.
- [rust-executor/src/helpers.rs](../../../../rust-executor/src/helpers.rs): `can_access_perspective_with_did`.
- [docs-src/pages/multi-user.mdx](../../../../docs-src/pages/multi-user.mdx).

</details>
