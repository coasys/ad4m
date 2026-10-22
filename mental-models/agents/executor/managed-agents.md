# Managed agents

A **managed agent** is an agent that the executor hosts for a **user account** in **multi-user mode**. It has its own DID and its own key in the executor's wallet; `AgentContext::Managed { did }` names it. The user account (`users` row: email, DID, password hash) is only the login. The email resolves to the DID at sign-up, at login and where an RPC parameter names a user. Sessions, perspective ownership, billing and notifications use the DID. A managed agent is an account on the executor: the executor holds its key (in a local wallet, under the operator's passphrase), and the key has no export ([Wallet and keys](../identity/wallet-and-keys.md)). Entanglement proofs are for the main agent only.

**Why:** A host can serve people who run no executor, with the same code that serves the main agent. Everything is keyed by DID. This removed bugs where an email and a DID named the same agent in different places, and it keeps the email out of signed data.

**Parent:** [Executor](README.md)

<details><summary>How it works</summary>

1. The operator turns on multi-user mode (`enable_multi_user` at start, or `user.setMultiUserEnabled`). Anonymous callers then get sign-up, login and verify capabilities ([Capabilities](../access/capabilities.md)).
2. `user.create { email, password }` calls `user_management::create_user`. It creates a wallet key named by the new DID (by the email on a shared wallet), saves the keystore and adds the `users` row (Argon2 hash). The handler then stores an empty profile, publishes it to the agent language and emails a sign-up code.
3. `user.login { email, password }`, or `user.verifyEmail { email, code }` after a code mail, calls `generate_user_jwt`. It issues a **user token**: a JWT with `sub` = the DID, `user_email` and `get_user_default_capabilities()`, valid 180 days.
4. Each connection with that token opens a `Session` with `Principal::Agent(Managed { did })`. Before that, `managed_agent_for_subject` checks that a `users` row owns the DID and that the wallet holds its key.
5. Handlers act as `ctx.acts_as()`. Perspectives that the user creates get `owner = DID`, and the `perspective.*` handlers let a user reach only perspectives it owns (`get_perspective_with_access`, [Access checks](../access/checks.md)). Each language has one runtime per executor, shared by every agent. Expression writes run language code with `set_runtime_agent_context(ctx)` and sign as that agent. Link-language calls (commit, sync, render, signals) run as the main agent ([Identity](../identity/README.md)).
6. Calls on the executor itself refuse user sessions (`refuse_user_session`).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/user_management.rs](../../../rust-executor/src/user_management.rs): `create_user`, `login_user`, `signup_user`, `request_login_code`, `verify_and_login`, `generate_user_jwt`, `import_user_from_shared_db`.
- [rust-executor/src/api/users_ws.rs](../../../rust-executor/src/api/users_ws.rs): `user.*` RPC methods.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `AgentContext::Managed`, `create_managed_agent_key`, `did_for_email`, `email_for_did`, `managed_agent_for_email`, `managed_agent_for_subject`, `wallet_key_for_did`.
- [rust-executor/src/helpers.rs](../../../rust-executor/src/helpers.rs): `can_access_perspective_with_did`.
- [rust-executor/src/email_service.rs](../../../rust-executor/src/email_service.rs): verification mails (SMTP).
- [rust-executor/src/agent/capabilities/defs.rs](../../../rust-executor/src/agent/capabilities/defs.rs): `get_user_default_capabilities`.
- [docs-src/pages/multi-user.mdx](../../../docs-src/pages/multi-user.mdx).

</details>
