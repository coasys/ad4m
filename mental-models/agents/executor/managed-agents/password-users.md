# Password users

A **password user** is a managed agent whose login is a **user account**: a `users` row with an email, a DID and an Argon2 password hash. It has no key log. The email resolves to the DID at sign-up, at login and where an RPC parameter names a user; everything else uses the DID. A login issues a **user token**, a JWT with `sub` = the DID. This path works as before identities, on every multi-user executor, beside `host.join`.

**Why:** Existing users, tools and MCP sign-up keep working while tenants arrive through the key log. Everything is keyed by DID, which removed bugs where an email and a DID named the same agent in different places, and keeps the email out of signed data. The sign-up setting, invites and the email check gate `host.join` only; the option that ended this path and made `host.join` the one way in lost.

**Parent:** [Managed agents](README.md)

<details><summary>How it works</summary>

1. With multi-user mode on, anonymous callers get sign-up, login and verify capabilities ([Access](../../access/README.md)).
2. `user.create { email, password }` calls `user_management::create_user`. It creates a wallet key named by the new DID, saves the keystore, adds the `users` row and a person data key under `User(username)`. The handler then stores an empty profile, publishes it to the agent language and emails a sign-up code (only with SMTP configured).
3. `user.login { email, password }`, or `user.verifyEmail { code }` after a code mail, calls `generate_user_jwt`. It issues a user token with `user_email` and `get_user_default_capabilities()`, valid 180 days, backed by a grant in the grants store.
4. Each connection with that token opens a `Session` with `Principal::Agent(Managed { did })`. Before that, `managed_agent_for_subject` checks that a `users` row owns the DID and that the wallet holds its key.
5. A password login never creates, changes or reads a tenant record; a key login never maps through a password user. The operator keeps `user.create` for its own use, and such a user is a password user too.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/user_management.rs](../../../../rust-executor/src/user_management.rs): `create_user`, `login_user`, `signup_user`, `request_login_code`, `verify_and_login`, `generate_user_jwt`, `import_user_from_shared_db`.
- [rust-executor/src/api/users_ws.rs](../../../../rust-executor/src/api/users_ws.rs): `user.*` RPC methods.
- [rust-executor/src/agent/mod.rs](../../../../rust-executor/src/agent/mod.rs): `create_managed_agent_key`, `did_for_email`, `email_for_did`, `managed_agent_for_email`, `managed_agent_for_subject`.
- [rust-executor/src/email_service.rs](../../../../rust-executor/src/email_service.rs): verification mails (SMTP).
- [rust-executor/src/agent/capabilities/defs.rs](../../../../rust-executor/src/agent/capabilities/defs.rs): `get_user_default_capabilities`.

</details>
