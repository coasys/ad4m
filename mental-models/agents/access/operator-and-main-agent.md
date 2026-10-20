# Operator and main agent

The **operator** is the executor's administrator role. Whoever presents the **admin credential** gets `Principal::Operator` and every capability (`ALL_CAPABILITY`). The operator is not an agent: it acts as the **main agent** (`AgentContext::Main`), which `agent.generate` creates under the wallet key `main`.

**Why:** The executor needs one party who can unlock it, approve apps and manage users, apart from any agent's data. A caller with an empty token is the operator only when all three hold: the executor has no admin credential, the listener is loopback, and the request has no forwarding header (`is_admin_credential_token_on` in [capabilities/mod.rs](../../../rust-executor/src/agent/capabilities/mod.rs)). `run` starts without an admin credential only with `--insecure-no-admin-credential` (`check_admin_credential` in [config.rs](../../../rust-executor/src/config.rs)), so on a running executor the first condition implies that flag. The loopback and header conditions exist because a proxy or a tunnel can make a remote caller look local.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

Who becomes the operator (the full token rules are on [Sessions](sessions.md)):

| Token on the connection | Listener reach | Result |
|---|---|---|
| equals `admin_credential` (constant-time) | any | `Principal::Operator`, `ALL_CAPABILITY` |
| empty, executor has no credential | loopback, no forwarding header | `Principal::Operator` |
| empty, executor has no credential | network, or a `Forwarded`/`X-Forwarded-For`/`X-Real-IP` header | `Principal::Anonymous` |

Who may make which calls:

| Calls | Allowed for |
|---|---|
| calls that change the executor itself (lock, unlock, trusted agents, languages) | the operator, or an app grant on the main agent |
| multi-user mode and free access | the operator alone |

1. The launcher generates an admin credential and passes it to the embedded executor (`ExecutorSecrets.admin_credential`). The launcher's UI is the operator.
2. A headless operator sets `AD4M_ADMIN_CREDENTIAL_FILE` (or the variable). It talks to the executor with the CLI (`ad4m --admin-credential`) or the SDK.
3. Operator-only checks in handlers use `ctx.is_operator()` (`user.setMultiUserEnabled`, `user.freeAccess`). Main-agent checks use `refuse_user_session`, which lets the operator and main-agent grants through.
4. `api::listener_router` marks each listener with its `ListenerReach`. `listener_reach()` reads a request with a forwarding header as `Network`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Principal::Operator`, `Session::open`, `is_operator`, `acts_as`.
- [rust-executor/src/agent/capabilities/mod.rs](../../../rust-executor/src/agent/capabilities/mod.rs): `ListenerReach`, `is_admin_credential_token_on`.
- [rust-executor/src/api/auth.rs](../../../rust-executor/src/api/auth.rs): `listener_reach`, `FORWARDING_HEADERS`.
- [rust-executor/src/api/mod.rs](../../../rust-executor/src/api/mod.rs): `listener_router`, `bind_api_listeners`.
- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs): `refuse_user_session`.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `AgentContext::Main`, `AgentService` (the main agent's state).
- [docs-src/pages/auth.mdx](../../../docs-src/pages/auth.mdx): "Admin Credential Override".

</details>
