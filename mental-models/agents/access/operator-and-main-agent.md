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
| calls that change the executor itself (lock, unlock, trusted agents, languages): `Guard::Executor` | the operator, or an app grant on the main agent |
| calls that act for the caller's own agent but bind the main agent for good (`identity.cosignIncept`, `identity.publish`): `Guard::UserOrOperator` | the operator, or a session on a managed agent |
| multi-user mode, free access, credits, host rates, notification grants, data import, config, logs, restarts, local AI, opening links, Unyt, joining or resetting an identity, `sfu.ensureMembership`: `Guard::Operator` | the operator alone |
| `.local_only()` methods: `agent.unlock`, `runtime.quit`, `runtime.restart`, `identity.rekey`, `identity.custodyStatus` | the operator on a loopback listener only |

1. The launcher generates an admin credential and passes it to the embedded executor (`ExecutorSecrets.admin_credential`). The launcher's UI is the operator.
2. A headless operator sets `AD4M_ADMIN_CREDENTIAL_FILE` (or the variable). It talks to the executor with the CLI (`ad4m --admin-credential`) or the SDK.
3. Operator-only methods register `Guard::Operator`, which checks `ctx.is_operator()` (for example `user.setMultiUserEnabled`, `user.freeAccess`, `runtime.setConfig`, `runtime.logTail`, `runtime.restartHolochain`, `ai.localInstall`, `identity.claimOffer`, `identity.resetLog`, `sfu.ensureMembership`; every one is in the table in [api/AGENTS.md](../../../rust-executor/src/api/AGENTS.md)); every other caller gets 403 "Admin credential required". Main-agent methods register `Guard::Executor`, which lets the operator and main-agent grants through ([Access checks](checks.md)).
4. `api::listener_router` marks each listener with its `ListenerReach`. `listener_reach()` reads a request with a forwarding header as `Network`.
5. `ad4m-executor init --operator <did:scid>` records which person operates the executor. The main agent's first link to an identity must go to that person; managed agents are not bound by the record ([Person identity](../identity/person-identity.md)).
6. `identity.rekey` gives the main agent a new wallet key and so a new DID ([Wallet and keys](../identity/wallet-and-keys.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Principal::Operator`, `Session::open`, `is_operator`, `acts_as`.
- [rust-executor/src/agent/capabilities/mod.rs](../../../rust-executor/src/agent/capabilities/mod.rs): `ListenerReach`, `is_admin_credential_token_on`.
- [rust-executor/src/api/auth.rs](../../../rust-executor/src/api/auth.rs): `listener_reach`, `FORWARDING_HEADERS`.
- [rust-executor/src/api/mod.rs](../../../rust-executor/src/api/mod.rs): `listener_router`, `bind_api_listeners`.
- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs): `Guard::Operator`, `Guard::Executor`, `Guard::UserOrOperator`, `OPERATOR_ONLY`, `LOCAL_ONLY`.
- [rust-executor/src/identity/operator.rs](../../../rust-executor/src/identity/operator.rs): `record_operator_at_init`, `first_link_check`.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `AgentContext::Main`, `AgentService` (the main agent's state).
- [docs-src/pages/auth.mdx](../../../docs-src/pages/auth.mdx): "Admin Credential Override".

</details>
