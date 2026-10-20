# Operator and main agent

The **operator** is the identity that runs an executor. The executor keeps one **operator record**: the operator's `did:scid`, set at first run and changed only by that operator. The operator role (`Principal::Operator`, every capability) has four sources (`OperatorSource`): the **admin credential**, the loopback rule, a key login by the recorded identity from the person's own app, and the desktop's **local session**. The operator acts as the **main agent** (`AgentContext::Main`), whose wallet key is the executor's own device key in the operator's key log. A few calls are **local-only**: they answer only the admin credential or the loopback caller, never a remote or browser session.

**Why:** The operator's rights belong to the person, so they must reach the executor from any of the person's devices; a key login checked against the key index does that ([Key login](key-login.md)). The record is named at first run, and the first device link must come from that identity: "the first identity to link becomes the operator" would let a stranger who reaches a fresh server first claim it. Only the recorded operator changes the record. A reset with the admin credential lost: it would let anyone with shell access on the machine take the executor's rights, so a lost operator identity means a new data folder. The admin credential stays in the desktop shell's main process, and the main window gets a 1-hour local session instead, so no web view or template holds the credential. Calls that touch the machine (files, the wallet key, the storage secret, quitting) stay local, because a remote stop or a remote key change strands the computer.

**Parent:** [Access](README.md)

<details><summary>How it works</summary>

| `OperatorSource` | Comes from | Grant | Local-only calls |
|---|---|---|---|
| `Credential` | the admin credential (desktop shell, CLI, `ad4m-executor storage unlock`) | none | yes |
| `Loopback` | an empty token, no credential configured, loopback listener, no forwarding header | none | yes |
| `Key` | a key login by the recorded identity, native app or first-party web origin | `role: Operator`, key-bound, 1 h | no (`403 local-only`) |
| `Local` | `runtime.localSession` (the shell, with the credential) | `role: Operator`, `client: Local`, 1 h | no |

Setting the record:

| Who | How | When |
|---|---|---|
| a server's first run | `ad4m-executor init --operator <did:scid>` or `run --operator` | record empty; a different value later stops the start |
| the desktop first run | the shell calls `runtime.setOperator` with the admin credential | record empty |
| the recorded operator | `runtime.setOperator` from a key session of that identity | any time; revokes every operator grant, publishes `operator-changed` |

1. `runtime.operator` reads the record (`agent:READ`); the identity is public.
2. `OperatorRecord::admits_first_link(offer_scid)`: while the executor's own key is in no log, a claim for the main agent passes only for an offer from the recorded identity. A server with no record opens no link route ([Device linking](../identity/devices/linking.md)).
3. Local-only methods (`.local_only()`): `runtime.importData`, `runtime.exportData`, `runtime.localSession`, `runtime.quit`, `runtime.setConfig` for `machine` keys; `agent.generate`, `agent.import`, `agent.lock`, `agent.unlock`, `agent.signSessionStatement`; `storage.unlock`, `storage.rotateSecret`, `storage.reencrypt`, `storage.migrateAgentPassphrase`; `identity.prepareDevice`, `identity.inbox`, `identity.inboxAck`, `identity.rekey`, `identity.takeMaster` (the credential only, on an executor started with `--custody-channel`; [Master key](../identity/devices/master-key.md)).
4. The home-network listener marks every request `ListenerReach::Network`: the loopback rule does not apply there, and every local-only call refuses ([RPC API](../interfaces/rpc-api.md)).
5. Multi-user mode needs an admin credential: without one, a tokenless loopback caller is the operator, and that caller must never be a user's neighbour.
6. `ad4m-executor rekey` and `ad4m-executor storage unlock` reach their local-only calls with the admin credential over loopback ([Rust client and CLI](../interfaces/rust-client-and-cli.md)). On a server, the machine's owner can replace the main wallet key this way, but not the operator record.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/session.rs](../../../rust-executor/src/agent/session.rs): `Principal::Operator`, `OperatorSource`, `operator_source`, `is_operator`, `acts_as`.
- `OperatorRecord` and the `operator_record` table (one row: `scid`, `set_at`, `set_by`); `runtime.operator`, `runtime.setOperator`, `runtime.localSession` in [runtime_ws.rs](../../../rust-executor/src/api/runtime_ws.rs).
- [rust-executor/src/agent/capabilities/mod.rs](../../../rust-executor/src/agent/capabilities/mod.rs): `ListenerReach`, `is_admin_credential_token_on`.
- [rust-executor/src/api/auth.rs](../../../rust-executor/src/api/auth.rs): `listener_reach`, `FORWARDING_HEADERS`.
- [rust-executor/src/api/ws_handler.rs](../../../rust-executor/src/api/ws_handler.rs): the `.local_only()` method flag.
- [rust-executor/src/config.rs](../../../rust-executor/src/config.rs): `check_admin_credential`, `refuse_multi_user_from_db`.
- [cli/src/ad4m_executor.rs](../../../cli/src/ad4m_executor.rs): `--operator` on `init` and `run`.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `AgentContext::Main`, `AgentService`.

</details>
