# Identity

A person's **identity** is `did:scid:coasys:1:<scid>`. Its public **key log** lists the **master key**, which signs every change to the log, and every **device key** that may sign as the person. An executor holds device keys only: its wallet key `main`, and one wallet key per managed agent. Each signs as an agent with its `did:key`, and the executor finds the person behind any `did:key` through the key log. An agent with no key log is a **device-only identity**: its DID is its `did:key`, as every agent was before identities.

**Why:** A `did:key` needs no registry, but it cannot rotate and names one key. A hash-chained key log that commits to the next master key gives one person many keys that can be added, revoked and replaced, and any party checks it without trusting Coasys: the shared store can withhold an event but never forge one. Links keep naming the device key, so the existing link format, signatures and apps keep working. The master key never rests in an executor, because an executor runs language code from the network and may serve other people.

**Parent:** [Agents](../README.md)

## Parts
- [Key log](key-log/README.md): the identifier, the five events, the hash-chain module, and the executor's cache and resolver.
- [Devices](devices/README.md): how a device, an executor or a bot joins a key log; the device inbox and the private record; where the master key lives.
- [Wallet and keys](wallet-and-keys.md): the executor's store of device keys, its file, and the passphrase derived from the storage secret.
- [Signing and verification](signing-and-verification.md): signing domains, anchored links, the one verification dispatch, entanglement proofs.
- [Agent profiles](profiles.md): the agent record, where it is stored, how it is published and looked up.

<details><summary>How it works</summary>

```
 12 words ──► master key #n (custody service, outside the executor)
                 │ signs incept, add, revoke, rotate, scope
                 ▼
 key log of did:scid:coasys:1:<scid> ──► shared store (agent language) ──► KelCache in each executor
                 │ lists device keys d1, d2, … (+ bot mark)
                 ▼
 AgentContext ──wallet_key_name()──► wallet key = device key dN ──► did:key:z6Mk…
      │                                   └── sign(payload) ──► Ed25519 signature
      ▼
 create_signed_expression(data, ctx) ──► Expression { author: did:key, timestamp, data, proof { key, signature, anchor } }
      │
      ▼
 reader: resolver.author_of(did:key) ──► Person { did:scid } │ Bot { owner } │ NoLog │ Unavailable
```

| Thing | Where it lives | Who changes it |
|---|---|---|
| Identity (`did:scid`) and key log | shared store; copies in executor caches and the phone app | the master key ([Key log](key-log/README.md)) |
| Master key | the person's apps, behind an OS unlock or a password ([Master key](devices/master-key.md)) | made from the 12 words |
| Device key of an executor | the executor's wallet ([Wallet and keys](wallet-and-keys.md)) | made from random bytes, listed by an `add` or `incept` |
| The identity of each agent context | `AgentService.scid`, and the managed agent's `AgentData.scid` | `identity.publish`, `identity.joinStatus` |

1. Rust code passes the `AgentContext` explicitly. Language code signs as the agent that its runtime runs for at that moment (`get_runtime_agent_context()`).
2. An anchored link carries the key-log position at which it was signed. Readers check it against the log ([Signing and verification](signing-and-verification.md)) and store one of four [link verdicts](../../perspectives/links/verdicts.md).
3. `agent.get` keeps `did` as the device `did:key` and adds `identity`, the `did:scid` or `null`.
4. An agent has two code shapes. `AgentService`, a global singleton, holds the main agent (`did`, `agent`, `signing_key_id`, `scid`). A managed agent is a `users` row with a wallet key, and a tenant also a `tenants` row; code reaches it through static functions in `agent/mod.rs` ([Managed agents](../executor/managed-agents/README.md)).
5. The language ops `agent_did` and the signing ops follow `get_runtime_agent_context()`. The ops `agent_did_document` and `agent` return the main agent's DID document and profile ([agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs)).

</details>

<details><summary>Where in the code</summary>

- [ad4m-identity-core/](../../../ad4m-identity-core) (new): events, replay, the words, the signing domains; native and WASM.
- [ad4m-hash-chain/](../../../ad4m-hash-chain) (new): the generic hash-chain module under the key log.
- [rust-executor/src/identity/](../../../rust-executor/src/identity) (new): the executor's key-log cache, resolver, device linking, inbox, bot log and `identity.*` RPC.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `AgentContext`, `AgentService` (gains `scid`), `did_for_context`, `sign_for_context`, `create_signed_expression`.
- [rust-executor/src/agent/signatures.rs](../../../rust-executor/src/agent/signatures.rs): verification.
- [rust-executor/src/wallet.rs](../../../rust-executor/src/wallet.rs): `WalletBackend`, `LocalWallet`, `SharedWallet`.
- [rust-executor/src/js_core/agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs): the agent ops that language code calls.
- [docs-src/identity/did-scid-coasys-1.md](../../../docs-src/identity/did-scid-coasys-1.md) (new): the format spec with test vectors.
- The agent language carries every identity call: [Agent language](../../languages/other-kinds/agent-language/README.md).

</details>
