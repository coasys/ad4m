# Person identity

A person's identity is a `did:scid:coasys:1` DID: a key log signed by the person's master keys. An agent's `did:key` can be a **device key** in that log, and then it speaks for the person. The executor holds device keys only; the master keys and the 12 words stay in the person's apps.

**Why:** A master key that never reaches an executor cannot leak from it. The person adds and removes devices in the log, and every executor checks a key against the same log. Logs arrive through the agent language (the identity store), so a deployment can change the store without a change to the executor.

**Parent:** [Identity](README.md)

<details><summary>How it works</summary>

1. `KelCache` keeps each log the executor checked (`SqliteChainStore` `coasys:kel`, and the accepted head in `identity_heads`). The **trusted seq** is the seq of the store-signed head it accepted under a key of `identity.storeKeys` (`KelCache::trusted`). `IdentitySettings::from_config` takes `identity.storeUrl` and `identity.storeKeys` from the config, else the bootstrap seed's `identityStore`. A configured store URL never takes the seed's keys: it needs its own `identity.storeKeys`, else `identity.publish` and `identity.claimOffer` refuse with `no-store-keys`.
2. `record_agent_if_listed` binds a local agent's `did:key` to the SCID in `identity_agents` when the key is valid at the trusted seq and carries no bot mark. On the first binding it publishes the **person profile** ([Agent profiles](profiles.md)). `identity_of_agent` reads the binding; `agent.get` and `agent.status` carry it as `identity`.
3. `LanguageController::verify_expression_proof` checks an expression by a `did:scid` author with `identity::verify_expression`: the signing key must be valid in the log up to the trusted seq ([Signing and verification](signing-and-verification.md)).
4. Join: `ad4m-executor init --operator <did:scid>` records the operator (`record_operator_at_init`). `identity.claimOffer` (`ClaimService::claim_for_main`) claims an offer with the main wallet key. The first link of the main agent must go to the recorded operator (`first_link_check`: `already-linked`, `no-operator`, `first-link`). Managed agents are not bound by the operator record.
5. A different valid event at a held seq is a fork: the executor sends `identity-fork` to the local agents that speak for the identity. `identity.resetLog` drops the cached log.
6. `identity.rekey` (local only) gives the main agent a new wallet key; nothing changes in a log ([Wallet and keys](wallet-and-keys.md)). It revokes the main agent's grants, moves owners from the retired DID, and republishes the profile.

RPC calls: `identity.status`, `log`, `resolve`, `resetLog`, `cosignIncept`, `publish`, `openOffer`, `offers`, `claimOffer`, `joinStatus`, `devices`, `rekey`, `custodyStatus`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/identity/](../../../rust-executor/src/identity): the executor's side; file map and rules in its [AGENTS.md](../../../rust-executor/src/identity/AGENTS.md).
- [rust-executor/src/api/identity_ws.rs](../../../rust-executor/src/api/identity_ws.rs): the `identity.*` methods and their guards.
- [rust-executor/src/db/identity.rs](../../../rust-executor/src/db/identity.rs): `operator_record`, `identity_agents`, `identity_joins`.
- [ad4m-hash-chain/](../../../ad4m-hash-chain), [ad4m-identity-core/](../../../ad4m-identity-core): the log and its rules. [ad4m-identity-client/](../../../ad4m-identity-client): the master-key side. [identity-store/](../../../identity-store): the store.
- [core/src/identity/IdentityClient.ts](../../../core/src/identity/IdentityClient.ts): the SDK calls. [cli/src/identity.rs](../../../cli/src/identity.rs): `init --operator`, `claim`, `rekey`.
- [docs-src/identity/did-scid-coasys-1.md](../../../docs-src/identity/did-scid-coasys-1.md): the DID method spec.

</details>
