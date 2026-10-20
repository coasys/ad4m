# Key log

The **key log** (KEL) of an identity is a hash chain of signed events. Its first event, `incept`, names master key #0, a commitment to the next master key, and the first device keys. The hash of that event is the **SCID**, so `did:scid:coasys:1:<scid>` commits to all of them. Later events add, revoke or limit device keys and replace the master key. One Rust library with no I/O, the **identity core**, builds, signs, checks and replays the events on every side: the executor, the shared store, the apps and the auth domain.

**Why:** The format follows KERI's rules (a current key and a committed next key) in did:webvh's encoding, with no consensus. Only the 12 words make the next master key, so master key #n alone cannot rotate. The log holds no names, addresses or emails; only the bot mark enters it, because it changes what a signature means.

**Parent:** [Identity](../README.md)

## Parts
- [Key-log events](events.md): the identifier, the five events, who signs them, the replay rules, keys from the 12 words.
- [Hash-chain module](hash-chain.md): `ad4m-hash-chain`, the generic chain under the key log: hashes, links, proofs, forks, storage adapters.
- [Key-log cache and resolver](cache.md): the executor's verified copy of key logs, `author_of` and `KeyResolver`, forks, the `identity.*` calls.

The shared store that keeps every key log is on [Shared store](../../../languages/other-kinds/agent-language/shared-store.md). The executor reaches it only through the [agent language](../../../languages/other-kinds/agent-language/README.md).

<details><summary>How it works</summary>

```
 users       executor (KelCache, resolver)   shared store (identity-store)   apps (custody, phone, web, auth domain)
                   │                                  │                                │
                   └──────────────────────────────────┼────────────────────────────────┘
 chain type  KEL layer: ad4m-identity-core — Event, Op, KelPolicy, KeyState, words, pow, domain "coasys:kel"
                                              │ implements Entry and ChainPolicy
 module      ad4m-hash-chain — entry hash, links, proofs, Chain, fold, import, compare, ForkEvidence, ChainStore
```

1. A builder fetches the head, builds the next event on it, and appends. The store answers `409` with its head when another event came first; the builder replays and builds again.
2. Two events with one parent are a fork. Every holder detects it and keeps both entries; no code picks a branch.

</details>

<details><summary>Where in the code</summary>

- `ad4m-identity-core/` (new): the KEL layer ([Key-log events](events.md)).
- `ad4m-hash-chain/` (new): the chain module ([Hash-chain module](hash-chain.md)).
- `rust-executor/src/identity/` (new): `KelCache` and the resolver ([Key-log cache and resolver](cache.md)).

</details>
