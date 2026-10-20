# Key log

The **key log** (KEL) of an identity is a hash chain of signed events. Its first event, `incept`, names master key #0, a commitment to the next master key, and the first device keys. The hash of that event is the **SCID**, so `did:scid:coasys:1:<scid>` commits to all of them. Later events add, revoke or limit device keys and replace the master key. One Rust library with no I/O, the **identity core**, builds, signs, checks and replays the events on every side: the executor, the shared store, the apps and the auth domain.

**Why:** The format follows KERI's rules (a current key and a committed next key) in did:webvh's encoding, with no consensus. Master key #n alone cannot rotate: only the 12 words make master key #n+1. Two events with one parent are a fork; the store appends only at its head, and every holder detects a fork and keeps both entries. The log holds no names, addresses or emails; only the bot mark enters it, because it changes what a signature means. The generic chain mechanics sit in a separate module, so the store, the executor and the apps share one tested copy instead of three.

**Parent:** [Identity](../README.md)

## Parts
- [Key-log events](events.md): the identifier, the five events, who signs them, the replay rules, keys from the 12 words.
- [Hash-chain module](hash-chain.md): `ad4m-hash-chain`, the generic chain under the key log: hashes, links, proofs, forks, storage adapters.
- [Key-log cache and resolver](cache.md): the executor's verified copy of key logs, `author_of` and `KeyResolver`, forks, the `identity.*` calls.

The shared store that keeps every key log, the key index and the brake on new logs is on [Shared store](../../../languages/other-kinds/agent-language/shared-store.md). The executor reaches it only through the [agent language](../../../languages/other-kinds/agent-language/README.md).

<details><summary>How it works</summary>

```
 users       executor (KelCache, resolver)   shared store (identity-store)   apps (custody, phone, web, auth domain)
                   │                                  │                                │
                   └──────────────────────────────────┼────────────────────────────────┘
 chain type  KEL layer: ad4m-identity-core — Event, Op, KelPolicy, KeyState, words, pow, domain "coasys:kel"
                                              │ implements Entry and ChainPolicy
 module      ad4m-hash-chain — entry hash, links, proofs, Chain, fold, import, compare, ForkEvidence, ChainStore
                                              │
 storage     MemoryChainStore (tests) · SqliteChainStore (store, executor) · IndexedDbChainStore (apps)
```

| Copy of a log | Holder | Trusted for |
|---|---|---|
| `chain_entries` in the store's SQLite file | the shared store | staying up; not for being complete or honest |
| `KelCache` in the executor database | every executor that met the identity | its own history: it never accepts a shorter log or a different hash at a held position |
| the phone app's copy | the phone app | the same rule |

1. A builder fetches the head, builds the next event on it, and appends. The store answers `409` with its head when another event came first; the builder replays and builds again.
2. An executor accepts a correctly signed event from any source (the store, a neighbourhood, an app, another executor), not only from the store.
3. A new log enters the store with a **vouching note** (an email was checked, at web sign-up) or through the **brake** (a proof of work and rate limits; [Shared store](../../../languages/other-kinds/agent-language/shared-store.md)). The executor passes the proof of work through and computes nothing.
4. A fork is detected and recorded, and no code picks a branch: the module keeps both entries, and the KEL layer has no branch choice. An executor raises the `identity-fork` alarm and keeps its own history.

</details>

<details><summary>Where in the code</summary>

- [ad4m-identity-core/](../../../../ad4m-identity-core) (new): crate `ad4m-identity-core`; features `words` and `build`.
- [ad4m-hash-chain/](../../../../ad4m-hash-chain) (new): crate `ad4m-hash-chain`.
- [identity-store/](../../../../identity-store) (new): the shared store's binary, on both crates.
- [rust-executor/src/identity/](../../../../rust-executor/src/identity) (new): `KelCache`, the resolver and the RPC.
- [ad4m-ldk/rust/src/identity.rs](../../../../ad4m-ldk/rust/src/identity.rs) (new) and [ad4m-ldk/js/src/types.ts](../../../../ad4m-ldk/js/src/types.ts): the wire types (`LogPage`, `SignedHead`, `AppendRequest`, `KeyIndexEntry`, `SignedRequest`, `AgentRecord`, `VouchNote`, `PowParams`); events travel inside them as opaque JSON objects.
- [docs-src/identity/did-scid-coasys-1.md](../../../../docs-src/identity/did-scid-coasys-1.md) (new): the format spec and its test vectors.

</details>
