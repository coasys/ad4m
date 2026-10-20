# Hash-chain module

`ad4m-hash-chain` is the generic library under the key log. It knows bytes, hashes, signatures, order and forks; it does not know what an entry means. A **chain type** gives the meaning through `ChainPolicy`: the key log is the one chain type, `KelPolicy` in the identity core. The module checks each entry's `seq` and `prev` link and its proofs, then calls the policy. It reports two different entries at one position as `ForkEvidence` and keeps both; it never picks a branch. `ChainStore` is its storage interface, with an adapter per place that holds chains.

**Why:** Three holders keep key logs: the shared store (SQLite), the executor's cache (the executor database) and the apps (IndexedDB). One tested copy of hashing, link checks, append with an expected head and fork records replaces three hand-written ones. The core is linear (one predecessor per entry, no DAG): the key log is linear, and a DAG would make every check pay for multi-parent order and make "fork" ambiguous. One Rust implementation runs natively and as WASM, with shared test vectors; a second TypeScript implementation lost, because two copies of each rule risk a fork between them. The module serves the key log only; other chain types wait until a use needs them.

**Parent:** [Key log](README.md)

<details><summary>How it works</summary>

| Piece | What it does |
|---|---|
| `entry_hash` | `multihash_b58(sha256(JCS(entry without "proof")))`; a receiver always computes the hash itself |
| `signing_input`, `verify_proof` | the `eddsa-jcs-2022` proof check against a key and a domain; which key must sign is the policy's question |
| `Chain<P>` | `genesis`, `append`, `head`, `hash_at`, `state`; nothing changes when a check fails |
| `fold`, `import` | replay a list; `import` sorts by `seq` and answers `Appended`, `AlreadyHave`, `Gap { have, first }` or `Fork(ForkEvidence)` |
| `compare` | `Same`, `TheyExtend`, `WeExtend`, `Fork`; never truncates. The key log treats a shorter peer log (`WeExtend`) as a fork |
| `ChainStore` | `head`, `range`, `append(expect)` (refuses with `Conflict(head)` when the stored head differs), `record_fork`, `forks`; `entries_since` for sync |

Storage adapters: `MemoryChainStore` (tests), `SqliteChainStore` (the shared store and the executor; `append_in(tx, …)` joins the caller's transaction), and `IndexedDbChainStore` (TypeScript, for the apps). The SQLite tables are `chain_entries`, `chain_heads` and `chain_forks`, keyed by chain type (`coasys:kel`) and chain id (the SCID).

The module adds nothing to the `coasys/1` bytes: an event hashes and verifies to the same values with or without it. Not in the module: networking, gossip, consensus, encryption, timestamps, pruning, DAGs. The centralised agent language does not run it: it signs nothing and passes events through as opaque JSON objects.

</details>

<details><summary>Where in the code</summary>

- [ad4m-hash-chain/src/lib.rs](../../../../ad4m-hash-chain/src/lib.rs) (new): `Entry`, `entry_hash`, `Proof`, `verify_proof`, `ChainPolicy`, `Chain`, `Head`, `fold`, `import`, `ImportOutcome`, `compare`, `ForkEvidence`, `ChainError`. No I/O, no clock, no randomness.
- [ad4m-hash-chain/src/store.rs](../../../../ad4m-hash-chain/src/store.rs) (new): `ChainStore`, `StoredEntry`, `StoreError`, `entries_since`; the SQLite adapter behind the feature `sqlite`.
- [ad4m-hash-chain/vectors/](../../../../ad4m-hash-chain/vectors) (new): the shared test vectors for the native and WASM builds.
- The TypeScript `ChainStore` interface and its memory and IndexedDB adapters ship with the module's WASM package.
- `KelPolicy` and `KelChain` in [ad4m-identity-core/src/lib.rs](../../../../ad4m-identity-core/src/lib.rs) (new): the key log as a chain type ([Key-log events](events.md)).

</details>
