# ad4m-hash-chain — agent guide

The generic module under the key log of `did:scid:coasys:1`
(`docs-src/identity/did-scid-coasys-1.md`, sections 2, 4 and 5).
It stores and checks append-only, **linear** chains of signed JSON entries. It knows
bytes, hashes, signatures, order and forks; it does not know what an entry means. A
chain type gives the meaning through `ChainPolicy` (the KEL's `KelPolicy` lives in the
identity core, not here).

| File | What |
|---|---|
| `src/lib.rs` | `Entry`, `entry_hash`, `entry_hash_json`, `ChainPolicy`, `Chain<P>`, `fold`, `import`, `compare`, `ForkEvidence`, `ChainError`, `ImportOutcome` |
| `src/jcs.rs` | RFC 8785 canonical JSON; refuses floats and integers outside ±(2^53 − 1) |
| `src/codec.rs` | SHA-256 multihash (`0x12 0x20`, base58btc, no multibase prefix, `Qm…`), Ed25519 multikey (`z6Mk…`), proof value |
| `src/proof.rs` | `Proof`, `signing_input`, `verify_proof`, `verify_proof_json` (`eddsa-jcs-2022` with `domain`) |
| `src/store.rs` | feature `store`: `ChainStore`, `StoredEntry`, `StoreError`, `MemoryChainStore` |
| `src/store/sqlite.rs` | feature `sqlite`: `SqliteChainStore` (tables `chain_heads`, `chain_entries`, `chain_forks`), `append_in(&Transaction, …)` |
| `src/wasm.rs` | feature `wasm`: wasm-bindgen `entryHash`, `compare` |
| `vectors/*.json` | shared vectors; every build target must give the same answers |
| `tests/common/mod.rs` | the test chain type (`test:chain`) and the vector builder/checker |

## Rules

- **The core does no I/O, reads no clock, draws no randomness.** Only the store adapters
  touch storage. The core (no features) must build for `wasm32-unknown-unknown`.
- The entry hash is `multihash_b58(sha256(JCS(entry without "proof")))`. A receiver
  always computes it itself; never trust a hash a sender gives. Only multihash code
  `0x12` is accepted.
- The signing input is `sha256(JCS(proof without proofValue)) ‖ sha256(JCS(entry without
  proof))`; a verifier states the domain it expects. Which key must sign is the policy's
  question, never the module's.
- No function panics on outside data and there are no convenience twins: `entry_hash`
  and `signing_input` return `Result<_, CanonError>` (an entry with a float, an unsafe
  integer or no object form has no canonical form). A `*_json` variant exists only where
  the crate itself needs it (the wasm export and the store hash JSON; `verify_proof`
  builds on `verify_proof_json`).
- `Chain::append` and `import` change nothing when a check fails. Order of checks:
  canonical form, `seq`, `prev`, chain id, then the policy.
- **Fork detection only.** Two entries at one seq give `ForkEvidence`; nothing appends;
  the store keeps both (`record_fork`). The module never truncates a chain, never picks a
  branch, and a shorter peer chain is `WeExtend` (the chain type decides if that alarms).
- A store checks nothing but the expected head (and that the batch runs on from it); the
  caller runs `Chain::append`/`import` first.
- Linear core, no DAG. No networking, gossip, consensus, timestamps in entries, pruning or
  cross-call buffering of early entries: those belong to a chain type or a backend.
- Changing an encoding changes every KEL hash: the `coasys/1` bytes must not change.

## Tests

```bash
cargo test -p ad4m-hash-chain --all-features
cargo clippy -p ad4m-hash-chain --all-features --all-targets -- -D warnings
cargo fmt -p ad4m-hash-chain --check
cargo build -p ad4m-hash-chain --target wasm32-unknown-unknown --no-default-features
```

The vectors on wasm need a `wasm-bindgen-test-runner` of the same version as the locked
`wasm-bindgen` (see `Cargo.lock`), for example installed into the target dir:

```bash
cargo install wasm-bindgen-cli --version <locked version> --locked --root target/wasm-tools
CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=$PWD/target/wasm-tools/bin/wasm-bindgen-test-runner \
  cargo test -p ad4m-hash-chain --target wasm32-unknown-unknown --features wasm --test vectors --test wasm_exports
```

`tests/vectors_fresh.rs` fails when the committed vectors differ from what the generator
builds from its fixed seeds. After an intended change, regenerate and review the diff:
`AD4M_HASH_CHAIN_WRITE_VECTORS=1 cargo test -p ad4m-hash-chain --test vectors_fresh`.
A changed hash in `entry-hash.json` or `signing-input.json` is a format change.
