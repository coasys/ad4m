# ad4m-identity-core — agent guide

The identity core of `did:scid:coasys:1` (AR-01 4.2–4.4, 4.7; build plan PR 1.1, "Key log").
The key event log (KEL) is one chain type on `ad4m-hash-chain`: `Event` implements its
`Entry`, `KelPolicy` holds rules 2–10 of AR-01 4.3.3, and `fold` gives a `KelChain`
(`Chain<KelPolicy>`) whose `state()` is the `KeyState`. The format spec with every rule is
`docs-src/identity/did-scid-coasys-1.md`; the vectors are `vectors/*.json`.

| File | What |
|---|---|
| `src/lib.rs` | `FORMAT`, `DID_PREFIX`, `KEL_DOMAIN`, `KelChain`, `fold`, `fold_json`, `import`, `import_json`, `event_hash`, `derive_scid`, `commitment`, `error_code`, `did_of`, `scid_of`; re-exports of the module (`Hash`, `Proof`, `Head`, `Chain`, `ChainError`, `ChainPolicy`, `Entry`, `ForkEvidence`, `ImportOutcome`) |
| `src/event.rs` | `Event`, `Op`, `DeviceKey`, `KeyLife`, `BotScope`, `Kind`; the strict JSON parser `Event::from_json` (also behind `Deserialize`) |
| `src/policy.rs` | `KelPolicy` (`genesis`, `apply`), `FoldError`, `FormatError` |
| `src/state.rs` | `KeyState`, `KeyRecord`: `key_valid_at`, `key_valid_at_time`, `bot_scope_at`, `current_master`, `key_id_of`, `next_device_id`, `encryption_keys_at` |
| `src/keys.rs` | X25519 multikeys; key-type checks |
| `src/domain.rs` | the signing-domain registry and labels of AR-01 4.2; `signing_bytes`, `verify_signed`, `check_domain` |
| `src/pow.rs` | `Pow`, `solve`, `solve_range`, `check` (the brake's proof of work, bound to the SCID) |
| `src/link.rs` | AR-02 4.3–4.5: `Offer`, `SignedOffer`, `Claim`, `ClaimBody`, `ClaimView`, `OfferState`, `LinkError`; `offer_commit`, `sign_offer`/`verify_offer`, `make_claim`/`verify_claim`, `check_secret`, `match_code`, `check_add` (rule A7's conditions 2–5), `OfferString` (offer string and link), `DeviceString` |
| `src/build.rs` | feature `build`: `*_unsigned` builders, `signing_input`, `attach_proof`, `Signer`, `cosign`, `incept_human`, `add_claimed`, `revoke`, `scope`, `rotate`, `sign_domain` |
| `src/words.rs` | feature `words`: `generate`, `parse`, `seed`, `master_key`, `salt`, `next_commitment`, `recovery_secret`, `derive_ed25519` |
| `src/wasm.rs` | feature `wasm`: the `KelChain` class (`fold`, `import`) and `KelHandle` |
| `tests/common/mod.rs` | fixed keys, the `Log` builder, the refusal cases, the vector builders and checkers |

## Rules

- **No I/O and no clock.** Callers pass `now` to `key_valid_at_time`. Randomness only in
  `words::generate` (OS source) and `words::generate_with` (caller's RNG). Every feature builds
  for `wasm32-unknown-unknown`.
- The executor and the store enable `build`, never `words`: no node can make the words.
- `fold(&[Event])` takes typed events; anything from outside goes through `Event::from_json`
  (or `fold_json`/`import_json`), which refuses unknown ops and fields, floats, integers at or
  above 2^53, other kinds, and JSON that does not serialise back to itself (an empty `proof`
  or rotate `revoke` must be left out). That keeps the receiver's hash equal to the sender's.
- Wrong `seq`, `prev` and `scid` are the module's `ChainError::Seq`, `Prev`, `WrongChain`; a
  typed event with an integer ≥ 2^53 is `ChainError::Malformed`. Every KEL rule is
  `ChainError::Policy(FoldError)`. `FoldError::name()` is the `reason` of the error code.
- Who signs is checked strictly: each required signer has exactly one proof, and any other
  proof fails the event (`BadProof`).
- Changing an encoding, a field or a derivation changes the vectors: that is a format change
  and needs the spec updated with it.

## Tests

```bash
cargo test -p ad4m-identity-core --all-features
cargo clippy -p ad4m-identity-core --all-features --all-targets -- -D warnings
cargo fmt -p ad4m-identity-core --check
cargo build -p ad4m-identity-core --target wasm32-unknown-unknown --no-default-features
CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=$PWD/target/wasm-tools/bin/wasm-bindgen-test-runner \
  cargo test -p ad4m-identity-core --target wasm32-unknown-unknown --features build,words,wasm
```

The wasm runner must match the locked `wasm-bindgen` (see `ad4m-hash-chain/AGENTS.md` to
install it). `tests/vectors_fresh.rs` fails when the committed vectors differ from what the
generator builds; after an intended change regenerate and review the diff:
`AD4M_IDENTITY_WRITE_VECTORS=1 cargo test -p ad4m-identity-core --all-features --test vectors_fresh`.
`tests/link.rs` covers offers, claims, the match code, `add_claimed` and each condition of
`check_add`; `vectors/link.json` pins the offer commit, the claim and offer signing bytes,
the match code, the offer and device strings, and the `add` events (device and bot) that
spend a claim.
`tests/fold.rs` has one test per refusal case and fails if a case in `refusal_cases()` has none.
