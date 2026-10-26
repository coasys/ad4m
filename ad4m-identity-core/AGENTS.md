# ad4m-identity-core — agent guide

The identity core of `did:scid:coasys:1`.
The key event log (KEL) is one chain type on `ad4m-hash-chain`: `Event` implements its
`Entry`, `KelPolicy` holds the spec's rules K1–K15, and `fold` gives a `KelChain`
(`Chain<KelPolicy>`) whose `state()` is the `KeyState`; `did_document` derives the DID Core
document at a seq. The spec with every rule is `docs-src/identity/did-scid-coasys-1.md`; the
vectors are `vectors/*.json`. The spec has three parts: Part A, the format (sections 1–12:
events, replay, key validity, resolution, the DID document), binds every verifier and
resolver; Part B, the host profile (13–14: the HTTPS log route, and admission such as the
proof of work as host policy), binds hosts; Part C, the wallet profile (15–16: the 12 words,
the signing domains other than `coasys:kel`), is informative.

| File | What |
|---|---|
| `src/lib.rs` | `FORMAT`, `DID_PREFIX`, `KEL_DOMAIN`, `KelChain`, `fold`, `fold_json`, `import`, `import_json`, `event_hash`, `derive_scid`, `commitment`, `error_code`, `did_of`, `scid_of`; re-exports of the module (`Hash`, `Proof`, `Head`, `Chain`, `ChainError`, `ChainPolicy`, `Entry`, `ForkEvidence`, `ImportOutcome`) |
| `src/event.rs` | `Event`, `Op`, `DeviceKey`, `KeyLife`, `BotScope`, `Kind`; the strict JSON parser `Event::from_json` (also behind `Deserialize`) |
| `src/policy.rs` | `KelPolicy` (`genesis`, `apply`), `FoldError`, `FormatError` |
| `src/state.rs` | `KeyState`, `KeyRecord`: `key_valid_at`, `key_valid_at_time`, `bot_scope_at`, `current_master`, `key_id_of`, `next_device_id`, `encryption_keys_at` |
| `src/keys.rs` | X25519 multikeys; key-type checks |
| `src/document.rs` | `did_document(chain, version, also_known_as)`: `DidDocument`, `VerificationMethod`, `DocumentMetadata`, `DocumentError` (spec section 9); the contexts `DID_CONTEXT`, `MULTIKEY_CONTEXT`, `COASYS_CONTEXT` |
| `src/domain.rs` | the signing-domain registry and labels (spec sections 7 and 16); `signing_bytes`, `verify_signed`, `check_domain`; `LINK_DOMAIN`, `HEAD_DOMAIN`, `REQUEST_DOMAIN`; `HeadStatement` (what the store signs for a head; the store signs it, executors verify it) |
| `src/pow.rs` | `Pow`, `solve`, `solve_range`, `check` (the brake's proof of work, bound to the SCID; host policy, spec section 14) |
| `src/link.rs` | device linking: `Offer`, `SignedOffer`, `Claim`, `ClaimBody`, `ClaimView`, `OfferState`, `LinkError`; `offer_commit`, `sign_offer`/`verify_offer`, `make_claim`/`verify_claim`, `check_secret`, `match_code`, `check_add` (conditions 2–5 of the store's claim check, `identity-store/AGENTS.md`), `OfferString` (offer string and link), `DeviceString` |
| `src/build.rs` | feature `build`: `*_unsigned` builders, `signing_input`, `attach_proof`, `Signer`, `cosign`, `incept_human`, `add_claimed`, `revoke`, `scope`, `rotate`, `sign_domain` |
| `src/words.rs` | feature `words`: `generate`, `parse`, `seed`, `master_key`, `salt`, `next_commitment`, `recovery_secret`, `derive_ed25519` |
| `src/wasm.rs` | feature `wasm`: the `KelChain` class (`fold`, `import`) and `KelHandle` (`state`, `didDocument(version?, alsoKnownAs?)`) |
| `tests/common/mod.rs` | fixed keys, the `Log` builder, the refusal cases, the vector builders and checkers |

## Rules

- **No I/O and no clock.** Callers pass `now` to `key_valid_at_time`. Randomness only in
  `words::generate` (OS source) and `words::generate_with` (caller's RNG). Every feature builds
  for `wasm32-unknown-unknown`.
- The executor and the store enable `build`, never `words`: no node can make the words.
- The DID document is time-free: it lists the keys `key_valid_at` gives at the seq and carries
  an expiry as `coasys:expires` / `coasys:bot`, never as a filter. Bot keys go only in
  `capabilityInvocation`; masters are never in the document. `deactivated` says the log has
  ended, whatever the version.
- Signed or long-lived data (log events, link anchors, agent records, expressions) carries the
  bare DID, never a DID URL with `src`. `src` belongs only where a reader needs a place to look
  (shared links, QR codes, `alsoKnownAs`).
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
`tests/document.rs` covers the DID document mapping (live, revoked, `Until`, bot, rotated,
ended, past the head, member order); `vectors/document.json` pins the document and metadata
of the log of `log.json` at the head and after every seq.
`tests/fold.rs` has one test per refusal case and fails if a case in `refusal_cases()` has none.
