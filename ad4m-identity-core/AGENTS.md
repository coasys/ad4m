# ad4m-identity-core — agent guide

The identity core of `did:scid:coasys:1`.
The key event log (KEL) is one chain type on `ad4m-hash-chain`: `Event` implements its
`Entry`, `KelPolicy` holds the spec's rules K1–K15, and `fold` gives a `KelChain`
(`Chain<KelPolicy>`) whose `state()` is the `KeyState`; `did_document` derives the DID Core
document at a seq. The spec with every rule is `docs-src/identity/did-scid-coasys-1.md`; the
vectors are `vectors/*.json`. The spec has three parts: Part A, the format (sections 1–12:
events, replay, key validity, resolution, the DID document), binds every verifier and
resolver; Part B, the host profile (13–14: the HTTPS log route, and admission such as the
proof of work as host policy), binds hosts; Part C, the wallet profile (15–17: the 12 words,
the signing domains other than `coasys:kel`, the device inbox, the private record and the
request box), is informative.

| File | What |
|---|---|
| `src/lib.rs` | `FORMAT`, `DID_PREFIX`, `KEL_DOMAIN`, `KelChain`, `fold`, `fold_stored` (feature `stored-fold`), `fold_json`, `import`, `import_json`, `event_hash`, `derive_scid`, `commitment`, `error_code`, `did_of`, `scid_of`; re-exports of the module (`Hash`, `Proof`, `Head`, `Chain`, `ChainError`, `ChainPolicy`, `Entry`, `ForkEvidence`, `ImportOutcome`) |
| `src/event.rs` | `Event`, `Op`, `DeviceKey`, `KeyLife`, `BotScope`, `Kind`; the strict JSON parser `Event::from_json`, and `Event::from_json_text` for text (also behind `Deserialize`; refuses a duplicate member name) |
| `src/policy.rs` | `KelPolicy` (`genesis`, `apply`), `FoldError`, `FormatError` |
| `src/state.rs` | `KeyState`, `KeyRecord`: `key_valid_at`, `key_valid_at_time`, `bot_scope_at`, `current_master`, `key_id_of`, `device_key_id_of` (content signers: device keys only), `next_device_id`, `encryption_keys_at` |
| `src/keys.rs` | X25519 multikeys; key-type checks; `X25519Secret`, `x25519_multikey_of`, `x25519_secret_of_ed25519` (an executor key's `enc` secret) |
| `src/seal.rs` | the one HPKE construction (RFC 9180 base mode, X25519/HKDF-SHA256/ChaCha20-Poly1305, `base64url(enc ‖ ct)`), crate-private; used by `inbox` and `record` |
| `src/inbox.rs` | the device inbox (spec section 17): `InboxAddress`, `pre_address`, `InboxPayload`, `InboxBody` (`OfferTicket`, `Master`), `sign_payload`, `seal`/`open`, `check_payload` → `Accept` (`ClaimOffer`, `KeepMaster`, `Stale`), `PreInboxAuth` (`sign_pre_auth`, `verify_pre_auth`), `check_sealed` (the store's shape check), the limits |
| `src/record.rs` | the private details record: `SealedRecord`, `PrivateRecord`, `DeviceEntry` (with `holdsMaster: true`, `masterIndex` and `vault`), `VaultId` (16 random bytes per identity and physical vault, base58btc on the wire; copies are vaults), `DeviceKind`, `Place`, `HoldsMaster`, `PendingRevocation`, `RevocationReason`; `readers` (who a record is wrapped to), `seal_record` (signs the envelope), `open_record` (verifies the receipt and the envelope first), `verify_envelope` → `Writer` (at the head), `envelope_signing_bytes`, `check_sealed` (the store's check), the store's `RecordReceipt` (`sign_receipt`, `verify_receipt` → `Writer` at the receipt's seq, `envelope_hash`, `RECEIPT_DOMAIN`), `record_aad`, `wrap_info`; the recovery key's `RecoveryAttestation`, `Recovery`, `attest_recovery_key`, `verify_attestation`, `recovery_of`, `attestation_signing_bytes` |
| `src/requests.rs` | the request box: `DeviceRequestBody` (`SendMaster`, `ConfirmRevoke`), `PostedRequest`, `check_post`, `REQUEST_TTL_S`, `MAX_OPEN_REQUESTS` |
| `src/document.rs` | `did_document(chain, version, also_known_as)`: `DidDocument`, `VerificationMethod`, `DocumentMetadata`, `DocumentError` (spec section 9); the contexts `DID_CONTEXT`, `MULTIKEY_CONTEXT`, `COASYS_CONTEXT` |
| `src/domain.rs` | the signing-domain registry and labels (spec sections 7 and 16); `signing_bytes`, `verify_signed`, `check_domain`; `LINK_DOMAIN`, `AGENT_RECORD_DOMAIN`, `HEAD_DOMAIN`, `REQUEST_DOMAIN`, `SESSION_DOMAIN` (key statements: a device key's login on an executor); `HeadStatement` (what the store signs for a head; the store signs it, executors verify it) |
| `src/pow.rs` | `Pow`, `solve`, `solve_range`, `check` (the brake's proof of work, bound to the SCID; host policy, spec section 14) |
| `src/link.rs` | device linking: `Offer`, `SignedOffer`, `Claim`, `ClaimBody`, `ClaimView`, `OfferState`, `LinkError`; `offer_commit`, `sign_offer`/`verify_offer`, `make_claim`/`verify_claim`, `verify_claim_view` (the holder's check of the store's claim view before `add_claimed`), `check_secret`, `match_code`, `check_add` (conditions 2–5 of the store's claim check, `identity-store/AGENTS.md`), `may_keep_master` (whether the log lets a device key keep the master key: a standing key of the incept, or an `add` whose co-signature is over a claim that said `canKeepMaster`; never the record's kind), `OfferString` (offer string and link), `DeviceString` |
| `src/build.rs` | feature `build`: `*_unsigned` builders, `signing_input`, `attach_proof`, `Signer`, `cosign`, `incept_human`, `add_claimed`, `revoke`, `scope`, `rotate`, `sign_domain` |
| `src/words.rs` | feature `words`: `generate`, `parse`, `seed`, `master_key`, `salt`, `next_commitment`, `recovery_secret`, `derive_ed25519` |
| `src/wasm.rs` | feature `wasm`: the `KelChain` class (`fold`, `import`) and `KelHandle` (`state`, `didDocument(version?, alsoKnownAs?)`) |
| `tests/common/mod.rs` | fixed keys, the `Log` builder, the refusal cases, the vector builders and checkers |

## Rules

- **No I/O and no clock.** Callers pass `now` to `key_valid_at_time`. Randomness only in
  `words::generate` (OS source) and in the caller's RNG (`words::generate_with`, `inbox::seal`,
  `record::seal_record`). Every feature builds for `wasm32-unknown-unknown`.
- **One seal.** Everything sealed to an X25519 key goes through `seal.rs`; its `info` is a
  label, `0x00` and the place (`coasys:inbox` + address, `coasys:record` + scid), so a seal for
  one place never opens at another. `hpke` stays on 0.12 (rand_core 0.6, like the rest of the
  workspace); 0.13 moved to rand_core 0.9.
- **Secrets are wiped and never printed.** The words, the seed, a record key, an opened seal's
  plaintext and a master key decoded from the inbox sit in `zeroize::Zeroizing`; `Debug` of
  `Words`, `Seed` and `InboxBody::Master` leaves the secret out.
- **A record is wrapped to `record::readers`**: every device key valid at the head without the
  bot mark, plus `"recovery"` exactly when `recoveryKey` is set; a fresh record key and nonce
  every version. The store checks the set with `record::check_sealed`.
- **A record is signed by its writer** (`coasys:private-record`, `record::verify_envelope`): a
  device key without the bot mark valid at the head, or the current master, when it writes.
  `check_sealed` (the store, on a write) verifies the signature against the log at its head;
  the AAD binds `scid`, `version` and `recoveryKey`.
- **A reader trusts the store's receipt for the seq** (`record::RecordReceipt`,
  `coasys:record-receipt` over `{scid, version, envelope, seq, store}`, signed by the store's
  head key; `sign_receipt`, `verify_receipt`). `open_record` takes the receipt and the trusted
  store multikeys, checks that the receipt names this scid, version and envelope
  (`envelope_hash`, the signed envelope with `sig`) and that its seq is at or below the
  reader's head, then checks the writer at the receipt's seq, not at the head: a record
  written before its writer's revocation or a rotation still opens. A new write is checked at
  the head (`seal_record`, `check_sealed`), so a revoked key cannot write again. A store can
  serve back an older envelope with its receipt, so a reader keeps the highest version it has
  read per identity and refuses a lower one; that high-water mark is the client's state, not
  the core's.
- **A recovery key counts only with the current master's attestation**
  (`record::RecoveryAttestation`, `coasys:recovery-key` over `{scid, recoveryKey, keyId}`;
  `attest_recovery_key`, `verify_attestation`, `recovery_of`). A device writer passes
  `recovery_of(old record)` to `seal_record` unchanged. A master writer installs or replaces
  the key with `attest_recovery_key`. After a rotation the old attestation no longer counts:
  `recovery_of` gives `None`, and `check_sealed` and `seal_record` refuse it. `open_record`
  still opens such a record, and refuses an attestation no master of the log signed. The store
  lets only a master-signed write change the recovery key. So after a rotation the master's
  holder attests the recovery key with the new master and rewrites the record promptly.
- An internally tagged unit variant ignores extra fields under `deny_unknown_fields`; a kind
  with no fields is an empty struct variant (`DeviceRequestBody::SendMaster {}`).
- The executor and the store enable `build`, never `words`: no node can make the words. Only
  the store enables `stored-fold`.
- Only a bot scope expires. A device key's validity over time comes from the log's events, so
  `fold` refuses `expires` on a key without `kind: "bot"` (`ExpiresWithoutBotMark`).
- The DID document is time-free: it lists the keys `key_valid_at` gives at the seq and carries
  a bot scope's expiry in `coasys:bot`, never as a filter. Bot keys go only in
  `capabilityInvocation`; masters are never in the document. `deactivated` says the log has
  ended, whatever the version.
- Signed or long-lived data (log events, link anchors, agent records, expressions) carries the
  bare DID, never a DID URL with `src`. `src` belongs only where a reader needs a place to look
  (shared links, QR codes, `alsoKnownAs`).
- `fold(&[Event])` takes typed events; anything from outside goes through `Event::from_json`
  (or `fold_json`/`import_json`), which refuses unknown ops and fields, floats, integers at or
  above 2^53, other kinds, and JSON that does not serialise back to itself (an empty `proof`
  or rotate `revoke` must be left out). That keeps the receiver's hash equal to the sender's.
  Event text goes through `Event::from_json_text` (or serde's `Deserialize` for `Event`), never
  through a `serde_json::Value` first: those refuse an object that names a member twice
  (`DuplicateMember`, rule P6), which a `Value` has already collapsed to its last member.
- `encryption_keys_at` gives the standing keys only, as `keyAgreement` does: a bot has no inbox.
- `verify_claim_view` refuses a view whose `deviceKey` is not a bound offer's `deviceKey`
  (`Bound`): a store can swap the whole key with a fresh co-signature, which only the binding
  catches.
- Wrong `seq`, `prev` and `scid` are the module's `ChainError::Seq`, `Prev`, `WrongChain`; a
  typed event with an integer ≥ 2^53 is `ChainError::Malformed`. Every KEL rule is
  `ChainError::Policy(FoldError)`. `FoldError::name()` is the `reason` of the error code.
- Who signs is checked strictly: each required signer has exactly one proof, and any other
  proof fails the event (`BadProof`).
- `fold_stored` is for a host's own stored, already-verified log only (the identity store on
  a miss in its held folds). It leaves out the signature checks and the duplicate-key scan
  (K8, quadratic over a long log), both of which only refuse, and gives the same chain as
  `fold` for every log `fold` accepts (`tests/fold_stored.rs`). Never use it on events from a
  peer, a client or a wallet: a forged proof or a reused key would pass. Executors and
  wallets use `fold` and `import`. It sits behind the `stored-fold` feature, which only the
  identity store enables, so no other crate reaches it by accident. Every check that is not a
  signature or the duplicate-key scan runs in it, the bot actions of a `scope` included.
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
`tests/inbox.rs` covers seals (round trip, wrong key, wrong address, tampering), every refusal
of `check_payload`, offer tickets and pre-join reads; `tests/record.rs` the wrapping (device
keys and the recovery key, never a bot key), a revoked device and the next version, the AAD,
the store's check and the record JSON; `tests/requests.rs` expiry and `confirm-revoke` keys.
`vectors/devices.json` pins them with fixed X25519 secrets and the RNG stream of
`tests/common::TestRng`.
`tests/document.rs` covers the DID document mapping (live, revoked, bot, rotated,
ended, past the head, member order); `vectors/document.json` pins the document and metadata
of the log of `log.json` at the head and after every seq.
`tests/fold.rs` has one test per refusal case and fails if a case in `refusal_cases()` has none.
`tests/fold_stored.rs` pins `fold_stored` to `fold`: the same chain at every seq of a log of
every op, the same refusal for every case but a bad signature or a duplicate key, and the
proofs' presence and purpose still checked.
`vectors/fold.json` holds `cases` (events as JSON values) and `textCases` (events as JSON text,
for what a value cannot hold: a duplicate member name), from `refusal_cases()` and
`text_refusal_cases()`.
