# ad4m-keyring — agent guide

Symmetric keys and the two ways a key is kept for someone else: wrapped under another
symmetric key (data at rest, opened by the executor on its own), or sealed to an X25519
public key (room keys, opened by one member's executor). No executor dependency; native and
`wasm32-unknown-unknown`.

| File | What |
|---|---|
| `src/lib.rs` | re-exports; `tags` (`DEK_WRAP_V1`, `ROOM_KEY_SEAL_V1`) |
| `src/key.rs` | `SecretKey` (one heap buffer, zeroed before it is freed, no `Clone`, `Debug` without bytes), `random_key`, `hkdf_subkey`, `KEY_LEN` |
| `src/wrap.rs` | `wrap` / `unwrap`, `SymWrap` (`alg`, `nonce`, `ct`; hex in JSON), `WrapAlg`, `WrapContext`, `KeyRef` |
| `src/seal.rs` | the room-key seal: `seal_to_x25519`, `seal_to_x25519_with_rng`, `open_from_x25519`, `SealedEnvelope` (`{ephemeralPublicKey, nonce, ciphertext}`, hex); `x25519_public_of_ed25519`, `x25519_secret_of_ed25519` |
| `src/version.rs` | `KeyVersion`, `VersionedKey<W>` |
| `src/error.rs` | `KeyringError` |
| `vectors/room-key-seal.json` | envelopes made by the server link language's `sealToDid`; read by `tests/seal.rs` and by `bootstrap-languages/server-link-language/tests/encryption.test.ts` |

## Rules

- **The seal is the language's, byte for byte.** `seal.rs` is `sealToDid` in
  `bootstrap-languages/server-link-language/src/encryption.ts` and the executor's
  `agent/sealed_box.rs`: X25519, `HKDF-SHA256(shared, salt = ephemeral public ‖ recipient
  public, info = "ad4m:sealed-box:v1")`, AES-256-GCM with no associated data, randomness
  drawn as ephemeral secret then nonce. Changing any of it makes every sealed room key
  unreadable. The vectors file is shared with the language's tests; add a vector there by
  running `sealToDid` with a fixed ephemeral secret and nonce, never by running this crate.
- **A wrap opens only in its place.** The AEAD key is HKDF of the wrapping key with
  `DEK_WRAP_V1` (never the raw key); the associated data is the tag, the keyring id, and the
  wrapped key and its wrapper as `(kind, id, version)`, each variable field length-prefixed.
  A random 24-byte nonce per wrap; never a fixed one.
- **No freed buffer holds a key.** Plaintext key bytes live only in `SecretKey` or
  `zeroize::Zeroizing`; cipher buffers are sized for the tag up front so they never
  reallocate. `tests/freed_buffers.rs` checks every buffer freed during `wrap`, `unwrap`,
  `hkdf_subkey`, the seal and the open; keep that file to one test (it installs the global
  allocator).
- **Versions only grow.** `VersionedKey`: the current version is the highest held; a new one
  is current + 1, never a reused number; the current version cannot be retired; the
  deserializer refuses anything else.
- No I/O, no clock. Randomness from the OS source (`getrandom`, `crypto.getRandomValues` on
  wasm32), or the caller's RNG in `seal_to_x25519_with_rng`.

## Commands

```bash
cargo test --release -p ad4m-keyring
cargo check --release -p ad4m-keyring --target wasm32-unknown-unknown
# the same tests on wasm32 under Node; the runner is wasm-bindgen-cli at Cargo.lock's wasm-bindgen version
CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=wasm-bindgen-test-runner cargo test --release -p ad4m-keyring --target wasm32-unknown-unknown
cargo clippy --release -p ad4m-keyring --all-targets
cargo fmt -p ad4m-keyring --check
```
