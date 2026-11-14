# ad4m-keyring — agent guide

Symmetric keys and the two ways a key is kept for someone else: wrapped under another
symmetric key (data at rest, opened by the executor on its own), or sealed to an X25519
public key (room keys, opened by one member's executor). No executor dependency; native and
`wasm32-unknown-unknown`.

| File | What |
|---|---|
| `src/lib.rs` | re-exports; `tags` (`DEK_WRAP_V1`, `ROOM_KEY_SEAL_V1`, `ROOM_KEY_SEAL_V2`, `LINK_HASH_V1`) |
| `src/key.rs` | `SecretKey` (one heap buffer, zeroed before it is freed, no `Clone`, `Debug` without bytes), `random_key`, `hkdf_subkey`, `KEY_LEN` |
| `src/wrap.rs` | `wrap` / `unwrap`, `SymWrap` (`alg`, `nonce`, `ct`; hex in JSON), `WrapAlg`, `WrapContext`, `KeyRef` |
| `src/seal.rs` | the room-key seal: v1 `seal_to_x25519`, `seal_to_x25519_with_rng`, `open_from_x25519`; v2, bound to a room and version, `seal_room_key`, `seal_room_key_with_rng`, `open_room_key`, `room_key_context`; `SealedEnvelope` (`{ephemeralPublicKey, nonce, ciphertext}`, hex); `x25519_public_of_ed25519`, `x25519_secret_of_ed25519` |
| `src/version.rs` | `KeyVersion`, `VersionedKey<W>` |
| `src/error.rs` | `KeyringError` |
| `wasm/` | the JS binding (AR-08 K9), crate `ad4m-keyring-wasm`: `sealRoomKey(roomKey, recipient, context)`, `openRoomKey(sealed, secret, context)` and `roomKeyContext(roomId, version)` (seal v2), `x25519Public`, `linkHashKey` (HKDF of the room key's version 1 with `LINK_HASH_V1`), `randomRoomKey`; errors are thrown as `Error` with no key bytes. `wasm/tests/vectors.rs` runs the seal vectors through the exports on wasm32 |
| `wasm/build.sh` | builds the binding twice from one wasm: `--target deno` into `bootstrap-languages/server-link-language/keyring/keyring.js` (one ES module, the wasm inlined as base64 by `wasm/inline.mjs`) and `--target nodejs` into `link-server/keyring/`. Both outputs are generated and gitignored |
| `vectors/room-key-seal.json` | v1 envelopes made by the server link language's former `sealToDid`; read by `tests/seal.rs` |
| `vectors/room-key-seal-v2.json` | v2 envelopes (with `roomId`, `version`, `context`) made by the noble reference `bootstrap-languages/server-link-language/tests/room-key-seal-reference.ts`; read by `tests/room_key_seal.rs`, `wasm/tests/vectors.rs` and the language's `tests/keyring.test.ts`, `tests/encryption.test.ts` |

## Rules

- **The seal is the language's, byte for byte.** `seal.rs` is `sealToDid` in
  `bootstrap-languages/server-link-language/src/encryption.ts` and the executor's
  `agent/sealed_box.rs`: X25519, `HKDF-SHA256(shared, salt = ephemeral public ‖ recipient
  public, info = "ad4m:sealed-box:v1")`, AES-256-GCM with no associated data, randomness
  drawn as ephemeral secret then nonce. Changing any of it makes every sealed room key
  unreadable. The vectors file is shared with the language's tests; add a vector there by
  running `sealToDid` with a fixed ephemeral secret and nonce, never by running this crate.
- **A room key is sealed for one room and one version (seal v2, K9).** `seal_room_key` is v1's
  construction with HKDF `info = "ad4m:room-key-seal:v2"` and the context
  `coasys:room-key:<roomId>:<version>` (UTF-8) as the AES-GCM associated data; an empty
  context is refused (`EmptyContext`). A box opens only under the context it was sealed for,
  and a v1 box never opens as v2 nor the reverse, so a member cannot pass off another room's
  key or an old version as the room's current key. The language and the link server use v2
  only; v1 stays for its vectors. Add a v2 vector with the noble reference
  (`room-key-seal-reference.ts`), never by running this crate.
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
# the JS binding: its tests on wasm32, then the two builds (wasm-bindgen-cli at the locked version,
# looked up as $WASM_BINDGEN, target/wasm-tools/bin, then PATH)
CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=$PWD/target/wasm-tools/bin/wasm-bindgen-test-runner \
  cargo test --release -p ad4m-keyring-wasm --target wasm32-unknown-unknown
bash ad4m-keyring/wasm/build.sh
```

The language's `tests/keyring.test.ts` checks the deno build against the vectors byte for
byte and against noble's X25519 and HKDF.
