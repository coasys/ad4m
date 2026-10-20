# Room keys

A **room key** is a versioned AES-256-GCM key that encrypts the links of one link-server room on the wire. Each member's language seals each version to the X25519 key of every admitted member, and the link server stores only these sealed envelopes. The X25519 key of an agent derives from a **room-key seed** that the executor gives the language through one narrow host call, `agentRoomKeySeed()`.

**Why:** The link server must relay a neighbourhood without reading it, so the key never reaches it in plaintext. The seed keeps the earlier derivation, so existing members keep their keys; it sits behind its own call, so the seed comes from one place only and no general signing call produces it. Any admitted member grants and rotates, not only the creator: the option that lost made a new member wait while the creator's computer slept.

**Parent:** [Server link language](README.md)

<details><summary>How it works</summary>

| Step | What happens |
|---|---|
| Seed | `agentRoomKeySeed()` returns `sha256(signature over "adam-server-link-language:x25519-seed:v1")` for the calling agent context. Every other signing API (`agentSign`, `agentSignStringHex`, `agent.sign`, entanglement preflight, `create_signed_expression`) refuses that message. |
| Ownership | The agent signs its X25519 public key under `coasys:room-key`. A member seals only to a key whose ownership signature by an admitted, valid device key checks, and skips a recipient with none. Rooms on a link server older than protocol 2 keep the ownership signature with no domain, so the language checks both forms. |
| Seal | Ephemeral X25519 ECDH, HKDF-SHA256, AES-256-GCM, through the WASM build of the `ad4m-keyring` crate; the bytes equal the older `encryption.ts` seal. |
| Version 1 | The creator's instance makes it at genesis. |
| Grant | Any admitted member that holds the latest version answers `GET keys/missing` with `POST keys/grant`. The server refuses a second grant of one version to one key (`409`); the others treat that as done. |
| Rotate | On `rotate-needed` (a `removed` link or a key revocation), or on a member's call. The server refuses (`409`) a rotation whose recipients are not exactly the admitted keys with registered X25519 keys. |
| Link hash | `HMAC-SHA256(hash_key, canonical link)`, with `hash_key = HKDF(room key v1, "adam-server-link-language:link-hash:v1")`, so a removal matches its addition and the server cannot test a guessed link. |
| Report | After each `rotate-needed` and each rotation, the language calls the host import `roomKeyEvent({ kind, removedKeys, version })`; it carries no key material. The executor uses it for the reach of a revocation. |

Key-ring states: `none` → `ready` (genesis), `pending` → `ready` (a grant), `error` (an envelope does not open; retry after 10 s), `removed` (close `4005`). `commit()` sends nothing outside `ready`; the executor keeps the diff and reports `LinkLanguageInstalledButNotSynced`.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/server-link-language/src/encryption.ts](../../../../bootstrap-languages/server-link-language/src/encryption.ts): X25519 derivation from the seed, `verifyX25519Ownership`, link encryption and the keyed link hash.
- [bootstrap-languages/server-link-language/src/key-ring-policy.ts](../../../../bootstrap-languages/server-link-language/src/key-ring-policy.ts), [index.ts](../../../../bootstrap-languages/server-link-language/index.ts): grants and rotation by any member; key-ring states.
- `ad4m-keyring` (WASM build): room-key generation, seal and open, shared with the executor and the link server.
- [rust-executor/src/agent/mod.rs](../../../../rust-executor/src/agent/mod.rs): `room_key_seed_for_context`, and `sign_for_context`, which refuses the seed message.
- [rust-executor/src/js_core/agent_extension.rs](../../../../rust-executor/src/js_core/agent_extension.rs): op behind `agentRoomKeySeed`; [ad4m-ldk/js/src/host.d.ts](../../../../ad4m-ldk/js/src/host.d.ts): its declaration.

</details>
