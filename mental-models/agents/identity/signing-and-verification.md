# Signing and verification

The executor signs an **expression** as an agent with that agent's device key. When the key sits in a key log, the expression proof carries an **anchor** (the identity, the key id, and the log position and hash at signing time), and the signature covers data, timestamp and anchor under the signing domain `coasys:link`. Without an anchor, the executor signs and checks as before identities: SHA-256 of data and timestamp, checked against the key inside the `did:key`. `signatures::verify` is the one dispatch point for both. Every signature names a **signing domain**, so a signature made for one purpose never verifies for another. An **entanglement proof** links the main agent's DID to a key outside the key log.

**Why:** The anchor fixes the position at which the key had to be valid, so nobody who relays a link can move it to a position where the key fails, and a later revocation does not void earlier links. A device-only agent and every link from before identities keep their earlier bytes, so nothing is re-signed. Domains close a class of mix-ups: before them, a link signature and any other signed message shared one scheme. The receiving executor computes the verdict itself and ignores the sender's.

**Parent:** [Identity](README.md)

<details><summary>How it works</summary>

1. `create_signed_expression(data, ctx)` builds the expression:

   | Field | No anchor (device-only key) | Anchored (key in a log) |
   |---|---|---|
   | signed bytes | `SHA-256(serde_json(data) ‖ timestamp_rfc3339_millis)` | `signing_bytes("coasys:link", {data, timestamp, anchor})` = `JCS(value + {domain})` |
   | `author` | `did_for_context(ctx)`, the device `did:key` | the same |
   | `proof` | `{ key, signature }` | `{ key, signature, anchor: { kel_did, key_id, kel_seq, kel_hash } }` |

   Unanchored, the hash covers `serde_json::to_vec(data)`. The ops that language code calls sort JSON keys first (`sort_json_value`). Rust callers hash their structs in field order, so the signed bytes follow the field order of Rust types such as `Link`. Anchored, the bytes are JCS.

2. `verify(expr)`: no anchor → the `did:key` check (`PatchedKeyPair::try_from`). Anchor → the log through the cache, the key's validity at `kel_seq` by the executor's clock, the log's hash at `kel_seq`, the signature, the first-seen rule. The result is a [link verdict](../../perspectives/links/verdicts.md), stored per link.
3. `sign_string_hex_for_context(text, ctx)` hashes the text with SHA-256 and signs. `agent.sign` uses it (`AgentSignature`), and so do entanglement proofs.
4. Language code signs through the ops in `agent_extension.rs` as `get_runtime_agent_context()` ([Language isolates](../../languages/runtime/isolates.md)). The server link language gets its X25519 seed only through `agentRoomKeySeed()`. Every other signing call, the RPCs and `agentSignStringHex` included, refuses the seed's fixed derivation message `adam-server-link-language:x25519-seed:v1`.
5. Entanglement proofs (`EntanglementProof`) are for the main agent only. The handlers refuse user sessions, and `sign_device_key` signs with the main key. `agent.entanglementProofPreflight` signs a key with the main key; the caller adds the reverse signature; `agent.addEntanglementProofs` stores the proof in SQLite. They stay the route for existing DIDs such as an Ethereum address. The key log is the route for a person's own devices.

| Signing domain | Signs | Signer |
|---|---|---|
| `coasys:kel` | key-log events | master keys, device keys at `incept` |
| `coasys:link` | anchored links and agent records | the device key |
| `coasys:store-request`, `coasys:head`, `coasys:vouch` | requests to the shared store, its heads, vouching notes | a device or master key; the store; the platform |
| `coasys:offer`, `coasys:claim`, `coasys:pause`, `coasys:inbox` | offers, claim co-signatures, bot pause records, inbox messages | master key; new device key; owner's device key; sender |
| `coasys:session` | key statements for a key login, and an executor's sign-in to the hosting index | the device key (`agent.signSessionStatement`, local only; `index_client::sign_index_statement`) |
| `coasys:account`, `coasys:room-key`, `coasys:bot-grant` | wrapped-key store requests; a member's X25519 key; bot grants | the identity's keys; the server link language; the master key |

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `create_signed_expression`, `sign_for_context`, `sign_string_hex_for_context`, `AgentSignature`.
- [rust-executor/src/agent/signatures.rs](../../../rust-executor/src/agent/signatures.rs): `verify` (the dispatch), `hash_data_and_timestamp`, `hash_message`, `verify_or_false`, `verify_string_signed_by_did`.
- [rust-executor/src/types/core.rs](../../../rust-executor/src/types/core.rs): `ExpressionProof` gains `anchor: Option<Anchor>`.
- `signing_bytes` and the domain registry in [ad4m-identity-core/src/lib.rs](../../../ad4m-identity-core/src/lib.rs) (new).
- [rust-executor/src/js_core/agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs): `agent_sign`, `agent_create_signed_expression`, `agent_create_signed_expression_for_user`; the new `agentRoomKeySeed` import.
- [rust-executor/src/entanglement_service/mod.rs](../../../rust-executor/src/entanglement_service/mod.rs): `sign_device_key`, `generate_holochain_proof`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `agent.sign`, `agent.signSessionStatement`, `agent.*EntanglementProofs`, `link_expression_input_to_decorated`.

</details>
