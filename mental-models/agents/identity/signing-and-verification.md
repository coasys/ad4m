# Signing and verification

The executor signs an **expression** as an agent. It hashes the data and the timestamp with SHA-256 and signs the hash with the agent's Ed25519 key. The result is `Expression { author, timestamp, data, proof }`. Any executor checks the **expression proof** from the author DID alone. An **entanglement proof** links the main agent's DID to another key, such as a device key or a Holochain agent key.

**Why:** Every link and expression carries its author and a proof. Data that arrives through any language can therefore be checked where it arrives. The receiving executor computes the `valid` verdict itself and ignores the sender's.

**Parent:** [Identity](README.md)

<details><summary>How it works</summary>

1. `create_signed_expression(data, ctx)` builds the expression:

   | Field | Value |
   |---|---|
   | hash (not stored) | `SHA-256(serde_json(data) ‖ timestamp_rfc3339_millis)` |
   | `proof.signature` | `hex(sign_for_context(hash, ctx))` |
   | `author` | `did_for_context(ctx)` |
   | `proof.key` | `signing_key_id_for_context(ctx)` |

   The hash covers `serde_json::to_vec(data)`. The ops that language code calls sort JSON keys first (`sort_json_value`). Rust callers hash their structs in field order, so the signed bytes follow the field order of Rust types such as `Link`.

2. `sign_string_hex_for_context(text, ctx)` hashes the text with SHA-256, then signs. `agent.sign` uses it (`AgentSignature`), and so do entanglement proofs.
3. `verify(expr)` rebuilds the hash, parses the DID as a key (`PatchedKeyPair::try_from`) and checks the signature. `verify_or_false` logs a malformed proof and answers `false`.
4. RPC `agent.sign` signs as `ctx.acts_as()`. A user session signs as its managed agent, never as the main agent.
5. Language code signs through the ops in `agent_extension.rs` ([Language isolates](../../languages/runtime/isolates.md)), as `get_runtime_agent_context()`. `agent_create_signed_expression_for_user(email, data)` refuses unless the runtime runs for that user.

Entanglement proofs (`EntanglementProof`) are for the main agent only. The handlers refuse user sessions, and `sign_device_key` signs with the main key (`sign_string_hex`):

1. `agent.entanglementProofPreflight` signs a device key with the main key.
2. The caller adds the reverse signature.
3. `agent.addEntanglementProofs` stores the proof in SQLite (`Ad4mDb`).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `create_signed_expression`, `sign_for_context`, `sign_string_hex_for_context`, `AgentSignature`.
- [rust-executor/src/agent/signatures.rs](../../../rust-executor/src/agent/signatures.rs): `hash_data_and_timestamp`, `hash_message`, `verify`, `verify_or_false`, `verify_string_signed_by_did`.
- [rust-executor/src/js_core/agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs): `agent_sign`, `agent_create_signed_expression`, `agent_create_signed_expression_for_user`.
- [rust-executor/src/languages/language_runtime.rs](../../../rust-executor/src/languages/language_runtime.rs): `set_runtime_agent_context`, `get_runtime_agent_context`.
- [rust-executor/src/entanglement_service/mod.rs](../../../rust-executor/src/entanglement_service/mod.rs): `sign_device_key`, `generate_holochain_proof`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `agent.sign`, `agent.*EntanglementProofs`, `link_expression_input_to_decorated`.

</details>
