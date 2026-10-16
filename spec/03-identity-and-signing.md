# 3. Identity & Signing

This section specifies the identity model — how agents are named, where their keys live, how Expressions get signed, and how signatures are verified. The wire shape of an `Expression` is defined in [§2.2](./02-core-data-model.md#22-expression); this section says what's inside `proof` and how to compute / check it.

## 3.1 Identity model

Every AD4M agent is identified by a single W3C **Decentralized Identifier (DID)**.

A conforming executor MUST support the **`did:key` method** ([did:key spec](https://w3c-ccg.github.io/did-method-key/)) with **Ed25519** key pairs as the baseline. A `did:key` DID has the form:

```text
did:key:z6Mk...
```

For `did:key`:

- The DID is derived deterministically from the Ed25519 public key. No resolver round-trip is needed.
- The DID Document is generated locally from the DID itself and contains exactly one verification method, whose ID is `<did>#<key-fragment>`.

Implementations MAY support additional DID methods (e.g. `did:web`, `did:pkh`, `did:ion`), in which case:

- DID resolution MUST yield a DID Document containing a verification method whose ID matches `proof.key` on Expressions to be verified.
- The private key MUST be imported into the wallet — the executor cannot generate keys for non-`did:key` methods because those DIDs are bound to externally-managed keys.
- The signature-verification algorithm MUST match what the DID Document's verification method declares.

`did:key` remains the protocol baseline that **every** conforming implementation MUST support; other methods are optional for cross-implementation interop.

## 3.2 Wallet

The executor MUST maintain a **wallet** — an encrypted local keystore — that holds the agent's private key(s) and the corresponding DID Document(s).

The wallet:

- Holds at minimum one key, conventionally named `"main"`, which is the agent's identity key.
- Encrypts keys at rest. The reference implementation derives an encryption key from the user's passphrase using **Argon2id** (`m=19456, t=2, p=1`) and encrypts with **XSalsa20-Poly1305** via NaCl's `SalsaBox`. See [`rust-executor/src/wallet.rs`](../rust-executor/src/wallet.rs).
- Is unlocked once per session with the passphrase. Until unlocked, the executor SHOULD reject any operation that requires signing.

The wallet exposes (to the rest of the executor, not to apps) three primitives:

- `did(name) → string`
- `did_document(name) → Document` (containing the verification method ID, used as `proof.key`)
- `sign(name, payload: &[u8]) → Vec<u8>`

Apps never see the wallet directly. They obtain authority via capability tokens (§4) and request signatures or signed Expressions through RPC (§7).

## 3.3 Signing

A signed `Expression<T>` is constructed as follows.

```text
1. Serialize `data` to JSON bytes:  json_bytes = serialize_json(data)
2. Format the timestamp as RFC 3339 UTC with millisecond precision:
       timestamp_str = "YYYY-MM-DDTHH:MM:SS.mmmZ"
3. Compute the message hash:
       hash = SHA256(json_bytes || utf8(timestamp_str))
4. Sign the 32-byte hash with the agent's Ed25519 private key:
       signature = Ed25519_sign(hash, private_key)
5. Hex-encode the signature (lowercase):
       proof.signature = hex(signature)
6. Set proof.key to the verification-method ID for the signing key:
       proof.key = "<did>#<key-fragment>"
7. Set author to the agent's DID.
```

Reference: [`rust-executor/src/agent/signatures.rs`](../rust-executor/src/agent/signatures.rs) — `hash_data_and_timestamp`, `inner_verify`.

### 3.3.1 JSON serialization

Step 1 requires producing **the same bytes** on every implementation for the signature to be portable. A conforming implementation MUST serialize `data` using **[RFC 8785 JSON Canonicalization Scheme (JCS)](https://www.rfc-editor.org/rfc/rfc8785)**:

- Object members serialized in ascending Unicode code-point order of their keys.
- No insignificant whitespace.
- Numbers in the canonical form defined by JCS (which delegates to ECMA-404 / IEEE-754 round-trip).
- UTF-8 encoding.

> **Implementation Note.** The reference Rust executor currently relies on `serde_json::to_vec` with default settings, which serializes struct fields in declaration order (not lexicographic order). For Expression payloads whose Rust-side struct definition happens to match alphabetical order, the resulting bytes coincide with JCS; for others (notably nested user data), they will not. Two reference-implementation executors interoperate today because both use the same struct definitions, but a second implementation cannot rely on that. **Adopting JCS in the reference impl is required for cross-implementation signature compatibility** and is a known pre-1.0 work item.

### 3.3.2 Timestamp format

Step 2 uses the exact format `YYYY-MM-DDTHH:MM:SS.mmmZ`:

- Always UTC (literal `Z` suffix; no numeric offset).
- Always exactly three decimal digits of milliseconds (zero-padded if needed).
- 24-hour clock.

Rust uses `chrono::SecondsFormat::Millis` with `true` for UTC; JavaScript uses `date.toISOString()` (which produces the same shape).

### 3.3.3 Status field exclusion

For `LinkExpression`, the optional `status` field (`"shared" | "local"`) MUST NOT contribute to the signed bytes. It is local-only routing metadata. See [§2.3](./02-core-data-model.md#23-linkexpression).

## 3.4 Verification

To verify an `Expression<T>`:

```text
1. Resolve `author` to its DID Document.
   - For did:key, derive deterministically from the DID.
   - For other methods, use the appropriate resolver.
2. Locate the verification method in the DID Document whose ID
   matches `proof.key`. Extract its public key.
3. Re-derive json_bytes and timestamp_str exactly as in §3.3.
4. Recompute hash = SHA256(json_bytes || utf8(timestamp_str)).
5. signature_bytes = hex_decode(proof.signature)
6. Run the verification algorithm declared by the verification method
   (Ed25519 for the did:key baseline) over (hash, signature_bytes, public_key).
   The Expression is valid iff verification returns true.
```

Reference: [`rust-executor/src/agent/signatures.rs`](../rust-executor/src/agent/signatures.rs).

> **Implementation Note.** The reference impl currently shortcuts step 2 for `did:key` by parsing `author` directly as a key (since for `did:key` the DID *is* the public key). This works only for the baseline method and ignores `proof.key`. To support multi-key agents and non-`did:key` methods properly, implementations SHOULD do real DID-Document lookup and key selection by `proof.key` ID.

### 3.4.1 Why `proof.key` matters

Using `proof.key` (rather than `author` directly) decouples *who* signed from *which key was used*. This lets:

- Agents rotate keys without changing their DID (by publishing a new DID Document with both old and new verification methods, then retiring the old).
- Agents hold multiple keys for different purposes (e.g. a device key alongside a primary key).
- Non-`did:key` DID methods work — they always need lookup because the DID isn't the key.

## 3.5 What signatures cover and don't cover

A signature on an Expression attests to:

- The serialized `data` payload (per §3.3.1).
- The `timestamp`.
- Indirectly, the binding to `author` and `proof.key` (since these are needed for verification — though they're not themselves in the hash).

A signature does **not** attest to:

- `status` on a LinkExpression (local-only).
- `proof.valid` / `proof.invalid` on a DecoratedExpressionProof (computed locally each time).
- The fact that the link is currently present in any particular Perspective. Signed links live independently of where they're stored; an executor might hold a signed link from another agent without that other agent currently agreeing it should exist.

For a complete threat-model discussion (replay, key revocation, signature scope across Languages), see §10.

## 3.6 Entanglement proofs (optional)

For binding an AD4M identity to an external key system (e.g. a hardware wallet, a Holochain device key, a smart-contract address), an executor MAY support **entanglement proofs** — a small bidirectional-signature artefact:

```typescript
interface EntanglementProof {
  did: string;                  // the AD4M DID
  didSigningKeyId: string;      // proof.key on the AD4M side
  deviceKeyType: string;        // e.g. "holochain", "secp256k1"
  deviceKey: string;            // the foreign public key (encoded per type)
  deviceKeySignedByDid: string; // signature of deviceKey by the DID key
  didSignedByDeviceKey?: string;// signature of the DID by the device key
}
```

Definition in [`core/src/agent/Agent.ts`](../core/src/agent/Agent.ts).

Supporting entanglement proofs is OPTIONAL. Implementations that do not support them MUST simply preserve such proofs unchanged when they appear in Perspectives (they are stored as any other link payload).
