# 10. Security Considerations

AD4M is a signed, agent-centric protocol with no central authority. That property forces specific security obligations on conforming executors and on the apps that use them. This section enumerates the threat model the protocol assumes, the protections it provides, and the things it deliberately does not protect against.

This section is **normative** where it uses RFC 2119 keywords, and informative where it discusses threats.

## 10.1 Trust model summary

The atomic trust unit is the **DID-signed Expression**. Anyone with the author's public key can verify what the author signed. Beyond that, the protocol assumes:

- Each agent trusts their own executor with their wallet (i.e. with their private key).
- Each executor trusts itself to issue and verify its own JWT capability tokens (§4.1.1).
- Each executor trusts the DIDs listed in its `trustedAgents` set to publish Language source code (§8.5).
- No agent trusts any other agent's executor by default.
- No central authority exists. There is no canonical "who is right" oracle.

Everything else — Neighbourhood membership, social-layer trust, content moderation — is built on top by applications.

## 10.2 What signatures cover

Signatures protect:

| Attack | Protection |
|---|---|
| **Forgery** — claiming an agent wrote something they didn't | An attacker without the private key cannot produce a verifying signature. |
| **Tampering** — modifying a signed Expression in transit or storage | Modified bytes fail signature verification (§3.4). |
| **Imputation** — attributing one agent's data to another | The DID + signature binds payload to author cryptographically. |

Signatures do not protect:

| Attack | Why not |
|---|---|
| **Replay** — re-sending an old, validly signed Expression | The timestamp is signed but there is no protocol-level "valid once" semantic. Apps that need replay protection MUST add it at the application layer (e.g. monotonic counters, nonces in `data`). |
| **Equivocation** — an agent signing two contradictory statements | Both are valid signed Expressions. Resolving conflicts is a Neighbourhood-level concern (typically handled by the Link Language's sync semantics). |
| **Selective disclosure** — an agent revealing a signed Expression in a context they didn't intend | Once signed, the bytes are portable. Sensitive content SHOULD NOT be signed at all (and the protocol's `local` link status keeps unsynced data off the network). |
| **Compromised wallets** — keys exfiltrated from a victim's executor | Signatures by the stolen key are indistinguishable from legitimate ones. See §10.5 (key rotation). |

## 10.3 Capability-token security

The capability token (§4) is the executor's only authentication mechanism for apps. Implementations MUST:

- Use a cryptographically-strong random `nonce` per token.
- Enforce token expiry (`exp`) strictly — past-`exp` tokens MUST be rejected.
- Re-check revocation on every dispatched RPC, not only at WebSocket upgrade. Revocation MUST be immediate (§4.6).
- Reject tokens with the wrong `aud` if audience binding is in use.
- Treat the admin credential (§4.5) as equivalent to root access; production deployments MUST NOT enable it by default and SHOULD log every use.

### 10.3.1 Token-signing-key sensitivity

Because the reference implementation signs tokens with **HS256** using the wallet's main private key bytes as the HMAC secret (§4.1.1), an attacker who obtains the executor's binary memory at runtime can:

- Issue arbitrary capability tokens that the executor will accept.
- Forge signed Expressions as the agent.

Both of these flow from wallet compromise. Conforming executors SHOULD:

- Wipe wallet key material from memory promptly when locked.
- Avoid swapping decrypted key material to disk where the platform allows.
- Use OS-level protected memory (e.g. `mlock`) where available.

A future protocol revision may decouple token signing from the identity key (e.g. use an HMAC key derived per-session). This is non-normative today.

## 10.4 Bootstrap trust

The bootstrap seed (§8.3) is the root of trust for installed Languages. Compromise of any of the following compromises the entire installation:

- The seed file itself (an attacker can swap `trustedAgents` for their own DID and then publish malicious Languages).
- Any DID in `trustedAgents` (an attacker with that wallet can publish Languages that the executor will install without further checks).
- The Language Language bundle (it is loaded inline from the seed without any signature check — it is the trust root for everything else).

A conforming executor MUST:

- Treat the seed file as integrity-critical. Recommended: ship a signed seed file and verify the signature before loading, with the seed-signer's public key compiled into the binary.
- Apply the §8.5 install check (signature + trusted-agents membership) on **every** Language install — including system Languages on bootstrap, except the Language Language itself.
- Reject Languages whose `Expression<LanguageSource>` author DID is not in `trustedAgents`.

The protocol does **not** sandbox Languages once installed. A loaded Language runs with full access to the executor's host imports (signing, storage, network, etc.). The trust boundary is the trusted-agents check at install time, not runtime isolation.

> **Implementation Note.** Strengthening Language isolation (per-Language storage namespaces, capability-restricted host imports, time/CPU limits) is a desirable future direction. Today the protection is "don't trust a DID that would sign a malicious Language."

## 10.5 Key rotation and loss

### 10.5.1 Rotation

To rotate keys without changing identity (only viable when `proof.key` is being honored — see §3.4.1):

1. Publish a new DID Document with both the old and new verification methods listed.
2. Start signing new Expressions with the new key and the new `proof.key` value.
3. After a transition window, publish a new DID Document with only the new key.

Older signed Expressions remain verifiable because their `proof.key` points at a verification method that was still in the DID Document at signing time.

### 10.5.2 Loss

If the private key is lost, the DID is unusable for future signing. Existing signed Expressions remain readable (anyone holding them can still verify), but the agent cannot author new ones from that identity. There is no protocol-level recovery mechanism. Applications that need durable identity SHOULD implement application-level recovery (e.g. social recovery, seed-phrase backup).

## 10.6 Wallet encryption

The reference implementation encrypts wallet contents with:

- Argon2id KDF (`m=19456, t=2, p=1`) over the passphrase.
- XSalsa20-Poly1305 (NaCl `SalsaBox`) over the derived key.

Conforming implementations SHOULD:

- Use a memory-hard KDF for passphrase derivation.
- Use an authenticated symmetric cipher for encryption.
- Use a fresh, non-deterministic nonce per encryption.

> **Implementation Note.** The reference impl currently uses a **constant zero nonce** for wallet encryption. Because each call derives the same key from the same passphrase, the resulting ciphertext is functionally a deterministic encryption of the wallet — fine for a local on-disk file accessed serially, but it would leak information if the same passphrase ever encrypted two different plaintexts. Switching to a random nonce stored alongside the ciphertext is a recommended hardening.

## 10.7 Replay protection at the application layer

The protocol does not detect replays. If your application semantics depend on at-most-once delivery or monotonic state, you MUST add protection at the application layer. Recommended patterns:

- **Sequence numbers** in the payload data: `data: { ..., seq: N }`, rejecting any `seq <= last_seen` from a given author.
- **Per-action nonces** stored locally (and signed in the payload), with a bounded retention window.
- **Reifier-IRI deduplication** at the SPARQL level — since two replays of the same signed link compute the same reifier IRI (§2.10.1), an executor that inserts a duplicate sees it as the same triple. This catches accidental replay but does not catch deliberate replay across different Neighbourhoods.

## 10.8 Capability scope and least privilege

Applications SHOULD request only the minimum capability set they need. Conforming executors SHOULD:

- Surface every requested capability clearly to the user during the consent flow (§4.8).
- Refuse to grant `*` (wildcard) capability domains except via the explicit admin credential.
- Display the full capability list of an active token in the management UI.

Users SHOULD be cautioned that revoking a capability mid-session immediately terminates the corresponding access (per §4.6).

## 10.9 Confidentiality

The protocol provides **integrity and authenticity** through signatures but **does not provide confidentiality** by default. Synced links in a Neighbourhood are visible to every agent in that Neighbourhood. Apps that need end-to-end encryption MUST encrypt the link `target` (or the relevant payload) at the application layer before signing.

The recommended pattern for DM-style private content is:

- Use a templated Link Language with the recipient DID baked in.
- Encrypt the message payload to the recipient's public key (extracted from their DID Document) before constructing the `Link`.
- Sign the encrypted payload as usual.

## 10.10 Trust boundaries summary

| Boundary | Who controls | Compromise impact |
|---|---|---|
| **Wallet** | The agent | All identity, signing, token issuance |
| **`trustedAgents` set** | The executor's local config | What Language source the executor will run |
| **Capability token** | The app holding it | Whatever the token's capabilities allow |
| **Admin credential** | The executor's operator | Full executor access |
| **DID-key material** | Whoever holds the private key | Ability to sign as that agent forever |
| **Bootstrap seed file** | Whoever installed the executor | Root of trust for everything downstream |

A protocol-conforming implementation MUST make these boundaries visible — operators and users SHOULD be able to inspect every active token, every trusted agent, and the contents of the seed file at any time.
