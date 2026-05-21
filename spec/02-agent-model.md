# 2. Agent Model

## 2.1 Identity

Every AD4M agent is identified by a [W3C DID](https://www.w3.org/TR/did-core/).

The reference implementation currently uses the `did:key` method ([did:key spec](https://w3c-ccg.github.io/did-method-key/)) with **Ed25519** key pairs:

```
did:key:z6Mk...
```

For `did:key`, an agent's DID is derived deterministically from their Ed25519 public key, and the DID Document is generated from the DID itself and contains a single verification method.

### Other DID Methods

The protocol is not intentionally restricted to `did:key`. Implementations MAY support additional DID methods (e.g. `did:web`, `did:pkh`, `did:ion`), in which case:

- DID resolution MUST yield a DID Document containing the verification method referenced by `proof.key` (see [§1.2 Verification](./01-core-data-model.md#verification)).
- The agent's private key MUST be imported into the Wallet (the executor does not generate keys for non-`did:key` methods, since the DID is bound to externally-managed keys).
- Signature verification follows the algorithm declared by the verification method in the DID Document.

`did:key` remains the baseline that all conforming implementations MUST support; other methods are optional for interoperability.

## 2.2 Key Management (Wallet)

The executor maintains a **Wallet** — an encrypted keystore containing Ed25519 key pairs.

### Key Storage

- Keys are stored encrypted using **Argon2id** to derive an encryption key from the passphrase, then encrypting with **XSalsa20-Poly1305** (via NaCl's `crypto_box`). The derived key (not the raw passphrase) is used as input to XSalsa20-Poly1305.
- A "main" key is created during agent initialization; additional keys can be created for multi-user mode.

### Signing

All signing operations use Ed25519:
1. The message is SHA-256 hashed (see [Expression signing](./01-core-data-model.md#signing-scheme))
2. The hash is signed with the Ed25519 private key
3. Verification uses the public key extracted from the `did:key`

### Agent Data

```typescript
interface AgentData {
  did: string;             // e.g., "did:key:z6Mk..."
  didDocument: string;     // JSON-serialized DID Document
  signingKeyId: string;    // Verification method ID from DID Document
  walletKeyName: string;   // Internal key name in the wallet
}
```

## 2.3 Agent Expression

An agent publishes their profile as an Expression in the Agent Language:

```typescript
interface Agent {
  did: string;
  perspective?: Perspective;       // Profile data as links
}
```

The agent's profile `perspective` contains links that represent profile properties (name, avatar, etc.) — the schema is application-defined through SDNA.

> The Agent shape does not include a `directMessageLanguage` field. Inbox discovery is handled via the `ad4m://inbox` predicate in the agent's public perspective (see [Social Conventions](../docs-src/ad4m-social-conventions.md) §3.1).
>

> ```sparql
> SELECT ?inbox WHERE {
>   <did:key:z6Mk...> <ad4m://inbox> ?inbox .
> }
> ```

### Social Conventions

The following well-known `ad4m://` predicates are defined for social-layer interoperability (see [Social Conventions](../docs-src/ad4m-social-conventions.md)):

| Predicate | Meaning | Example |
|-----------|---------|--------|
| `ad4m://inbox` | Language instance the agent uses as their DM inbox | `(agentDid) →[ad4m://inbox]→ (languageRef)` |
| `ad4m://friend-of` | Agent considers another agent a friend (asymmetric) | `(did1) →[ad4m://friend-of]→ (did2)` |
| `ad4m://profile` | Expression URI resolving to agent's profile | `(agentDid) →[ad4m://profile]→ (expressionUri)` |
| `ad4m://presence` | Language instance exposing agent's real-time presence | `(agentDid) →[ad4m://presence]→ (languageRef)` |

These predicates are stored as links in perspectives. Which perspective (public, private, shared) is an application/runtime choice. Applications that want to interoperate across AD4M implementations SHOULD use these predicates.

Key conventions:
- **Direct Messages** are not a first-class capability. A DM inbox is a Language exporting `perspective-commit` with the recipient DID baked into the template. See [Language Interface §3.7](./03-language-interface.md#37-direct-messages-not-a-capability).
- **Friends** are `ad4m://friend-of` links in a perspective. Friendship is asymmetric by default; applications may layer symmetric/handshake semantics on top.
- **Encryption** is the template's responsibility, not the spec's.

## 2.4 Agent Status

```typescript
interface AgentStatus {
  did?: string;
  didDocument?: string;
  error?: string;
  isInitialized: boolean;
  isUnlocked: boolean;
}
```

An agent transitions through states:
1. **Uninitialized** — No key pair generated yet
2. **Initialized + Locked** — Keys exist but wallet is locked (passphrase not entered)
3. **Initialized + Unlocked** — Ready for operation

## 2.5 Capability Tokens

AD4M uses a capability-based authorization model for client applications connecting to the executor.

### Token Format

Capabilities are issued as **JWT** tokens containing:

```typescript
interface Claims {
  iss: string;              // Issuer (executor DID)
  sub?: string;             // Subject (user email for multi-user)
  aud: string;              // Audience
  exp: number;              // Expiration (Unix timestamp)
  iat: number;              // Issued at
  nonce: string;            // UUID v4
  capabilities: AuthInfo;   // Granted capabilities
}

interface AuthInfo {
  appName: string;
  appDesc: string;
  appDomain?: string;
  appUrl?: string;
  appIconPath?: string;
  capabilities?: Capability[];
}

interface Capability {
  with: Resource;
  can: string[];   // e.g., ["READ", "CREATE", "UPDATE", "DELETE"]
}

interface Resource {
  domain: string;       // e.g., "agent", "perspective", "runtime", "language"
  pointers: string[];   // e.g., ["*"] or specific UUIDs
}
```

### Capability Domains

The following resource domains are defined:

| Domain | Description | Operations |
|--------|-------------|------------|
| `agent` | Agent identity operations | READ, CREATE, UPDATE, DELETE, PERMIT |
| `perspective` | Perspective CRUD + links | READ, CREATE, UPDATE, DELETE |
| `language` | Language management | READ, CREATE, UPDATE, DELETE |
| `runtime` | Runtime configuration | READ, CREATE, UPDATE, DELETE |
| `neighbourhood` | Neighbourhood operations | CREATE, READ |

### Admin Credential

The executor supports an `admin_credential` token — a pre-shared secret that grants full access without capability checks. This is used by the launcher/UI for initial setup.

## 2.6 Entanglement Proofs

Entanglement proofs link an AD4M DID to external identity systems:

```typescript
interface EntanglementProof {
  deviceKey: string;
  deviceKeySignedByDid: string;
  deviceKeyType: string;
  did: string;
  didSignedByDeviceKey?: string;
  didSigningKeyId: string;
}
```

This allows proving ownership of external accounts (e.g., Ethereum addresses) by cross-signing between the AD4M key and the external key.
