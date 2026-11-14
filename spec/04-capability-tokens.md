# 4. Capability Tokens

This section specifies the **capability-token** format that client apps present to the executor to authenticate WebSocket RPC calls. Token-based auth lets multiple apps share a single executor, each with a different — and individually revocable — set of permissions.

## 4.1 Token format

A capability token is a **JWT** ([RFC 7519](https://www.rfc-editor.org/rfc/rfc7519)) issued by the executor.

A conforming executor MUST:

- Issue tokens via the `agent.generateJwt` RPC (§7 / Appendix A) after the user has approved the app's capability request through the executor's UI flow.
- Validate every WebSocket RPC call's `?token=` query parameter against the issued tokens.
- Honor token revocation immediately (see §4.6).

### 4.1.1 Signing algorithm

The reference implementation signs tokens with **HS256** (HMAC-SHA256) using the wallet's main private-key bytes as the HMAC secret. See [`rust-executor/src/agent/capabilities/token.rs`](../rust-executor/src/agent/capabilities/token.rs).

A consequence of HS256 with a private-key-derived secret is that **only the issuing executor can verify its own tokens** — they are not portable across executors, and they are not externally verifiable. This is intentional: capability tokens authorize a session against a specific executor, not against the network. Implementations MAY use a different JWT signing algorithm provided the same self-issued-and-self-verified property holds.

### 4.1.2 Claims

The claims structure:

```typescript
interface Claims {
  iss: string;          // the issuing agent's DID
  sub: string | null;   // optional user email, for multi-user executors
  aud: string;          // audience — the app the token was issued to
  exp: number;          // expiry (Unix seconds)
  iat: number;          // issued-at (Unix seconds)
  nonce: string;        // UUID v4, prevents collisions
  capabilities: AuthInfo;
}

interface AuthInfo {
  appName: string;
  appDesc: string;
  appDomain?: string;
  appUrl?: string;
  appIconPath?: string;
  capabilities?: Capability[];
  userEmail?: string;   // for multi-user tokens
}

interface Capability {
  with: { domain: string; pointers: string[] };
  can: string[];   // operations
}
```

Reference: [`rust-executor/src/agent/capabilities/types.rs`](../rust-executor/src/agent/capabilities/types.rs).

## 4.2 Capability shape

Each capability in the `capabilities` array authorizes a set of operations against a set of resources:

```json
{ "with": { "domain": "perspective", "pointers": ["*"] }, "can": ["READ", "CREATE", "UPDATE"] }
```

- `with.domain` — the capability domain (§4.3).
- `with.pointers` — resource selectors within the domain. `"*"` matches anything; otherwise an opaque domain-specific string (e.g. a Perspective UUID).
- `can` — the allowed operations (§4.4). `"*"` is the wildcard.

A request is authorized if **at least one** capability in the token matches both the requested domain (with pointer match) and the requested operation.

## 4.3 Capability domains

The defined domains, per [`rust-executor/src/agent/capabilities/defs.rs`](../rust-executor/src/agent/capabilities/defs.rs):

| Domain | Scope |
|---|---|
| `agent` | Agent identity, key, profile, status |
| `expression` | Expression create / read / update |
| `language` | Language install, publish, uninstall, query |
| `perspective` | Perspective CRUD and link operations |
| `neighbourhood` | Joining and publishing Neighbourhoods |
| `runtime` | General runtime operations |
| `runtime.trusted_agents` | Managing the `trustedAgents` set (§8) |
| `runtime.known_link_languages` | Managing `knownLinkLanguages` (§8) |
| `runtime.friends` | Managing the friends list |
| `runtime.messages` | Reading sent / received DM cache |
| `runtime.user_management` | Multi-user (sub-account) management |
| `runtime.hosting` | Hosting-mode features |
| `artificial intelligence` | AI model invocation (PROMPT, TRANSCRIBE) |

The wildcard domain `*` matches any of the above (admin only — see §4.5).

## 4.4 Operations

Standard operations:

| Operation | Used in |
|---|---|
| `READ` | all read-only domains |
| `CREATE` | adding new resources |
| `UPDATE` | mutating existing resources |
| `DELETE` | removing resources |
| `SUBSCRIBE` | subscribing to event streams |
| `PROMPT` | AI model inference |
| `TRANSCRIBE` | AI audio transcription |
| `VERIFY` | signature verification |
| `AUTHENTICATE` | obtaining a new token |
| `LOCK` / `UNLOCK` | wallet lock state (`agent` domain) |
| `PERMIT` | granting consent for a capability request (`agent` domain) |
| `SIGN` | requesting a signature (`agent` domain) |

`*` matches any operation.

Implementations MAY define additional operations within their own custom domains. Operations within the standard domains MUST be one of the above set.

## 4.5 Admin credential

An executor MAY be configured with an **admin credential** — a long string set at startup. Any token sent in the WebSocket query that equals the admin credential is implicitly granted the full-wildcard capability:

```json
{ "with": { "domain": "*", "pointers": ["*"] }, "can": ["*"] }
```

Admin tokens bypass the per-domain checks entirely. They are intended for local management UIs and CI; conforming executors MUST NOT enable an admin credential by default in production builds and SHOULD log usage.

## 4.6 Revocation

An executor MUST honor token revocation immediately for both new and **existing** WebSocket connections. The reference implementation re-checks revocation on every dispatched RPC ([`rust-executor/src/api/ws_rpc.rs`](../rust-executor/src/api/ws_rpc.rs) `check_token_revoked`).

A revoked token MUST cause:

- Any new request on an existing connection to receive an error response with code `401`.
- New connection attempts using that token to be rejected.

## 4.7 Expiration

Tokens carry an `exp` claim (Unix seconds). After `exp`:

- The executor MUST reject any request bearing the token with code `401`.
- The executor MAY proactively close existing WS connections holding the token, or wait until the next RPC attempt to reject.

Apps SHOULD treat expiration as a normal session boundary and re-request a fresh token through the standard consent flow.

## 4.8 The consent flow (informative)

The standard way an app acquires a token (informative; the wire details are in the `agent.requestCapability` / `agent.permitCapability` RPCs):

1. App calls `agent.requestCapability` with the desired `AuthInfo`. The executor stores a pending request and returns a `requestId`.
2. The executor surfaces the request in its UI (the user reviews the requested capabilities).
3. The user approves (`agent.permitCapability`) or rejects.
4. On approval, the executor calls `agent.generateJwt(requestId, ...)` and returns the signed token to the app.
5. The app opens a WebSocket to `/api/v1/ws?token=<jwt>` and begins making RPC calls.

The flow's specifics (UI, prompts, multi-user vs single-user accounts) are implementation choices. The protocol surface is just the JWT structure and validation rules above.
