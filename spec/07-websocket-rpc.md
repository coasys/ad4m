# 7. WebSocket RPC Wire Format

This section specifies the wire-level RPC contract between client apps and a conforming executor. The operation catalog (which `type` strings exist, what their `params` and `result` look like) is reference material in [Appendix A](./appendix-a-rpc-reference.md); this chapter pins down the envelope and connection rules.

Reference: [`rust-executor/src/api/ws_rpc.rs`](../rust-executor/src/api/ws_rpc.rs).

## 7.1 Endpoints

A conforming executor MUST expose:

| Endpoint | Purpose |
|---|---|
| `GET /api/v1/ws` | Upgrade to the single per-client RPC + events WebSocket. |

The reference executor also exposes `GET /health` as a liveness probe (200 OK when the server is accepting requests). This is optional.

## 7.2 Authentication

Authentication is performed once at WebSocket upgrade via a query parameter:

```
GET /api/v1/ws?token=<jwt>
```

The token is the JWT capability token issued by the executor (§4). The executor MUST:

- Resolve the token's capabilities at upgrade time and attach them to the connection's request-context.
- Re-check token revocation on **every** dispatched RPC, not just at upgrade — so that `revokeToken` takes effect on existing connections.
- Reject upgrades with a missing or syntactically invalid token with the standard WebSocket close code.

The admin credential (§4.5) is recognised as a special token that grants the full-wildcard capability.

## 7.3 Message envelope

All messages are JSON text frames.

### 7.3.1 Request (client → executor)

```json
{
  "id": "<client-chosen correlation id>",
  "type": "<operation name>",
  "params": { ... operation-specific ... }
}
```

- `id`: any string; the executor echoes it back unchanged on the matching response. Clients SHOULD use unique values per outstanding request.
- `type`: the operation name, dot-separated by domain (e.g. `perspective.addLink`, `agent.generateJwt`). MUST be one of the operations in [Appendix A](./appendix-a-rpc-reference.md) (or an extension the executor advertises).
- `params`: operation-specific JSON object. MAY be omitted; the executor treats absent `params` as `{}`.

### 7.3.2 Response — success (executor → client)

```json
{ "id": "<same id>", "result": <any JSON value> }
```

### 7.3.3 Response — error (executor → client)

```json
{
  "id": "<same id>",
  "error": { "code": <integer>, "message": "<string>" }
}
```

Error codes:

| Code | Meaning |
|---|---|
| `400` | Bad request — malformed JSON, missing `type`, invalid params. |
| `401` | Unauthorized — missing, expired, or revoked token; capability check failed. |
| `404` | Operation `type` not recognised. |
| `500` | Internal executor error. |

Executors MAY define additional codes ≥ 1000 for domain-specific errors.

### 7.3.4 Event (executor → client, unsolicited)

```json
{
  "type": "<event type>",
  ...event-specific payload fields...
}
```

Events have a `type` but no `id` field. Clients distinguish events from responses by the absence of `id`. The event types and their payloads are listed in Appendix A.

## 7.4 Keepalive

Either side MAY send a ping frame at any time. WebSocket-protocol pings (frame opcode `0x9`) MUST be answered with the corresponding pong frame.

In addition, application-level keepalive is supported:

```json
// client → executor
{ "type": "ping" }

// executor → client
{ "type": "pong" }
```

The reference implementation also tracks `last_seen` per token on every dispatched RPC (throttled to one DB write per 5 min per user).

## 7.5 Concurrency

Multiple requests MAY be in flight on a single connection simultaneously. The executor MUST dispatch each request asynchronously and MUST NOT serialize unrelated requests behind one another. Responses MAY be delivered in any order; correlation is solely via `id`.

## 7.6 Event multiplexing

Both events and RPC responses share a single socket. Clients MUST:

- Route incoming messages by presence of `id` (response) vs absence (event).
- Tolerate events interleaved with responses in any order.
- Tolerate events arriving before they have explicitly subscribed (some executors push side-effect events such as `runtime.exception`).

## 7.7 Operation domains

Operations are organised by domain. The domain is the first dot-segment of `type` and MUST match a capability domain in §4.3 for the per-call authorization check.

The defined domains (full operation list in Appendix A):

| Domain | Purpose |
|---|---|
| `agent` | Identity, key, profile, capability requests, JWT issuance |
| `expression` | Create / get Expressions |
| `language` | Install, publish, query, template Languages |
| `perspective` | CRUD Perspectives and their links, run SPARQL / model queries |
| `neighbourhood` | Publish / join / leave Neighbourhoods |
| `runtime` | Runtime info, friends, trusted agents, messages, hosting |
| `ai` | AI model invocation (PROMPT, TRANSCRIBE) |
| `users` | Multi-user / sub-account management |

Operations introduced by extensions MUST use a separate top-level domain to avoid name collision.

## 7.8 Wire-format compatibility rules

### 7.8.1 JSON conventions

| Type | Wire form |
|---|---|
| Timestamps | RFC 3339 milliseconds UTC (`2026-05-20T10:30:00.000Z`) |
| DIDs | Full DID string (`did:key:z6Mk...`) |
| Signatures | Hex-encoded, lowercase |
| Verification-method IDs | `<did>#<fragment>` |
| UUIDs | Lowercase, hyphenated (`550e8400-e29b-41d4-a716-446655440000`) |
| Absent optionals | Either omitted from the JSON or set to `null`. Both MUST be accepted on input. |
| All field names | **camelCase**. |

The SHACL JSON wire format (§5.1) is the only data structure on the wire that does not use camelCase — its field names are snake_case as enumerated in §5.1.

### 7.8.2 Expression URL format

The two URL forms an executor MUST parse and produce:

```
<language_address>://<expression_address>     # general
literal:<type>:<percent-encoded-value>         # inline literal (no //)
did:<method>:<id>                              # DID (no ://)
```

All AD4M URIs MUST be valid IRIs without further escaping (§2.1.3).

## 7.9 Subscriptions

Subscription operations follow the standard request/response shape on the wire — the *response* to a subscribe call returns a subscription handle, and **events** keyed by that handle then begin arriving asynchronously.

Conforming executors MUST:

- Accept a corresponding `*.unsubscribe` for every subscribe operation.
- Cease event delivery for a subscription within a bounded time after `unsubscribe` returns.
- Stop all subscriptions when the underlying WS connection closes.
