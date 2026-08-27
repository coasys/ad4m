# AGENTS.md — link-server

Self-hostable link language server for AD4M. Fastify + ws + better-sqlite3, single process, single SQLite file per deployment. No external services (no Redis, no Postgres) — this is meant to run on a community's own small hardware.

## Build / run / test

```bash
npm install        # NODE_ENV must not be "production", or npm skips devDependencies
                    # (typescript, tsx) and both `npm run build` and `npm test` break.
npm run build       # tsc -> dist/, then chmod +x dist/index.js (keeps the npx shebang usable)
npm test            # node --import tsx --test tests/*.test.ts (node:test, no mocks of the server)
npm run dev          # tsx src/index.ts directly, no build step
node dist/index.js --port 3456 --data ./my-data --self-url https://example.org
```

If `npm install` reports "N packages have install scripts not yet covered by allowScripts" (better-sqlite3's native build, esbuild's postinstall), run `npm approve-scripts <pkg>` for each and re-run install. This writes an `allowScripts` block into `package.json` — that's expected and should stay committed.

## Architecture

Composition root is `src/server.ts` (`buildServer(opts)`), wired in this dependency order (no cycles):

```
types.ts        shared types + pure hash/canonicalization helpers, no deps
db.ts           better-sqlite3 wrapper, schema, all queries incl. applyDiffAndAppend (the OR-Set merge transaction)
auth.ts         did:key <-> raw ed25519 pubkey (base58btc hand-rolled, no bs58 dep), sign/verify, challenge store, JWT session manager
encryption.ts   E2E key distribution: ECIES-style room-key sealing (X25519 ECDH + HKDF-SHA256 + AES-256-GCM), X25519 unseal for tests
rate-limit.ts   sliding-window limiter, fully standalone
telepresence.ts online/offline + grace-period timers; DOES NOT import ws.ts (returns state, caller broadcasts) to avoid a cycle
federation.ts   server-to-server diff relay + reconciliation; imports ws.ts to push received diffs to local clients
ws.ts           WebSocket connection registry + telepresence message routing; imports auth.ts + telepresence.ts
routes.ts       all HTTP handlers; imports everything above
server.ts       composition root: builds db/auth/telepresence/ws/federation, registers routes, starts the reconciliation timer
index.ts        CLI arg parsing, calls buildServer + app.listen
```

`RouteContext` (defined in `routes.ts`) is the one bag of wired dependencies passed into `registerRoutes`. If you add a new module that needs to reach into storage or auth, wire it through `server.ts` and add it to that context rather than reaching for a singleton/global.

### Data model

- `links` table = the current active OR-Set (add-wins insert by hash, remove deletes by hash). `diffs` table = append-only history, replay-only, never mutated.
- **Revision is a content hash, not a sequence number**: `sha256(sorted(linkHashes).join(","))`. Two servers with the same active links converge to the same revision no matter what order or how many diffs got them there — this is load-bearing for federation reconciliation and for tests that assert convergence.
- **Removals resend the exact original `LinkExpression`** (same author/timestamp/data/proof as the addition), not a new tombstone. `linkHash()` of a removal entry is therefore identical to the hash of the link it removes, which is what makes "remove by hash" work. This also means only the *original author* can remove their own links via commit — there's no admin force-remove.
- `linkHash()`/`canonicalLinkPayload()` (types.ts) are shared by hashing AND signature verification. For E2E-encrypted links, `ciphertext`+`nonce` stand in for `source`/`predicate`/`target` in that canonical payload — same code path, same function, so a client signs and the server verifies identically whether or not the room is encrypted.

### Auth

- **Critical signing convention:** The AD4M executor's `agentSignStringHex` signs `SHA-256(message.as_bytes())`, NOT the raw message bytes. The auth route hashes the challenge with SHA-256 before verifying the ed25519 signature. If you add any new endpoint that verifies a DID-signed payload, use `hashMessageForVerify()` (exported from `auth.ts`) before calling `verifyHex()`. Federation auth (server identity keys, not DID challenge-response) signs raw bytes — the SHA-256 convention applies ONLY to DID signatures.
- DID pubkey extraction follows the `did:key` ed25519 convention exactly: strip `did:key:z`, base58btc-decode, drop the 2-byte multicodec prefix (`0xed, 0x01`), left with the raw 32-byte pubkey. No external DID resolution — the key is self-contained in the string.
- JWTs are real (jose, HS256, secret persisted in `server_identity` under `key_type='jwt-secret'`) **and** backed by a `sessions` row. Both must be valid. This lets ACL removal revoke access immediately (delete the session row) rather than waiting for JWT expiry — removing a DID from a room's ACL returns `401 session expired or revoked` on their very next request, not a lazy `403` on next ACL check.
- ACL membership is re-checked on *every* authenticated request (not just at login), so revocation is immediate everywhere, not just on new logins.

### `GET /rooms/:roomId/peers`

The spec text was internally ambiguous — the endpoint table calls it "authenticated DIDs for this room" while the telepresence section ties `/peers` to the online-agent list. This implementation returns **currently-online agents** (from `telepresence.getOnlineAgents`), matching the more detailed telepresence section. The full ACL member list (online or not) is `GET /rooms/:roomId/acl`.

### E2E encryption trust model — opaque link data

**The server treats link data as opaque.** It never parses, validates, or decrypts the `data` field of committed links — encrypted rooms and plaintext rooms follow the same commit path. The server's role in E2E is limited to key distribution: it generates room keys, seals them per-member, and serves the sealed copies. All encryption/decryption of actual link content happens client-side (in the server-link-language).

There's no dedicated "enable E2E" endpoint. **The first call to `POST /rooms/:roomId/keys/rotate` both enables E2E and generates key version 1**; every subsequent call rotates to a new version. The server generates the AES-256-GCM room key, seals a copy to every *current* ACL member's registered X25519 public key (ephemeral-X25519 ECDH + HKDF-SHA256 + AES-GCM, see `encryption.ts`), stores only the sealed copies, and never persists the plaintext key. Members whose X25519 key has not yet been registered (they haven't completed auth since E2E was set up) get skipped — they receive their sealed copy on the next rotation after they authenticate. A DID added to the ACL *after* a rotation has no key until the *next* rotation (`GET /keys` 404s until then) — the server cannot re-seal a key it no longer holds in memory, which is the honest tradeoff of not retaining plaintext at rest.

**X25519 key derivation**: clients derive their X25519 keypair from their Ed25519 *signing capability* (sha256(sign(FIXED_MESSAGE))), NOT from direct Ed25519→X25519 Montgomery conversion, because the Deno sandbox never exposes the raw Ed25519 private key. The public key is sent during the DID auth challenge-response (step 2, `x25519PublicKey` field) and stored in the `acl` table's `x25519_public_key` column. The server never derives X25519 keys from DIDs.

### Federation trust model

Peers are per-room, keyed by URL (`federation_peers` table, extended with a `peer_public_key` column beyond the spec's base schema). Trust for an inbound `/federate` or `/reconcile` call requires:
1. **Freshness**: a `timestamp` field within 5 minutes of the receiver's clock (`FEDERATION_PAYLOAD_MAX_AGE_MS`). Missing or stale timestamps return 400. This prevents replay attacks from captured federation payloads.
2. **Peer identity**: a valid ed25519 signature from a `serverPublicKey` that's either:
   - already pinned for that room (`peer_public_key` matches), or
   - verified on first contact by fetching `${serverUrl}/server/identity` — if the live identity matches the claimed key, the key gets pinned. This closes the TOFU race where an attacker could pin a rogue key before the real peer came online.

Link signatures inside a federated diff travel as metadata — the server stores and relays them as-is without per-link verification. Downstream consumers can verify if they choose.

**WebSocket keepalive**: connections use a 30-second ping/pong heartbeat. Sockets that miss a pong get terminated. This prevents stale connections from accumulating on the server.

**Anti-loop policy**: this server only forwards diffs it originated locally (via `POST /commit`). Diffs learned via `/federate` or `/reconcile` are applied and pushed to local WebSocket clients but never re-forwarded onward. That keeps the topology to pairwise peers instead of a gossip mesh and avoids needing per-diff provenance tracking to prevent forwarding loops. If you need transitive federation (A-B-C without a direct B-C peering), that's a deliberate gap, not an oversight.

Reconciliation direction: the *initiator* sends its own active-link-hash set; the receiver replies with whatever it has that the initiator's set is missing. So "B is missing data from A" is fixed by **B** calling `federation.reconcileRoom(roomId)` (which POSTs to A), not the other way around.

## Known gotchas

- **better-sqlite3 must be `^13`, not the `^11` a stale brief might specify.** `^11.x`'s native addon uses V8 APIs (`Object::GetPrototype`, `PropertyCallbackInfo::This`, `Context::GetIsolate`) removed in the V8 shipped with newer Node majors — it fails to compile with an opaque `node-gyp`/`make` error, not a clear version-mismatch message. `^13` ships bundled prebuilds for common platforms (linux-x64/arm64, darwin, win32) and needs no native toolchain on those.
- **Dockerfile still installs `build-essential`/`python3`** in the build stage as a fallback in case a deploy target's platform/libc isn't covered by better-sqlite3's bundled prebuilds (e.g. unusual arches). The final runtime image doesn't carry these — the build stage's `node_modules` (already pruned to `--omit=dev`) is copied forward, not reinstalled.
- **WebSocket test pitfall**: the server sends `online-agents` synchronously as part of completing the upgrade. If a test attaches its `message` listener *after* `await`-ing the socket's `open` event, that first message can already have fired and be lost — attach listeners immediately after `new WebSocket(...)`, before awaiting open. See `tests/helpers.ts`'s `collectMessages` usage in `tests/ws.test.ts`.
- `tests/helpers.ts`'s `waitFor()` accepts a predicate that itself returns a `Promise<boolean>` — always `await` it internally. Passing an async predicate to a naive `while (!predicate())` loop is a classic bug: a Promise is always truthy, so the loop exits on the first check without ever awaiting the real result.

## Testing conventions

Every test file boots a real server (`tests/helpers.ts` `startTestServer`) on `127.0.0.1:0` (random free port) with a fresh temp-dir SQLite file, and drives it over real HTTP/WebSocket via `fetch`/`ws`. Nothing about the server is mocked. `startTestServer` defaults `reconcileIntervalMs` to 1 hour (so the background federation sweep doesn't fire mid-assertion) and `telepresenceGraceMs` to 300ms (so offline-transition tests don't need multi-second sleeps) — override either via its `opts` when a test needs the real production defaults. Federation tests spin up two full server instances and talk between them over real loopback HTTP; there's no stubbed peer.
