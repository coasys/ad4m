# AGENTS.md — link-server

Self-hostable link language server for AD4M. Fastify + ws + better-sqlite3, single process, single SQLite file per deployment. No external services (no Redis, no Postgres) — this is meant to run on a community's own small hardware.

## Build / run / test

```bash
npm install        # NODE_ENV must not be "production", or npm skips devDependencies
                    # (typescript, tsx) and both `npm run build` and `npm test` break.
npm run build       # tsc -> dist/, then chmod +x dist/index.js (keeps the npx shebang usable)
npm test            # node --import tsx --test tests/*.test.ts (node:test, no mocks of the server)
npm run dev          # tsx src/index.ts directly, no build step
node dist/index.js --port 3456 --data ./my-data
```

If `npm install` reports "N packages have install scripts not yet covered by allowScripts" (better-sqlite3's native build, esbuild's postinstall), run `npm approve-scripts <pkg>` for each and re-run install. This writes an `allowScripts` block into `package.json` — that's expected and should stay committed.

## Architecture

Composition root is `src/server.ts` (`buildServer(opts)`), wired in this dependency order (no cycles):

```
types.ts        shared types + pure hash/canonicalization helpers, no deps
db.ts           better-sqlite3 wrapper, schema, all queries incl. applyDiffAndAppend (the OR-Set merge transaction)
auth.ts         did:key <-> raw ed25519 pubkey (base58btc hand-rolled, no bs58 dep), sign/verify, challenge store, JWT session manager
encryption.ts   the SealedBox type; sealToDid/openSealed (the AD4M executor's sealed box) for tests acting as clients
rate-limit.ts   sliding-window limiter, fully standalone
telepresence.ts online/offline + grace-period timers; DOES NOT import ws.ts (returns state, caller broadcasts) to avoid a cycle
ws.ts           WebSocket connection registry + telepresence message routing; imports auth.ts + telepresence.ts
routes.ts       all HTTP handlers; imports everything above
log-tail.ts     in-memory ring buffer of recent log lines (both apps' pino output, teed from stdout) for the operator's live log view
operator.ts     optional loopback-only operator app (admission UI + /api, incl. the /api/logs SSE stream); shares db/auth/ws with routes.ts, page in operator-page.ts
server.ts       composition root: builds db/auth/telepresence/ws, registers routes, builds the operator app when asked
index.ts        CLI arg parsing, calls buildServer + app.listen
```

`RouteContext` (defined in `routes.ts`) is the one bag of wired dependencies passed into `registerRoutes`. If you add a new module that needs to reach into storage or auth, wire it through `server.ts` and add it to that context rather than reaching for a singleton/global.

### Data model

- `links` table = the current active OR-Set (add-wins insert by hash, remove deletes by hash). `diffs` table = append-only history, replay-only, never mutated.
- **Revision is a content hash, not a sequence number**: `sha256(sorted(linkHashes).join(","))`. Two rooms with the same active links converge to the same revision no matter what order or how many diffs got them there — this is load-bearing for tests that assert convergence.
- **Removals resend the exact original `LinkExpression`** (same author/timestamp/data/proof as the addition), not a new tombstone. `linkHash()` of a removal entry is therefore identical to the hash of the link it removes, which is what makes "remove by hash" work. This also means only the *original author* can remove their own links via commit — there's no admin force-remove.
- `linkHash()`/`canonicalLinkPayload()` (types.ts) are shared by hashing AND signature verification. For E2E-encrypted links, `ciphertext`+`nonce` stand in for `source`/`predicate`/`target` in that canonical payload — same code path, same function, so a client signs and the server verifies identically whether or not the room is encrypted.

### Auth

- **Critical signing convention:** The AD4M executor's `agentSignStringHex` signs `SHA-256(message.as_bytes())`, NOT the raw message bytes. The auth route hashes the challenge with SHA-256 before verifying the ed25519 signature. If you add any new endpoint that verifies a DID-signed payload, use `hashMessageForVerify()` (exported from `auth.ts`) before calling `verifyHex()`.
- DID pubkey extraction follows the `did:key` ed25519 convention exactly: strip `did:key:z`, base58btc-decode, drop the 2-byte multicodec prefix (`0xed, 0x01`), left with the raw 32-byte pubkey. No external DID resolution — the key is self-contained in the string.
- JWTs are real (jose, HS256, secret persisted in `server_identity` under `key_type='jwt-secret'`) **and** backed by a `sessions` row. Both must be valid. This lets ACL removal revoke access immediately (delete the session row) rather than waiting for JWT expiry — removing a DID from a room's ACL returns `401 session expired or revoked` on their very next request, not a lazy `403` on next ACL check.
- ACL membership is re-checked on *every* authenticated request (not just at login), so revocation is immediate everywhere, not just on new logins.

### `GET /rooms/:roomId/peers`

The spec text was internally ambiguous — the endpoint table calls it "authenticated DIDs for this room" while the telepresence section ties `/peers` to the online-agent list. This implementation returns **currently-online agents** (from `telepresence.getOnlineAgents`), matching the more detailed telepresence section. The full ACL member list (online or not) is `GET /rooms/:roomId/acl`.

### E2E encryption trust model — opaque link data

**The server treats link data as opaque.** It never parses, validates, or decrypts the `data` field of committed links — encrypted rooms and plaintext rooms follow the same commit path. The server's role in E2E is limited to key *storage and distribution*: it stores sealed (encrypted) room key envelopes and serves them to the appropriate members. All key generation, sealing, and encryption/decryption of actual link content happens client-side (in the server-link-language).

There's no dedicated "enable E2E" endpoint. **The first call to `POST /rooms/:roomId/keys/rotate` both enables E2E and stores key version 1**; every subsequent call rotates to a new version. Rotation is admin-only. The admin's language client generates the AES-256-GCM room key locally, seals it to the DID of every ACL member, connected or not (ephemeral-X25519 ECDH + HKDF-SHA256 + AES-GCM, see `encryption.ts`), and POSTs only the sealed boxes. The server validates that each DID exists in the ACL and that each box has the expected shape, then stores the sealed copies. **The server never sees the plaintext room key.** A DID added to the ACL *after* a rotation appears in `membersNeedingHistoricalKeys` (`GET /keys/missing`, any member); any member holding a version may `POST /keys/grant` it to that DID. The grant route refuses (403) a version the caller does not hold and validates every entry before storing any; `addRoomKeyIfMissing` keeps the first box per member and version.

**Member keys are DIDs**: a member's X25519 key is the Montgomery form of the Ed25519 key in its `did:key`, so clients compute it from the DID and the server stores none. `/auth` takes `{did, challenge, signature}` only; the ACL response is `[{did}]`. Old databases still carry `acl.x25519_public_key`/`x25519_signature` columns: nothing reads them (no migration; rooms created before re-key).

**WebSocket keepalive**: connections use a 30-second ping/pong heartbeat. Sockets that miss a pong get terminated. This prevents stale connections from accumulating on the server.

### Operator listener

`--operator-port` + `--operator-token-file` start a second Fastify app (`operator.ts`) on loopback. It lets the server operator list rooms and add/remove DIDs without the room admin's DID key. It is a separate app, not a route prefix, so the public port can never serve it. Any change to who can reach it must keep these properties, each covered by `tests/operator.test.ts`:
- The token comes from a mode-600 file only (never argv/URL), and the request log never sees headers.
- Every `/api` call carries the signed-in login (`X-Forwarded-User`, set by the proxy; trusted only because the listener is loopback). Every ACL change is stored in `acl_changes` with that login. The `/acl` route stores the admin DID.
- Every POST carries an allowed `Origin` (or `Referer`), as a CSRF guard for the cookie sign-in.
- Removal goes through `removeMember()` in `routes.ts` (ACL row, sessions and sockets together). The page script builds DOM with `textContent` only, because room ids and DIDs are client-chosen strings (log lines carry them too).
- Admission (`tests/admission.test.ts`): allowlist unless `autoAdmit`. The mode is logged at startup and returned by `/api/whoami`; the page shows it. The server keeps no queue of refused joins: the operator admits a DID the person sends them.
- `GET /api/logs` (`tests/log-stream.test.ts`) streams `LogTail` as Server-Sent Events and hijacks the reply, so the `onSend` hook does not run for it: it sets its own headers. Its open streams are ended in a `preClose` hook, or `close()` would wait on them. Events carry no id: every (re)connect gets the whole buffer and the page clears its list on `open`, keeping at most as many rows as `LogTail` holds (1000), so a row goes when its line leaves the buffer. Both apps log through one tee destination (`server.ts`); tests pass `logger: true` with a `logDestination` sink to keep stdout quiet.

## Known gotchas

- **better-sqlite3 must be `^13`, not the `^11` a stale brief might specify.** `^11.x`'s native addon uses V8 APIs (`Object::GetPrototype`, `PropertyCallbackInfo::This`, `Context::GetIsolate`) removed in the V8 shipped with newer Node majors — it fails to compile with an opaque `node-gyp`/`make` error, not a clear version-mismatch message. `^13` ships bundled prebuilds for common platforms (linux-x64/arm64, darwin, win32) and needs no native toolchain on those.
- **Dockerfile still installs `build-essential`/`python3`** in the build stage as a fallback in case a deploy target's platform/libc isn't covered by better-sqlite3's bundled prebuilds (e.g. unusual arches). The final runtime image doesn't carry these — the build stage's `node_modules` (already pruned to `--omit=dev`) is copied forward, not reinstalled.
- **WebSocket test pitfall**: the server sends `online-agents` synchronously as part of completing the upgrade. If a test attaches its `message` listener *after* `await`-ing the socket's `open` event, that first message can already have fired and be lost — attach listeners immediately after `new WebSocket(...)`, before awaiting open. See `tests/helpers.ts`'s `collectMessages` usage in `tests/ws.test.ts`.
- `tests/helpers.ts`'s `waitFor()` accepts a predicate that itself returns a `Promise<boolean>` — always `await` it internally. Passing an async predicate to a naive `while (!predicate())` loop is a classic bug: a Promise is always truthy, so the loop exits on the first check without ever awaiting the real result.

## Testing conventions

Every test file boots a real server (`tests/helpers.ts` `startTestServer`) on `127.0.0.1:0` (random free port) with a fresh temp-dir SQLite file, and drives it over real HTTP/WebSocket via `fetch`/`ws`. Nothing about the server is mocked. `startTestServer` defaults `telepresenceGraceMs` to 300ms (so offline-transition tests don't need multi-second sleeps) — override via its `opts` when a test needs the real production defaults.
