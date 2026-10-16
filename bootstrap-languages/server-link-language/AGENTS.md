# AGENTS.md — server-link-language

Persistent, repo-scoped memory for this project. Update this file whenever
an architectural decision, gotcha, or convention changes — it is the source
of truth reloaded on every session in this tree.

## What this is

An AD4M link language (`@coasys/ad4m-ldk`, modern `defineLanguage()`
pattern) that syncs a perspective through a self-hosted `link-server`
instance: HTTP for auth/commit/catch-up-sync/peers/ACL/E2E-key-fetch, native
WebSocket for real-time diff push and telepresence. Implements all five
link-language capabilities: `perspective-commit`, `perspective-sync`,
`perspective-query`, `peers`, `telepresence`.

Scaffolded from `ad4m-link-language-template` (pure/impure adapter
separation, esbuild + Deno bundling, Node/tsx unit tests). See `README.md`
for the full architecture writeup.

Lives in the `ad4m` monorepo at `bootstrap-languages/server-link-language/`.
`package.json`'s `@coasys/ad4m-ldk` dependency uses `workspace:*`, and the
`ad4m-ldk` resolution in `esbuild.ts`/`tsconfig.json` points to the
monorepo's own layout (`../../ad4m-ldk/...`, same convention every other
`bootstrap-languages/*` package uses) — see "Build / test / typecheck"
below.

## Build / test / typecheck

Run from this directory (turbo also drives `build`/`test` via
`@coasys/server-link-language#build` / the root `pnpm test`):

```bash
pnpm install                          # workspace install runs from the monorepo root
deno run --allow-all esbuild.ts       # -> build/bundle.js
node --experimental-vm-modules --import tsx --test tests/*.test.ts
npx tsc --noEmit
```

`AD4M_LDK_ENTRY` env var overrides the default `../../ad4m-ldk/js/lib/index.js`
resolution (both in `esbuild.ts` and `tsconfig.json`'s `paths`) — same
override mechanism as the standalone repo, just a different default target.

## Repo layout

```
index.ts              — defineLanguage() wiring; all module state lives here (fresh per perspective instance)
esbuild.ts             — Deno build script; bundles @noble/* IN, externalizes only "ad4m:host"
src/types.ts            — AD4M types + server wire types (WireLinkExpression, WS message unions, ...)
src/adapters.ts          — pure interfaces + init/get singletons: Transport, StorageAdapter, AgentAdapter, RuntimeAdapter, WebSocketFactory, RoomConfig
src/adapters-deno.ts      — ad4m:host + native WebSocket implementations of the above (only imported from index.ts)
src/store.ts               — local link store (indexed) + sync cursor (revision, sequence)
src/api.ts                  — typed HTTP client, one function per server endpoint
src/auth.ts                  — DID challenge-response + JWT-expiry-driven token refresh
src/ws-client.ts               — WebSocket lifecycle: typed dispatch, send queue, exponential-backoff reconnect
src/sync.ts                     — commit (chunked) / catch-up / bootstrap; the ONE inbound-diff choke point (applyInboundWireDiff)
src/key-ring-manager.ts          — KeyRingManager: E2E key ring load, admin rotate/grants, pending-grant poll (instance per language, testable in Node)
src/telepresence.ts              — online-agent roster + outbound signal/broadcast/status
src/encryption.ts                 — X25519 derivation, sealed room-key exchange, AES-256-GCM link encryption
tests/*.test.ts                    — node:test + tsx, one file per pure module, all use mock adapters (no real network/WS/Deno)
```

## Non-obvious conventions

- **Adapter/singleton pattern everywhere.** Every module that needs I/O
  (`api.ts`, `sync.ts`, `store.ts`, `ws-client.ts`) pulls its dependency
  from a swappable module-level singleton in `src/adapters.ts`
  (`initTransport`/`getTransport`, etc.), never imports `ad4m:host`
  directly. This is what makes every module under `tests/` runnable in
  plain Node with mocks. **Import rules:** `src/adapters-deno.ts` is the only
  file allowed to import runtime-specific APIs (`ad4m:host`, Deno globals).
  Only `index.ts` is allowed to import `adapters-deno.ts`. `index.ts` also
  imports `defineLanguage` and `hash` directly from `@coasys/ad4m-ldk` —
  these are module-surface exports (not I/O adapters), so the direct import
  does not break the swappable-adapter guarantee. Pure library
  imports (`@noble/curves`, `@noble/ciphers`, `@noble/hashes`) are fine
  anywhere — they contain no runtime coupling.
- **Commit: the store follows the server, nothing is echoed.**
  `sync.commit()` splits a diff into POSTs of at most `COMMIT_CHUNK_SIZE`
  (200) links — all additions first, then removals, which is how one POST
  applies them — encrypts each chunk just before sending it, and writes a
  chunk to the language's store only after the server accepted it. A
  failed commit leaves no local-only links; a failure mid-way leaves the
  accepted chunks, and the executor's retry re-sends them, which is safe
  (server OR-Set keyed by link hash; store keyed by link hash). The
  committed diff is NOT emitted back to the executor: it committed those
  links and already holds them; the echo made it re-persist, re-index and
  re-publish every link (`diff_from_link_language`). After a commit whose
  server sequence is exactly cursor + 1 the cursor moves past it, so
  catch-up does not fetch our own links back; otherwise it stays.
- **The executor runs one call at a time per language.**
  `rust-executor/src/languages/language_runtime.rs` awaits each execute
  before taking the next request; the JS event loop keeps running inside
  it. The yield between commit chunks lets the language's own async work
  (WS diffs, key poll, admin grants) run during a long commit, but
  `sync()`/`render()`/the next commit from the executor still wait for the
  whole commit. Keep commit cheap; anything long-running inside a call
  blocks every other call to this perspective's language.
- **`applyInboundWireDiff` (src/sync.ts) is the only place that calls
  `emitDiff`.** The AD4M executor discards `perspectiveSyncSync()`'s return
  value — `emitPerspectiveDiff` is the only thing that makes an inbound
  link queryable. HTTP catch-up (`catchUp()`) and the WebSocket `"diff"`
  handler (wired in `index.ts`) both funnel through this one function. If
  you add a new inbound-diff source, route it through
  `applyInboundWireDiff` — do not call `store.applyDiff` + emit separately.
- **`sync.sync()` always does an HTTP catch-up**, regardless of WebSocket
  connection state. This is deliberate, not a missing optimization: because
  `since` only advances after a diff is actually applied, a live WebSocket
  that already pushed everything makes the catch-up a no-op (empty
  `diffs[]`) by construction. Do not add an `if (wsConnected) return early`
  branch — it duplicates state that's already correctly tracked by the
  sequence cursor and is an easy place to reintroduce a missed-diff bug.
- **Local store is always plaintext.** E2E encryption operates only at the
  wire boundary (`encryptLinkForWire`/`decryptLinkFromWire` in
  `src/encryption.ts`, called from `src/sync.ts`'s `toWireLink`/
  `fromWireLink`). `render()`/`queryLinks()` must never need to know
  whether the room is E2E — if you're touching decryption logic outside
  `sync.ts`'s wire-translation functions, you're probably in the wrong
  place.
- **Module state resets are almost free.** Per the AD4M language-interface
  spec, the runtime loads a fresh module instance per perspective (cache-
  busted import), so `let` bindings at module scope are naturally
  per-instance. `teardown()` still explicitly closes the WebSocket
  (mandatory — an unclosed client leaves a live reconnect-backoff timer
  running) and calls `resetAdapters()` / `auth.resetAuth()` for hygiene,
  but a fresh instance would start clean regardless.

## Gotchas

- **Session loss.** A 401 on HTTP drops the cached JWT and retries once
  (`withToken`, src/sync.ts); a WS `auth-error` drops it too
  (`invalidateToken` option of `WsClient`) before the reconnect, which
  then authenticates afresh. Without that the dead token is reused
  forever (link-server data reset, revoked session).
- **No WS message for a key grant.** The link-server pushes nothing when
  the admin grants keys, so a member whose `/keys` says "E2E room, no keys
  for you" (`pending`) polls `/keys` itself: every 500 ms for 10 s, then
  every 2 s up to 60 s (`PENDING_POLL_SCHEDULE_MS`), and restarts that
  schedule on every `peer-joined` (the admin may just have come online).
  When keys arrive it re-reads the room (bootstrap + emit). While the
  poll runs, background refreshes (`refresh()`, 10 s cooldown) defer to
  it. The admin grants on `peer-joined`, after every sync, at init, and
  right after a rotation; `/keys/missing` lists members with no keys at
  all, so new joiners are covered. Grants are single-flight; a request
  during a run triggers one more run.
- **Index.ts state is not loadable in Node** (it imports `ad4m:host`).
  Logic that needs tests goes in a pure `src/` module with explicit deps
  (see `KeyRingManager`); `tests/integration.test.ts` runs two managers
  (admin, joiner) side by side against an in-process link-server. The
  module singletons (adapters, auth session, store, sync) hold one agent
  at a time, so those tests authenticate each agent with its own token
  getter and re-point the sync module/store with `wireMember`.
- **`httpFetch` return shape varies by executor version.**
  Executor builds before `2b65ebbf0` return a plain string on success
  and throw `"http_fetch METHOD URL -> STATUS: body"` for non-ok HTTP.
  Current dev (`2b65ebbf0`+) returns `{ status, body }` for all
  responses. `DenoTransport.fetch()` handles both shapes — do not
  remove the `typeof res === "string"` guard until the old executor
  version falls out of deployment.

## Known limitations / follow-ups

- **Durable outbox is limited to in-memory retry.** `enqueueCommitBatched`
  (src/sync.ts) coalesces contiguous commits into segments and retries
  each segment up to 3 times with exponential backoff. If all attempts
  fail, the failed segments re-enqueue for the next flush cycle and the
  language emits `LinkLanguageInstalledButNotSynced`. The language's store
  only gets links the server accepted, so queued links stay out of it until
  the server recovers, and a process restart loses the
  queue — a durable pending-commits queue (persisted in the KV store,
  retried by `sync()`) would close this gap.
- **E2E encryption activates automatically — no plaintext mode.** The admin's
  language instance generates the initial room key during `init()` when no E2E
  exists yet. Subsequent rotations require explicit admin action.
  `performRotation()` generates a fresh room key client-side, seals it to every
  ACL member's X25519 public key, and POSTs only sealed envelopes — the server
  never sees plaintext. It does NOT fire on presence events (`onPeerJoined` is
  reconnect/wake, not ACL change — rotating there would mint a new version on
  every reconnect). `onPeerJoined` only calls `KeyRingManager.onPeerJoined()`:
  the admin grants keys to members who lack them (idempotent), a pending
  member restarts its `/keys` poll. The key ring
  loads during `init()`'s `keyMgr.setup()` and the `version` field in
  `KeysResponse` remains unused for mid-session detection — a periodic `/keys`
  re-fetch or a dedicated WS push message would close that gap.
- **E2E encryption wire format is unified.** Both client and server use
  the same `EncryptedLinkData` shape (`{ciphertext, nonce}`) in the link's
  `data` field for encrypted rooms — no separate `encrypted` field. The
  server treats link data as opaque (no E2E validation at commit time)
  and stores only sealed `encryptedKey` envelopes (ECIES: ephemeral X25519
  ECDH + HKDF-SHA256 + AES-256-GCM). Key generation and sealing happen
  exclusively client-side. The server accepts client-registered X25519
  public keys during auth.
- **E2E integration coverage via test matrix.** The integration suite
  (`tests/js/tests/integration.test.ts`) runs the full neighbourhood test
  battery for both `[holochain]` and `[server-link]` link-language
  configurations. Since E2E activates automatically for every room, the
  `[server-link]` leg exercises the full init → key generation → encrypt →
  sync → decrypt → grant pipeline through two real AD4M executors.

## Testing approach

Every `src/*.ts` module except `index.ts` and `adapters-deno.ts` is pure
(no `ad4m:host`, no real network, no real WebSocket) and has a corresponding
`tests/*.test.ts` using hand-rolled mock adapters (`MockStorage`,
`MockTransport`, `MockAgent` — defined locally per test file, not shared,
matching the template's convention). `tests/encryption.test.ts` runs the
real `@noble/*` primitives end-to-end (no mocking crypto) so a broken wire
format or swapped key genuinely fails the round trip.
