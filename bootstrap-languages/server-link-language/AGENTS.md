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
src/sync.ts                     — commit / catch-up / bootstrap; the ONE inbound-diff choke point (applyInboundWireDiff)
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
  Only `index.ts` is allowed to import `adapters-deno.ts`. Pure library
  imports (`@noble/curves`, `@noble/ciphers`, `@noble/hashes`) are fine
  anywhere — they contain no runtime coupling.
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

## Known limitations / follow-ups

- **Durable outbox is limited to in-memory retry.** `enqueueCommitBatched`
  (src/sync.ts) coalesces contiguous commits into segments and retries
  each segment up to 3 times with exponential backoff. If all attempts
  fail, the failed segments re-enqueue for the next flush cycle and the
  language emits `LinkLanguageInstalledButNotSynced`. This keeps links
  local-only until the server recovers, but a process restart loses the
  queue — a durable pending-commits queue (persisted in the KV store,
  retried by `sync()`) would close this gap.
- **No E2E key rotation handling.** The room key is fetched once during
  `init()`'s `setupRoomKey()` and held for the perspective's lifetime. The
  `version` field in `KeysResponse` is captured but unused — a real
  rotation flow would need to detect a version bump (e.g. periodic
  `/keys` re-fetch, or a dedicated WS push message not in the current
  server API) and re-key in place.
- **E2E encryption wire format is unified.** Both client and server use
  the same `EncryptedLinkData` shape (`{ciphertext, nonce}`) in the link's
  `data` field for encrypted rooms — no separate `encrypted` field. The
  server treats link data as opaque (no E2E validation at commit time),
  uses HKDF-SHA256 for key sealing (matching the client's KDF), returns
  the `encryptedKey` envelope as a typed `SealedRoomKeyEnvelope` object,
  and accepts client-registered X25519 public keys during auth.
- **`peers.remote()` and E2E setup are not covered by the automated test
  suite directly** (only indirectly, through `api.ts`/`encryption.ts` unit
  tests) — there's no `index.ts`-level integration test because `index.ts`
  requires the Deno `ad4m:host` bootstrap to exercise for real. Verify
  those paths against a running `link-server` once one exists.

## Testing approach

Every `src/*.ts` module except `index.ts` and `adapters-deno.ts` is pure
(no `ad4m:host`, no real network, no real WebSocket) and has a corresponding
`tests/*.test.ts` using hand-rolled mock adapters (`MockStorage`,
`MockTransport`, `MockAgent` — defined locally per test file, not shared,
matching the template's convention). `tests/encryption.test.ts` runs the
real `@noble/*` primitives end-to-end (no mocking crypto) so a broken wire
format or swapped key genuinely fails the round trip.
