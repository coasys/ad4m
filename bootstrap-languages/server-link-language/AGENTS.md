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
src/encryption.ts                 — room keys sealed to DIDs via the executor, key ring, AES-256-GCM link encryption
src/room-keys.ts                   — admin rotation (seals to every ACL member) and any member's grants
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
- **`applyInboundWireDiff` (src/sync.ts) is the only place that calls
  `emitDiff`.** The AD4M executor discards `perspectiveSyncSync()`'s return
  value — `emitPerspectiveDiff` is the only thing that makes an inbound
  link queryable. HTTP catch-up (`catchUp()`) and the WebSocket `"diff"`
  handler (wired in `index.ts`) both funnel through this one function. If
  you add a new inbound-diff source, route it through
  `applyInboundWireDiff` — do not call `store.applyDiff` + emit separately.
- **Commit: the store follows the server, nothing is echoed.**
  `sync.commit()` writes a diff to the language's store only after the
  server accepted the POST, so a failed commit leaves no local-only links.
  It does NOT emit the diff back to the executor: the executor committed
  those links and already holds them (the echo made it re-persist,
  re-index and re-publish every link). When the server's answer puts the
  commit at exactly cursor + 1 the cursor moves past it, so catch-up does
  not fetch our own links back; otherwise another agent's diff is in
  between and the cursor stays.
- **A 401 replaces the session.** `commit`, `bootstrap` and `catchUp` run
  their request through `withToken` (src/sync.ts): on a 401 it drops the
  cached token (`invalidateToken` → `auth.invalidateSession()`),
  re-authenticates and retries once. A WebSocket `auth-error` drops the
  token before the reconnect for the same reason.
- **The KV write cost is the host's.** `ad4m:host`'s KV
  (rust-executor/src/js_core/host.js) keeps the store in memory and
  rewrites its file once at the end of the turn that changed it, so the
  store's one key per link is cheap within one synchronous `applyDiff`.
  Keep store writes for a diff in one turn (no `await` between them).
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
  `performRotation()` (admin only) generates a fresh room key client-side,
  seals it to the DID of every ACL member, including members that never
  connected, and POSTs only sealed boxes — the server never sees plaintext.
  It does NOT fire on presence events (`onPeerJoined` is reconnect/wake, not
  ACL change — rotating there would mint a new version on every reconnect).
  `performKeyGrants()` runs on every member that holds keys (at init, after
  every sync, on `onPeerJoined`) and seals the versions it holds to ACL
  members lacking them (idempotent; the server keeps the first box per member
  and version and refuses versions the granter lacks). A member whose key
  status is `pending` (no box it holds opens: no member holding a key has
  been online since it joined, or its boxes are in an older format; an admin
  in that state rotates) re-reads `/keys` on every sync, skipping the 10 s refresh cooldown
  (`refreshSkippedByCooldown`, src/key-ring-policy.ts), so it sees the room on
  the first sync after the grant. A version whose box does not open is left
  out of the ring (`buildKeyRing`), so one bad grant costs that version, not
  the room. The key ring
  loads once during `init()`'s `setupKeyRing()` and the `version` field in
  `KeysResponse` remains unused for mid-session detection — a periodic `/keys`
  re-fetch or a dedicated WS push message would close that gap.
- **Room keys are sealed to DIDs by the executor.** A member's X25519 key is
  the Montgomery form of the Ed25519 key in its `did:key`. The language seals
  with the `agentSealToDid(did, hex)` host import and opens with
  `agentOpenSealed(box)` (`AgentAdapter.sealToDid` / `openSealed`); it never
  holds an agent secret, and auth sends no key. `tests/sealed-box.ts` is a
  noble reference of the executor's format for the mock agents; its test
  vector in `tests/encryption.test.ts` is pinned in
  `rust-executor/src/agent/sealed_box.rs` too: changing the format orphans
  every sealed room key.
- **E2E encryption wire format is unified.** Both client and server use
  the same `EncryptedLinkData` shape (`{ciphertext, nonce}`) in the link's
  `data` field for encrypted rooms — no separate `encrypted` field. The
  server treats link data as opaque (no E2E validation at commit time)
  and stores only sealed `encryptedKey` boxes (the executor's sealed box:
  ephemeral X25519 ECDH + HKDF-SHA256 + AES-256-GCM). Key generation and
  sealing happen exclusively client-side. The server stores no member keys:
  the ACL is DIDs.
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
