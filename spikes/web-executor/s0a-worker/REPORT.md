# S0a (T3): server-link-language in a browser Web Worker

> Copied from the spike worker's scratch directory on 2026-11-19. Local paths are replaced with `<placeholders>`.
> The scripts in this directory now read paths from env vars; see `../README.md` for how to run them.

**Result: both acceptance items PASS, in 3 of 3 runs.** A headless Chrome tab runs the published, templated `server-link-language` bundle in a module Worker. A 184-line shim replaces `ad4m:host`. A link added in a native executor arrives in the tab as `emitPerspectiveDiff` (about 28 ms). A link committed from the tab arrives in the native executor (about 36 ms), and the native executor marks its signature `valid: true`. A removal from the tab also arrives (about 30 ms). No executor serves the tab. The tab talks only to the link-server, through a test CORS proxy.

Code: coasys/ad4m `origin/dev` at 425311398, in the throw-away worktree `<ad4m-worktree>`. The worktree is removed at the end; there are no commits and no pushes. Everything is under `<s0a-worker>/`.

## 1. Acceptance

| # | Item | Result | Evidence |
|---|---|---|---|
| a | A link added in the native executor arrives in the tab as `emitPerspectiveDiff` | **PASS** (3/3, latency 29 / 29 / 27 ms) | `logs/browser-run{,2,3}.json`, step `ACCEPT-A` |
| b | A commit from the tab arrives in the native executor | **PASS** (3/3, 35 / 37 / 37 ms to `query-links`) | step `ACCEPT-B`; `logs/native-snapshot.txt` |
| – | The tab-signed link is valid in the native executor | **PASS** | snapshot: author `did:key:z6MkghMC…` (the tab), `proof.valid: Some(true)`, `invalid: Some(false)`, `status: SHARED` |
| – | A removal from the tab reaches the native executor | **PASS** (2/2, 27 / 33 ms) | step `REMOVAL` in runs 2 and 3 |
| – | A link from before the tab joined reaches the tab | **PASS** (82–96 ms after `init`) | step `pre-join link arrived`. It is held until the native admin grants the room key, then emitted. |
| – | Worker calls the exports by name | **PASS** | `init`, `perspectiveSyncSync`, `perspectiveSyncCurrentRevision`, `perspectiveSyncRender`, `perspectiveQuerySupportedKinds`, `perspectiveQueryRun`, `perspectiveCommit` |
| – | No executor serves the tab | yes | The page and Worker come from a static Node server on :47458. The tab's only other origin is the CORS proxy :47457 → link-server :47456. |

Log excerpt from run 1 (driver stdout, cut short):
```
STEP init {"ok":true,"result":null,"ms":48}
STEP pre-join link arrived {"afterMs":82,"payload":{"additions":[{"author":"did:key:z6MkoaiGU…(native)","data":{"predicate":"s0a://x1","source":"s0a://native-before",…},"status":"SHARED"}]}}
STEP native add-link {"target":"s0a://native-live-1795068338185"}
STEP ACCEPT-A native link arrived as emitPerspectiveDiff {"latencyMs":29,"payload":{"additions":[{…"source":"s0a://native-live"…}]}}
STEP call perspectiveSyncSync {"ok":true,"result":{"additions":[],"removals":[]}}
STEP call perspectiveSyncCurrentRevision {"ok":true,"result":"307a5dd4f861…"}
STEP call perspectiveSyncRender {"ok":true,"result":{"links":[…2 links…]}}
STEP tab perspectiveCommit {"ok":true,"result":{"author":"did:key:z6MkghMC…(tab)",…}}
STEP ACCEPT-B tab link found in native query-links {"latencyMs":35,"lines":["[2026-11-19T06:05:38.737Z] s0a://tab --ad4m://has--> s0a://from-tab-1795068338736 (did:key:z6MkghMC…)"]}
```
Native executor log (the native language is the room admin):
```
[server-link-language] admin holds no room key (pending) — generating a new one
[server-link-language] client-side key rotation complete: version 1, 1 recipient(s)
[server-link-language] granted key versions [1] to did:key:z6MkghMCM29a5WMWNu9ZUz11AUWuHok9Zip6aiVP75DQaPhL   ← the tab
[server-link-language] commit: 1 adds, 0 removes (keyRingStatus=ready)
```

## 2. How it was run (exact commands)

```bash
# worktree + generated files (not committed)
git -C <ad4m-repo> fetch -q origin dev && git -C <ad4m-repo> worktree add --detach <ad4m-worktree> origin/dev
cp <other-built-worktree>/rust-executor/CUSTOM_DENO_SNAPSHOT.* rust-executor/ && cp -r <ad4m-repo>/rust-executor/dapp/dist rust-executor/dapp/
# executor + CLI (shared target; 4m12s, EXIT=0; the copied snapshot was accepted)
CARGO_TARGET_DIR=<cargo-target> cargo build --release -p ad4m      # builds bins ad4m + ad4m-executor
# language bundle
(cd ad4m-ldk/js && npm ci && npx tsc)
#   @noble/{ciphers,curves,hashes} installed in /tmp and copied to server-link-language/node_modules
#   (`npm install` in the package fails on the "workspace:*" dependency)
(cd bootstrap-languages/server-link-language && deno run --allow-all esbuild.ts)       # build/bundle.js, 152,628 B
# link-server (local only, temp data, auto-admit)
(cd link-server && npm ci && npx tsc)
node link-server/dist/index.js --port 47456 --host 127.0.0.1 --data /tmp/s0a-linkserver-XXXX --auto-admit
node scripts/cors-proxy.mjs 47457 47456 logs/cors.log      # test-only CORS + WS-upgrade pass-through
node scripts/egress-proxy.mjs 47459 logs/egress.log        # deny proxy, loopback only
node scripts/static-server.mjs 47458 web/                  # serves the test page
# native executor: local seed, no Holochain, temp data dir, deny proxy in env
node bootstrap-languages/local/generate-seed.mjs bootstrap-languages/local seed
scripts/start-executor.sh     # init with seed/docker_seed.json, copy seed/languages/*, run with HTTP(S)_PROXY/ALL_PROXY=:47459,
                              # --run-holochain false, --hc-bootstrap-url/--hc-relay-url http://127.0.0.1:47499 (dead),
                              # --local-ai-autostart false, --insecure-no-admin-credential, port 47460
C="ad4m -n --executor-url http://127.0.0.1:47460"
$C agent generate -p passphrase
$C languages publish $PWD/server-link-bundle.js -n server-link-language -d s0a -p SERVER_URL,UID,name,description -s http://127.0.0.1/none
#   → QmzSYwdmwWVdTKTMuHwNXPyb5RjW7gjRZAwX1MZhsRwcvJjNogv  (to the LOCAL language-language KV; no network)
$C languages apply-template-and-publish QmzSYwdmwW… '{"SERVER_URL":"http://127.0.0.1:47457","UID":"s0a-room-1","name":"s0a-link","description":"s0a"}'
#   → QmzSYwdnbFz1QaKr7GoQV9jdjLV21WxmD3crhBaZrQTPSaoHLA9
$C perspectives add s0a-native                                   # → ccaef96d-…
$C perspectives add-link ccaef96d-… s0a://native-before ad4m://has s0a://x1
$C neighbourhoods create ccaef96d-… QmzSYwdnbFz1…               # → neighbourhood://QmzSYwdamdgGAxmg9EFXGg3M8wBBs3PFeP71pdQQRAtuMzmKmYX
# the tab: the TEMPLATED bundle is copied from <data>/ad4m/languages/QmzSYwdnbFz1…/bundle.js to web/templated-bundle.js
LANG_ADDR=QmzSYwdnbFz1… PERSP=ccaef96d-… OUT=browser-run node scripts/drive.mjs
```

Notes on the native side:
- The local-seed route (`generate-seed.mjs` plus pre-copied bundles, as `docker-entrypoint.sh` does) worked the first time. I did not need the tests/js `prepare-test` chain.
- The CLI needs `-n` (no capability token); without it, it asks for an interactive approval and fails with `EOF`. `languages publish` also needs `-s` or it panics reading stdin (`cli/src/languages.rs:144`).
- CLI `perspectives add-link` takes **source, target, predicate**, not source, predicate, target. Run 1 therefore stored `predicate=s0a://x1`. This is harmless here, and runs 2 and 3 use the right order.
- Native warning, not a browser issue: `share_links_after_publish: the push to the new neighbourhood failed; queued 1 shared links … for the pending-diffs loop`. The first push ran before the admin had rotated the room key. The pending-diffs loop sent it 10 s later (the commit is at 06:04:40, the neighbourhood was made at 06:04:30).

## 3. The shim (`web/shim-src.js`, 184 lines → `web/shim.js`, 99.7 KB with noble)

How the bundle is loaded: the Worker fetches the bundle text and replaces the one `from "ad4m:host"` with the absolute URL of `shim.js`. It imports the result from a `blob:` URL. The Worker imports the same `shim.js` URL itself, so both share one module instance (key, KV, event sink). No other byte of the bundle changes. Workers have no import maps, so a rewrite (or a build-time alias) is needed.

Shim contents: an Ed25519 key from `@noble/curves` (random per tab, `did:key` from the public key); an in-memory `Map` KV; `hash`; `httpFetch` over `fetch` with a 10 s `AbortSignal.timeout`; `agentOpenSealed` (Ed25519→X25519 secret, ECDH, HKDF-SHA256 `ad4m:sealed-box:v1`, AES-256-GCM); events forwarded to the page with `postMessage`. Holochain, the `…ForUser` functions and file IO throw "not offered".

Self-test (`node selftest.mjs`) before the browser runs: a box sealed exactly as `encryption.ts sealToDid` does opens in the shim. `agentSignStringHex` verifies the way link-server does it (Ed25519 over SHA-256 of the string). `hash` matches the executor algorithm on 4 vectors (`""`, `hello`, a JSON link, a unicode string). Runtime check: the shim's `hash(bundle)` equals the address the executor gave the published language (`QmzSYwdmwWVdTKTMuHwNXPyb5RjW7gjRZAwX1MZhsRwcvJjNogv`). `UTILS.hash` (`utils_extension.rs:10`) and `calculate_language_hash` (`languages/mod.rs:489`) are the same code: CIDv1, codec 0x00, sha2-256, base58btc, prefix `Qm`. The comment "DAG-Protobuf" in `host.js:265` and `utils_extension.rs` is wrong; the codec is 0x00, which is neither raw (0x55) nor DAG-Protobuf (0x70).

### Host functions the bundle really called (run 3, 182 calls, counter in `logs/browser-run3.json`)

| Function | Calls | Sync? | Notes |
|---|---|---|---|
| `storageGet` | 68 | sync | render/progress, sequence, revision, held/*, links/* |
| `storagePut` | 45 | sync | |
| `hash` | 17 | **sync** | local store keys. Web Crypto `digest` is async, so the shim must carry a JS SHA-256. |
| `httpFetch` | 15 | async | auth ×2, acl, keys, render, sync, commit |
| `storageListKeys` | 14 | sync | prefixes `held/`, `links/` |
| `storageDelete` | 10 | sync | |
| `emitSyncStateChange` | 7 | sync | always `"Synced"` |
| `agentDid` | 2 | sync | |
| `emitPerspectiveDiff` | 2 | sync | the pre-join link and the live native link |
| `agentSignStringHex` | 1 | **sync, needs the private key** | the link-server auth challenge |
| `agentOpenSealed` | 1 | **sync, needs the private key** | opens the granted room key |

Called 0 times: `agentSigningKeyId`, `agentSign`, `agentCreateSignedExpression*`, `agentGetAllLocalUserDids`, `agentDidForUser`, all `holochain*`, `languageAddress`, `languageSettings`, `languageStorageDirectory`, `emitTelepresenceSignal`, `emitSignal`, `readStorageFile`, `writeStorageFile`. The bundle still **imports** all 27 names (the ALDK's `imports.js` re-exports all of them), so a host module must export every name, even as a stub. If one is missing, the module fails to link.

So the C1 problem is real but small for this language: **2 sync calls that need the key** (`agentSignStringHex` once per auth, `agentOpenSealed` once per key version), plus a sync `hash`. With C1 (key in the same Worker) they are direct calls, and they worked.

### Globals beyond the contract (run 3, counted by wrapping them in the Worker; attributed by the innermost non-worker stack frame)

| Global | Bundle | Shim | What uses it |
|---|---|---|---|
| `new WebSocket` | 1 | – | ws-client to `/rooms/:id/ws` (auth is the first message, so a browser WebSocket works) |
| `crypto.getRandomValues` | 2 | 1 | room-key and nonce generation (and the shim's key) |
| `atob` / `btoa` | 11 / 4 | – | JWT `exp` parse, AES-GCM wire encoding |
| `new TextEncoder` / `new TextDecoder` | 12 / 6 | 20 / – | |
| `fetch` | **0** | 15 | the bundle uses only `httpFetch` |
| `AbortSignal.timeout` | 0 | 15 | shim only |
| `setTimeout`/`clearTimeout`/`setInterval` | 0 counted | – | used only on reconnect, and no reconnect happened (`ws-client.ts:243,88`) |
| `Deno`, `Buffer`, `process`, `window`, `document`, `HOLOCHAIN_SERVICE`, `RUNTIME_SERVICE`, `SIGNATURE`, `AGENT`, `LANGUAGE_CONTROLLER`, `UTILS` | **0 reads** | – | traps installed on `self`; none fired |

This confirms T5 for this bundle: it is runtime-clean. The only non-contract I/O is `WebSocket`.

## 4. What `init` really needs

- From the host: `agentDid`, the KV (it starts the render cursor at once: `render/progress`, `sequence`, `held/`), `httpFetch`, sync `agentSignStringHex`, and later sync `agentOpenSealed` and `hash`. It needs **no** `languageAddress`/`languageSettings`/storage directory: `SERVER_URL` and `UID` are baked into the templated source. A browser host must therefore get the **templated bundle** (from the language store or the neighbourhood), not the template.
- From the server: CORS (see 5), a DID admitted to the room (here `--auto-admit`), and **a member online to grant the room key**. Until then the language holds every link it cannot open (`held/…`) and commits throw "holds no room key". In this test the native admin granted on `peer-joined` within ~90 ms.
- `init` returns quickly (17–52 ms) and **resolves even when everything failed**. In the negative run, every fetch failed and `init` still resolved and logged "init complete"; the language keeps retrying through WebSocket reconnect backoff. A host cannot tell "ready" from `init`. *(Corrected after Lal review: `Synced` and a resolved sync are not ready gates either — see "Review corrections" below.)*

## 5. CORS and auth needs of the link-server

- Direct, without the proxy (`logs/browser-negative.json`, room `s0a-cors-negative`): the preflight `OPTIONS /rooms/:id/auth` gets **404 and no CORS headers**, so every `httpFetch` fails with "Failed to fetch". This is the same as the probe of link.ad4m.dev in PLAN.md.
- Preflights the browser really sends (`logs/cors.log`): `POST /auth` (header `content-type`); `GET /acl`, `GET /keys`, `GET /keys/missing`, `GET /render`, `GET /sync`, `POST /commit` (headers `authorization, content-type`). The server must therefore answer `OPTIONS` on `/rooms/*` with `Access-Control-Allow-Origin` (an echoed origin or `*`), methods `GET, POST, DELETE` (`DELETE` for `deleteRoomKey`; not seen in this run), and headers `authorization, content-type`. It must also put ACAO on the real responses. No credentials or cookies are needed: auth is a bearer JWT from the DID challenge.
- WebSocket: the upgrade went through the proxy with `Origin: http://127.0.0.1:47458`. The link-server does not check `Origin`, and WebSockets need no CORS, so a direct WebSocket would work too (I did not test it directly). Auth is the first message, not a header, so it works from a browser.
- Admission: in the default allowlist mode, a fresh browser DID gets 403 until someone admits it (README). Only `--auto-admit` was tested.

## 6. Failures and workarounds = to-do list for host C

1. **CORS on link-server** (needed; the test used a proxy). Add an `OPTIONS` handler and ACAO as listed in 5, for `/rooms/*` and `/health`. Check store.ad4m.dev and the other backends (T12).
2. **`ad4m:host` resolution in a Worker**: no import maps in Workers. Either rewrite the specifier text (done here; one site), or the host build resolves `ad4m:host` to its own module. The host module must export **all 27 names**.
3. **Sync `hash`** needs a JS SHA-256 (noble) because Web Crypto `digest` is async. Fix or test-vector the "DAG-Protobuf" comment (it is codec 0x00).
4. **Signing links is the host's job**: the language commits the `LinkExpression`s it gets, so the web core must sign them like the executor: `SHA-256(serde_json(Link) ‖ RFC3339-millis-Z timestamp)`, with the `Link` key order **predicate, source, target** (`types/core.rs:78`). `JSON.stringify` only matches serde when the keys are in that order; it worked here (`valid: true`). A canonical-JSON rule or a test vector belongs in the protocol (T6).
5. **Two sync key functions** (`agentSignStringHex`, `agentOpenSealed`) are enough for this language. C1 (same Worker) works. For C2/C3/C4, this language needs only these two to change.
6. **`init` does not report failure**: the host needs a readiness signal other than `init` resolving (sync state, or a probe call).
7. **Room key needs an online member**: a browser guest that joins alone waits in "held" until the admin or a key-holding member is online. This is a product question for the guest view (F.1 Q3, Q10).
8. **KV persistence**: the shim's KV is memory only. On reload the tab gets a new DID and starts over with a full render. A real host needs IndexedDB/OPFS behind the sync KV, for example a sync in-memory map that is loaded before `init` and written back later, as host A does.
9. Small CLI papercuts found on the way (not host C): `languages publish` panics without `-s` on non-TTY stdin; `add-link` argument order is not what the help suggests.

## 7. Egress proxy log (`logs/egress.log`)

```
2026-11-19T06:02:16.329Z egress proxy listening on 47459
2026-11-19T06:02:18.296Z REFUSED HTTP GET example.com/        ← my own self-test of the proxy (curl -x), not the executor
```
**The executor made zero requests through the proxy**, allowed or refused. With the local seed (no `identityStore`, no Holochain, local language-language in the KV), it did not try mock.ad4m.dev, bootstrap.ad4m.dev or the public language store. Its log names no external URL. Publishing went to the local language-language KV only. Limits: the proxy sees only clients that honour `HTTP(S)_PROXY`. I could not attribute sockets to the executor process (`lsof -p` and `ss -p` showed nothing for it in this environment), so direct (non-proxy) connections are not proven absent. The SFU bound 127.0.0.1 and no STUN server was set (no probe).

## 8. What I did NOT test

- link.ad4m.dev or any non-local server; TLS (`wss://`, `https://`).
- Allowlist admission (only `--auto-admit`); admin actions from the tab; key rotation by the tab; a tab as the room admin.
- Telepresence, `peersRemote`, `peersSetLocal`, `teardown`, WebSocket reconnect, token refresh after JWT expiry, paged render of a large room, catch-up after the tab was offline.
- Persistence across a page reload; two tabs; C2 (one Worker per language with `SharedArrayBuffer`/`Atomics`); COOP/COEP.
- Any other language (Rust/wasm-bindgen languages, perspective-language DOM stubs, language-language through `axiod`).
- A direct (non-proxied) WebSocket from the tab to link-server.
- That `UTILS.hash` as a JS op returns the same as `calculate_language_hash` at runtime (same Rust code, checked by reading; the runtime vector used the published language address).
- The executor's own outbound sockets at the OS level (see 7).

## Files

- `web/shim-src.js` (shim source), `web/shim.js` (bundled), `web/worker.js`, `web/index.html`, `web/templated-bundle.js` (the bundle the tab ran), `web/negative-bundle.js`
- `scripts/drive.mjs` (playwright-core + /usr/bin/google-chrome headless), `scripts/egress-proxy.mjs`, `scripts/cors-proxy.mjs`, `scripts/static-server.mjs`, `scripts/start-executor.sh`, `selftest.mjs`
- `logs/browser-run.json`, `browser-run2.json`, `browser-run3.json` (steps, events, every shim call, console), `browser-negative.json`, `cors.log`, `egress.log`, `executor.log`, `link-server.log`, `native-snapshot.txt`, `build.log`
- `seed/` (generated local seed)

## Cleanup (done)

Killed: the executor, link-server, CORS proxy, egress proxy and static server (ports 47456–47460 are free again). Chrome closed with the driver. Removed: the worktree `<ad4m-worktree>` (`git worktree remove --force`; its only change was `deno.lock` from the Deno build), `/tmp/s0a-exec-*`, `/tmp/s0a-linkserver-*`, `/tmp/s0a-noble`. The shared target `<cargo-target>` keeps the release `ad4m` and `ad4m-executor` built from 425311398. To run it again: `npm install esbuild playwright-core @noble/ciphers@1 @noble/curves@1 @noble/hashes@1` in `/tmp/s0a-noble` (drive.mjs imports playwright-core from there), rebuild `web/shim.js` with esbuild, then follow section 2.

## Review corrections (Lal, 2026-11-19 07:13, CORRECT WITH GAPS — agent-mail/lal-web-exec-s0a-review.md)

- **Ready state.** Neither `Synced` nor a resolved `performSync` is a ready gate: `Synced` fires before catch-up, `sync.ts` can emit `Synced` with held links still unopened, and `performSync` resolves an empty diff on failure. Host C needs separate states: configured, authed, caught-up, decrypt-ready, write-ready.
- **init claim** holds for SLL network-startup failures (`index.ts:379-409`), not for every failure.
- **Signing:** verified (`core.rs:78-83`, `signatures.rs:84-99`). Add: an empty predicate serializes as `"predicate": null` (not omitted); the verifier reformats the timestamp, so sign the normalized UTC text (RFC 3339, millis, `Z`).
- **27 exports:** ldk `imports.ts` re-exports all 27 by name, so likely required; the emitted bundle was not checked, so this is reported, not verified.
- **CORS:** GET, POST, DELETE + `authorization`, `content-type` covers SLL; answer OPTIONS before auth and send ACAO on error responses too. The WS endpoint has no Origin check; keep the operator listener separate.
- **Missing from the host-C to-do list (§6):** DID and key persistence across reloads; the periodic sync and retry scheduler; KV durability and namespace isolation; `httpFetch` parity with `host.js`.
- **Scope of evidence:** 3 of 3 runs support narrow local feasibility only.
