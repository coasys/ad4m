# AD4M monorepo — agent guide

Canonical instructions for this directory. `CLAUDE.md` next to this file is a
Claude Code entrypoint that contains only `@AGENTS.md` so Claude inlines this
file. Edit this file; do not put unique rules in `CLAUDE.md`.

## Package map

| Path | What | Language |
|---|---|---|
| `rust-executor/` | The AD4M runtime: WS RPC server, perspectives/graph store, languages runtime (Deno), Holochain conductor, AI service, MCP server. **Start at `rust-executor/AGENTS.md`.** | Rust |
| `cli/` | `ad4m` CLI binary; wraps `rust-executor` (`ad4m-executor` subcommand) and `rust-client` | Rust |
| `rust-client/` | Rust client for the executor's WS RPC | Rust |
| `ad4m-hash-chain/` | Generic linear hash chain of signed JSON entries (entry hash, `eddsa-jcs-2022` proofs, fold/import, fork detection, `ChainStore` with memory/SQLite adapters; native + WASM). Under the key log. See `ad4m-hash-chain/AGENTS.md` | Rust |
| `ad4m-identity-core/` | The `did:scid:coasys:1` identity core: the key log as a chain type on `ad4m-hash-chain` (`Event`, `KelPolicy`, `fold`, `KeyState`), the DID document (`did_document`), signing domains, proof of work, event builders (`build`), the 12 words (`words`), WASM exports (`wasm`). Spec `docs-src/identity/did-scid-coasys-1.md`. See `ad4m-identity-core/AGENTS.md` | Rust |
| `ad4m-identity-client/` | The master-key side of `did:scid:coasys:1` for apps: `Vault` (`MemoryVault` behind `test-vault`), `MasterSigner`, `ViaExecutor` (the executor's `identity.*` calls over any `ExecutorRpc`; feature `ws` for `ad4m-client`), flows `create`, `link`, `revoke`. Builds for wasm32. See `ad4m-identity-client/AGENTS.md` | Rust |
| `identity-store/` | The shared store of key logs: `identity-store` binary (axum, SQLite on `ad4m-hash-chain`'s `SqliteChainStore`), signed heads, the key index, signed requests, agent records, offers and claims (an `add` spends a claim: the claim check). Its log and head reads are the public did:scid host profile (any origin, cached). Reached by executors only through the agent language. See `identity-store/AGENTS.md` | Rust |
| `core/` | TypeScript SDK (`@coasys/ad4m`): `Ad4mClient`, types, model/SHACL decorators, generated RPC contracts (`src/generated/api/RpcMethods.ts`) | TS |
| `connect/` | Browser/Node connection helper (`@coasys/ad4m-connect`) | TS |
| `bootstrap-languages/` | The system Languages (agent, perspective-diff-sync, etc.) bundled into the executor | TS/Rust |
| `ad4m-ldk/` | ALDK = AD4M Language Development Kit (Rust + JS crates for writing Languages) | Rust/TS |
| `ad4m-hooks/`, `hooks/` | React/Vue hooks for the SDK | TS |
| `ui/` | Launcher UI (Tauri) | TS/Rust |
| `dapp/` | Web dapp bundled into the executor (`dapp_server.rs`). Builds against the **published** `@coasys/ad4m` from npm, not `core/`: SDK changes reach it only after a release | TS |
| `tests/js/` | Integration test suites run against a built `ad4m-executor` binary | TS |
| `test-runner/` | Language test harness | TS |
| `docs-src/` | Docs site sources + language interface specs (`language-interface-spec.md`, `host-contract.md`) | MD |
| `planning/` | Dated design + refactoring specs. Current: `rust-executor-refactoring-spec-2026-09-04.md` | MD |

There is no `executor/` package any more: the JS executor was folded into
`rust-executor/src/js_core` and then mostly rewritten in Rust. Ignore references to
it in older docs.

## Repo-wide rules

- Package manager is `pnpm`, never `npm`. Test commands: `pnpm run test-main`
  (integration), `cd rust-executor && pnpm test` (crate unit tests, serial).
- JS/TS files embedded into the Deno snapshot (`rust-executor/src/js_core/*.js`
  and extension `.js` files) must be pure ASCII: non-ASCII fails const-eval in
  `ascii_str_include!`.
- Commit messages: Conventional Commits (`feat:`, `fix:`, `refactor:`, `docs:`, `test:`).
- **Do not edit `CHANGELOG` in a PR.** The changelog is written once, at release
  time, from the merged PRs. Every PR that edits it conflicts with the next one
  that does. Resolving that conflict is a push, and a push dismisses the PR's
  approvals, so it costs a full CI run and a new review for a one-line text clash.
  Put the release note in the PR description instead, under a `### Changelog`
  heading: what changes for users, apps or operators, and **Breaking:** first
  when it is. Internal-only PRs (tests, CI, refactors with no visible change)
  write "none". If a branch already carries a `CHANGELOG` entry, leave it; do not
  push only to remove it.
- Design docs go to `planning/<topic>-<yyyy-mm-dd>.md`; delete stale ones rather
  than leaving them beside current ones.
- Per-directory agent docs: canonical file is `AGENTS.md`. Sibling `CLAUDE.md`
  contains only `@AGENTS.md`.
- **RPC contracts are generated.** Every executor method registers
  `map.method::<Params, Result>(name, handler)` (`.read()` for idempotent reads,
  `.long()` for calls that run for minutes); dispatch rejects params outside the
  contract. `core`'s `ApiClient.call` takes its types from
  `core/src/generated/api/RpcMethods.ts`. After changing a method or a type it
  reaches, regenerate: `cd core && pnpm run generate:api-types`. A unit test fails
  when the committed files are stale.
- **Events are typed and opt-in.** Every pushed event has a name in
  `events_ws::events` and a payload type in `event_specs()` (generates
  `Events.ts`); SDK code listens with `ApiClient.on(name, handler)`. A socket
  gets no event until it sends `events.watch` naming it, so a raw-WebSocket test
  client must send the watch and wait for its reply before it triggers the
  event (see `tests/js/helpers/sfu/events.ts`).
- `connect/` tests (vitest + happy-dom) fail under Node 26 (`localStorage.clear`
  undefined); run them under Node 24.

## Holochain DHT and GetStrategy

**Important**: Holochain currently only implements **full-arc (full-sync) DHT mode** where every node gossips and stores all data. This means:

- `GetStrategy::Local` is the correct choice for DHT lookups because all nodes will eventually have all data once gossip completes
- `GetStrategy::Network` is NOT needed until Holochain implements actual sharding/partial-arc storage
- Flaky tests related to cross-agent data visibility are **gossip timing issues**, not strategy issues
- The fix for such flaky tests is to add retry logic with appropriate timeouts, not to change from Local to Network strategy

When debugging cross-agent communication issues:
1. First check if it's a gossip timing issue (data not yet propagated)
2. Add retry logic in tests rather than changing GetStrategy
3. Ensure agent info exchange is working (K2 spaces must exist before adding agent infos)

## Holochain K2 Spaces (Kitsune2)

After the Holochain 0.7.0 update with PR #5550:
- K2 spaces are only created by the `join` function
- `add_agent_infos` will NOT create spaces - they must exist first
- If trying to add agent info for a space that doesn't exist, you'll get `K2SpaceNotFound`
- Retry logic should handle this by waiting for spaces to be created, then skipping if they truly don't exist (agent not in that DNA)

## Running Integration Tests

The integration tests are in `tests/js`. Four suites, one CI job each:

| Script (in `tests/js`) | CI job | Executors |
|---|---|---|
| `pnpm run test-main` (= `test-main-local`) | `integration-tests-js` (required check) | single-executor suites + Alice/Bob on `bootstrap-languages/local/*`, `--run-holochain false` |
| `pnpm run test-model`, then `pnpm run test-flow` | `integration-tests-model` | the `Ad4mModel` and flow suites. Not part of `test-main`, so a CI run does not run them twice. |
| `pnpm run test-main-server-link` | `integration-tests-multi-node-server-link` | Alice + Bob on local languages, links over the server-link-language and a link-server the suite starts (`tests/js/tests/integration-server-link.test.ts`) |
| `pnpm run test-main-multi-node-holochain` | `integration-tests-multi-node-holochain` | Alice + Bob with Holochain: agent language + p-diff-sync (`tests/js/tests/integration.test.ts`) |

Where a two-executor test belongs: if it only needs one executor to see
languages, neighbourhoods or agent profiles the other published, the local
suite — `startExecutor` points the local language-language, neighbourhood store
and agent-language of every executor at shared directories under
`tests/js/tst-tmp` (`tests/js/utils/sharedStores.ts`). If it needs links to sync
between executors, the server-link suite (and, for p-diff-sync itself, the
Holochain suite); such suites take a `LinkLangConfig` (`utils/linkLangConfig.ts`)
so the same file runs on both link languages.

### Nightly (`nightly-llm-e2e` workflow)

A pipeline with `run-nightly-llm: true` runs only this workflow, on `coasys/marvin`
(local Ollama). Each job has its own serial group. The jobs: `llm-e2e`,
`integration-tests-sfu`, `integration-tests-sfu-browsers` (see below) and
`coasys-spaces`.

`coasys-spaces` runs WE's product suite (`pnpm --filter @we/app-web test:spaces`,
WE `apps/we-web/tests/live/coasys-spaces/`): a Coasys space, a call, its
transcript (executor Whisper), extraction on Ollama `qwen3.8-27b-48k` (the
model staging and `llm-e2e` use, which Marvin keeps loaded), and flows.
Ollama's OpenAI endpoint runs a model with the server's default context window
and cuts a longer prompt silently. So when the model's own `num_ctx` is below
`COASYS_SPACES_LLM_NUM_CTX` (16384), `llm-check` makes a variant such as
`coasys-spaces-gemma3-12b:ctx16384` (`/api/create`) and the suite runs on it.
A model with a large enough `num_ctx` (the 48k tag) runs as it is, so the GPU
holds one load. It uses this commit's executor and SDK:

- The executor is the one the `build` job published for the commit
  (`$EXECUTOR_PUBLISH_DIR/<sha>/manifest.json`, sha256 checked). If there is none,
  the job builds it.
- `core` and `connect` are built and packed. WE's ad4m pin (root `pnpm.overrides`)
  points at the two tarballs.
- WE comes from the pipeline parameter `we-ref` (branch, tag or full commit;
  default `dev`). WE installs with its own pnpm (10).

The job halts green when the same ad4m commit, WE commit, tier and model already
passed on the host (`~/.local/state/ad4m-ci/coasys-spaces/last-success`). Set
`COASYS_SPACES_FORCE=1` to run anyway. A checkout with changes (a run by hand
with local edits) runs, but neither skips nor records a pass. It shares the serial group
`nightly-llm` with `llm-e2e`: both use the host's Ollama and GPU, so they run
one after the other. Screenshots, traces and logs are under the
job's artifacts, `coasys-spaces/`.

Each step is a subcommand of `scripts/coasys-spaces.sh`. `bash
scripts/coasys-spaces.sh all` runs the whole job on a machine with Ollama and the
model (`COASYS_SPACES_TIER=stub` needs no model). `bash
scripts/coasys-spaces.test.sh` tests the script.

To run the workflow by hand on a branch, with another WE ref (the definition id
is in Project Settings → Pipelines):

```bash
curl -X POST https://circleci.com/api/v2/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4/pipeline/run \
  -H "Circle-Token: $CIRCLECI_TOKEN" -H 'content-type: application/json' \
  -d '{"definition_id": "<definition id>",
       "config": {"branch": "<branch>"}, "checkout": {"branch": "<branch>"},
       "parameters": {"run-nightly-llm": true, "we-ref": "<WE branch>"}}'
```

This runs all four nightly jobs.

### SFU wind tunnel

| Suite | Languages | CI |
|---|---|---|
| `test-sfu-e2e`: single executor, end to end (about 4 min) | local, no Holochain: `pnpm run prepare-test-local` (`test-main-sfu-e2e` does both) | every push, `integration-tests-sfu-e2e` |
| `test-sfu-browsers`: SFU and mesh calls in real Chromium, Firefox and WebKit, mixed (about 1 min past the world's start) | local, no Holochain: `pnpm run prepare-test-local` (`test-main-sfu-browsers` does both) | nightly, `integration-tests-sfu-browsers` |
| `test-sfu`: `sfu-cascade`, `sfu-session` (T20, T21), `sfu-mesh` (network baselines) | Holochain: `pnpm run prepare-test` | nightly, `integration-tests-sfu` |

- **Browser suite (`tests/js/tests/sfu-browsers.test.ts`, helpers in
  `tests/js/helpers/sfu/browser/`).** Each user calls from its own
  Playwright browser context, loading `core/lib/index.js` from a loopback
  page server, and joins through `createSession` as an app does. Per
  topology: Chromium with each browser, a Chromium + Firefox + WebKit call,
  and a Firefox tab lost mid-call that rejoins. Asserts come from getStats
  (packets and decoded frames growing), a Web Audio analyser (audible) and a
  canvas (not black; headless WebKit never plays a `<video>`, so not there).
  On the SFU, Chromium + Firefox also asserts the payload types differ per
  leg and each receiver gets its own numbers (#1684).
- The suite runs the executor's default SFU binding: loopback plus the
  LAN address, both offered. Firefox needs the LAN one: it gathers no
  loopback candidate and never pairs its LAN candidates with an SFU on
  127.0.0.1 only (ICE stays `checking`, the SFU's DTLS times out), while
  Chromium and WebKit connect. `SFU_BROWSERS_SFU_ADDR` binds the SFU there
  instead (`AD4M_SFU_BIND_ADDR`); `127.0.0.1` shows the loopback case.
- Install the browsers with `pnpm exec playwright-core install --with-deps
  chromium firefox webkit` in `tests/js`. On an Ubuntu release Playwright has
  no builds for (CI's runner is 25.04), it falls back to the 24.04 builds,
  whose WebKit needs ICU 74: set `PLAYWRIGHT_HOST_PLATFORM_OVERRIDE` to the
  build with the host's library versions (`debian13-x64` on 25.04), with its
  own `PLAYWRIGHT_BROWSERS_PATH`. A browser that cannot start makes
  its tests pending with the reason; `SFU_BROWSERS_REQUIRED=1` (CI) fails
  instead. Without root for WebKit's system libraries, extract the missing
  packages (`apt-get download`, `dpkg -x`) and list the `.so` files,
  dependencies first, in `WEBKIT_LD_PRELOAD`. `SFU_BROWSERS_DEBUG=1` dumps
  every tab's SDPs when a wait times out.
- Each test hangs up (`session.leave()`) before closing its tabs: a tab
  closed mid-call keeps its SFU slot until the server drops it, and a DID
  is in at most 4 calls per node.

- **E2E worlds use one executor without Holochain.** Users of one executor
  share a neighbourhood's perspective as its owners, so links, the call
  policy, presence and signals stay on the executor; the local link language
  (a fresh `uid` per neighbourhood) only gives each neighbourhood its own
  address. The world refuses to start on Holochain-prepared languages.
- **E2E scenarios (`tests/js/helpers/sfu/e2e/`) call only through the SDK.**
  Each participant is a managed user in its own Node process
  (`participant-child.ts`) running `createSession` on `@roamhq/wrtc`, as WE's
  adapter does. Test code must not use raw call RPCs, `ensureMembership` or
  `WebRtcPeer`; the first test in `sfu-e2e.test.ts` fails if it does (E11's
  outsider is the one exemption). The admin client only builds the world.
- Each user sends its own tone (`toneFor(i)`) and a video of known size, so
  receivers check *who* they hear and at which simulcast layer.
- E6 shapes the loopback with `tc netem`, only for its world's ports
  (`setNetemOnPorts`: executor API and SFU UDP), and skips without
  passwordless `sudo -n tc`. Never impair the whole of `lo` in a PR job:
  machine runners on one host share it, and every other job's localhost
  RPC times out (seen as `executeCommands`/`otherAgents` 30 s timeouts).
- E14 runs the SFU behind a simulated NAT: the executor binds 127.0.0.1 on a
  fixed port and advertises 127.0.0.2 (`executorEnv` in the world spec);
  `reachOnlyThrough` DNATs 127.0.0.2:P to 127.0.0.1:P and drops direct UDP
  to 127.0.0.1:P, for that port only. It asserts each caller's selected
  candidate pair (`Snapshot.selectedRemotes`, from WebRTC stats) is the
  advertised address. Skips without passwordless `sudo -n iptables -t nat`.
- E17 switches one participant's network: `socketPorts(pid)` reads the
  participant process's TCP and UDP sockets from `/proc`, and the lease
  drops all their traffic (netem 100 % loss, matched by protocol and port)
  while sockets opened later work. Skips without passwordless `sudo -n tc`.
  E18 makes the same cut, then fires `online` in the participant process
  (`Participant.networkChanged`). Both assert the call came back in place:
  u2 saw no departures, so no rebuild.
- A participant process killed by a signal nobody sent (the libwebrtc binding
  crashes rarely while closing) reruns the scenario once; an assertion failure
  never does.
- Set `E2E_DEBUG=1` to trace each description (connection, SDP version,
  DTLS role, refusals), tracks, sent simulcast layers, peer states and
  recoveries per participant, and world phase timings. `E2E_DEBUG=stack`
  adds the caller of each local description.

SFU behaviour the scenarios pin: one session per device. `sfu.callJoin`
names a `deviceId`; a join from the same DID and device (another tab)
replaces the earlier session, which gets `sfu-session-replaced` and stops
without rejoining, even mid-reconnect (E20); another device joins beside it as a participant of
its own (E22; at most `MAX_DEVICES_PER_DID` = 4 per room). Every call about
a session (`callLeave`, `callAnswerServerOffer`, `addIceCandidate`,
`callSetQualityPreference`, `callRestartIce`, `sendData`) names the `participantId` its join
returned, and acts only on that session of the caller's DID; the SDK sends
none of them without one, so a tab that never joined cannot end another
tab's call (E19, E21). The executor publishes `sfu-room-roster` (to the
listed DIDs, plus the departed session's DID on departure) after every join
and departure, listing sessions (`participantId` + `agentDid`), and the SDK
keys participants by participant id from it; when the roster stops listing
this session (its `participantId`) for 3 s, the SDK rebuilds the call, but a
replaced session never does; tracks are attributed only by the `trackMapping`
mids (which name
the participant id); a session never receives its own media or its own
`sfu-data` (`senderParticipantId`), but does receive its agent's other
device; room capacity comes from the
space's policy (`maxMeshParticipants × 4`, 128 without one), and cascade
per-node capacity only drives redirects; the server drops a peer only after
10 s of ICE `Disconnected`, or, once the client asked for an ICE restart
(`sfu.callRestartIce`), 10 s after the request or after the restart's answer;
on ICE `disconnected` or a network change the SDK restarts ICE in place (the
SFU stays the only offerer: the restart comes as a server renegotiation
offer), and rebuilds the call if that has not reconnected within 5 s; a
failed connection is rebuilt at once, and a rebuild retries the join for
120 s. The restart names the session's `participantId` (only that session
restarts, never another device of the agent), and a manager with no session
(join not back, left, replaced) sends none. A room roster that leaves this
session out for 3 s also rebuilds the call (the executor sends the departure
roster to the departed DID too); a roster that lists it, as during a restart
in place, changes nothing.
`ApiClient` drops an executor socket whose ping stays unanswered for a ping
interval (10 s, `PING_INTERVAL_MS`; longer while a large write it sent may
still be crossing a 16 kB/s link) and dials a new one, so a
socket a network switch left OPEN is noticed in 10 to 20 s. `online` redials
at once; `change` on `navigator.connection` and ICE `disconnected` run
`checkConnection`, which drops a socket silent for 2 s after its ping.

SFU media addresses (`rust-executor/src/sfu/{hosts,server,reachability}.rs`):
with no `sfu_bind_addr` (the default, whatever `--localhost` says) the SFU
binds two sockets on one port: 127.0.0.1 and the LAN address
(`detect_outbound_ip`, the default route's interface), and every answer
offers both as host candidates. Firefox needs the LAN one (it gathers no
loopback candidate); other machines on the LAN reach it too. In localhost
mode the plain RPC port stays on loopback: the SFU adds only its media UDP
port on the LAN, guarded by ICE credentials and DTLS (a `tls_config`
listener binds `0.0.0.0` on its own). The LAN address is detected again
for every answer and pipe: after a move to another network the next call
binds the new address on the same port (old sockets stay, at most 8, in case
the address comes back). A call in progress keeps the addresses it was
offered: a LAN pair whose address went away fails its ICE restart in place
and the SDK rebuilds the call, which gets the new address; a pair over
loopback carries on. No LAN (offline): answers offer loopback only, logged,
and each call looks again. One socket per address, never `0.0.0.0`: str0m
matches inbound STUN to a host candidate by destination, which a socket
bound to one address knows (the event loop passes the receiving socket's
address), and replies leave from the address the caller sent to. An
explicit `sfu_bind_addr` binds that address only, as before (malformed or
unspecified: loopback).

SFU behind NAT: the SFU binds on `sfu_udp_port` (0/unset: ephemeral; a fixed
port that is taken, on loopback or the LAN address, stops startup), runs the
STUN probe on the LAN (or explicit) socket before the event loop owns it,
and `assess` decides the advertised address (`sfu_public_addr`, else the
probe's reflexive IP on the fixed port) and the reachability (`public` once
STUN sees the advertised IP). Answers carry the host candidates and, when
advertised, a srflx candidate whose base is the LAN (or explicit) socket.
Cascade pipes offer the host candidates only. The public address is read at
startup: an IP change needs a restart. The four `sfu_*` keys live in the
config file too; a build without them refuses them as unknown keys.

Mesh signalling rules the scenarios pin (`core/src/neighbourhood/MeshManager.ts`):
offers are numbered per connection and answers name theirs, so a late answer
or an overtaken offer is dropped; `onnegotiationneeded` only ever offers and
the answering path only ever answers (an implicit description picked from the
state swaps them during a rollback); the answerer holds its own first offer
until the pair connects; `join` waits for the channel to listen before the
session announces presence; the buffer for not-yet-listed peers never drops a
description.

### AI wind tunnel

`pnpm run ai-wind-tunnel` (in `tests/js`) runs every cell of providers (Ollama,
llama-server, vLLM, whisper-server, speaches) × services (llm, completion,
embeddings, stt, tts) for real through a fresh executor, verifies each answer,
measures it and writes `ai-wind-tunnel/evidence/<class>/<date>-<id>.json`;
`pnpm run ai-wind-tunnel-recommend` derives `ai-wind-tunnel/recommendations.json`
from all evidence. Not in any CI job (it needs real models); the unit tests
(`pnpm run test-ai-wind-tunnel-unit`) are cheap. See `ai-wind-tunnel/README.md`.

- Every measured call goes through the executor as an app's would: endpoints
  and models registered with `ai.addEndpoint`/`ai.addModel` (Ollama models
  pulled with `ai.pullModel`), requests over the SDK and `/v1`. Direct calls
  to the provider only give the executor's overhead.
- A cell is `pass`, `fail` (with the failed check), `unsupported` (the provider
  cannot serve it: `ai-wind-tunnel/matrix.json`) or `skipped` (not available on
  this machine). Never record timing without the checks.
- Shared data the executor will read: the runtime/model manifest at
  `rust-executor/src/ai_service/local/manifest.json`, the hardware-class rules
  `ai-wind-tunnel/hardware-classes.json` with their fixtures
  (`hardware-class-fixtures.json`, which a Rust classifier must pass too), and
  `recommendations.json` (schema v1). Change their meaning only with a schema
  version bump.
- Recommendations cite evidence; a class without evidence has none. Never edit
  `recommendations.json` by hand: add evidence and regenerate (a unit test
  fails when it is stale).
- The run binds free ports on 127.0.0.1 only, never the `AI_WT_RESERVED_PORTS`,
  stops only processes it started and removes only `ai-wt-<run id>-*`
  containers. ts-mocha needs Node 24 (it fails to load under Node 26).

### Port Conflicts

Sometimes an old `ad4m-executor` binary is still running from a previous test run, causing port conflicts. Before running tests, kill any lingering processes:

```bash
pkill -9 ad4m-executor
```

### Rebuild Requirements

The integration tests use the `ad4m-executor` CLI binary. Depending on what code was changed, different rebuilds are required:

| What Changed | Required Rebuild |
|--------------|------------------|
| Rust code in `cli/` | `cargo build --release` in `cli/` |
| Rust code in `rust-executor/` | `cargo build --release` in `cli/` |
| Deno JS (`js_core/*.js`, `*_extension.js`) | `pnpm build` in `rust-executor/` (rebuilds the Deno snapshot) |

**Deno Snapshot**: Anything that changes the content of the Deno JS engine at startup (language bootstrap, `ad4m:host`, or `#[op2]` extension `.js` files) requires rebuilding the snapshot. `pnpm build` in `rust-executor/` does that; `cargo build --release` in `cli/` does not.

## bootstrap-languages/*/esbuild.ts: the `@coasys/ad4m-ldk` relative path

Every `bootstrap-languages/*/esbuild.ts` resolves `@coasys/ad4m-ldk` via a
hardcoded relative path from the language's own directory. Inside this monorepo
that path must be `../../ad4m-ldk/js/lib/index.js` (two levels up:
`bootstrap-languages/<lang>/` → `bootstrap-languages/` → repo root →
`ad4m-ldk/js/lib/index.js`) — this convention applies to all bootstrap-languages.
Verify with:

```bash
grep -n "ad4m-ldk/js/lib" bootstrap-languages/*/esbuild.ts
```

If you copy/scaffold a language from a **standalone repo** (one developed as
a sibling checkout next to `ad4m/`, e.g. via `ad4m-link-language-template`),
its `esbuild.ts` and `tsconfig.json` `paths` will default to a sibling-repo
path like `../ad4m/ad4m-ldk/js/lib/index.js` instead — that resolves to a
nonexistent location once the language lives inside the monorepo and must be
repointed to the `../../ad4m-ldk/...` form in both files before `build`/
`typecheck` will work. (`bootstrap-languages/server-link-language` needed
this fix when imported from its standalone repo.)

## link-server and server-link-language

`link-server/` (self-hosted Fastify/SQLite link-persistence server) and
`bootstrap-languages/server-link-language/` (the AD4M link language that
syncs through it) were imported from standalone repos as an alternative to
the default Holochain-based `p-diff-sync` link language — see the README's
"Link languages: Holochain or self-hosted" section. Each has its own
AGENTS.md with build/test commands and architecture notes.
