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
- **Tests wait for a signal, never for time.** Do not put a fixed sleep before
  an assertion or before the action that a test triggers. A sleep costs its
  whole length on every run, and on a slow CI machine it can still be too
  short. Wait for the signal that the state is ready instead:
  - an event, a callback, or the reply of the call that set the state up
    (the `events.watch` reply; a subscribe reply, because the executor
    registers the subscription before it computes the first result);
  - when no signal exists, a poll of the condition at a short interval (about
    100 ms or less). A timeout is only the bound at which the test fails;
  - to prove that something did not happen, a sentinel: after the action,
    cause an event on the same channel that must arrive, wait for it, then
    check that the other event did not arrive. This holds only where that
    channel keeps the order of its events;
  - for code that waits on time itself (a deadline, a debounce, a stale
    bound), a test override for that duration, a paused tokio clock
    (`#[tokio::test(start_paused = true)]`) or fake timers (`jest.useFakeTimers`, `vi.useFakeTimers`).
    Do not pause the tokio clock in a test that talks to a real language
    runtime thread: that thread has its own clock, and the test's clock
    advances while it waits on the thread.
- `connect/` tests (vitest + happy-dom) fail under Node 26 (`localStorage.clear`
  undefined); run them under Node 24.

## Client data mirror (`core/src/mirror/`)

Each `ApiClient` owns one `Mirror` (`apiClient.mirror`): one `Entry` per data
point the SDK reads from the executor, keyed by scope (`executor | agent |
{perspective} | {neighbourhood}`) + RPC name + canonical params. Two clients
share nothing. Views read an entry through `Live<T>` (`getSnapshot`,
`subscribe`, `read`).

- States: `unknown | loading | fresh | stale | error`. A read of an unknown,
  stale or failed entry sends one request; concurrent reads share it. A value
  push (`entry.push`) replaces the value and notifies unless it is equal; an
  invalidation or the
  `lagged` marker (`{"type":"query-subscription-update","lagged":true}`; the socket
  is up) calls `invalidate`: the entry is stale and reloads at once only if it
  has subscribers. One rule for the absence of a socket: the loss of the socket
  that had opened (`ApiClient._onClose`), a refused token (`ApiClient._failAuth`:
  close code 1008 or a 401, after which no reconnect comes) and `closeAll()`
  (`Mirror.reset`) mark every value stale and send **no request**
  (`markAllStale(false)`, `invalidate(false)`, `markStale(false)`). Every socket
  open (`ApiClient._onOpen`, a reused client's first open included) then calls
  `Mirror.reloadWatched()`: each value with a subscriber that is stale or failed (a read
  made while the socket was down ends in `error`) loads once; fresh values and reads in
  flight are left alone. A reset (`closeAll()`)
  or a refusal that a subscriber makes while the loss is announced (or while a socket-open
  reload, or the stored value of a `persistentStale` key, is being shown) stops the reads the
  announcement had queued, and `_onClose` then asks for no reconnect. So a watched value never dials a
  socket by itself, and a reconnect reads each watched value once, after the
  socket is back. A read the app started before `closeAll()` and that a keyed
  cache had only queued (the tick's batch) can still dial once after it. A refused
  token (`Mirror.refuse`) also makes the expression cache stale and blocks its
  persistent tier, so after it every read fails with 401: a cache the mirror creates
  after the refusal is refused from the start, and a call made while the refusal
  notifies (`_authFailed` is set while the socket is still open) rejects with 401
  without a frame. `markAllStale` and `refuse` mark every entry, cache key and live
  result before they tell any subscriber (two phases), so a subscriber told first
  cannot read another value still shown as fresh. Until the
  1008 close arrives, a client whose token was revoked still answers from its
  stored values.
- An answer is kept only if nothing changed the entry since its request went
  out: one sent before a reconnect reaches its readers once and is dropped. A
  failure is never kept.
- What keeps an entry fresh is written in its spec, not in a kind: the events
  it listens to (`on`) and its `load`. An `Entry` is keyed by its RPC name
  (`mirror.entry({ method, load, on })`: one entry per method per client); the
  perspective uuid in a key belongs only to live queries and one-shot reads
  (`live`, `liveResult`, `oneShot`, `wrote`). The loss of the socket, `closeAll()` or `lagged` makes every
  entry stale, and every keyed cache except the expressions (`staleOnReconnect: false`). Entries (see the sections below and the code for each one's events):
  - `agent.status`: pushed by `agent-status-changed` and the results of
    `agent.unlock/lock/generate/import`; read through `client.agent.status()` and
    `client.agent.did()`; a status with no DID is not kept.
  - `sfu.status`: fixed for an executor process (`client.runtime.sfuStatus()`); a
    failure is not kept.
  - `agent.get`, `hosting.info` and `perspective.all`: below.
  `NeighbourhoodProxy.createSession` reads `agent.status` and `sfu.status` together;
  `PerspectiveClient.getNeighbourhoodProxy(uuid)` returns one proxy per uuid, and
  a `perspective-removed` event or `remove()` drops that uuid's proxy.
- A mounted view (a `Live` with a subscriber, a hook) reloads on an executor
  event that changes the value, on a reconnect or `lagged` marker, on a
  remount, or on a read. It does not reload on the age of a keyed value
  (`REMOTE_AGENT_TTL_MS`, `EXPRESSION_TTL_MS`) by itself: an aged value is old
  and no one is told, so the next read or subscribe loads it. There is no expiry
  timer.
- A keyed cache answers `null` for an unknown key and does not keep it (the next
  `get()` asks again), but a key with a subscriber shows it as `{ state: 'fresh',
  value: null }`. Its maps hold only keys with a value, a read in flight or a
  subscriber (a failed read's error is kept only while a view watches the key, even when the key has a value, until
  its next read, a reconnect, or the last subscriber leaving); a read that finds
  nothing for a key that had a value drops the value and its persisted copy;
  `storedUsed` follows the held values.
- Profiles and expressions. The own agent (`agent.get`, read through
  `client.agent.me()` / `client.agent.live.me()`) is pushed: `agent-updated`
  for the own DID (while the own DID is not known yet, the event only makes the entry
  stale: a read in flight is used once and not kept) and the result of `updatePublicPerspective` /
  `updateDirectMessageLanguage` (read-your-writes) replace it; a write's reply is dropped when
  an event or an invalidation changed the entry since the write went out. `agent-status-changed`
  with another `identity` makes the held agent stale, and an agent pushed when the entry is not
  fresh (it carries no identity) makes it stale instead of keeping a revoked one. Other agents
  (`agent.byDIDs`, `client.agent.byDID(did)` / `live.byDID(did)`) are
  keyed: fresh for `AgentClient.REMOTE_AGENT_TTL_MS` (5 min), replaced at
  once by `agent-updated`, stale after a reconnect or `lagged`; bounded to
  `MAX_MEM_ENTRIES` (200) and `MAX_MEM_BYTES` (5 MB) in memory. Expressions
  (`expression.get`) are keyed too, with `staleOnReconnect: false`: bounded to
  200 entries / 5 MB, fresh for `ExpressionClient.EXPRESSION_TTL_MS` (5 min) in
  memory and in the persistent tier, kept through a reconnect and `lagged`,
  dropped by that age, `clearCache()` or an `interact`.
- The perspective list (`perspective.all`, `client.perspective.all()` /
  `client.perspective.live.all()`) is pushed: `perspective-added`,
  `-updated` and `-removed` change it, and so do the handles an own write
  returns (`add`, `update`, `remove`, `neighbourhood.joinFromUrl`), with no
  read. An event or write on a list that is not fresh makes it stale instead
  (no sound base to patch). An own `update` compares the list's handle before and after the
  call; with no fresh list before it, the reply is not applied and the list is made stale
  (a perspective removed meanwhile does not come back). The hosting account (`hosting.info`,
  `client.agent.hostingUserInfo()` / `client.agent.live.hostingUserInfo()`) is
  pushed: `hosting-user-info-changed` for its own email replaces it, an event
  for another email (an operator hears every user's) and `setHotWalletAddress`
  make it stale. `connect` reads it, so it no longer polls. `agent.getApps` is
  not mirrored: `apps-changed` is sent only when a grant is minted and carries
  one app; `removeApp` and `revokeToken` send nothing, so no push source says
  when the list is stale.
- Keyed values (profiles, expressions) live in a `KeyedCache` the mirror owns, one
  per RPC name (`mirror.cache({method, staleOnReconnect?, options, on?})`; the first
  call's spec wins), so two clients share nothing. It has its own per-key state (same
  states and rules as `Entry`) and adds tick batching through `loadMany` (50 `byDID`
  misses in one tick = one `agent.byDIDs`; the per-key retry applies only to an `RpcError`
  500 with more than one DID, any other failure rejects the whole shared batch; a failure is
  never kept), `maxAgeMs`, the memory limits and the persistent tier. The limits never
  evict a key a view is subscribed to (its view would go blank): while views hold them,
  watched keys may take a cache past its limits.
  `mirror.persistent(db, store)` is the IndexedDB tier, keyed `origin|acting DID|key`, where the
  origin includes the executor's path (two executors on one host share nothing); the DID
  is the one the client already knows (the status entry, never a request), so a cold read
  waits on nothing, and while it is unknown nothing is stored or read (Node has none).
  `clear()` removes only the `origin|DID|` prefix of this client. A profile loaded from the
  tier is shown `stale` and read again once per session (`persistentStale`); an expression
  is used as fresh while younger than `persistentMaxAgeMs`. `markAllStale()` skips caches
  with `staleOnReconnect: false`.
- The Mirror registers an entry's events itself: the spec's `on` map (event name to
  `(event, entry) => void`) is registered once, when the Mirror creates the entry. The
  mirror registers them without dialling. `ApiClient.closeAll()` clears the handlers and
  calls `mirror.reset()`: every value is stale, the pooled live subscriptions end, and the
  handlers are registered again, so a client used after `closeAll()` reads from the executor
  and hears events (the lazy `Entry` fields of `AgentClient` stay valid: entries are kept).
  The expressions survive `closeAll()` (`staleOnReconnect: false`): only their age, `clearCache()`,
  an `interact` or a refused token expires them.
- Live queries (`core/src/mirror/live.ts`). `mirror.live` keeps one
  `LiveSubscription` per perspective + RPC + canonical params; each caller gets its own
  `LiveQuery` view (own `first`, own projection, own listeners; `dispose()` releases only that
  view). The subscription holds the executor's raw result; each view applies its own `project`
  (model hydration) to it, so views with one key and different projections share one executor
  subscription. A `project` that throws fails only its view: before the view had data its
  status is `error` and its `first` rejects; after, the view keeps its old data and logs
  'Live query result could not be read:'. A view's `first` is the current result when the view opens,
  and that result is the view's `initial` one (a view that joins an open subscription is not
  handed the current result as a change). After
  the last view is released the subscription stays open `keepAliveMs` (default 30000,
  `LiveOptions`), then is disposed; a view in the grace answers at once with no request.
  `LiveSubscription` resyncs itself on `lagged` and reconnect. The mirror marks the live
  results stale when the socket is lost (`ApiClient._onClose`), on `lagged` and on reconnect
  (`markAllStale`): snapshots keep the last value for display, but one-shot reads go to the
  executor until the resync delivers a result.
  `PerspectiveClient.modelQuery` / `querySparql` answer from a ready live handle with the
  same key (no `after` cursor). After an own write (`mirror.wrote`) they ask the executor
  until an update changes the result, because the executor sends updates 250 ms or more
  after the write's reply. A write made while a result is on its way (a resync, an open) also
  keeps them closed past the updates already buffered behind it: those came before the write,
  so the updates replayed behind a resync's result do not count in `changes`. A loss notice
  (`stale()`) never lowers a write's barrier. A view that opens after a write whose update has
  not come does not join the pooled subscription (its result may be from before the write, so
  its `first` would be old): the mirror opens a new one, and its first result is that view's
  `initial` one; the old one ends at once if no view holds it, else when its last view is
  released (after the grace) or when the client is reset, whichever comes first. While a resync's result and its
  buffered updates are applied, the result answers nothing. A live SPARQL result is a multiset: row order can differ from a
  fresh query with no `ORDER BY`. The Waker keeps its own unshared subscription.
  The write counter lives in `Mirror.wrote(scope)` only. Its cost: a write that does not
  change a query's result sends every later one-shot of that query to the executor until
  an update changes the result. The same counter keys `mirror.oneShot`: identical
  one-shot requests (scope, RPC, canonical params, no `options`) in flight share one
  request, and a call made after a write does not join one sent before it. Nothing is kept
  after it settles.
- The hooks (`ad4m-hooks` react and vue `useMe`, `useAgent`, `usePerspectives`) subscribe to these
  `Live` views; they keep no state of their own.
- A test that reuses one client across tests must not expect a fresh read:
  the mirror keeps the last status. Use a new client.

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

A pipeline with `run-nightly-llm: true` runs only this workflow, on `coasys/marvin`.
Each job has its own serial group. The jobs: `llm-e2e`,
`integration-tests-sfu`, `integration-tests-sfu-browsers` (see below),
`coasys-spaces`, `integration-tests-agent-harness` and `feed-scale`.

`llm-e2e` runs the ignored Rust interpretation suites and the JS suites gated
by `LLM_E2E=1`. It does not start a model server. It prompts staging's
llama-server at `http://127.0.0.1:12460/v1` (alias `qwen3.8-27b-ud-q4_k_m`,
kind `LLAMA_SERVER`, context budget 57344 under the server's 65536-token
slot) and embeds with Ollama's `nomic-embed-text` on `:11434`. A preflight
checks `/health` and that `/v1/models` lists the alias, and fails the job
when the unit is down. Requests share that one slot with staging's own AI
processing.

`coasys-spaces` runs WE's product suite (`pnpm --filter @we/app-web test:spaces`,
WE `apps/we-web/tests/live/coasys-spaces/`): a Coasys space, a call, its
transcript (executor Whisper), extraction on staging's llama-server
(`http://127.0.0.1:12460/v1`, model `qwen3.8-27b-ud-q4_k_m`), and flows.
That server is already running (`llama-qwen27b.service`, one 98304-token slot)
and already holds the GPU. The job does not start it and does not load a model.
`COASYS_SPACES_LLM_NUM_CTX` is empty, so `llm-check` does not make a context
variant. `/api/create` runs only when the server is Ollama (`/api/version`
returns a version) and `COASYS_SPACES_LLM_NUM_CTX` is set: a hand run against
Ollama can still make a variant such as `coasys-spaces-gemma3-12b:ctx16384`
when the model's own `num_ctx` is smaller. A model whose own `num_ctx` is
already that large is used as it is. The job also sets
`COASYS_SPACES_LLM_ENDPOINT_KIND=LLAMA_SERVER`,
`COASYS_SPACES_LLM_NATIVE_TOOLS=true`,
`COASYS_SPACES_LLM_REQUEST_TIMEOUT_SECS=900` and
`COASYS_SPACES_LLM_MAX_NUM_CTX=90112`. The script passes those through;
WE's real tier (we#468) registers them on the endpoint. The 900s limit
includes time queued on the one slot. It uses this commit's executor and SDK:

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
`nightly-llm` with `llm-e2e`, so the two do not run at once: both prompt
staging's llama-server on :12460, and `llm-e2e`'s embeddings stay on Ollama.
A completion in this job may wait a few minutes on llama-server's one
slot, behind staging. Screenshots, traces and logs are under the
job's artifacts, `coasys-spaces/`.

Each step is a subcommand of `scripts/coasys-spaces.sh`. `bash
scripts/coasys-spaces.sh all` runs the whole job. The real tier needs the model
server named by `COASYS_SPACES_LLM_*` (the nightly job uses staging's
llama-server). `COASYS_SPACES_TIER=stub` needs no model. `bash
scripts/coasys-spaces.test.sh` tests the script.

`feed-scale` measures WE's Feed in a space with a million messages
(`pnpm --filter @we/app-web test:feed-scale`, WE `apps/we-web/e2e/feed-scale/`).
It needs no model and no GPU, and has its own serial group, `nightly-feed-scale`.

- The suite seeds `FEED_SCALE_MESSAGES` (1000000) TextBlock messages, 12 links each.
  It generates every link already signed by the message's author (one of 40 seed
  agents; the Feed shows its author and time) into one file, on all cores, and loads the
  file into the space's perspective with `runtime.importData` (`type: "perspective"`,
  Oxigraph's bulk loader, one commit; nothing goes to the link language). The import
  only adds links new to the perspective: it refuses the whole file (a 400) if a link is
  already stored, repeats in the file, or is ended by a tombstone in the file (a tombstone
  that ends a link of the file, or a link the store holds, refuses it too). A link that a
  tombstone already in the store ended is not loaded and counts as `covered`; the same
  link twice in the file is refused. The import takes the perspective's `persist_lock`,
  so it lands before or after a diff's write, never between. Then WE web opens the
  space in Chromium five times (cold, then warm) and loads the newest page and one
  older page. The links file (~6 GB at a million) lives in the data dir, beside the
  store.
- WE registers an auto-processor for the space's own collection when the space opens
  (`we-call-<collection>`, intended behaviour). The suite removes it for the seeding
  only, which is setup. The app registers it again for the loads, and the job measures
  its cost: `executor.cpu_seconds.ui_phase` (the executor's CPU over the loads) and
  `executor.idle_cpu_percent.after_ui` (idle with the watch on). A fix to the
  auto-processor's tick shows in these two.
- It writes `metrics.json` (metrics in the performance-history record format, job
  `feed-scale`, params `{"messages": N}`; the failure counts and
  `executor.cpu_seconds.ui_phase` add `"loads": L`, the number of loads, so a run with
  another number of loads makes its own series) and `results.json` (the same, with the
  per-call detail). The `perf` step gives `metrics.json` to `scripts/perf/record`,
  draws the trend page with `scripts/perf/report`, and for a passed run asks
  `scripts/perf/check` for the verdict, by the `feed-scale` rules in
  `scripts/perf/thresholds.json`. The records and the trend page are in the artifacts
  under `feed-scale/perf/`.
- The executor, SDK, WE and we-web steps are `coasys-spaces.sh`'s, run in
  `~/.cache/ad4m-ci/feed-scale`. The executor's data goes there too, not to /tmp
  (a shared tmpfs): a million messages take tens of GB.
- It halts green when the same ad4m commit, WE commit and size were already measured
  (`~/.local/state/ad4m-ci/feed-scale/last-success`; `FEED_SCALE_FORCE=1` runs anyway).
  Results, logs and failure screenshots are under the job's artifacts, `feed-scale/`. The
  job log ends with the suite's summary line.

`bash scripts/feed-scale.sh all` runs the whole job by hand
(`FEED_SCALE_MESSAGES=10000` for a short run). `bash scripts/feed-scale.test.sh` tests the
script.

To run the workflow by hand on a branch, with another WE ref (the definition id
is in Project Settings → Pipelines):

```bash
curl -X POST https://circleci.com/api/v2/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4/pipeline/run \
  -H "Circle-Token: $CIRCLECI_TOKEN" -H 'content-type: application/json' \
  -d '{"definition_id": "<definition id>",
       "config": {"branch": "<branch>"}, "checkout": {"branch": "<branch>"},
       "parameters": {"run-nightly-llm": true, "we-ref": "<WE branch>"}}'
```

This runs all six nightly jobs.

### Nightly performance history (`scripts/perf/`)

`integration-tests-sfu` and `llm-e2e` record their numbers on every run (passed
or failed), fail on a serious regression against their own history, and draw a
trend page. The full contract is the header of `scripts/perf/perf.mjs`.

- **The history is a git repository**: folder `nightly/` on branch `main` of
  `coasys/ad4m-wind-tunnel`. `nightly/<job>.jsonl` holds one line per metric per
  run, `nightly/<job>.checks.jsonl` one verdict per check. Both are append-only.
  `record` commits `perf(<job>): <sha9> <run_at>` in a fresh shallow clone
  (`$TMPDIR/ad4m-perf-history-<job>-*`, removed when the command ends), and
  pushes. When another run pushed first, it fetches, puts its lines on the new
  head and pushes again, up to 5 times. git runs with hooks off and its own ssh
  command (no `~/.ssh/config`): the runner also runs PR jobs, so nothing of the
  history, and not the key, stays on it. `scripts/perf/history-key` moves the
  key out of `~/.ssh` (`perf_history_key`) and deletes it, by fixed paths, with
  any clone left (`perf_history_cleanup`, the end of `perf_finish`). A job with
  its own record step calls those two commands. Env: `PERF_HISTORY_REPO`, `PERF_HISTORY_BRANCH`,
  `PERF_HISTORY_SUBDIR`, `PERF_HISTORY_SSH_KEY`.
- **The default repository is written only when `CI=true`.** For a local run,
  set `PERF_HISTORY_REPO` to a local bare repo. Never push test runs to the
  real history.
- **The push key.** CI pushes with a write deploy key on the wind-tunnel repo.
  Add it in Project Settings → SSH Keys → Additional SSH Keys (hostname
  `github.com`), and put its fingerprint in the pipeline parameter
  `perf-history-key-fingerprint`. If the key is missing or a push fails, the job
  does not fail. `record` prints `history not pushed: <reason>`, and the records
  stay in the job's artifacts (`perf/records.jsonl`). A `check` that cannot read
  the history only reports.
- **`check`** fails the job when a value is worse than 1.5× the median of the
  last 7 passing runs with the same job, metric and params. Per-metric limits
  (factor, min_delta, an absolute ceiling or floor) live only in
  `scripts/perf/thresholds.json`; a ceiling or floor fails at any history. With fewer than 3 earlier
  runs it only reports. Values with different `params` (such as another LLM
  model) never compare. A regressed run stays out of later baselines.
  `scripts/perf/check --job J --accept <run_id>` takes it as the new normal: it
  resets the baseline, so later runs compare only with it and the runs after it.
- **What each job measures.**
  - `integration-tests-sfu`: every scenario writes `$PERF_RESULTS_DIR/sfu-<id>.json`
    (`tests/js/helpers/sfu/perf.ts`), and the step adds `sfu.suite.duration_ms`.
  - `llm-e2e`: raw logs go to `$PERF_RESULTS_DIR/raw`:
    - the Rust tests through `scripts/perf/libtest-timer.mjs`;
    - mocha through `PERF_MOCHA_LOG` (`tests/js/utils/perfMochaHooks.ts`);
    - each step's output as `<suite>.log`, where the executor logs every prompt
      (`✅ 🤖 prompt done ... latency=…ms tokens_out=…` at info, `❌ 🤖 prompt
      failed` at error, `rust_executor::ai_service`). The Rust step sets
      `RUST_LOG=warn,rust_executor::ai_service=info` (its tests default to warn).

    `scripts/perf/llm-e2e-results.mjs` turns them into metrics (calls, failures,
    latency, output tokens/s), with params `{model}`.
- A new nightly measurement writes a results JSON file (`{"metrics": [...]}`)
  into `$PERF_RESULTS_DIR`. Wrap the job's tests in `perf_setup` and
  `perf_finish` (`.circleci/config.yml`).
- **Tests**: run `pnpm run test-perf-unit` in `tests/js`. It runs mocha for
  `sfu-perf.unit.test.ts` and `node --test 'scripts/perf/*.test.mjs'`. The git
  tests push only to bare repos in a temp directory. The `js-unit-tests` CI job
  runs them.

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
- The page keeps one muted `<video>` per session (`participantId`), as an
  app keeps one tile per participant, and removes it when the session
  leaves the list. Never one per agent DID: an agent listed twice (two
  devices, or a lost tab beside its successor) then swaps two streams on
  one element at every render, and headless WebKit's tab hangs or crashes
  (`Target crashed`). A tab that crashes or does not report within 30 s
  fails with its user, browser and last console errors.
- On a host with no audio server (CI's runner: no PulseAudio or PipeWire),
  Firefox keeps every `AudioContext` `suspended` (`currentTime` 0) and its
  analyser reads silence, though the audio arrives and decodes. No Firefox
  pref fixes it (`media.cubeb.force_null_context` makes any host silent;
  `media.cubeb.backend` does not help). So where the page's context does not
  run, `audioPeak` is the receiver's `inbound-rtp` `audioLevel` instead
  (`audioMeter: "rtp-stats"`, shown in the failure). To reproduce on a
  desktop, run the suite with `XDG_RUNTIME_DIR` set to an empty directory
  and `PULSE_SERVER=unix:/nonexistent PIPEWIRE_REMOTE=/nonexistent`.
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
  One exception: a `WorldSpec` user with `main: true` is the executor's main
  agent, called with the operator credential, still only through the SDK.
  E23 uses it, since only the main agent's own devices show how the executor
  routes the main agent's broadcasts.
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
- `@roamhq/wrtc` fires `iceconnectionstatechange` when libwebrtc's legacy
  ICE state changes, but `iceConnectionState` returns the standardized
  state, which libwebrtc changes just after. On a loaded host a listener
  reads the old state, and no event comes for the new one: E17's cut read
  "completed", the SDK did not restart ICE, and the call stayed silent.
  `specPeerConnection` (`e2e/webrtc.ts`) looks at the state again 10 ms,
  100 ms and 1 s after each event and fires one more event when it changed.
  A browser fires the event for the state it reports. Do not take this out,
  and do not read a state change from the binding's event alone.
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
and departure, listing sessions (`participantId` + `agentDid`) and a
`revision` (one sequence for every roster this process publishes: 1 the
first time, then one more on every publish, a resume included; a later
room, and a room created again, continues it, so a roster from an earlier
life of the room is older), so a client can drop a roster whose
`revision` is older than one it already applied. The SDK
keys participants by participant id from it; when the roster stops listing
this session (its `participantId`) for 3 s and `sfu.callResume` does not list
it either, the SDK rebuilds the call, but a replaced session never does; tracks are attributed only by the `trackMapping`
mids (which name
the participant id); a session never receives its own media or its own
`sfu-data` (`senderParticipantId`), but does receive its agent's other
device; room capacity comes from the
space's policy (`maxMeshParticipants × 4`, 128 without one), and cascade
per-node capacity only drives redirects; the server drops a peer after
10 s of ICE `Disconnected`, or, once the client asked for an ICE restart
(`sfu.callRestartIce`), 15 s after the request. The restart's answer ends
that grace and the 10 s disconnect clock starts again. An unanswered server
offer is resent at 10 s and 30 s, then every 30 s, and never drops a client
peer (#1802 dropped it at 70 s, also while its media flowed). A client keeps
that offer. An established pipe whose answer str0m rejects locally, or
whose offer is still unanswered after those two resends, is disconnected.
That rejection is local: a pipe answer carries no refusal, and a node that
rejects an incoming offer only logs it, so the sender waits until the offer
goes unanswered. The other end drops the pipe when its ICE fails. The end
that dials (the lexically higher DID) redials at once the first time,
whether it ended the offer or only saw the drop. The next consecutive
failure waits 30 s, then 60 s, then 120 s, and stays at 120 s. A pipe that
stayed connected for 120 s clears that count, so the next drop is free
again. A timeout, a pipe that never connected, does not wait. When the new
pipe is usable it is offered the origins already in the room, so media resumes
without a new track. An Announce arrives every 10 s, so the announce alone
would dial again on the next heartbeat. A crossed offer (`ChangesOutOfOrder`)
keeps the pipe and queues the tracks again. An initial pipe that never connects stays
through the second resend plus the 15 s ICE-restart grace, then is dropped.
The offer carries
an `offerId` that a resend and `sfu.callResume` repeat; the answer echoes
it. `ParticipantId` (`P{n}`) and that offer id (`o{n}`) are minted from a
counter that starts, once per process, at a random value at least `2^63`,
so a restarted process does not hand a client's old id to a new session. `sfu.callAnswerServerOffer` answers `{ accepted: true }`, or
`{ accepted: false, reason }` (`stale`, `no-pending`, `unknown-session`),
after the event loop applies the answer. A repeat of the id accepted last
for that session is `{ accepted: true }` and changes nothing; an older
accepted id is `stale`. `stale` also covers an answer that does not parse,
or whose mids are not the pending offer's, under the pending offer's own
id: that offer stays pending. A refused answer is that value, not an RPC
error. The RPC errors are 403 (a session of another agent) and 408 (the
event loop did not answer, or its command channel is closed).
`sfu.callResume` (the session's
`participantId`) sends the pending offer and the roster again at once, for
a client whose socket came back, and answers `{ listed: true, offer?,
roster: { revision, participants } }` with the same offer, the same sessions,
and the same `revision` as the roster event that resume just published, or
`{ listed: false }` when the room or the session is not there. A session of
another agent is 403. `sfu.callAnswerServerOffer` and `sfu.callResume` are
408 when they have not finished within 5 s. That wait starts before the
rooms lock, so a join that holds the lock counts, and it covers the resume
roster publish as well as the command waiting for a slot in the bounded queue.
on ICE `disconnected` or a network change the SDK restarts ICE in place (the
SFU stays the only offerer: the restart comes as a server renegotiation
offer), and rebuilds the call if that has not reconnected within 15 s; after
a socket reconnect with no network change it restarts ICE only if ICE is not
connected 2 s later; a failed connection is rebuilt at once, a rebuild
retries the join for 120 s, and either terminal give-up — the 120 s spent
without a rejoin, or a 4th rebuild inside 2 min — ends the call, in this
order: `participant-left` for each participant, then `error`, then the
connection closes, the `sfu.callLeave` request starts (the end does not wait
for its reply) and `call-ended` (`{ reason: "gave-up" }`, also on the
`Session`, which goes idle) is emitted for the UI to show; a new `join`
starts over. A rebuild keeps the participants and clears only their media:
it refreshes `hasAudio` and `hasVideo`, then emits one `stream-removed` per
track, and a track that ends later does not emit a second one. Only an SDP the connection
cannot apply rebuilds from a renegotiation: the answer names its offer's
`offerId`, and the server refuses one naming another — `stale` or
`no-pending` (a settled or superseded offer, or the server's own SDP step
refusing it, as `ChangesOutOfOrder`) is non-acceptance: repair is left to
media and session evidence, not a rebuild; `unknown-session` rebuilds the
call. The client keeps its last answer (`lastAnswer`) and resends
nothing by itself: an answer a socket fault lost (503, 408) goes out
again from the cache when the executor resends its offer or the resume
reply carries it. Once the socket is back, `sfu.callResume` returns what went by — the
session's pending offer and the room's roster, applied like the events
that carry them; a resume the socket fails keeps the call, and a 403 ends
it (`call-ended`, `{ reason: "refused" }`: the executor refused the
resume — the session is another agent's, or this agent is no longer a
member). Of the server offers waiting, only the newest
is applied; no RPC runs on the offer queue (an answer is sent without
waiting for its reply) and SDP steps run to their end; a resume's ask
waits off the queue, bounded by `RESUME_STEP_TIMEOUT_MS` (10 s) and
by the restart's remaining window when one is in flight, so the restart
offer it triggers is answered while the reply is still out. The restart names the session's `participantId` (only that session
restarts, never another device of the agent), and a manager with no session
(join not back, left, replaced) sends none. A room roster that leaves this
session out for 3 s asks `sfu.callResume` and rebuilds the call when its
reply says the session is not listed — an ask the socket fails is asked
again within the grace while the roster still omits it; a roster that
lists it, as during a restart in place, changes nothing.
`ApiClient` drops an executor socket whose ping stays unanswered for a ping
interval (10 s, `PING_INTERVAL_MS`; longer while a large write it sent may
still be crossing a 16 kB/s link) and dials a new one, so a
socket a network switch left OPEN is noticed in 10 to 20 s. `onNetworkChange`
fires only for platform network events: `online` redials at once and tells
it once the new socket is open, and `change` on `navigator.connection`
marks a change — told once an open socket is proven — only when its
`type` or `navigator.onLine` moved: the local path changed, so media must
restart. On a platform that reports no `type` (desktop Chromium) a `change`
with the reported state unmoved only has the socket judged — a real
switch kills it and its drop redials; a moved estimate keeps it and marks
nothing. The socket is judged by socket evidence through
`checkConnection`, which also runs when a request
has had no reply for 4 s (`PENDING_PROBE_MS`) on a socket nothing
arrived on since it was sent. A request that cannot wait that long goes
out urgent (`CallOptions.urgent`; `sfu.callRestartIce`, `sfu.callResume`
and `sfu.callAnswerServerOffer`): its wait is 1 s
(`URGENT_PENDING_PROBE_MS`) and the probe it starts judges a silent
socket in 3 s (`URGENT_CHECK_CONNECTION_DEADLINE_MS`, drain allowance
kept) instead of 8 s — it shortens only that request's timing, and a probe
already running gets its verdict moved earlier to that wait, never later.
The probe sends no ping when the socket
delivered a message in the last 5 s and drops a socket that answered
nothing for 8 s after one probe (at least 8 s: while a large write it sent
may still be crossing the link, the wait grows by that write's drain time);
any frame proves the socket, so a slow
request on a socket that keeps delivering fails as an RPC timeout, not a
dead socket. A probe that gives up only redials, at once and without
backoff — socket evidence is not a network change, so no `onNetworkChange`
fires for it; `onReconnect` on the new socket is its signal, and the SFU
answers that with `socketBack` (resend what a fault kept, `sfu.callResume`,
`checkIceSoon`), not an immediate ICE restart. The call layer never probes
the socket: ICE `disconnected` is media evidence, and its whole answer is
the ICE restart.

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

Mesh presence (`core/src/neighbourhood/Session.ts`): each device in a mesh
call puts two links into its agent's one online status, once, at join: a
presence link (source `did#device`, `ad4m://session/in-call`, target the
room) and a marker (same source, `ad4m://session/in-call-beat`, target the
room) that says the device beats. The beat itself is never persisted: every
20 s, and at once on join, the device sends a `beat` message on the call's
telepresence channel (`{v, call, to: "*", kind: "beat"}` through
`MeshManager.beat`, so it is counted like any send), on its own timer, so a
roster poll that hangs cannot stop it. Never write liveness into the online
status: p-diff-sync keeps every `set_online_status` as a permanent chain
entry and `get_online_status` scans them all. A receiver ties a beat to the
link author the executor verified plus the device in the payload, so an
agent can vouch only for its own devices, and a beat never makes a device: a
device is in the call only through its own agent's presence link. A reader
records, on its own clock, when it last heard each device with the marker
(or first saw it, if it never heard it) and leaves it out after 90 s with no
beat; a new beat brings it back at the next poll (3 s). A poll that finds
the reader's pair with the device connected counts as hearing it, so a call
whose signals fail while media flows keeps its peers. The executor routes a
broadcast to every agent it hosts in the neighbourhood, the sender's agent
too (main or managed), so two devices of one agent on one executor hear each
other; each device drops its own messages. So a device that
died without leaving (a crash, a closed browser, a lost network) leaves every
roster within about 90 s + a poll, even while another device of its agent
keeps the status alive, a live device survives three lost beats in a row,
and a reader that joins late keeps a device it has never heard for up to
90 s. A device with no marker (an older SDK, which drops the unknown `beat`
kind without a word) never goes stale. Status writes are join, leave, and
one repair when a poll finds a stale device of this agent (its links go) or
this device's presence link or marker missing (a sibling took them out while it missed
beats; they go back), at most once per 90 s; nothing is written over a
status that reads as none. The beat stops on leave, destroy and `replaced`.
A tab reopened in the same browser profile keeps its device id
(`localStorage`) and simply takes its links over. `sfu-browsers.test.ts` pins
both: the same-profile rejoin lists the user once, and a dead second device
leaves everyone's list within `GHOST_GONE_MS`.

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
