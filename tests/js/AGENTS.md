# tests/js — agent guide

Integration suites run against a built `ad4m-executor`. The SFU wind tunnel has its own
guide: `helpers/sfu/AGENTS.md`. CI-side notes: `../../.circleci/AGENTS.md`; nightly perf
history: `../../scripts/perf/AGENTS.md`.

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

## AI wind tunnel

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

## Port Conflicts

Sometimes an old `ad4m-executor` binary is still running from a previous test run, causing port conflicts. Before running tests, kill any lingering processes:

```bash
pkill -9 ad4m-executor
```

## Rebuild Requirements

The integration tests use the `ad4m-executor` CLI binary. Depending on what code was changed, different rebuilds are required:

| What Changed | Required Rebuild |
|--------------|------------------|
| Rust code in `cli/` | `cargo build --release` in `cli/` |
| Rust code in `rust-executor/` | `cargo build --release` in `cli/` |
| Deno JS (`js_core/*.js`, `*_extension.js`) | `pnpm build` in `rust-executor/` (rebuilds the Deno snapshot) |

**Deno Snapshot**: Anything that changes the content of the Deno JS engine at startup (language bootstrap, `ad4m:host`, or `#[op2]` extension `.js` files) requires rebuilding the snapshot. `pnpm build` in `rust-executor/` does that; `cargo build --release` in `cli/` does not.

## Agent-scale wind tunnel

How the work of ONE executor grows with the number of agents it holds. One executor in
multi-user mode, no Holochain, at 10, 20, 50 and 100 agents; each count gets a fresh
executor and data dir. `tests/js/tests/agent-scale.test.ts` runs, per count: onboarding
(`createUser` + `loginUser`, one after the other), a sign-in of all agents at once, writes
(each user a perspective and `AGENT_SCALE_LINKS` links, default 200, in batches of 20, all
users at once), reads (a SPARQL count, all users at once, 5 rounds), events (each user's own
`link-added`, and none of another user's), one shared neighbourhood on the local link
language (all users join; the first writes; time until every member sees it), 30 s idle CPU,
and a restart on the same data (time to ready, and from ready to each user's first answer).
RSS and threads come from `/proc` every second; open perspectives and SPARQL stores from the
executor's `/metrics`.

- Run it: `pnpm run prepare-test-local` once, then `pnpm run test:agent-scale` in `tests/js`
  (`pnpm run test-main-agent-scale` does both). `AGENT_SCALE_COUNTS=10,20` for a short run,
  `AGENT_SCALE_LINKS`, `AGENT_SCALE_IDLE_SECONDS` (30), `EXECUTOR_PATH` as for the other
  suites. Set `PERF_RESULTS_DIR` to get `agent-scale.json`. Run it under a memory cap
  (`systemd-run --user --scope -p MemoryMax=16G -p MemorySwapMax=0`); a count of 100 is a
  few hundred MB of executor. Mocha runs under Node 24 here: `ts-mocha` fails under Node 26.
- It prints each metric by count with `growth_<smallest>_<largest>` and a flag: **a time of
  one call (median, p95, per-agent memory) above 2x is flagged, a total (wall time, RSS,
  threads) above 15x.** A flag never fails the test; `scripts/perf/check` fails a regression
  against the job's own history (rules `agent-scale` in `thresholds.json`). The test fails
  only on a functional error: a user that cannot log in, an event that reaches another user,
  a link that is lost, a perspective that is not back after the restart.
- The pure helpers (percentile, growth, flags, results file, table, `/proc` parsers) are in
  `tests/js/helpers/agent-scale.ts`, with their unit test in `test-perf-unit`.
- `fds_peak` needs root: the executor forbids core dumps (`forbid_core_dumps`), so the
  kernel makes its `/proc/<pid>/fd` readable by root only. Without root the metric is
  left out, not recorded as 0.
- The restart waits for each user's perspective: `agent.unlock` answers before the
  executor has loaded every perspective, so a query at once finds none.
