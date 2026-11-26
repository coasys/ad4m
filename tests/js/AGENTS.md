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

