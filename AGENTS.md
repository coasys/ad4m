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
