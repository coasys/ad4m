# AD4M monorepo — agent guide

Canonical instructions for this directory. `CLAUDE.md` next to this file is a
Claude Code entrypoint that contains only `@AGENTS.md` so Claude inlines this
file. Edit this file; do not put unique rules in `CLAUDE.md`.

Area detail lives in the `AGENTS.md` of the folder it describes. Read that file
before you work there.

## Package map

| Path | Start at |
|---|---|
| `rust-executor/` | `rust-executor/AGENTS.md` (the runtime; the biggest package) |
| `cli/`, `rust-client/` | `ad4m` CLI binary; Rust client for the executor's WS RPC |
| `ad4m-hash-chain/`, `ad4m-identity-core/`, `ad4m-identity-client/` | their own `AGENTS.md` (`did:scid:coasys:1`: chain, key log, master-key side) |
| `identity-store/` | `identity-store/AGENTS.md` (shared store of key logs) |
| `core/` | TS SDK `@coasys/ad4m` (`Ad4mClient`, generated RPC contracts) |
| `connect/` | `@coasys/ad4m-connect`, browser/Node connection helper |
| `bootstrap-languages/` | `bootstrap-languages/AGENTS.md` (system Languages) |
| `link-server/` | `link-server/AGENTS.md` (self-hosted link server) |
| `ad4m-ldk/` | Language Development Kit (Rust + JS crates) |
| `ad4m-hooks/`, `hooks/` | React/Vue hooks for the SDK |
| `ui/` | Launcher UI (Tauri) |
| `dapp/` | Web dapp bundled into the executor; builds against the **published** `@coasys/ad4m`, so SDK changes reach it only after a release |
| `tests/js/` | `tests/js/AGENTS.md` (integration suites) |
| `test-runner/` | Language test harness |
| `docs-src/` | Docs site and language interface specs (`language-interface-spec.md`, `host-contract.md`) |
| `planning/` | Dated design specs |

There is no `executor/` package any more: ignore references to it in older docs.

## Repo-wide rules

- Package manager is `pnpm`, never `npm`. Tests: `pnpm run test-main`
  (integration), `cd rust-executor && pnpm test` (crate unit tests, serial).
- JS/TS files embedded into the Deno snapshot (`rust-executor/src/js_core/*.js`
  and extension `.js` files) must be pure ASCII: non-ASCII fails const-eval in
  `ascii_str_include!`.
- Commit messages: Conventional Commits (`feat:`, `fix:`, `refactor:`, `docs:`, `test:`).
- **Do not edit `CHANGELOG` in a PR.** It is written at release time. Every
  edit conflicts with the next PR's, and resolving it is a push that dismisses
  approvals. Put the release note in the PR description under a `### Changelog` heading (what
  changes for users, apps or operators; **Breaking:** first when it is; "none"
  for internal-only PRs). If a branch already carries an entry, leave it.
- Design docs go to `planning/<topic>-<yyyy-mm-dd>.md`; delete stale ones.
- Per-directory agent docs: canonical file is `AGENTS.md`. Sibling `CLAUDE.md`
  contains only `@AGENTS.md`. `bash scripts/check-agents-claude.sh` checks every
  pair (CI runs it).
- **RPC contracts are generated.** Every executor method registers
  `map.method::<Params, Result>(name, handler)` (`.read()`, `.long()`). After
  changing a method or a type it reaches, run `cd core && pnpm run generate:api-types`;
  a unit test fails when the committed files are stale. Never hand-merge
  `core/src/generated/api/*` or the guard table in `rust-executor/src/api/AGENTS.md`
  (in a conflict too): rerun the generator.
- **Events are typed and opt-in.** Every pushed event has a name in
  `events_ws::events` and a payload type in `event_specs()` (generates
  `Events.ts`). A socket gets no event until it sends `events.watch` naming it,
  so a raw-WebSocket test client must send the watch and wait for its reply
  before it triggers the event (see `tests/js/helpers/sfu/events.ts`).
- **Tests wait for a signal, never for time.** No fixed sleep before an
  assertion or before the action a test triggers. Wait for an event, a callback
  or the reply of the call that set the state up (a subscribe reply works: the
  executor registers the subscription before it computes the first result);
  else poll at 100 ms or less. To prove something did not happen, cause a
  sentinel event on the same channel, wait for it, then check; this holds only
  where the channel keeps event order. For code that waits on time, use a test
  override, `#[tokio::test(start_paused = true)]` or fake timers. Do not pause
  the tokio clock in a test that talks to a real language runtime thread: that
  thread has its own clock.
- `connect/` tests (vitest + happy-dom) fail under Node 26; use Node 24.
- Run `cargo fmt --all` (and push) from the worktree's own top level. In the
  main checkout it exits 0 and changes nothing in a linked worktree, so CI
  fails `fmt --check`.

## PR rules

- A change the TS client can see needs integration tests under `tests/js` and
  docs, not only Rust tests.
- Zero unresolved CodeRabbit threads before the PR goes to a human reviewer.
- Bot commits and PRs carry no `Co-Authored-By: Claude` trailer and no
  "Generated with" line.

## Where the detail lives

- Integration suites, test lanes, port conflicts, rebuild requirements, AI wind tunnel: `tests/js/AGENTS.md`
- SFU wind tunnel (e2e, browsers, cascade), and the mesh signalling and presence
  rules of `core/src/neighbourhood/` it pins: `tests/js/helpers/sfu/AGENTS.md`
- CI, nightly `nightly-llm-e2e`, runner failures, K2 bootstrap-srv install: `.circleci/AGENTS.md`
- Nightly performance history: `scripts/perf/AGENTS.md`
- Lib-test prerequisites, `CARGO_TARGET_DIR`: `rust-executor/AGENTS.md`
- Holochain DHT, `GetStrategy`, K2 spaces: `rust-executor/src/holochain_service/AGENTS.md`
- `@coasys/ad4m-ldk` path in `esbuild.ts`: `bootstrap-languages/AGENTS.md`
- Link languages (`link-server/`, `bootstrap-languages/server-link-language/`): their own `AGENTS.md`
