# AD4M monorepo — agent guide

Canonical instructions for this directory. Claude Code (v2.1.281 or later),
Codex, Devin and Cursor read `AGENTS.md` files directly.

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
| `mental-models/` | `mental-models/README.md` (the mental models of this code base: a tree of short pages, one concept each, from ADAM down to links into the code) |
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
- Per-directory agent docs live in `AGENTS.md` only. Do not add a `CLAUDE.md`:
  Claude Code reads `AGENTS.md` only when no `CLAUDE.md`, `.claude/CLAUDE.md` or
  `CLAUDE.local.md` sits in the working folder or above it, so one such file
  stops every `AGENTS.md` below it from loading. To keep a personal
  `CLAUDE.local.md`, set Claude Code's Project instructions to
  `claude-md-and-agents-md` (`/config`).
- **Keep `mental-models/` true in the same PR.** A change to how the system works
  (a component, a flow, who decides what, where a rule lives, what a term means)
  updates the page that owns that concept in the same PR. A change that only
  touches internals no page describes updates nothing. Follow
  `mental-models/CONVENTIONS.md`, and keep the update small:
  - Change only the sentences the code makes untrue, with the fewest words, at
    the level the change lives. A parent page changes only when its own model
    changes; detail goes into the leaf page and its diagram.
  - A new concept gets its own page under the right parent, plus one line in the
    parent's Parts list. A removed concept loses its page and that line.
  - State the code as it is after the PR: no plans, open questions, defects,
    PR numbers or history. Relative links to code, no line numbers.
  - Use the terms in `mental-models/GLOSSARY.md`; add a term there when a page
    needs a new ADAM term (general technical terms get no entry).
  - Before pushing, check that every relative link in the changed pages resolves.
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
- **A change ships the test that proves it.** A PR that adds or changes behaviour
  adds a test that fails without the change, and the test asserts the result,
  not only that the code ran. Reviewers ask for this more than for any other rule.
- **A test fails when it cannot run.** A missing precondition (a fixture, a
  built binary, a service the test needs) is a failure, not a skip. The only
  skips are machine capabilities that CI provides by setting a flag, as
  `SFU_BROWSERS_REQUIRED=1` does; with the flag set, the test fails instead.
- **Reuse before you write.** Use the existing type, helper or function for the
  same job; do not add a second one beside it. When the existing one does not
  fit, change it.
- **New code goes in the module that owns the concept.** Do not add a helper in
  the caller's file when a module for that concept exists.
- **Remove what the change leaves behind.** Delete the dead branch, the
  unused helper and the duplicate path in the same PR.
- **Docs move with the code.** A PR that changes behaviour, a name or a path
  updates every doc and code comment that states it, in the same PR.
- **Code comments do not cite PRs or issues.** A comment says why the code is
  the way it is; the PR number lives in the commit and in `git blame`. A
  workaround is the exception: it names the issue that will remove it.
- **Split a file past about 1,000 lines** into parts along the concepts it
  holds, in a PR of its own when the split is large.
- **Commit only the change.** No review-reply notes, scratch files, logs or
  local planning files. Design docs go to `planning/` (rule above).
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

- Integration suites, test lanes, port conflicts, rebuild requirements, AI and agent-scale wind tunnels: `tests/js/AGENTS.md`
- SFU wind tunnel (e2e, browsers, cascade), and the mesh signalling and presence
  rules of `core/src/neighbourhood/` it pins: `tests/js/helpers/sfu/AGENTS.md`
- CI, nightly `nightly-llm-e2e`, runner failures, K2 bootstrap-srv install: `.circleci/AGENTS.md`
- Nightly performance history: `scripts/perf/AGENTS.md`
- Lib-test prerequisites, `CARGO_TARGET_DIR`: `rust-executor/AGENTS.md`
- Holochain DHT, `GetStrategy`, K2 spaces: `rust-executor/src/holochain_service/AGENTS.md`
- `@coasys/ad4m-ldk` path in `esbuild.ts`: `bootstrap-languages/AGENTS.md`
- Link languages (`link-server/`, `bootstrap-languages/server-link-language/`): their own `AGENTS.md`
