# AD4M monorepo — agent guide

@AGENTS.md

## Package map

| Path | What | Language |
|---|---|---|
| `rust-executor/` | The AD4M runtime: WS RPC server, perspectives/graph store, languages runtime (Deno), Holochain conductor, AI service, MCP server. **Start at `rust-executor/CLAUDE.md`.** | Rust |
| `cli/` | `ad4m` CLI binary; wraps `rust-executor` (`ad4m-executor` subcommand) and `rust-client` | Rust |
| `rust-client/` | Rust client for the executor's WS RPC | Rust |
| `core/` | TypeScript SDK (`@coasys/ad4m`): `Ad4mClient`, types, model/SHACL decorators, generated RPC types | TS |
| `connect/` | Browser/Node connection helper (`@coasys/ad4m-connect`) | TS |
| `bootstrap-languages/` | The system Languages (agent, perspective-diff-sync, etc.) bundled into the executor | TS/Rust |
| `ad4m-ldk/` | ALDK = AD4M Language Development Kit (Rust + JS crates for writing Languages) | Rust/TS |
| `ad4m-hooks/`, `hooks/` | React/Vue hooks for the SDK | TS |
| `ui/` | Launcher UI (Tauri) | TS/Rust |
| `dapp/` | Web dapp bundled into the executor (`dapp_server.rs`) | TS |
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
- Design docs go to `planning/<topic>-<yyyy-mm-dd>.md`; delete stale ones rather
  than leaving them beside current ones.
