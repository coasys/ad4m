# AGENTS.md — AD4M

## Architecture Overview

- **`rust-executor/`** — Rust binary. Hosts Holochain, Deno JS runtime, Oxigraph SPARQL store, GraphQL API (Juniper). The "brain."
- **`core/`** — TypeScript SDK (`@coasys/ad4m`). Ad4mClient, PerspectiveProxy, Ad4mModel ORM, SHACL generation, query builders. Published to npm.
- **`connect/`** — TypeScript web component (`@coasys/ad4m-connect`). Auth UI, executor detection. **CRITICAL:** esbuild bundles `@coasys/ad4m` inline — must rebuild after core changes.
- **`cli/`** — Rust CLI binary wrapping the executor for terminal use.
- **`tests/`** — Integration tests (JS/Mocha) that start a real executor and run operations.
- **`bootstrap-languages/`** — Holochain DNAs for agent identity, neighbourhood sync, file storage.
- **`ad4m-hooks/`** — React/Vue/helper hooks for frontend integration.
- **`packages/`** — Additional packages (ad4m-devtools chrome extension, etc.)

## Dependency Chain

```
core (tsc + rollup) → connect (esbuild re-bundles core) → Flux/apps
                    → ad4m-hooks
rust-executor (cargo build) → cli
                            → integration tests (use built binary)
```

## Build Commands

### TypeScript SDK
```bash
cd core && pnpm exec tsc && pnpm run bundle
# Done when: lib/index.cjs and lib/index.js created
```

### Connect (MUST rebuild after core changes)
```bash
cd connect && pnpm run build
# Done when: dist/ updated. This re-bundles core via esbuild.
```

### Rust Executor
```bash
pnpm build-libs
# Done when: target/release/ad4m-executor exists
```

### Full SDK + Executor
```bash
pnpm build-libs  # builds everything in correct order
```

### Unit Tests (TypeScript)
```bash
cd core && pnpm test
# Done when: all suites pass, exit 0
```

### Integration Tests
```bash
pnpm run test-integration  # or specific: pnpm run test-integration -- --grep "simple"
# Requires: executor binary built first (pnpm build-libs)
```

## CI Pipeline

```
cargo fmt --check → pnpm install → build SDK → build executor → cargo clippy → pnpm test → integration tests
```

Use `scripts/ci-logs.sh coasys/ad4m <PR#>` to fetch CI failure logs from terminal.

## Definition of Done

A task is complete when ALL of:
1. `cargo fmt --check` exits 0
2. `cargo clippy -p ad4m-executor` exits 0 (warnings OK)
3. `cd core && pnpm exec tsc` exits 0
4. `cd core && pnpm test` exits 0
5. Changes committed with descriptive message

## Key Gotchas

### Connect esbuild Bundling Trap
`connect/scripts/esbuild_index.js` uses `bundle: true` with NO externals. `@coasys/ad4m` is inlined. pnpm overrides and `file:` links do NOT affect the bundled copy. You MUST run `pnpm run build` in connect/ after any core/ changes.

### Holochain DHT and GetStrategy
Holochain currently only implements **full-arc (full-sync) DHT mode** where every node gossips and stores all data:
- `GetStrategy::Local` is correct — all nodes eventually have all data once gossip completes
- `GetStrategy::Network` is NOT needed until Holochain implements sharding/partial-arc
- Flaky tests related to cross-agent data visibility are **gossip timing issues**, not strategy issues
- Fix flaky tests with retry logic and timeouts, not by changing GetStrategy

### Holochain K2 Spaces (Kitsune2)
After Holochain 0.7.0 (PR #5550):
- K2 spaces are only created by the `join` function
- `add_agent_infos` will NOT create spaces — they must exist first
- `K2SpaceNotFound` means the space hasn't been joined yet
- Retry logic should wait for spaces to be created

### Build & Caching
- Vite pre-bundles in `node_modules/.vite/` — clear when swapping SDK versions
- Turborepo caching can serve stale builds — `rm -rf .turbo node_modules/.cache`
- Vue Proxy breaks TS private fields (WeakMap) — use `toRaw()` or `shallowRef`

### Deno Snapshot
JS executor code is embedded in `rust-executor/`. If you change JS executor code:
1. Build in `executor/`
2. Then build in `rust-executor/` (rebuilds Deno snapshot)
A mere `cargo build --release` in `cli/` is NOT sufficient.

## When Blocked

- Tests fail after 3 attempts → stop and report the failing test with full output
- `cargo build` fails with linker errors → check `.cargo/config.toml` Xcode path
- Integration tests timeout → check port conflicts (12000-12100 range)
- Kill lingering processes: `pkill -9 ad4m-executor`

## Never

- Push to `dev` or `main` branches
- Delete lockfiles to resolve conflicts
- Skip `cargo fmt` or tests
- Force push without explicit request

## When Working on core/ (TypeScript SDK)

- Build: `cd core && pnpm exec tsc && pnpm run bundle`
- Test: `cd core && pnpm test`
- After changes: MUST rebuild connect/ (esbuild re-bundles core)
- Ad4mModel uses decorator metadata + SHACL generation
- SPARQL queries generated in `model/query-builder.ts`
- Query cache with TTL in `model/query-cache.ts`

## When Working on rust-executor/ (Rust)

- Check: `cargo check -p ad4m-executor`
- Format: `cargo fmt`
- Clippy: `cargo clippy -p ad4m-executor`
- SPARQL store: `src/perspectives/sparql_store.rs` (Oxigraph)
- GraphQL resolvers: `src/graphql/`
- REST API: `src/rest/` (alongside GraphQL)
- Feature flags: `--features sfu` (optional SFU support)

## When Working on tests/

- Integration tests use a real executor binary
- Tests in `tests/js/tests/*.test.ts` (Mocha)
- Test utilities in `tests/js/utils/`
- Each test file gets its own executor instance on a unique port
- `wipePerspective()` clears links + SHACL cache between tests

## Rebuild Requirements

| What Changed | Required Rebuild |
|---|---|
| Rust code in `cli/` | `cargo build --release` in `cli/` |
| Rust code in `rust-executor/` | `cargo build --release` in `cli/` |
| JS code in `executor/` | `pnpm build` in `executor/` AND `pnpm build` in `rust-executor/` |
| JS executor or Deno extensions | `pnpm build` in `rust-executor/` (rebuilds Deno snapshot) |
