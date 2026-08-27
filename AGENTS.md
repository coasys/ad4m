# AD4M Project Context for AI Agents

This file contains important context and knowledge that AI coding assistants should be aware of when working on this codebase.

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

The integration tests are in `tests/js` and run with `pnpm run test-main`.

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
| JS code in `executor/` | `pnpm build` in `executor/` AND `pnpm build` in `rust-executor/` |
| JS executor or Deno extensions | `pnpm build` in `rust-executor/` (rebuilds Deno snapshot) |

**Important**: The JS package from `executor/` is included/embedded in `rust-executor/`. If you change JS executor code:
1. First build in `executor/`
2. Then build in `rust-executor/`

**Deno Snapshot**: Anything that changes the content of the Deno JS engine at startup (like the JS executor or extensions) requires rebuilding the Deno snapshot. This is done with `pnpm build` in `rust-executor/` - a mere `cargo build --release` in `cli/` is NOT sufficient.

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
AGENTS.md with build/test commands and architecture notes. Known: E2E
encryption between the two has 5 documented wire-format incompatibilities
(see `bootstrap-languages/server-link-language/AGENTS.md` "Known
limitations"); plaintext-room sync is verified end-to-end, E2E is not yet
production-ready.
