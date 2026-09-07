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
