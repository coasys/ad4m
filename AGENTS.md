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

## Graph-Expression Duality

Graph-backed subjects can be exported as content-addressed Expressions and mounted back:

- **Addressing**: `graph://<hex-sha256>` where the hash is over the *canonical* serialization (sorted canonical N-Triples, blank-node-free). The graph label is **excluded** from the hash so the same triples hash identically regardless of partition IRI.
- **Internal partition**: each subject's triples live in a named graph `ad4m://graph/<base>` (`make_graph_iri(base)` in `sparql_store.rs`). This internal IRI is never surfaced in the public API — callers address by subject base or by `graph://<hash>`.
- **Reifiers**: link metadata is stored as RDF 1.2 quoted triples — `<link:HASH> rdf:reifies <<(source predicate target)>>`. Every graph-backed subject therefore carries reifier triples, and the content hash covers direct triples + reifier triples + reifier metadata.
- **Snapshot proofs**: export attaches a signature over `"<iri>\n<timestamp>"` by the calling agent's DID. `mount_expression` verifies the proof *before* loading, then re-hashes the loaded triples and rolls back on mismatch. A proof is simply "signed by a DID": AD4M treats individual agents and groups (graphs carrying their own DID) identically, so verification resolves the signer's DID document regardless of what kind of identity it denotes — there is no separate agent/graph signer role.

### JSON-LD export is deferred (not a bug)

oxigraph 0.5.x's JSON-LD serializer errors with *"JSON-LD does not support RDF 1.2 yet"* on any reifier-bearing graph. Since every real graph carries reifiers, `jsonld` would always fail. Export therefore ships **three** formats — `nquads-canonical`, `nquads`, `turtle` — and defers `jsonld`, mirroring the earlier RDFC-1.0 deferral. Do not re-add `jsonld` to the format enum until oxigraph supports RDF 1.2 in its JSON-LD writer.

## Rust test harness: wallet / global-agent init

Tests that exercise signing (snapshot proofs, agent-signed anything) share a process-global agent and a process-global wallet. Two invariants must hold together:

- `test_utils::setup_wallet()` is **idempotent** — it only generates the shared `"main"` keypair if one does not already exist. `Wallet::generate_keypair` *overwrites*, so re-running it mid-suite would swap the key out from under the init-once global `AgentService` (whose DID is fixed on first init). Signing would then use a key whose DID no longer matches `AgentService.did`, and every sign→verify round-trip across that boundary fails as *"signature invalid"*.
- `AgentService::init_global_test_instance()` is likewise guarded to run once per process.

If you add a test module that signs, call these helpers — do not hand-roll wallet setup, or you risk the DID/key divergence above.

## Toolchain and the core lockfile

- **Build core with node 24 + pnpm 9.15.0** (the `packageManager` pin). Newer pnpm floats `@types/node: "*"` to a version tsc 4.9.5 cannot parse (`ffi.d.ts` syntax errors). Use `corepack` so the pinned pnpm resolves `@types/node` to the version in the lockfile (16.x).
- **Build core via the root workspace lockfile**, not `core/pnpm-lock.yaml`. `core` is a workspace member; a filtered root install (`pnpm install --frozen-lockfile --filter @coasys/ad4m...` from the repo root) resolves the correct, tsc-compatible pins. Set `NODE_ENV=development` on a fresh install or devDependencies (patch-package, typescript, rollup) are skipped.
- **Known defect**: `core/pnpm-lock.yaml` contains unresolved git merge-conflict markers (`<<<<<<<`/`=======`/`>>>>>>>`) introduced by an upstream merge. A standalone `core`-only frozen install fails to parse it ("duplicated mapping key"). The root lockfile is clean and is what CI uses — build from the root, and fix the core lock at its source branch.
