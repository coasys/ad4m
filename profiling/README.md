# AD4M Memory Profiling & Leak Investigation

Profiling of the AD4M executor's memory usage during neighbourhood operations, and investigation of memory leaks during resource lifecycle (create/destroy cycles).

## Results

- **[Profiling Results](profiling-results-2026-02-21.md)** — Baseline memory measurements, per-neighbourhood growth (~140 MB each), scaling projections
- **[Leak Investigation](leak-investigation-2026-02-21.md)** — Memory recovery tests showing 0% memory freed on neighbourhood/perspective teardown

## Key Findings

### Root Cause: Holochain Conductor Memory Retention

When a neighbourhood is created, the executor clones a link language, installs it as a Holochain app, and allocates ~140MB of anonymous mmap'd memory (wasmer WASM pages + LMDB environments). When the neighbourhood is removed:

1. **AD4M-layer cleanup works correctly** — SurrealDB databases shut down, signal streams removed, languages cleaned up, Holochain apps uninstalled via `uninstall_app`
2. **Holochain conductor does not release memory** — anonymous mmap'd regions persist, large allocation count remains unchanged, RSS shows 0.0% recovery even after 60s settling

This was confirmed by comparing an unpatched binary (no cleanup) against a patched binary (full teardown) — both show identical 0% memory recovery, proving the leak is below the AD4M layer in the Holochain conductor's wasmer/LMDB memory management.

### Comparison: Original vs Patched Binary

| Metric | Original | Patched |
|--------|----------|---------|
| Post-init RSS | 747 MB | 768 MB |
| 3 NHs + 50 links each | 1201 MB (+428) | 1224 MB (+430) |
| After removing NHs (60s settle) | 1201 MB (0.0% recovery) | 1224 MB (0.0% recovery) |
| Large anon mappings: before/create/remove | 25/50/50 | 25/53/52 |
| Teardown logs firing | ❌ None | ✅ Full cleanup |
| Language cloning cost | 9.4 MB/clone | 4.6 MB/clone |

### Additional Findings

1. **Bare perspectives leak ~2.6 MB each** on create/remove cycle (both binaries).
2. **Language cloning cost halved** with the patch (9.4 → 4.6 MB/clone).
3. **Snapshot queries do not leak** — 100 queries add <1 MB.
4. **Link accumulation** — 300 links in a single neighbourhood adds ~30 MB.

## Reproduction

### Prerequisites
- Ubuntu 22.04 (tested on x86_64, 32GB RAM)
- AD4M executor binary (v0.11.1 or from this branch)
- Node.js 18+
- Bootstrap languages published or available as seed

### Running the Leak Investigation

```bash
# From the ad4m/tests/js directory
node ../../docs/profiling/leak-investigation.mjs
```

The script:
1. Starts the executor with a prepared seed
2. Runs 5 test phases: bare perspective cycles, neighbourhood create/remove, language cloning, link accumulation, and snapshot query stress
3. Measures RSS via `/proc/<pid>/smaps_rollup` with detailed memory breakdowns
4. Outputs per-test deltas and recovery rates

### Code Fixes (this branch)

The `fix: Implement memory leak fixes` commit adds:
- **Perspective teardown** — proper cleanup of Prolog pools, SurrealDB, link languages, subscribed queries, batch stores
- **Language removal** — Rust LanguageController calls JS `languageRemove()` during teardown
- **Signal stream cleanup** — removes Holochain signal callbacks on language removal
- **Language reference counting** — tracks usage to prevent premature removal
- **SurrealDB shutdown** — drops perspective databases on teardown

These fixes are necessary but not sufficient — the Holochain conductor memory retention remains an upstream issue.
