# AD4M Memory Profiling & Leak Investigation

Profiling of the AD4M executor's memory usage during neighbourhood operations, and investigation of memory leaks during resource lifecycle (create/destroy cycles).

## Results

- **[Profiling Results](profiling-results-2026-02-21.md)** — Baseline memory measurements, per-neighbourhood growth (~78 MB each), scaling projections
- **[Leak Investigation](leak-investigation-2026-02-21.md)** — Memory recovery tests showing 0% memory freed on neighbourhood/perspective teardown

## Key Findings

1. **Neighbourhood teardown leaks 100% of allocated memory.** `perspectiveRemove` does not uninstall Holochain hApps or free WASM runtimes. 3 neighbourhoods allocated 416 MB; removing all 3 recovered 0 MB.
2. **Each neighbourhood costs ~78 MB** (Wasmer WASM linear memory + Holochain conductor state).
3. **Bare perspectives leak ~2.4 MB each** on create/remove.
4. **Language cloning accumulates ~4.2 MB per clone** even when unused.

## Reproduction

### Prerequisites
- Ubuntu 22.04 (tested on x86_64, 32GB RAM)
- AD4M v0.11.1 executor binary
- `kitsune2-bootstrap-srv` (from cargo)
- `hc` CLI for building bootstrap languages
- Node.js 18+

### Steps
1. Build bootstrap languages from `bootstrap-languages/` using `hc` CLI
2. Run `publish-langs.mjs` to publish languages and generate a prepared seed
3. Fix `storagePath` in the seed to point to `<ad4m-repo>/tests/js/tst-tmp/languages/`
4. Run `profiler-v9.mjs` or `leak-investigation.mjs` from `<ad4m-repo>/tests/js/` as CWD

### Scripts
- **[publish-langs.mjs](publish-langs.mjs)** — Publishes bootstrap languages via the language-language
- **[profiler-v9.mjs](profiler-v9.mjs)** — Memory profiling across neighbourhood creation
- **[leak-investigation.mjs](leak-investigation.mjs)** — Create/destroy cycle tests for leak detection

## Environment
- AD4M v0.11.1, Holochain 0.7.0-dev.10-coasys fork
- Single agent, local bootstrap, no proxy/relay
- Measured via `/proc/<pid>/smaps` (RSS, PSS, per-mapping breakdown)
