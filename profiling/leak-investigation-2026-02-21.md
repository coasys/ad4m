# AD4M Executor Memory Leak Investigation — 2026-02-21

## Setup
- Ubuntu 22.04, x86_64, 32GB RAM
- AD4M v0.11.1 executor, Holochain 0.7.0-dev.10-coasys
- Single agent, local bootstrap, no proxy
- Measurement: `/proc/<pid>/smaps` RSS/PSS + anonymous mapping counts

---

## Finding 1: Neighbourhood teardown releases ZERO memory

**This is the critical issue.**

Created 3 neighbourhoods (each with perspective-diff-sync clone + 50 links), then removed all 3 perspectives:

| State | RSS (MB) | Anonymous (MB) | Large anon mappings |
|-------|----------|----------------|---------------------|
| Baseline (post-init) | 797.1 | — | 26 |
| After 3 neighbourhoods + 50 links each | 1212.9 | 1037.5 | 51 |
| After removing all 3 perspectives (30s settle) | 1213.2 | 1037.7 | 51 |

**Recovery: -0.2 MB of 415.9 MB (0%)**

The anonymous mapping count stays at 51 even after removal — 25 new large (>10MB) anonymous RW mappings were created by neighbourhood operations and **none were released**. The disk usage also doesn't change (134 MB in `ad4m/h/`).

**Root cause:** `perspectiveRemove` removes the perspective from the AD4M layer but does NOT:
- Uninstall the cloned Holochain hApp
- Deallocate Wasmer WASM linear memory for the cloned language
- Clean up the language from the LanguageController
- Remove Holochain conductor cell state

Each neighbourhood creates a dedicated Holochain hApp instance with its own WASM runtime (~78 MB anonymous memory). Removing the perspective leaves these resources permanently allocated.

---

## Finding 2: Bare perspective lifecycle also leaks

Created and removed 10 plain perspectives (no neighbourhood, no link language):

| State | RSS (MB) |
|-------|----------|
| Baseline | 772.6 |
| After creating 10 perspectives | 796.3 |
| After removing all 10 perspectives | 797.1 |

**Leaked: 24.4 MB** — 2.4 MB per perspective that's never recovered. This is likely SurrealDB/Prolog state and JS runtime objects not being cleaned up on perspective removal.

---

## Finding 3: Language cloning accumulates permanently

Cloned perspective-diff-sync 10 times (template + publish) without creating any neighbourhoods:

| State | RSS (MB) |
|-------|----------|
| Baseline | 1213.2 |
| After 5 clones | 1238.1 |
| After 10 clones | 1255.4 |

**~4.2 MB per clone.** Each `languageApplyTemplateAndPublish` call:
- Unpacks/repacks hApp DNA
- Writes a new `bundle.js` to the data directory (8 language directories for 10 clones — some deduplication)
- Publishes the meta to the language-language
- Does NOT unload the cloned language even if it's never used for a perspective

Disk: 7.5 MB in `ad4m/languages/`, temp directory cleaned (4KB).

---

## Finding 4: Link accumulation within a neighbourhood is modest

500 links added to a single neighbourhood in batches of 100:

| Links | RSS (MB) | Δ from 0 links |
|-------|----------|-----------------|
| 0 (neighbourhood just created) | 1252.8 | — |
| 100 | 1285.9 | +33.1 |
| 200 | 1288.5 | +35.7 |
| 300 | 1291.4 | +38.6 |
| 400 | 1312.8 | +60.0 |
| 500 | 1315.6 | +62.8 |

Growth rate: ~0.13 MB per link — sub-linear, with step jumps (likely page allocation boundaries). This is reasonable.

Querying all 500 links added negligible memory (+0.1 MB). Link removal via GQL mutations failed (schema issue with `perspectiveRemoveLink`) so we couldn't test link cleanup, but the add pattern itself isn't concerning.

---

## Finding 5: WASM virtual memory reservation is extreme

From `/proc/maps` analysis:

| State | Large anon RW mappings (>10MB) | Total anon RW virtual |
|-------|-------------------------------|----------------------|
| Post-init | 26 | 1008 MB |
| 3 neighbourhoods | 51 | 1740 MB |
| After removing perspectives | 51 | 1738 MB |
| 5 neighbourhoods (test 4) | 52 | 1919 MB |

Each Holochain hApp instance creates approximately 1 large anonymous mapping. These are Wasmer WASM linear memory regions — they reserve large virtual address space and commit physical pages as the WASM module runs. They are **never unmapped**.

---

## Summary of Leaks

| Source | Leaked per unit | Recoverable? | Severity |
|--------|----------------|---------------|----------|
| Neighbourhood create/remove cycle | ~138 MB per NH | ❌ No | **Critical** |
| Bare perspective create/remove | ~2.4 MB per perspective | ❌ No | Medium |
| Language cloning (template+publish) | ~4.2 MB per clone | ❌ No | Medium |
| Link accumulation | ~0.13 MB per link | N/A (grows, not a leak) | Low |

## Recommended Fixes

### Critical: Holochain hApp lifecycle management
When a perspective is removed (especially one backed by a neighbourhood):
1. **Uninstall the Holochain hApp** — call the conductor admin API to disable/uninstall the cell
2. **Unload the language** — remove the JS language module from the LanguageController
3. **Free WASM memory** — ensure Wasmer instances are dropped so anonymous mappings can be reclaimed
4. **Clean up disk** — remove the cloned language bundle and Holochain cell state

### Medium: Perspective cleanup
- Audit what SurrealDB/Prolog state is created per perspective and ensure it's cleaned up on removal
- Check for JS event listener leaks on perspective objects

### Medium: Language deduplication
- Consider caching compiled WASM modules across languages that share the same DNA
- Share Holochain conductor cells where the DNA hash is identical (template parameters permitting)

### Architecture consideration
- The current model where each neighbourhood = its own hApp instance with dedicated WASM runtime is fundamentally expensive (~78 MB per NH)
- Consider a shared-conductor approach where multiple neighbourhoods can share a single Holochain cell with namespace isolation, reducing the per-NH overhead from ~78 MB to potentially single-digit MB
