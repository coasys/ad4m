# AD4M Executor Memory Profiling — 2026-02-21

## Setup

- **Server:** Ubuntu 22.04, x86_64, 32GB RAM
- **AD4M:** v0.11.1 (`ad4m-executor` prebuilt binary from GitHub release)
- **Holochain:** 0.7.0-dev.10-coasys fork
- **Bootstrap languages:** Built from source (p-diff-sync, agent-language, direct-message-language, perspective-language, neighbourhood-language, local-language-persistence, local-neighbourhood-persistence)
- **Network:** Local `kitsune2-bootstrap-srv`, no proxy, mDNS enabled
- **Test:** Single agent, creating 5 neighbourhoods sequentially, each with 10 links added via `perspectiveAddLink`
- **Measurement:** `/proc/<pid>/smaps` for memory breakdown, `ps` RSS/VSZ, 15s settle time between measurements

## Memory Progression

| Stage | RSS (MB) | Δ from previous |
|-------|----------|-----------------|
| Executor started (no agent) | 355.5 | — |
| Agent generated + languages loaded | 749.5 | +394.0 |
| 1 neighbourhood (+ 10 links) | 994.4 | +244.9 |
| 2 neighbourhoods (+ 10 links each) | 1086.4 | +92.0 |
| 3 neighbourhoods (+ 10 links each) | 1157.3 | +70.9 |
| 4 neighbourhoods (+ 10 links each) | 1221.0 | +63.7 |
| 5 neighbourhoods (+ 10 links each) | 1304.6 | +83.6 |

**Average growth per neighbourhood (2–5):** ~77.6 MB
**First neighbourhood cost:** ~245 MB (includes one-time Holochain conductor infrastructure)

## Memory Breakdown by Category (from `/proc/smaps`)

### At startup (355 MB)
| Category | MB | % |
|----------|-----|---|
| Anonymous mappings | 244.1 | 68% |
| ad4m-executor binary | 106.6 | 29% |
| libc/system | 2.5 | <1% |
| system-libs | 2.0 | <1% |
| heap | 0.2 | <1% |

### After init + languages (750 MB)
| Category | MB | % |
|----------|-----|---|
| Anonymous mappings | 599.8 | 80% |
| ad4m-executor binary | 144.7 | 19% |
| libc/system | 2.6 | <1% |
| system-libs | 2.1 | <1% |
| heap | 0.2 | <1% |

### At 3 neighbourhoods (1157 MB)
| Category | MB | % |
|----------|-----|---|
| Anonymous mappings | 979.9 | 84% |
| ad4m-executor binary | 153.8 | 13% |
| heap | 18.9 | 1% |
| libc/system | 2.6 | <1% |
| system-libs | 2.1 | <1% |

### At 5 neighbourhoods (1305 MB)

| Category | MB | % |
|----------|-----|---|
| Anonymous mappings | 1126.9 | 86% |
| ad4m-executor binary | 154.0 | 11% |
| heap | 18.9 | 1% |
| libc/system | 2.6 | <1% |
| system-libs | 2.1 | <1% |

## Disk Usage
- Data directory at 5 neighbourhoods: **148 MB**

## What the Numbers Mean

### The 355 MB baseline
Before any agent or language is created, the executor already uses 355 MB. This is the Rust runtime, V8/Deno JS engine, Holochain conductor initialisation, SurrealDB, Prolog service, and AI service (even with CUDA unavailable). The executor binary itself accounts for ~107 MB of mapped memory.

### The +394 MB init cost
Agent generation triggers bootstrap language resolution + installation. The direct-message language is cloned from template, which involves unpacking the hApp bundle, repacking the DNA with templated properties, installing it into Holochain, and loading the JS module. This is the cost of a single agent becoming operational.

### The ~78 MB per neighbourhood
Each `neighbourhoodPublishFromPerspective` call:
1. Clones perspective-diff-sync via `languageApplyTemplateAndPublish` (unpack hApp → template DNA → repack)
2. Installs the cloned hApp into Holochain (new WASM instance + SQLite database)
3. Loads the JS language module

The per-neighbourhood cost is dominated by the Holochain hApp instance — each gets its own Wasmer WASM linear memory allocation and SQLite storage. The "anonymous" category in smaps (which grows from 600 MB → 1127 MB across 5 neighbourhoods) captures these allocations.

### The first neighbourhood premium
The first neighbourhood costs 245 MB vs ~78 MB for subsequent ones. The extra ~167 MB likely includes one-time Holochain conductor infrastructure that's allocated on first hApp install after agent init (e.g., app interface setup, networking resources).

### Executor binary memory is stable
The `ad4m-executor` mapped memory stabilises at ~154 MB after init and doesn't grow with neighbourhoods. The growth is entirely in anonymous mappings (Holochain/WASM/SQLite).

### Heap stays modest
The explicit heap (`[heap]` in smaps) is only 19 MB even at 5 neighbourhoods. The real memory consumption is in mmap'd anonymous pages from Wasmer and SQLite.

## Scaling Projection

| Neighbourhoods | Estimated RSS |
|---------------|--------------|
| 0 (agent only) | ~750 MB |
| 5 | ~1.3 GB |
| 10 | ~1.7 GB |
| 20 | ~2.5 GB |
| 50 | ~4.6 GB |

These are single-agent, single-device numbers with no network sync activity. Real-world usage with active sync would likely be higher.

## Methodology Notes

- Languages were published locally using `languagePublish` mutation via the language-language (`local-language-persistence`), then the executor was restarted with a seed pointing to the published bundles
- The `languages` GQL query returns 0 even when languages are installed and functional — system/bootstrap languages appear to be filtered from this query. Languages were confirmed installed via executor log output
- Each neighbourhood creation involved: `perspectiveAdd` → `languageApplyTemplateAndPublish` → `neighbourhoodPublishFromPerspective` → 10× `perspectiveAddLink`
- All operations completed successfully with no errors
