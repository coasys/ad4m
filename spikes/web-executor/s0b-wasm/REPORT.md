# S0b — Wasm numbers for the AD4M query core (T1 + T2)

> Copied from the spike worker's scratch directory on 2026-11-19. Local paths are replaced with `<placeholders>`.
> The scripts in this directory now read paths from env vars; see `../README.md` for how to run them.

Worker: Opus (for Data), 2026-11-19. Reference code: coasys/ad4m `origin/dev` @ 425311398 (read-only worktree).
Machine: Data's box (Linux x86_64), Node v24.20.0. All numbers in this file are **measured** unless a cell says otherwise.

## Step 1 (T1): Oxigraph in Wasm

### Setup

- Crate: `crate/` (`s0b-oxigraph`), `oxigraph = "=0.5.13"` (same version as `rust-executor/Cargo.toml:251`),
  `default-features = false`, features `rdf-12` (+ `js` on wasm32). Release, `lto = true`, `codegen-units = 1`,
  wasm-opt `-O3` (wasm-pack 0.14.0 default pipeline).
- Data: 50,000 links = 10,000 "Todo" instances × 5 links: `ad4m://type → todo://Todo` (flag), `todo://state → todo://done|ready`
  (IRI), `todo://title → "…"` (simple literal, as `literal:string:` is stored), `todo://priority → "n"^^xsd:integer`, and
  `flux://chN ad4m://has_child todo://tN` (100 channels).
- Shape per link = what `write_link_quads_proof` (`sparql_store.rs:942-1050`) writes: direct triple, `link:HASH rdf:reifies <<( s p o )>>`,
  and 6 annotations on the reifier (author, timestamp, status, proofKey, proofSignature, proofValid; values at real lengths:
  did:key author, 128-hex signature). 8 quads per link, **400,000 quads**. In-memory `Store::new()` (the `store_mode: memory` shape).
- Load: one transaction per 1000 links (the `add_links` path). One transaction per link (`add_link` path) measured too: same time
  and memory within 2% (native 683 ms / 3338 B, wasm 1052 ms / 2888 B).
- Queries (`crate/src/lib.rs::queries`), shaped after `model_query/sparql_builder.rs` output and one `sparql_store.rs` test query:
  - Q1 TwoPhase id phase, sort by timestamp (`SortKey::Timestamp` + `build_timestamp_probe`), LIMIT 50
  - Q2 hydration of the 50 ids from Q1 (`decorated_links_query` with a `VALUES ?source {…}` block, 5 OPTIONALs)
  - Q3 TwoPhase id phase, where `state = done`, sort by property `title` (with the `ad4m://fn/parse_literal` custom function), LIMIT 50
  - Q4 `build_count_sparql` with the same where
  - Q5 `InstanceQueryPlan::Single`: every link of every instance joined to its reifier (40,000 rows), the heavy one
  - Q6 channel last activity, GROUP BY over reified `has_child` timestamps (the flux `recentConversations` test shape, `sparql_store.rs:5355`)
- Timing: one warm-up, then 10 runs, median. Each config run 3 times; the table shows the **median of the 3 run medians**
  (raw per-run files in `bench/*-run{1,2,3}.txt`).

### Commands

```bash
export CARGO_TARGET_DIR=<s0b-wasm>/target
cd crate && cargo build --release --bin native && ../target/release/native 50000          # native
cd crate && wasm-pack build --release --target nodejs --out-dir ../pkg-u64                # wasm (with sparopt patch, see below)
node bench/node-bench.cjs pkg-u64 50000                                                    # wasm timing in Node 24
gzip -9 -c pkg/s0b_oxigraph_bg.wasm | wc -c                                                # gzip CLI
# brotli q11: node zlib.brotliCompressSync inside node-bench.cjs (no brotli CLI on the box)
```

### Numbers (all measured)

| Metric | Native (x86_64) | Wasm, stock oxigraph 0.5.13 | Wasm, `sparopt` u64 patch | Ratio wasm(patched)/native | "Go" limit | Pass? |
|---|---|---|---|---|---|---|
| `.wasm` raw | – | 2,720,679 B | 2,719,904 B | – | – | – |
| `.wasm` gzip -9 (CLI) | – | 874,524 B | – | – | < 5 MB | **yes (0.87 MB)** |
| `.wasm` gzip -9 (node zlib) | – | 894,276 B | 893,456 B | – | < 5 MB | yes |
| `.wasm` brotli q11 | – | 599,213 B | 599,240 B | – | < 5 MB | **yes (0.60 MB)** |
| Instantiate (Node `require`, sync compile) | – | 8.2 ms | 10.2 ms | – | – | – |
| Load 50k links (400k quads) | 688 ms | 1072 ms | 1064 ms | 1.55× | ≤ 3× | yes |
| Q1 id phase, ts sort | 100.4 ms | 124.4 ms | 121.5 ms | 1.21× | ≤ 3× | yes |
| Q2 hydrate 50 ids | 1.90 ms | **160.9 ms (85×)** | 2.94 ms | 1.55× | ≤ 3× | stock **no**, patched yes |
| Q3 where + property sort | 32.6 ms | 42.3 ms | 42.3 ms | 1.30× | ≤ 3× | yes |
| Q4 count | 14.0 ms | 15.1 ms | 15.2 ms | 1.09× | ≤ 3× | yes |
| Q5 all instances + links (40k rows) | 243.9 ms | 300.2 ms | 293.0 ms | 1.20× | ≤ 3× | yes |
| Q6 channel GROUP BY | 51.5 ms | 60.8 ms | 60.9 ms | 1.18× | ≤ 3× | yes |
| Memory growth for load, per link | 3,340 B (RSS delta, glibc malloc) | 2,890 B (linear memory growth) | 2,890 B | 0.87× | ≤ 2× | yes |
| Memory after all queries (process / linear memory) | 174 MB RSS | 253 MB | 155 MB | 0.89× | – | – |

Notes on memory: Wasm linear memory never shrinks, so "after all queries" is the peak. Native is RSS (glibc keeps freed pages too).
Wasm uses Rust's default wasm allocator (dlmalloc). Per-link bytes are for the full 8-quad link (derived by division: ≈ 361 B per quad wasm, ≈ 418 B native).

### Cross-check with the repo toolchain (rustc 1.96.1, one run, measured)

The table above was built with rustc 1.88.0 (box default). Rebuilt with 1.96.1 (`RUSTUP_TOOLCHAIN=1.96.1`, separate target dir,
same patched crate; `bench/native-196.txt`, `bench/wasm-196.txt`):

| Metric | Native 1.96.1 | Wasm 1.96.1 (patched) | Ratio |
|---|---|---|---|
| `.wasm` raw / gzip -9 (CLI) / brotli q11 | – | 2,441,115 / 819,190 / 561,044 B | – |
| Load 50k links | 683 ms | 1027 ms | 1.50× |
| Q1 / Q2 / Q3 | 100.2 / 1.89 / 32.8 ms | 115.6 / 2.82 / 38.8 ms | 1.15× / 1.49× / 1.18× |
| Q4 / Q5 / Q6 | 14.1 / 247.9 / 53.4 ms | 13.9 / 279.6 / 57.8 ms | 0.99× / 1.13× / 1.08× |
| Memory per link | 3,340 B | 2,890 B | 0.87× |

The newer compiler gives a 10% smaller Wasm and slightly better Wasm times; the conclusions do not change.

### Browser (headless Chrome 146, measured once)

`wasm-pack build --release --target web --out-dir ../pkg-web` (same crate, with the sparopt patch), page `bench/browser.html`,
driver `node bench/browser-run.mjs <scratch dir>` (serves on 127.0.0.1, drives Chrome over CDP). Chrome rounds
`performance.now()` to 0.1 ms. Main thread, not a Worker.

| Metric | Chrome 146 | Node 24 (patched, median of 3) | Native |
|---|---|---|---|
| init (fetch + compile + instantiate, localhost) | 27.1 ms | 10.2 ms (sync `require`, no fetch) | – |
| Load 50k links | 886.7 ms | 1064 ms | 688 ms |
| Q1 / Q2 / Q3 | 119.6 / 3.6 / 41.9 ms | 121.5 / 2.94 / 42.3 ms | 100.4 / 1.90 / 32.6 ms |
| Q4 / Q5 / Q6 | 14.8 / 286.7 / 58.8 ms | 15.2 / 293.0 / 60.9 ms | 14.0 / 243.9 / 51.5 ms |
| Memory per link | 2,890 B | 2,890 B | 3,340 B |

### Scaling (one run each, measured; `bench/scaling.txt`)

| Links | Load native / wasm | Bytes per link native / wasm | Q1 native / wasm | Q5 native / wasm |
|---|---|---|---|---|
| 10,000 | 131 / 176 ms | 3,515 / 2,995 | 16.5 / 21.1 ms | 45.9 / 56.1 ms |
| 50,000 | 700 / 1065 ms | 3,339 / 2,890 | 100.0 / 121.5 ms | 244.9 / 290.7 ms |
| 100,000 | 1420 / 2323 ms | 3,302 / 2,882 | 218.0 / 265.1 ms | 515.8 / 606.9 ms |

Linear in the link count; the wasm/native ratios stay at 1.2–1.6× (load 1.3–1.6×). Q2 stays at ~2–3 ms (it reads 50 ids).

Note on a first scaling attempt: it ran while a wasm-pack build used the CPU and gave wasm ratios up to 2.7×; I threw it away
and re-ran on a quiet machine. Native and Node numbers here are single-machine, single-user; no CPU pinning.

### Finding: an Oxigraph planner bug on 32-bit targets (one-file fix)

Stock oxigraph makes Q2 (the hydration query every paginated `model_query` runs) **85× slower** in Wasm than native,
and it holds ~100 MB more linear memory afterwards. Cause, confirmed by patch: `sparopt` 0.3.9 (`src/optimizer.rs`) computes
join-cost estimates in `usize` with saturating math (`estimate_triple_pattern_size` returns up to 1e9, then
`saturating_mul`). On wasm32 `usize` is 32 bits, so the costs saturate at 4.29e9, the hash-join vs. for-loop (lateral)
choice for OPTIONAL goes the other way, and OPTIONAL runs row by row. Narrowed with `bench/q2b.txt`: one mandatory
reifier annotation followed by an OPTIONAL is enough (39 ms wasm vs 1.0 ms native); the same query without the OPTIONALs
runs at 1.05× native.

Fix: `usize` → `u64` in the cost functions, 11 changed lines, `bench/sparopt-u64.patch`, applied in `crate/vendor/sparopt` via
`[patch.crates-io]`. A no-op on 64-bit. With it Q2 is 1.55× native. This should go upstream to oxigraph
(and until then a `[patch]` in the core crate). Other plans may hit the same saturation; the patch fixes the root, not one query.

## Step 2 (T2): compile probe

### Setup

- Crate `crate/probe/` (`s0b-probe`). Copied **verbatim** from `rust-executor/src` @ 425311398: `perspectives/sparql_store.rs`,
  `perspectives/shacl_parser.rs` + `shacl_parser/role_gate_keys.rs`, `perspectives/model_query/` (all files), plus what they
  need: `types/` (core.rs, domain.rs), `perspectives/{write_log.rs, subject_classes_of.rs, ordering/, hardwired_sdna/}`,
  `languages/{literal.rs, error.rs}`, `secret.rs`, `agent/capabilities/types.rs`. About 46k lines incl. test files
  (test modules are `cfg(test)` and not compiled by `cargo check`).
- Stubbed in `src/lib.rs` (each stub = a seam the real core crate must cut): `agent::signatures::verify_or_false`,
  `agent::session::{Session, Principal, AuthError}` (+4 methods), `agent::AgentContext`, `agent::capabilities::ListenerReach`,
  `languages::LanguageController` (5 methods), `perspectives::flow_evaluator::LINKED_TO_KEYS`, `config::get_global_config`,
  `ai_service::providers::api_type_supports_native_tools`, `utils::constant_time_eq`, `config_file::REDACTED`.
- Dependencies: the executor's versions and its `Cargo.lock`; toolchain 1.96.1 (the repo's `rust-toolchain.toml`).
- Method: every stage runs `cargo check` for **wasm32-unknown-unknown and for native x86_64** (`bench/probe-check.sh`).
  An error in both is a probe gap; an error only on wasm32 is a real Wasm cause. Logs: `bench/r*-{wasm,native}.{log,err}`.

### Stages (all measured; "errors" = rustc's "due to N previous errors")

| Run | Change | wasm32 | native | What the difference shows |
|---|---|---|---|---|
| r1 | raw copy + stubs, no `deno_core`, wasm-safe tokio (`sync,macros,rt,time`), oxigraph without rocksdb | 87 | 87 | Same set: nothing Wasm-specific yet; probe gaps + `deno_core` + RocksDB API |
| r2 | probe gaps closed (`url/serde`, `uuid/v4` that the executor gets by feature unification; 3 stub additions: `Session` methods, `LanguageController`, `LINKED_TO_KEYS`) | 1 (in `uuid`, build stops) | 65 | **wasm-only**: `uuid` v4 needs a randomness source on wasm32 |
| r3 | wasm32: `uuid` features `["v4","js"]` | 65 | 65 | – |
| r4 | `deno_core` = a shim crate that re-exports `anyhow` + `error::AnyError` | 5 | 5 | `deno_core::anyhow` caused **60** errors (23 unresolved imports + 37 follow-on type errors) |
| r5 | my stub signature fixed | **4** | **4** | Left: only the RocksDB-only Oxigraph API |
| r6 | executor features: `tokio/full` + `oxigraph/rocksdb` | 48 (in `mio`, build stops) | **0** | Native 0 = the probe is complete. `tokio/full` breaks wasm32 |
| r7 | only `oxigraph/rocksdb` added (no tokio full) | 4 | – | Oxigraph drops RocksDB on wasm itself (`cfg(not(target_family="wasm"))`); no C++ build is tried. The same 4 API errors |
| r8 | only `tokio/full`, `--keep-going` | `mio` fails (48), nothing else | – | tokio `full` is the only dep-level blocker of the copied code |
| deno | the real `deno_core` (coasys fork, same commit, executor features), `--keep-going`, separate crate `crate/probe-deno` | fails in 4 crates: `mio` (48), `sys_traits` (1), `uuid` (1), `v8` build script (prebuilt download 404: no wasm32 V8) | – | `deno_core` cannot be in the core crate on wasm32 at all |

### Errors by cause (wasm32)

| Cause | Compile errors | Where | Change in the real crate | One-line? |
|---|---|---|---|---|
| `deno_core::anyhow` / `deno_core::error::AnyError` | 60 (23 + 37 follow-on); the real dep fails in `v8`, `mio`, `sys_traits`, `uuid` | 28 uses in 18 non-test files of the copied set | T10: `deno_core::anyhow` → `anyhow`, `AnyError` → `anyhow::Error`. Mechanical. | per file yes (sed); 18 files |
| tokio `full` | 48 (all in `mio`; the build stops there) | `rust-executor/Cargo.toml:87` | The core crate declares `tokio = { features = ["sync","macros","rt","time"] }`. **Measured: the copied code compiles with only these.** | yes (in the new crate's Cargo.toml) |
| RocksDB-only Oxigraph API | 4 | `sparql_store.rs:9` (`StoreOptions`), `:1265` (`open_with_options`), `:2332` (`flush`), `:2343` (`backup`) | T14: `#[cfg(feature = "rocksdb")]` around the persistent path (+ the `std::fs::create_dir_all` at `:1257`) | 4 small cfg gates |
| Randomness (`getrandom` backend) | 1 (`uuid`, r2) + 1 (`getrandom` 0.3 via oxigraph, seen before r1 until the probe set `oxigraph/js` on wasm32) | `types/core.rs:926`, `agent/capabilities/types.rs:107` (`Uuid::new_v4`); oxigraph | `uuid` feature `js`, `oxigraph` feature `js` on wasm32 | yes, 2 lines |
| Clock (`Instant`, `SystemTime`) | **0** — compiles, panics at run time | `sparql_store.rs:183,190` (`Instant::now`, inside the watchdog thread); `agent/capabilities/types.rs:101` (`SystemTime::now`); `ordering/linked_list.rs:322` `chrono::Utc::now` is fine (chrono `wasmbind` is on by default) | T11: `web-time` (or similar) | yes per site |
| Threads | **0** — compiles, wrong at run time | `sparql_store.rs:180` `Watchdog::arm` spawns a std thread for every request eval (`EvalGate::request`, `:126`). On wasm32 the spawn returns `Err`, and the code then **cancels the eval at once** (`:204-208`): every request-driven query would fail with a deadline error. `sparql_store.rs:2281,2310` `tokio::task::spawn_blocking` needs a blocking pool (no threads on wasm32) | A deadline that does not need a thread (check the clock inside oxigraph's cancellation polling, or a JS timer that cancels the token); call `store.query` inline on wasm | no; small design change |
| Config / paths | 0 (stubbed) | `sparql_store.rs:226` `crate::config::get_global_config()` (query deadline); `:1257` `std::fs` (persistent store only) | Pass the deadline in, do not read global config in the store | almost (1 site) |
| SQLite / rusqlite | **0** | only in `sparql_store.rs` tests (`crate::db::Ad4mDb`) | none for the core | – |
| Other executor coupling (not wasm-specific, but the core crate must cut it) | 0 after stubs (in r1: `url`/`uuid` gaps 11, `Session` methods 4, `LanguageController` 2, `flow_evaluator` 1) | `types/domain.rs` `RequestContext` → `agent::session`, `agent::capabilities`, `secret`; `types/core.rs` → `ai_service::providers`, `agent::signatures::verify_or_false`; `model_query/query.rs:1200-1240` → `LanguageController` (resolveLanguage transforms fetch expressions through the language runtime); `shacl_parser/role_gate_keys.rs:30` → `flow_evaluator::LINKED_TO_KEYS`; `sparql_store.rs:228` `query_gate` takes `RequestContext` | Split `types` (link/expression types vs request/auth/AI types); a trait for "resolve an expression" that the executor implements; move `LINKED_TO_KEYS` next to its reader | the constant: yes; the rest: no |

### Top items for the real crate (M6), in order of size

1. **Thread-based query watchdog** (`sparql_store.rs:168-210`). Compiles on wasm32, but with no threads every request query is cancelled at once. Needs a thread-free deadline. Also `spawn_blocking` at `:2281,:2310`. This is the only item that is a design change inside the query core.
2. **`types` split.** `types/domain.rs` drags in session/auth/capabilities/secret and AI provider code. The core needs `Link`, `LinkExpression`, `DecoratedLinkExpression`, `LinkStatus`, `ExpressionProof` and `verify_or_false` (ed25519; `ad4m-identity-core` already builds for wasm32).
3. **`LanguageController` call in `model_query/query.rs`** (resolveLanguage transforms): make it a trait object passed in.
4. **`deno_core::anyhow` → `anyhow`** (T10): 60 of the 87 raw errors, all mechanical.
5. **Cargo features**: tokio without `full` (one line, proved enough), `rocksdb` gate (4 sites, T14), `uuid`/`oxigraph` `js` on wasm32 (2 lines).
6. Clock (T11): 2 sites in the watchdog (go away with item 1), 1 `SystemTime::now` in capabilities (goes away with item 2).

## Verdict

**Go** for the Wasm query core, on the numbers of step 1 and the size of step 2 — with one condition:
the Oxigraph `sparopt` 32-bit cost fix (or the same fix upstream) must ship with it.

| Limit (PLAN.md S0b) | Measured | Result |
|---|---|---|
| Compressed Wasm < 5 MB | 0.60 MB brotli, 0.87 MB gzip (rustc 1.88); 0.56 MB brotli, 0.82 MB gzip (rustc 1.96.1). Oxigraph + this bench only | pass, 8× margin |
| Queries within 3× native | 1.09–1.55× (patched, rustc 1.88), 0.99–1.49× (rustc 1.96.1); stock Oxigraph: hydration query 85× | pass **with the patch**; fail without it |
| Memory per link within 2× native | 0.87× (2,890 vs 3,340 B per link) | pass |

Step 2: the copied query core (sparql_store + shacl_parser + model_query + their types) has **4 compile errors** on wasm32
once the mechanical items are done (`deno_core::anyhow` → `anyhow`, tokio without `full`, `js` features); those 4 are the
RocksDB gate (T14). The one real design item is the **thread-based query watchdog**, which compiles but would cancel every
request query on wasm32. The `types` split and the `LanguageController` call are the other seams M6 must cut.

## What I did NOT measure

- The real executor code running in Wasm. Step 1 is Oxigraph with the executor's quad shape and query shapes, not the
  executor's `SparqlStore`/`model_query` code; step 2 only type-checks (`cargo check`), it does not build or run the core.
- The full `model_query` pipeline cost (Rust-side hydration, JSON building, SHACL parsing): only the SPARQL part is timed.
- Queries copied 1:1 from `model_query` test output: Q1–Q5 are hand-written after `sparql_builder.rs` templates, adapted to
  the bench data; Q6 is a `sparql_store.rs` test query adapted to the data. I did not run the executor's tests to capture
  generated SPARQL.
- Size of a Wasm core with the executor's own code (`serde_json`, `regex`, `chrono`, `ts-rs` types, SHACL parser) on top of
  Oxigraph. The 0.6 MB brotli is Oxigraph + bench only.
- Write path cost beyond inserts: no removals, no `proof_write` read-before-write, no write log, no `added_delta` reads.
- Persistence (IndexedDB/OPFS) and load from persisted state; the in-memory store only.
- Wasm in a Worker, Firefox or Safari, mobile devices; wasm threads/SIMD builds.
- Memory under the 32-bit 4 GB linear-memory cap: 100k links grew linear memory by 288 MB (measured); by linear extrapolation (not measured) ~1M links
  would need ~2.9 GB, near the wasm32 cap. Not tried.
- Whether other Oxigraph plans hit the 32-bit cost saturation beyond the OPTIONAL case I found (the patch fixes the cost
  type everywhere, but I checked plans only for these 6 queries).
- Native numbers with the executor's RocksDB store (the native column is Oxigraph in-memory, like the Wasm column).

## Files

- `crate/` step-1 crate (`src/lib.rs` data + queries, `src/bin/native.rs`), `crate/vendor/sparopt` (patched copy)
- `crate/probe/` step-2 probe (copied executor modules + stubs), `crate/probe-deno/` real `deno_core` probe
- `bench/` runners (`node-bench.cjs`, `node-extra.cjs`, `browser.html`, `browser-run.mjs`, `probe-check.sh`), the patch
  (`sparopt-u64.patch`), every raw output (`*-run*.txt`, `r*-{wasm,native}.{log,err}`, `deno-wasm*.log`, `scaling.txt`)
- `pkg-u64/` patched Node build, `pkg/` stock Node build, `pkg-web/` patched browser build, `pkg-196/` patched Node build with rustc 1.96.1 (2.4–2.7 MB each, ~11 MB total). The `target/` dirs are deleted.
- Toolchains: step 1 built with rustc 1.88.0 (box default); step 2 with 1.96.1 (repo `rust-toolchain.toml`).

## Review (Lal, 2026-11-19 07:0x): CONFIRMED WITH CAVEATS

Full review: Lal's agent-mail `lal-web-exec-s0b-review.md`. Data reproduced Q2 in Node: stock 153.7 ms, patched 2.76 ms, 200 rows both.
- The sparopt diagnosis is right and broader: `usize` saturating math runs through `optimizer.rs` ~941-1125; the OPTIONAL lateral rewrite (~677) then favours lateral. The u64 patch covers every hunk; native is unchanged. This report has no query-plan dump (open).
- Watchdog: on wasm32 the thread `Builder::spawn` returns `Err` and `sparql_store.rs` 204-209 cancels at once, so every request-gated query fails. `query_async`/`query_cancellable` use `spawn_blocking` and need a wasm path. spareval 0.2.9's `CancellationToken` is only an `AtomicBool`, so a deadline needs a spareval change or interception; a JS timer cannot interrupt synchronous Wasm.
- Verdict scope: GO for an **in-memory Wasm query core**, NOT "browser ready". The 1.1-1.55x ratios are Node-only; Chrome was measured once (Q2 1.89x). Memory compares Wasm linear memory with native RSS, which are not the same measure.
