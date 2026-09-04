# prolog_service/ — agent guide

Scryer Prolog engines for Social DNA queries. **Disabled at runtime by default**
since 2026-02-11: `pub static PROLOG_MODE: PrologMode = PrologMode::Disabled`
(`mod.rs`). Kept as an option for the future; spec item 1 makes the mode
configurable (`Ad4mConfig.prolog_mode` / `AD4M_PROLOG_MODE`).

## Modes (`PrologMode`)

| Mode | Behaviour |
|---|---|
| `Disabled` | no engines; queries return empty/`False`; facts updates no-op |
| `Simple` | one engine per perspective, lazy fact reload on query (`SimpleEngine` in `mod.rs`) |
| `SdnaOnly` | engine with SDNA facts only, no link data (`sdna_pool.rs`) |
| `Pooled` | `engine_pool.rs`: N engines round-robin + per-source filtered sub-pools (`filtered_pool.rs`, gated by `FILTERING_THRESHOLD`) + SDNA pool |

## Files

| File | Role |
|---|---|
| `mod.rs` | `PrologService` global (`get_prolog_service()`), mode switch, `SimpleEngine`, fact assembly |
| `engine.rs` | Single scryer engine wrapper: load facts, run query, parse results |
| `engine_pool.rs` | `PrologEnginePool` (complete pool) + owns filtered/sdna pools, cleanup + state-logger tasks. 73% tests, many of which belong to the sub-pools |
| `filtered_pool.rs` | `FilteredPrologPool`: facts reachable from one source; reachability + regex chunking |
| `sdna_pool.rs` | SDNA-only pool |
| `pool_trait.rs` | `FilteredPool` trait + `PoolUtils` shared by all three pools |
| `source_filtering.rs`, `assert_utils.rs`, `embedding_cache.rs`, `types.rs` | fact filtering, assert-query parsing, cache, `QueryResolution`/`QueryMatch` |

Fact generators live in `perspectives/sdna.rs` (`get_data_facts`, `get_sdna_facts`,
`get_static_infrastructure_facts`) and `perspectives/shacl_to_prolog.rs` (SHACL →
`subject_class/…` facts). Term → JSON marshalling is in `perspectives/utils.rs`
(moving here as `term_json.rs`).

## Rules

- Every entry point must check the mode first and return an explicit error when
  disabled (spec item 1), never a silent empty result.
- The four copies of "static + sdna + data facts" assembly (`mod.rs`, `engine_pool.rs`,
  `filtered_pool.rs`, `sdna_pool.rs`) should become one function before adding a fifth.
- Pool tests: run with `--test-threads=1`; they share the global service.
