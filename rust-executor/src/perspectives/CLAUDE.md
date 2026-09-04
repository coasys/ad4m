# perspectives/ — agent guide

A Perspective = one local graph of links (+ optional neighbourhood sync). This
module owns the registry, the instance type, storage, and everything that queries
or writes the graph. Split plan: spec items 3, 7, 8.

## Files

| File | Role |
|---|---|
| `mod.rs` | Global registry `PERSPECTIVES` (uuid → `PerspectiveInstance`), `add/update/remove_perspective`, `get_perspective(uuid)`, `all_perspectives()`, link-language signal routing (`handle_*_from_link_language`), telepresence publish, import/export |
| `perspective_instance.rs` (7.6k) | **The** instance: link CRUD + batches, neighbourhood sync/commit loops, SDNA/SHACL registration, subject create/update/read, `execute_commands`, query subscriptions, notifications, telepresence signals, Prolog facade, auto-processor tick. One `impl`; being split into `perspective_instance/` |
| `sparql_store.rs` | Per-perspective oxigraph store. Links stored as reified triples. `add_link/remove_link`, `query_links`, `query_arbitrary` (read-only SPARQL from clients), custom SPARQL fns (`parse_literal`, `strip_html`), named-graph → reifier migration |
| `model_query/` | Typed queries over SHACL shapes: `shape.rs` loads `ModelShape` from SHACL triples → `sparql_builder.rs` → `SparqlStore` → `hydration.rs` → `filtering.rs` → `getters.rs`/`relations.rs`/`projection.rs`. Entry: `execute_model_query` (`query.rs`). `mod.rs` has the pipeline diagram |
| `shacl_parser.rs` | Writer side: SHACL class JSON → links (`parse_shacl_to_links`), flow JSON ↔ links (`parse_flow_to_links`, `parse_flow_from_links`). Types `SHACLShape`, `SHACLFlow`, `AD4MAction` |
| `subject_classes_of.rs` | Which classes a URI conforms to (SPARQL) |
| `sdna.rs`, `shacl_to_prolog.rs` | Prolog fact generation from links/SHACL. Only used when Prolog mode ≠ Disabled |
| `flow_classes.rs`, `flow_evaluator.rs`, `flow_spawn.rs`, `flow_semantic_check.rs`, `flow_context/` | SHACLFlow state machines: hard-wired classes, deterministic transition evaluation, spawn candidates, LLM flow context. Moving to `agentic/` (spec item 7) |
| `interpretation/` | LLM interpretation: text → typed subjects. `run.rs` orchestrates, `prompt.rs`, `parse.rs`, `dedup.rs`, `graph/{read,write}.rs`, `overlay/` (provenance + human accept). Depends on `ai_service::harness` |
| `auto_processor/` | Runs interpretation automatically over neighbourhood conversations: `config.rs`, `watcher.rs` (debounce/batch), `cursor.rs`, `claim.rs` (multi-agent claim), `events.rs` (pubsub step signals) |
| `ordering/` | CRDT ordering for `@HasMany`. Parsed into `ModelShape.ordering` but **not wired** into hydration/writes yet (spec D2) |
| `hardwired_class.rs` | Registration of hard-wired subject classes (used by flows and overlay) |
| `migration.rs` | rusqlite → SPARQL link migration and other one-shot data migrations |
| `memory_diagnostics.rs` | Periodic memory stats |
| `utils.rs` | Misnamed: Prolog `Term` → JSON marshalling only. Moving to `prolog_service/term_json.rs` (spec item 1) |
| `*_e2e.rs`, `interpretation_test_support.rs` | Real-LLM e2e tests and their scaffolding. Not `#[ignore]`d yet |

## Invariants

- Lock order inside `PerspectiveInstance`: `batch_store` → `persisted`. `prolog_update_mutex`
  write is held across `PrologService` awaits on purpose. Never hold `persisted` across an await;
  the idiom is `let h = self.persisted.lock().await.clone();`.
- Every link write goes: persist to `SparqlStore` → Prolog facts (no-op when disabled) →
  pubsub (`PERSPECTIVE_LINK_ADDED/REMOVED/UPDATED`, fanned out per owner) → commit to
  link language if `LinkStatus::Shared`. Use the same sequence; a helper `apply_diff` is planned.
- SHACL triples are the source of truth for shapes. `shape_cache` on the instance is
  invalidated by `add_sdna_inner`. `get_shape(class)` → `Arc<ModelShape>`.
- `get_subject_data` currently routes through Prolog and returns no properties while Prolog is
  disabled (spec item 1 fixes it). Prefer `model_query` for reads.
- Background loops (`nh_sync_loop`, `pending_diffs_loop`, `fallback_sync_loop`,
  `subscribed_queries_loop`, `notification_check_loop`, auto-processor supervisor) exit on
  `is_teardown`; `teardown_background_tasks` must be called on remove.

## Do / don't

- Don't add methods to `perspective_instance.rs`; add a new file with `impl PerspectiveInstance`.
- Don't decode `literal://` payloads by hand: several copies exist (spec item 8 unifies them);
  reuse `model_query::utils::parse_literal_value` until then.
- Tests that need a real instance: `interpretation_test_support.rs` (`pub(crate)`, `cfg(test)`).
