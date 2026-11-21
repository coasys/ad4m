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
| `write_log.rs` | The store's log of recent writes by generation: the links each one only added, or `Other` (`SparqlStore::additions_since`) |
| `model_query/` | Typed queries over SHACL shapes: `shape.rs` loads `ModelShape` from SHACL triples → `sparql_builder.rs` → `SparqlStore` → `hydration.rs` → `filtering.rs` → `getters.rs`/`relations.rs`/`projection.rs`. Entry: `execute_model_query` (`query.rs`). `mod.rs` has the pipeline diagram. `index_path.rs` gives the id phase from `scope_index/` when the query is one the index answers exactly (its module docs list the conditions); `index_path_tests.rs` checks it equals the SPARQL path on random queries |
| `scope_index/` | Ordered member lists per class and per `(parent, relation)` in the store's auxiliary key space (coasys/oxigraph#1), written in the same transaction as the quads (`SparqlStore::commit_indexed`). `mod.rs`: keys (`E`/`M`/`C`/`F`/`B`/`V`), `ScopeReader`, readiness. `writer.rs`: `RuleTable::apply`. `build.rs`: the resumable first build (a batch holds the writer lock for at most 512 members or 50 ms, then waits 5 ms without it; one build per store at a time, the others queued). `compact.rs`: head-removal tracking. A class is indexed when `PerspectiveShapeResolver::get_shape` answers a shape with a flag that `index_path::index_serves` accepts (its conformance is the flag alone) — the call also restarts a build that failed, on a cache hit too; other shapes always use SPARQL and get no index |
| `shacl_parser.rs` | Writer side: a class's links (`shape_links`; `parse_shacl_to_links` from JSON), which links on a class node are its own (`is_own_link`), links → shape (`shape_from_links`), `canonical_json`; flow JSON ↔ links (`parse_flow_to_links`, `parse_flow_from_links`). Types `SHACLShape`, `SHACLFlow`, `AD4MAction` |
| `perspective_instance/shapes.rs` | Class registration: `register_shapes` (diff, under `sdna_change_mutex`) and `shapes` / `class_names` over the cached registry |
| `instances/` | Validated instance writes (create, update, remove, collection items) against a class's SHACL shape: the shared core of the MCP `instance_*` tools and the `perspective.instance*` RPC methods. See `instances/AGENTS.md` |
| `subject_classes_of.rs` | Which classes a URI conforms to (SPARQL) |
| `sdna.rs`, `shacl_to_prolog.rs` | Prolog fact generation from links/SHACL. Only used when Prolog mode ≠ Disabled |
| `flow_classes.rs`, `flow_evaluator.rs`, `flow_spawn.rs`, `flow_semantic_check.rs`, `flow_context/` | SHACLFlow state machines: hard-wired classes, deterministic transition evaluation, spawn candidates, LLM flow context. Moving to `agentic/` (spec item 7) |
| `interpretation/` | LLM interpretation: text → typed subjects. `run.rs` orchestrates, `prompt.rs`, `parse.rs`, `dedup.rs`, `graph/{read,write}.rs`, `overlay/` (provenance + human accept). Depends on `ai_service::harness` |
| `agent_session.rs` | `perspective.runAgent`: a conversation with a model in one perspective over the static MCP tools in `SESSION_READS`, plus the writes the caller names from `SESSION_WRITES`. Both lists are explicit and perspective-scoped (a tool joins only by being added there), and `perspective_id` is forced. Same harness as interpretation, neutral prompt. Tests with a scripted model in `agent_session_tests.rs` |
| `auto_processor/` | Runs interpretation automatically over neighbourhood conversations: `config.rs`, `watcher.rs` (debounce/batch), `cursor.rs`, `claim.rs` (multi-agent claim), `events.rs` (pubsub step signals), `delta_read.rs` (a scope query's read of what added links gain it) |
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
  typed event (`Topic::LinkAdded/LinkRemoved/LinkUpdated`, one event to all owners via
  `publish_to_owners`, the main agent if unowned) → commit to
  link language if `LinkStatus::Shared`. Use the same sequence; a helper `apply_diff` is planned.
- SHACL triples are the source of truth for shapes. A class is its target class IRI; the name
  index is two links on it: `rdf://type ad4m://SubjectClass` and `ad4m://shape <shape URI>`.
  Links from older versions (`ad4m://has_subject_class`, `ad4m://sdna`, `ad4m://has_shacl`,
  `ad4m://shacl_shape_uri`) are not read and not written. `shape_cache` is keyed by the key
  the caller passed (a target class IRI or a name); it and the registry are cleared whole
  (`clear_shape_cache`) by `register_shapes` and by every local or inbound diff that touches
  shape links, so an IRI key and a name key never hold a stale or crossed shape.
  `get_shape(class)` → `Arc<ModelShape>`, `class` a target class IRI or a name.
- `register_shapes` compares the own links a shape gives with the own links stored, as
  triples. Equal sets write nothing (`unchanged`); otherwise one Shared diff adds what is
  missing and removes every copy of each stale own link. Copies of an expected triple stay
  (two peers that registered the same shape agree); links other apps put on the class, shape
  or property nodes stay. It answers once the registry and the shape cache hold the new shape.
- A class is addressed by its target class IRI; the SDK sends it on every call. Many classes
  may share a name (`class_name_of`, the IRI's local name): `we://Topic` and `flux://Topic`
  coexist. `model_query::shape::find_class` matches an IRI exactly; a bare name (MCP tools,
  old callers) reads the least target class IRI of that name. A class with two `ad4m://shape`
  links reads the least shape URI (`shacl_parser::current_shape_uri`), in the registry and
  in `load_shape` alike. `register_shapes` refuses, writing nothing, one target class twice
  in one call, a shape URI another class holds, and a shape URI not ending in `{Name}Shape`.
  A relation resolves through `ShapeRelation::target_class` (the class owning its
  `sh:class` shape), the name only when that shape is not stored.
- Every answer names a class by IRI: `RegisterResult`, `__subjectClass`/`__subjectClasses`
  (polymorphic hydration, unions, abstract reads; a union asked by name echoes the name as
  `__subjectClass`), `abstract_classes::Resolution` (classes, views) and its aliases, the
  containment edges, stored auto-processor `interpretation_classes` and signed flow
  `OutputRef`s (resolved by `get_shape` before storing/signing; a class not registered yet is
  kept as given). Abstract reads walk `concrete_subject_classes` and writes intersect
  `subject_class_iris_of` with the resolution, so two classes of one name read and write
  apart. MCP `describe_perspective` and the CLI list `class_keys` (the name, the IRI when
  shared); the per-class MCP tools are named by `class_names`, so a shared name gets tools
  for its least IRI only. `subject_classes_of` (the `subjectClassesOf` RPC, and flow spawn
  matching a flow's `inputTypes`) still answers names; `subject_class_iris_of` the IRIs.
- The executor's own paths address the six hard-wired classes by IRI (`ad4m://FlowInstance`,
  `flow_classes::FLOW_INSTANCE_CLASS` and the like), so an app's `app://FlowInstance` is
  stored beside them and never read in their place.
- Hard-wired classes (`hardwired_class::ensure_subject_classes`) are upgrade-only: written when
  missing or when the stored shape lacks the newest path / declares a removed one
  (`HardwiredClass`), never over a newer stored shape. Only the executor registers them:
  `create_subject` registers one not stored yet before its first instance
  (`hardwired_class::ensure_before_write`), and a model query of one not stored yet reads as
  empty without waiting or writing. A stored revision of it (an app's `@Model` copy, a
  newer executor's) is used as it is. The SDK's `@Model` copies (`FlowModels.ts`,
  `InterpretationModels.ts`) read them and are never registered by the SDK.
- `get_subject_data` currently routes through Prolog and, while Prolog is disabled, returns only
  `author`/`timestamp` with no other properties. Migrating it to `model_query` is planned (spec
  item 1). Prefer `model_query` for reads.
- `commit`: holds `commit_order` from its pending-queue check until the diff is sent or queued,
  so a commit whose language call fails is queued ahead of every later one. The immediate-commit
  counter has its own short lock: `set_immediate_commits` never waits for a language call.
- `pending_diffs_loop`: a failed `commit_pending_diffs` is logged at warn and retried after
  100 ms doubling to 30 s, reset on success (tests: `perspective_instance_pending_diffs_tests.rs`).
  A batch is one diff, and a receiver applies its removals before its additions, so the rows are
  never concatenated: `merge_pending_diffs` (db.rs) keeps only the last operation on each link
  expression (`link_identity`: author, timestamp, source, target, and the predicate normalised
  as the store keeps it, `Some("")` = `None`) in queue order, a row's removals before its
  additions. A later removal drops the earlier addition and stays; a later addition drops the
  earlier removal; a repeated `+x` or `-x` is sent once.
  One commit per batch still.
  When a batch of several diffs fails, the oldest diff is sent alone, then the rest, merged from
  the rest's own rows (`get_pending_diffs_among`). A diff that
  fails while the language accepts the rest is parked: its `perspective_diff` row stays with
  `is_pending = 0` (`Ad4mDb::park_pending_diffs`; the export lists it), logged at
  error level by row id (its links stay local). The failure may have been transient, so the row
  is kept, but it is never sent again after its successors: that would reorder it behind newer diffs.
  The fallback sync (`ensure_public_links_are_shared`) skips every unsent addition, queued or
  parked: the queue owns them. The commit loop
  holds `share_links_mutex` from its first send of a batch until the clear or the park, so a
  fallback sync runs either before the batch is sent or after it is cleared or parked. The lock
  spans up to three `link_language.commit` calls, and a language commit has no timeout on this
  path: a language whose commit hangs also holds up a neighbourhood publish's
  `ensure_public_links_are_shared` and the fallback pass. Immediate sends from `commit` are
  outside this lock, so a fallback sync can run while an immediate send is in flight and submit
  the same link again.
- The fallback sync never pushes most of the shared set (`fallback_sync.rs`,
  `fallback_push_refused`): more than 20 links and more than half of the Shared links missing
  from `render()` means the language's view of the remote is short (a partial or empty
  mirror), so it logs an error, commits nothing and returns false. A language that does not
  know the remote returns null from `render()` (skip). Only the creator's push at publish
  (`share_links_after_publish`: `publish` and `attach_link_language` in
  `NeighbourhoodCreationInitiated`) sends the whole set: the links made before the
  neighbourhood existed were never sent. When that push fails (commit error, or `render()`
  null while the language's first render runs), it queues them as one pending diff, so the
  queue, not the refusing fallback, delivers them.
- Background loops (`nh_sync_loop`, `pending_diffs_loop`, `fallback_sync_loop`,
  `subscribed_queries_loop`, `notification_check_loop`, auto-processor supervisor) exit on
  `is_teardown`; `teardown_background_tasks` must be called on remove.
- `SparqlStore` bumps a write generation after every committed write (`write_generation`,
  `watch_writes`). The auto-processor watch loop sleeps on it while idle (60 s fallback); a
  write-woken idle tick still waits out the old idle back-off (doubling 0.5 s to 4 s) since the
  last tick that read the store, so writes that add no turn never read more often than the poll
  did. It keeps its config, transcript and cursor reads in `WatcherState` (`TickReads`) while the
  generation is unchanged; the transcript keeps text only for turns that may still be queued
  (consumed or out-of-window turns are id + timestamp), one copy per processor per loop (one loop
  per perspective and per online managed user). After writes that only added links, the tick
  reads only what they add to the kept transcript (`SparqlStore::additions_since`,
  `auto_processor/delta_read.rs`: the scope query with one triple pattern at a time restricted
  by `VALUES` to the added terms) and merges it where a fresh read puts each turn (`ORDER BY
  ?timestamp` transcripts are kept sorted by timestamp, then turn id; any other `ORDER BY` is read
  whole: `delta_read::transcript_order`); a removal, a reload, a log that no longer reaches
  back (1024 links or 1024 writes; an `Other` write drops everything before it), or a scope query that is not monotone (`OPTIONAL`, `MINUS`, `EXISTS`,
  `LIMIT`, aggregates, paths) reads it whole. A new store write path must call
  `note_write(WriteDelta)`, `Added` only when it removed nothing and every reifier is new, or the
  loop misses that change until some other write lands. With turns pending but no default
  LLM set and spawned (`watcher::model_available`), a tick drains nothing and returns
  `TickOutcome::NoModel`, and the loop backs off as if idle, waiting on writes and on
  `AIService::watch_llm_readiness` (bumped by `set_default_model` and by every LLM worker
  `spawn_llm_model` registers), so a model configured while turns wait is used at once.
- **File descriptors.** Every perspective keeps one RocksDB store open
  (`SparqlStore::new`), and only `PerspectiveInstance` holds it: the store's descriptors close
  when the last clone of the removed instance drops, i.e. once its background loops have seen
  `is_teardown`. A store holds 17 fixed descriptors (LOCK, LOG, MANIFEST, WAL, one directory
  handle per column family) plus the SSTs in its table cache. `SparqlStore::new` opens with
  `SPARQL_STORE_MAX_OPEN_FILES` (266): oxigraph's default is the process fd limit minus 48 *per
  store*. RocksDB splits the table cache over 64 shards (oxigraph exposes `max_open_files` and
  `fd_reserve`, not the shard bits); 266 keeps four SSTs per shard. One per shard (74) made
  SSTs in the same shard evict each other, and lookups ran up to 8× slower. Budget:
  `SPARQL_STORE_FD_BUDGET` = 274 per store (a full cache); a typical store holds about 70 and
  a small one 20-30, so the soft fd limit must be raised at startup (`fd_limit`, #1462).
  `sparql_store_fd_tests.rs` (Linux, `/proc/self/fd`) pins the option through the OPTIONS
  file, measures a typical store against the budget, and checks the release on drop. Never open a second `SparqlStore` on a path that
  is already open: RocksDB's LOCK refuses it, and `PerspectiveInstance::new` panics.
- **Scope index writes.** Every store write path ends in `SparqlStore::commit_indexed`, under
  the writer lock, so the index commits with the quads. `E`/`M`/`F` keys are put only when absent
  and removed only with `aux_single_delete`; `C`/`B`/`V` keys only with `aux_delete` (a single
  delete over a key put twice leaves the older value). Membership is a flag link with
  `proofValid "true"`; a member's key is its earliest valid flag time. Read a class only when
  `scope_index_readiness` is `Ready`. A store this executor opened has the auxiliary column
  family: an executor without it cannot open the store.

## Do / don't

- Don't add methods to `perspective_instance.rs`; add a new file with `impl PerspectiveInstance`.
- Don't decode `literal://` payloads by hand: several copies exist (spec item 8 unifies them);
  reuse `model_query::utils::parse_literal_value` until then.
- Tests that need a real instance: `interpretation_test_support.rs` (`pub(crate)`, `cfg(test)`).
- `shape_links` is the only SHACL shape writer: the SDK's `registerShapes` and `@Model`
  registration both send `SHACLShape.toJSON()` to `perspective.registerShapes`. Every JSON
  literal is canonical (sorted keys) on both sides, so the same shape gives the same links
  and the same hash. A property node is `{ns}{Class}.{name}`. A new shape field needs the
  Rust field, a link here, and `SHACLShape.fromLinks` in core; `fixtures/shacl_writer_golden.json`
  pins the links and the SDK test decodes the same file, so update it in the same change.
- Flow transition URIs are `{ns}{flow}.transition/{from}/{to}/{action}`, each part
  percent-encoded; `SHACLFlow.transitionUri` in core builds the same string.
