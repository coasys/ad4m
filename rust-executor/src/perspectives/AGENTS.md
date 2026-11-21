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
| `write_log.rs` | Removed (#1794): the auto-processor's wake feed is now `subscription_triggers::Writes`, the per-commit write info `check_subscribed_queries` already builds |
| `model_query/` | Typed queries over SHACL shapes: `shape.rs` loads `ModelShape` from SHACL triples → `sparql_builder.rs` → `SparqlStore` → `hydration.rs` → `filtering.rs` → `getters.rs`/`relations.rs`/`projection.rs`. Entry: `execute_model_query` (`query.rs`). `mod.rs` has the pipeline diagram. A `path` read is an edge walk: every edge keeps the proof and link-status guards, but class conformance and `where` apply only to the reached nodes that are returned — an intermediate that fails either is walked through (`levels` keeps its per-level cut). Traversal (`walk_levels`): output de-dupes per node; the next-depth frontier de-dupes per depth — under a multi-hop `path` a node reported at one depth still walks at the next (its hop predicates differ); under `levels` only newly reported nodes walk on. A pushed `where` comparison on a `resolveLanguage` property keeps an undecodable expression IRI (a raw IRI, not a `literal:` value) for hydration to mark `__unresolved` rather than rejecting it at the store, so the retry clock can admit the row when the language arrives |
| `shacl_parser.rs` | Writer side: a class's links (`shape_links`; `parse_shacl_to_links` from JSON), which links on a class node are its own (`is_own_link`), links → shape (`shape_from_links`), `canonical_json`; flow JSON ↔ links (`parse_flow_to_links`, `parse_flow_from_links`). Types `SHACLShape`, `SHACLFlow`, `AD4MAction` |
| `perspective_instance/shapes.rs` | Class registration: `register_shapes` (diff, under `sdna_change_mutex`) and `shapes` / `class_names` over the cached registry |
| `instances/` | Validated instance writes (create, update, remove, collection items) against a class's SHACL shape: the shared core of the MCP `instance_*` tools and the `perspective.instance*` RPC methods. See `instances/AGENTS.md` |
| `subject_classes_of.rs` | Which classes a URI conforms to (SPARQL) |
| `sdna.rs`, `shacl_to_prolog.rs` | Prolog fact generation from links/SHACL. Only used when Prolog mode ≠ Disabled |
| `flow_classes.rs`, `flow_evaluator.rs`, `flow_spawn.rs`, `flow_semantic_check.rs`, `flow_context/` | SHACLFlow state machines: hard-wired classes, deterministic transition evaluation, spawn candidates, LLM flow context. Moving to `agentic/` (spec item 7) |
| `interpretation/` | LLM interpretation: text → typed subjects. `run.rs` orchestrates, `prompt.rs`, `parse.rs`, `dedup.rs`, `graph/{read,write}.rs`, `overlay/` (provenance + human accept). Depends on `ai_service::harness` |
| `agent_session.rs` | `perspective.runAgent`: a conversation with a model in one perspective over the static MCP tools in `SESSION_READS`, plus the writes the caller names from `SESSION_WRITES`. Both lists are explicit and perspective-scoped (a tool joins only by being added there), and `perspective_id` is forced. Same harness as interpretation, neutral prompt. Tests with a scripted model in `agent_session_tests.rs` |
| `auto_processor/` | Runs interpretation automatically over neighbourhood conversations: `config.rs`, `watcher.rs` (debounce/batch), `cursor.rs`, `claim.rs` (multi-agent claim), `events.rs` (pubsub step signals). The transcript scope is a model query built from `source_class` / `source_text` / `source_query` (#1794) |
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
  `watch_writes`). The auto-processor watch loop no longer wakes on the generation alone:
  `write_link_diff` and `persist_remote_diff` broadcast each batch's
  `subscription_triggers::Writes` (added and removed link terms) on `commit_writes`
  right where its store writes commit (`broadcast_commit_write`, under `persist_lock`,
  so an entry's place in the channel is its commit's place in the write order), each
  entry carrying `generations` — the write generations its own writes took, one for a
  transactional diff, one per link for the fallback — a **bounded** `broadcast` channel
  (`COMMIT_WRITES_CAPACITY`, 256 commits — it bounds memory, not correctness), and the loop
  sleeps on that (60 s fallback). Entries are drained independently: a matching diff is
  still readable after any number of unrelated diffs while all of them fit in the buffer —
  an unrelated write costs an evaluation, never coverage. A receiver that lags past the
  buffer gets `Lagged`, which must be treated as a wake: a missed write could have matched. A processor builds a
  `subscription_triggers::ModelTrigger` from its
  `source_class` + `source_query` — the same rules a model subscription would get — and the
  loop wakes only when a committed write's `Writes` match it (`ModelTrigger::matches`) or
  carry a config predicate (`config_write_predicates` → reload). `None` writes (a diff
  carried a predicate-less link) and an unbuilt trigger wake conservatively. A
  one-hop `parent` scope also gates `joins`/`scan` matches to nodes linked to a
  scope anchor through the scope's edge predicates (`TriggerRules::scoped`), so
  a class-flag write under another parent does not wake a scoped subscription
  or processor; deeper walks (`transitive`, `levels`, a multi-hop `path`) stay
  ungated, because one neighbour read cannot prove membership. While turns are
  pending, the kept transcript's freshness is decided by the same trigger —
  `auto_processor_mark_stale` records a matched write, a schema write
  (`Writes::touches_shacl`, the `inbound_touches_shacl` classification: a
  SHACL/SDNA batch changes the shape every kept read ran under, whatever the
  trigger's predicates), or a `Lagged` / unclassifiable one against the
  read's scope key so the tick re-reads instead
  of draining stale turns. The tick's own feed drain (`watcher.commit_feed`)
  must first prove coverage: the `generations` of all drained entries have to
  account for every generation in `(seen, current]`, `seen` being the
  watermark the last drain proved. On full coverage only matched transcripts
  stale and `seen` advances through the proven generations — including any
  proven above the generation the tick read (a fed write that landed
  mid-tick), which the drain keeps so the next tick does not owe it. A gap —
  a store write outside the commit path, a `Lagged` hole, a `None` entry —
  stales everything and moves `seen` past the tick's generation. There is no
  in-flight wait: a write that has taken its generation but not yet broadcast
  reads as a gap and stales all once, conservatively. Pending turns that fell out of the scope are dropped
  before drain, and a failed re-read keeps them pending and skips the drain. A wake re-runs
  the transcript as a full model query; `TickReads` (config, transcript, cursor) is still
  kept while the generation is unchanged, the transcript keeping text only for turns that
  may still be queued. The transcript query requests `links` provenance for
  `source_text`, and each turn's speaker/timestamp come from the link row hydration
  selected for the text — a `links` predicate the instance query already
  fetches is served from those same rows (one read, no second query), so the
  signer is provably the text link's; a body link signed by someone other
  than the instance creator attributes to its own signer; an instance with
  no provable text-link row is skipped.
  A turn whose `source_text` `resolveLanguage` fails (the resolver marks the property
  under `__unresolved`) is likewise skipped: it is never queued or cursor-recorded, and
  a transcript read that held unresolved turns expires on `UNRESOLVED_TEXT_RETRY_MS`
  rather than the write generation, so a later language install admits the turn without
  a store write. The cursor is a model query too (`load_processed_sources` over
  `InterpretationRun`, scoped `where processor = …`), so unverifiable run links cannot
  mark turns processed. With turns pending but no default
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
