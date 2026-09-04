# rust-executor refactoring spec

Date: 2026-09-04. Status: proposed. Owner: core team.

Companion navigation docs: `rust-executor/CLAUDE.md` and the per-module `CLAUDE.md`
files it links to. Keep those in sync as items here land.

## 0. Goals and ground rules

Goal: a crate whose structure can be held in one head. Humans and AI agents should
be able to answer "where does X live, what may I touch, what must I not break"
from a directory listing plus a 30-line `CLAUDE.md`, without reading 7,000-line
files.

Ground rules for every refactoring PR:

1. **Mechanical moves and behaviour changes never share a commit.** A "move
   code into submodules" commit must be reviewable by `git diff --stat` plus a
   compile. Behaviour fixes found on the way go into their own commit, called
   out in the PR description.
2. **No file over ~800 lines of production code, no `mod.rs` over ~300.** Tests
   count separately (see §5.3). Existing files above the cap are listed in §2 and
   are the ones this spec splits.
3. **Tests pass before and after:** `cd rust-executor && pnpm test` (which is
   `cargo test --release -- --test-threads=1`) plus the integration suites in
   `tests/js` (`pnpm run test-main` from the repo root). A PR that changes a WS
   handler or MCP tool must run the relevant integration suite.
4. **Prolog stays an option.** `PrologMode::Disabled` is the runtime default and
   has been since 2026-02-11 (`prolog_service/mod.rs:51`). The engine, pools and
   SHACL→Prolog bridge are kept, isolated, and made switchable through config so
   they can come back without a rewrite. Nothing user-visible may depend on Prolog
   silently returning empty results (see item 1).
5. **Every new directory gets a `CLAUDE.md`** (≤ 40 lines: purpose, files, entry
   points, invariants, "do not"), and `AGENTS.md` pointing at it.
6. **Keep `pub` surface stable** for `rust-client`, `cli` and the JS SDK. Moving a
   type between modules requires a `pub use` re-export at the old path for one
   release, unless the old path was crate-private.

## 1. Current state (measured 2026-09-04)

| Metric | Value |
|---|---|
| Rust files under `rust-executor/src` | 195 |
| Total lines | 124,170 |
| Test-ish lines (in-file `mod tests` + `*_tests.rs`/`*_e2e.rs`/support) | ~54,400 (44%) |
| Files > 2,000 lines | 9 |
| `lazy_static`/`OnceCell` service singletons | ~20, ~400 call sites |
| Reason `package.json` runs `--test-threads=1` | shared global singletons |

Files above the 800-line production cap, with the biggest structural issue each:

| File | Lines | Issue |
|---|---|---|
| `perspectives/perspective_instance.rs` | 7,608 | one 5,572-line `impl` block, 12 responsibility clusters |
| `perspectives/model_query/integration_tests.rs` | 7,385 | single test file, 98 tests, all subjects mixed |
| `db.rs` | 5,410 | one `impl Ad4mDb` with 124 methods; export/import alone 930 lines |
| `perspectives/sparql_store.rs` | 3,866 (62% tests) | store + term codec + custom SPARQL fns + link query builder + migration |
| `prolog_service/engine_pool.rs` | 3,772 (73% tests) | ~1,900 test lines belong to filtered/sdna pools |
| `languages/mod.rs` | 2,874 (0 tests) | language runtime + install + neighbourhood creation + DNA templating + expression layer |
| `ai_service/mod.rs` | 2,802 | models + LLM spawn (423-line fn) + tasks + embeddings + whisper + billing |
| `perspectives/interpretation_e2e.rs` | 2,583 | real-LLM e2e, not `#[ignore]`d |
| `api/perspectives_ws.rs` | 2,438 | 44 handlers, two 200-line interpretation handlers |
| `perspectives/model_query/sparql_builder.rs` | 2,243 | conformance + where-clause compiler + quantifiers; tests interleaved |
| `perspectives/shacl_parser.rs` | 2,189 | class writer + flow writer + flow reader + literal codec |

Cross-cutting findings that drive the ordering below:

- **Prolog is off, but three live paths still route through it.**
  `PerspectiveInstance::get_subject_data` (`perspective_instance.rs:4900`) queries
  Prolog, receives `Matches(vec![])`, and returns an object with only
  `author`/`timestamp`. It is reachable via WS `perspective.getSubjectData`, MCP
  `get_subject_data`, and the harness bridge. WS `perspective.queryProlog` and MCP
  `infer` advertise Prolog and return empty.
- **`ai_service::harness` ↔ `perspectives::interpretation` import cycle.**
  `harness/{mod,propose,flow_propose}.rs` import `perspectives::{interpretation,
  auto_processor::events, flow_context}` and `mcp::shacl`; `interpretation/run.rs`
  imports `harness`. `api/openai_compat/harness_bridge.rs` + `tool_grammar.rs`
  (~1,460 lines) are harness infrastructure parked inside the OpenAI wire shim,
  making `perspectives` depend on `api`.
- **MCP `tools/subjects.rs` duplicates `tools/dynamic.rs`** and has diverged:
  static `set_subject_property` lacks the batch wrapper the dynamic one has;
  static `get_subject_data` lacks the encoded-literal fallback. Two live bugs.
- **WS capability checks are copy-pasted 140×**; five registered methods have no
  check at all (`runtime.info`, `runtime.tlsDomain`, `runtime.freeHostingEnabled`,
  `runtime.hostRates`, `agent.isLocked`).
- **`db.rs` has no schema versioning**; four `ALTER TABLE` migrations run under
  `let _ =` on every boot (`db.rs:329-351`) and swallow all errors.
- **Duplicate types without conversions**: `core::Perspective` vs
  `domain::Perspective` (hand-converted in `agent/mod.rs:218-233`),
  `core::LanguageRef` == `domain::LanguageRef`, ~10 dead request structs in
  `api/types.rs`.
- **Stale docs**: `rust-executor/LANGUAGE_RUNTIME_PHASE1.md`, `PHASE1_FIX.md`,
  root `PHASE1_SUMMARY.md` describe the pre-Phase-2 JS language controller
  (`core.languageController` has zero hits in `src`). `rust-executor/README.md`
  still says js_core runs "legacy executor remainders" and Prolog serves SDNA
  queries. `SHACL_SDNA_ARCHITECTURE.md` says Prolog is "still available".

## 2. Target layout

```
rust-executor/src
├── lib.rs                  boot sequence only (see item 11: AppContext)
├── config.rs
├── app_context.rs          (item 11) owned service handles
├── db/                     (item 4)
│   ├── mod.rs              Ad4mDb struct, global accessor, Ad4mDbResult
│   ├── schema.rs           DDL + versioned migrations
│   ├── helpers.rs          json_col, query_all, query_opt, upsert_json
│   ├── perspectives.rs  links.rs  diffs.rs  expressions.rs
│   ├── ai.rs  notifications.rs  agent.rs  users.rs  billing.rs
│   ├── settings.rs  email_verification.rs  portability.rs
│   └── tests/
├── api/                    (item 6)
│   ├── ws_handler.rs       HandlerMap + CapSpec + typed registration
│   ├── *_ws.rs             one file per RPC namespace, ≤ 800 lines
│   ├── interpretation_ws.rs  shacl_ws.rs   (split out of perspectives_ws.rs)
│   └── openai_compat/      wire format only; harness bridge moves out
├── mcp/                    (item 2)
│   └── tools/              static tools delegate to dynamic handlers
├── agentic/                (item 7) — LLM interpretation subsystem
│   ├── harness/            ← ai_service/harness
│   ├── interpretation/     ← perspectives/interpretation
│   ├── flow/               ← perspectives/flow_*  + flow_context/
│   ├── auto_processor/     ← perspectives/auto_processor
│   ├── events.rs           ← auto_processor/events.rs
│   ├── bridges/            ← mcp/tools/harness_bridge.rs, api/openai_compat/harness_bridge.rs
│   └── tool_grammar.rs     ← api/openai_compat/tool_grammar.rs
├── perspectives/
│   ├── mod.rs              registry (PERSPECTIVES map) + add/update/remove
│   ├── routing.rs          link-language signal routing + telepresence publish
│   ├── perspective_instance/   (item 3)
│   │   ├── mod.rs          struct, new, shared helpers, lock-order doc
│   │   ├── types.rs        Action, Command, SubjectClass*, Parameter…
│   │   ├── links.rs  batches.rs  sync.rs  sdna.rs  commands.rs
│   │   ├── subscriptions.rs  telepresence.rs  notifications.rs
│   │   ├── lifecycle.rs  shapes.rs  prolog.rs  predicates.rs
│   │   └── tests/
│   ├── sparql_store/       (item 8)  mod / terms / functions / reification / link_query / migration / tests
│   ├── shacl/              (item 8)  types / flow_types / class_writer / flow_writer / flow_reader / literal / uri / vocab
│   ├── model_query/        already well factored; sparql_builder split + tests/ dir (item 8)
│   ├── ordering/           wire into hydration or delete (decision D2)
│   ├── subject_classes_of.rs  migration.rs  memory_diagnostics.rs  hardwired_class.rs
├── prolog_service/         (item 1) kept; mode from config; term_json.rs moved in; tests split
├── languages/              (item 9)  controller / install / registry / expressions; template → holochain_service
├── holochain_service/      (item 10) actor.rs (dispatch), bundle.rs (pack/unpack), dna_template.rs
├── ai_service/             (item 9)  models / llm / tasks / embed / transcription; harness moves to agentic/
├── unyt/                   (item 9)  dna.rs / client.rs / payments.rs
├── wallet/                 (item 9)  crypto / backend / local / shared
├── agent/                  (item 9)  context / signing / service / users / persistence / capabilities
├── js_core/                unchanged; delete dead placeholder + main.js (item 1)
├── types/                  core.rs (domain), domain.rs (wire) — dedup per item 5
└── runtime_service/  entanglement_service/  billing.rs  email_service.rs  user_management.rs  pubsub.rs
```

## 3. Work items

Ordered by (impact ÷ effort). Each item is one PR unless noted. Sizes: S ≤ 1 day,
M ≤ 3 days, L ≤ 2 weeks.

### Item 1 — Prolog isolation and dead-path fixes (M)

Scope: keep the engine, stop lying about it, make it switchable.

Steps:

1. **Config-driven mode.** Replace `pub static PROLOG_MODE: PrologMode =
   PrologMode::Disabled` with a `OnceLock<PrologMode>` set once at boot from
   `Ad4mConfig.prolog_mode: Option<PrologMode>` (serde, lowercase strings
   `disabled|simple|sdna_only|pooled`) with env override `AD4M_PROLOG_MODE`.
   Default stays `Disabled`. Accessor `prolog_service::prolog_mode() -> PrologMode`.
   All 28 read sites switch to the accessor. Tests that need a specific mode set it
   through a `#[cfg(test)] set_prolog_mode_for_test`.
2. **`get_subject_data` on model_query.** Reimplement
   `PerspectiveInstance::get_subject_data` using `model_query::execute_model_query`
   with a `where: { id = base_expression }` filter, returning the same JSON shape
   the WS/MCP callers expect. The Prolog path is deleted (SHACL is the source of
   truth for shapes regardless of Prolog mode). Add a test at the WS level
   (`api/tests/`) that a subject created via `perspective.createSubject` round-trips
   through `perspective.getSubjectData` with its properties.
3. **Explicit errors when Prolog is off.** `prolog_query_with_context`,
   `prolog_query_subscription*`, WS `perspective.queryProlog`, MCP `infer` return
   `Err("Prolog is disabled on this executor (prolog_mode = disabled)")` instead of
   empty results. MCP `infer` tool description updated to say it needs Prolog
   enabled. `subscribe_and_query` Prolog branch surfaces the same error to the
   subscriber rather than returning `"false"`.
4. **Move Prolog marshalling.** `perspectives/utils.rs` (Prolog `Term` → JSON) →
   `prolog_service/term_json.rs`. Delete `prolog_get_first_string_binding`,
   `prolog_get_all_bindings` (no callers). `sdna.rs`: delete `init_engine_facts`,
   `is_sdna_related_link` (no callers); keep fact generators (they are the Prolog
   data path).
5. **Delete** `src/prolog.rs` (scratch, not in `lib.rs`), `js_core/main.js`
   (unreferenced), `js_core/mod.rs:43-51` `LanguageController = ()` placeholder,
   `holochain_service/mod.rs` `_removed_await_initial_peer_discovery`, stale
   `.gitignore` entries for `prolog_service_extension.js` / `jwt_extension.js`.
6. **Docs.** Delete `rust-executor/LANGUAGE_RUNTIME_PHASE1.md`,
   `rust-executor/PHASE1_FIX.md`, root `PHASE1_SUMMARY.md`. Rewrite
   `rust-executor/README.md` bullets for js_core and Prolog. Update
   `SHACL_SDNA_ARCHITECTURE.md` "Implementation Status" to say Prolog is disabled by
   default and configurable.
7. **Panic messages.** `agent/mod.rs:383,396` and `runtime_service/mod.rs:75-77`
   say "Couldn't get lock on Ad4mDb" inside AgentService/RuntimeService. Fix.

Acceptance: `pnpm test` green; `perspective.getSubjectData` integration test
green; `AD4M_PROLOG_MODE=pooled cargo test -p ad4m-executor prolog` runs the pool
tests against a live engine; grep for `PROLOG_MODE` returns only the accessor.

Not in scope: deleting `engine_pool.rs`/`filtered_pool.rs`; splitting their tests
(item 8).

### Item 2 — MCP static tools delegate to dynamic handlers (S)

Files: `mcp/tools/subjects.rs`, `mcp/tools/dynamic.rs`, `mcp/shacl.rs`.

1. `query_subjects`, `get_subject_data`, `set_subject_property`,
   `get_subject_collection`, `add_to_collection`, `remove_from_collection`,
   `delete_subject` in `subjects.rs` become thin wrappers that build the args map
   and call the corresponding `handle_dynamic_*`. Removes ~500 lines and fixes the
   two divergences (batching on set-property, encoded-literal fallback on get).
2. `mcp/shacl.rs::load_class_properties` goes through `load_class` →
   `shape_to_shacl_class`; delete `load_class_properties_with_uri` (~240 lines).
   Add `impl From<&ModelShape> for ShaclClass` so the field mirror is one place.
3. Follow-up (separate PR, M): reimplement `handle_dynamic_query`/`_list`/`_get` on
   `perspective.model_query()` so MCP and WS agree on "instance of class X".

Acceptance: MCP integration tests in `tests/js` green; a test that sets a property
through the static tool then reads it back immediately (the race the dynamic path
fixed).

### Item 3 — `perspective_instance.rs` → `perspective_instance/` (M, three PRs)

Baseline facts: one `impl` at lines 527-6099; fields partition by cluster — only
`persisted`, `uuid`, `is_teardown`, `sparql_store` are shared. Lock order in
practice: `batch_store` → `persisted` (8 sites); `prolog_update_mutex` write is
held across service awaits deliberately (`:2634` comment).

**PR 3a — mechanical prep (S).**
- Change `&mut self` → `&self` on the 17 methods that take it (all fields are
  `Arc`/`Copy`; `get_perspective()` hands out clones anyway).
- Delete fields `created_from_join`, `is_fast_polling`, `retries` (never read).
- Move pure functions + their tests out: `extract_predicates_from_sparql` + regex
  statics + `is_sparql_query` → `predicates.rs`; `inbound_touches_shacl` + its
  test module → `shacl_trigger.rs`.
- Move DTO types (`SdnaType`, `Action`, `Command`, `SubjectClass*`, `Parameter`,
  `PorpertyValue` [sic — rename to `PropertyValue` with `pub use` alias],
  `SubjectClassOption`) → `types.rs`.

**PR 3b — split impl across files (M).** Directory with one `impl
PerspectiveInstance` per file. Target files and source clusters:

| File | Contents (source line starts) | ≈ lines |
|---|---|---|
| `mod.rs` | struct, `new`, `update_from_handle`, `no_link_language_error`, lock-order doc comment | 150 |
| `shapes.rs` | shape cache, `PerspectiveShapeResolver`, `sparql_query*`, `model_query`, `evaluate_getters`, `subject_classes_of` | 200 |
| `links.rs` | `add_link`…`remove_links`, `get_links*`, `persist_link_diff`, `pubsub_publish_diff` | 700 |
| `batches.rs` | `create_batch`, `discard_batch`, `commit_batch`, `TimestampedBatch` | 200 |
| `sync.rs` | `ensure_link_language`, `nh_sync_loop`, `pending_diffs_loop`, `commit*`, `ensure_public_links_are_shared`, `fallback_sync_loop`, `diff_from_link_language` | 940 |
| `prolog.rs` | every `prolog_*`/`ensure_prolog_engine_pool*`/`update_prolog_engine*`/`spawn_prolog_facts_update` | 1000 |
| `sdna.rs` | `add_sdna*`, `get_sdna_links_local`, `get_subject_classes_from_shacl`, `remove_subject_class_shacl_links` | 400 |
| `commands.rs` | `execute_commands`, `*_actions_from_shacl`, `resolve_property_value`, `create_subject`, `update_subject`, `get_subject_data` | 800 |
| `subscriptions.rs` | `subscribe_and_query`, `model_subscribe_and_query`, `check_subscribed_queries`, `subscribed_queries_loop`, `dispose_query_subscription`, `keepalive_query`, `record_changed_predicates` | 580 |
| `telepresence.rs` | `others`, `online_agents`, `set_online_status`, `send_signal`, `send_broadcast`, `update_local_agents`, `telepresence_signal_from_link_language` | 335 |
| `notifications.rs` | `notification_check_loop`, `calc_notification_trigger_matches`, `publish_notification_matches`, … | 190 |
| `lifecycle.rs` | `start_background_tasks`, supervisors, `teardown_background_tasks`, `memory_diagnostics*` | 240 |
| `auto_processor_loop.rs` | `auto_processor_watch_loop`, `run_auto_processor_tick` (or fold into `agentic/auto_processor` in item 7) | 200 |
| `tests/` | existing `mod tests` split by the same names | 1500 |

Visibility: private helpers called across files become `pub(super)`.

**PR 3c — dedup (M).** Behaviour-preserving, one commit per bullet:
- `apply_diff(&self, diff, decorated, status, opts)` replaces the 8× "persist →
  prolog → pubsub → commit-if-shared" sequence (`:1533, :1650, :1778, :1839,
  :1910, :2016, :2136, :6076`). Make the `update_prolog_engines` vs
  `spawn_prolog_facts_update` choice an explicit flag.
- `with_batch(&self, batch_id, |diff| …)` replaces 8× batch-lookup boilerplate.
- `publish_per_owner(handle, topic, make_payload)` replaces the 4× owners-fan-out
  (3 in `pubsub_publish_diff`, 1 inlined in `update_link :2021-2054`).
- Collapse `prolog_query_sdna`/`_with_context`, `prolog_query_subscription`/
  `_with_context`, `ensure_prolog_engine_pool`/`_for_context` into the
  `_with_context` variants taking `Option<&AgentContext>`.
- `send_signal`/`send_broadcast` share a skeleton behind
  `SignalTarget { Agent(String), Broadcast { loopback } }`.
- `bill_link_write(ctx, uuid, n, what)` replaces 4× billing block.
- Fix `mod.rs` `publish_telepresence_signal` vs `_sync` divergence
  (`!o.is_empty()` guard at `:606` missing at `:648`) — this is a live bug.
- Use `notification_pool_name()` at `:2662` instead of the inline `format!`.
- `mod.rs`: `PERSPECTIVES: RwLock<HashMap<String, RwLock<PerspectiveInstance>>>`
  → drop the inner `RwLock` (every accessor clones out immediately).

Later (item 12): extract `SyncState`, `SubscriptionRegistry`, `ShapeCache` as owned
structs.

### Item 4 — `db.rs` → `db/` (M, two PRs)

**PR 4a — schema versioning (S, behaviour change, ships first).**
- `db/schema.rs`: `const MIGRATIONS: &[(u32, &str)]` applied under
  `PRAGMA user_version` in a transaction. Existing `CREATE TABLE IF NOT EXISTS`
  body becomes migration 1; each existing `ALTER TABLE`/data-repair becomes its
  own numbered step. Remove the four `let _ = conn.execute(...)` swallows at
  `db.rs:329-351`.
- Replace the 8 `SELECT *` + positional `row.get(N)` queries (`db.rs:517, 539,
  778, 810, 836, 866, 1798, 1857`) with explicit column lists. Prerequisite for
  shared row mappers.

**PR 4b — split + helpers (M, mechanical).**
- One `impl Ad4mDb` per file as listed in §2. Zero call-site churn: the 260
  `with_global_instance` sites stay.
- `db/helpers.rs`: `json_col<T: DeserializeOwned>(row, idx)`, `query_all`,
  `query_opt`, `upsert_json`, `string_set_{all,add,remove}(table, col)`,
  `export_table<T>`, `import_table<T>`.
- Collapse: link row mapper (5× → 1), model mapper (2×54 lines → 1),
  notification mapper (4× → 1), `PerspectiveHandle`/`ComputeLogEntry`/
  `PaymentRequest` mappers (2× each), three string-set tables (`friends`,
  `known_link_languages`, `trusted_agent` — note `trusted_agent` lacks the
  `sort/dedup` the others have), export/import (930 → ~330 lines), bool-settings
  wrappers.
- Unify return types on `Ad4mDbResult` (27 methods return `rusqlite::Error`).
- Delete dead: `_get_perspective`, `cleanup_compute_log`, `get_compute_log_all`
  (0 callers); make `set_verification_code_expiry` `#[cfg(test)]`.
- Normalise the 11 raw `Ad4mDb::global_instance()` sites to
  `with_global_instance`.
- `db_backend.rs`: `LocalDb` is a stub (`delete` is a no-op) behind
  `if db_backend == "shared"` guards everywhere. Replace the trait global with
  `Option<Arc<SharedDb>>`; delete `LocalDb`.
- `clear_pending_diffs` builds `IN (…)` via `format!` — switch to bound params.

Estimated: −940 lines net, largest file ≤ 400.

### Item 5 — Type dedup (S)

- `impl From<domain::Perspective> for core::Perspective` (and back); delete the
  inline loop in `agent/mod.rs:218-233`.
- Delete `domain::LanguageRef` (identical to `core::LanguageRef`), point 3 users
  at core.
- Delete dead `api/types.rs` structs: `PerspectiveInput`, `LinkMutationRequest`,
  `LinkUpdateInput`, `QueryRequest`, `FeedTranscriptionRequest`,
  `ImportAgentRequest`, `SetFreeHostingEnabledRequest`, `SetStatusRequest`,
  `TrustedAgentsRequest`, `WriteSettingsRequest` — after confirming `core/`'s
  generated `.d.ts` does not consume the `ts-rs` exports.
- `impl From<api::types::NotificationInput> for domain::NotificationInput`;
  delete the field-by-field copy at `runtime_ws.rs:318-330`.
- Collapse `domain::UserInfo` into `core::UserInfo` (`last_seen: Option<i64>`).
- Remove the apology comment in `types/mod.rs:7-9` once collisions are gone.

### Item 6 — Declarative WS handler registration (M)

`api/ws_handler.rs` already has `HandlerMap` + `dispatch`. Add:

```rust
pub enum CapSpec {
    Static(&'static Capability),
    PerspectiveScoped(fn(Vec<String>) -> Capability), // reads "uuid" from params
    AdminOnly,
    None, // explicit and greppable
}
impl HandlerMap {
    pub fn register_with<T: DeserializeOwned, F, Fut>(&mut self, name: &str, cap: CapSpec, f: F)
    where F: Fn(T, Arc<RequestContext>) -> Fut + Send + Sync + 'static;
}
```

`dispatch` resolves `CapSpec` → 403, parses `T` → 400, then calls. Migrate one
namespace per commit (`perspectives_ws.rs` first). Decide explicitly, in the PR,
what the five currently unchecked methods should require (`runtime.info`,
`runtime.tlsDomain`, `runtime.freeHostingEnabled`, `runtime.hostRates`,
`agent.isLocked`).

Also:
- Split `perspectives_ws.rs`: interpretation handlers + `emit_one_shot_*`
  (`:1267-1810`) → `api/interpretation_ws.rs`; SHACL handlers (`:2146-2377`) →
  `api/shacl_ws.rs`.
- `WsRpcError`: add `kind: &'static str` and `From<LanguageError>`,
  `From<AIServiceError>`, `From<BillingError>`; retire the 137×
  `internal(format!(e))` pattern one namespace at a time.
- Events: `/api/v1/ws/events` is fully subsumed by the event stream inlined into
  the RPC socket (`ws_rpc.rs:99-118`). Either remove the endpoint or make RPC
  event fan-out opt-in via a first message. Delete `pubsub::subscribe_and_process`
  (dead). Generalise `did_stream!` to take `is_admin` + `LazyDid` so the two
  inlined auto-processor streams collapse.

Estimated: −1,000 lines.

### Item 7 — `agentic/` module and cycle break (M)

1. Move `perspectives/auto_processor/events.rs` → `agentic/events.rs` first (one
   file, breaks `harness → perspectives` for events).
2. Move `ai_service/harness/` → `agentic/harness/`; `ai_service` keeps only
   `ToolProvider`/`ToolSchema`/`CreditGate` traits re-exported from
   `agentic::harness::provider` (or move the traits into `ai_service/tools.rs`
   and have `agentic` implement against them — pick the direction that leaves
   `ai_service` with zero imports from `agentic`/`perspectives`).
3. Move `perspectives/interpretation/`, `flow_*`, `flow_context/`,
   `auto_processor/` → `agentic/`. `perspective_instance/auto_processor_loop.rs`
   stays a thin caller.
4. Move `api/openai_compat/tool_grammar.rs` → `agentic/tool_grammar.rs` and the
   two `harness_bridge.rs` files → `agentic/bridges/{mcp,openai}.rs`.
5. `agentic` depends on `perspectives`, `ai_service`, `mcp::shacl` one-way. Add a
   `#[cfg(test)]` module-dependency test (or `cargo modules`/`cargo deny` rule if
   adopted) that fails on `ai_service → agentic` or `perspectives → agentic`
   imports.
6. Move the four e2e files (`interpretation_e2e.rs`, `interpretation_harness_e2e.rs`,
   `flow_evaluator_e2e.rs`, `flow_context/real_llm_e2e.rs`) under
   `agentic/e2e/` and gate on `#[cfg(feature = "llm-e2e")]` so plain `cargo test`
   does not call real LLMs.

### Item 8 — Query-stack splits (M, one PR per file)

- `sparql_store.rs` → `sparql_store/{mod, terms, functions, reification,
  link_query, migration}.rs` + `tests/`. Break `for_each_matched_link` (205
  lines) into pattern-build / execute / decode.
- `shacl_parser.rs` → `shacl/{types, flow_types, class_writer, flow_writer,
  flow_reader, literal, uri, vocab}.rs`. `vocab.rs` holds the predicate IRIs
  (`sh://path`, `ad4m://setter`, …) used by both writer and `model_query/shape.rs`.
- `model_query/sparql_builder.rs` → `conformance.rs` / `where_clause.rs` /
  `quantifiers.rs`; move its 4 interleaved `#[cfg(test)]` blocks and
  `integration_tests.rs` into `model_query/tests/{...}` split by subject.
- Dedup literal codec: one `perspectives/literal.rs` replacing
  `sparql_store::{decode_literal_payload, try_unwrap_envelope}`,
  `model_query/utils.rs::parse_literal_value`, `shape.rs::decode_literal_*`,
  `shacl_parser::decode_literal_*`, `interpretation/graph/read.rs::decode_literal_string`,
  `ordering/mod.rs` inline decode. Also single `extract_local_name` (4 copies),
  `typed_number_literal` (2), `RFC3986_COMPONENT_ENCODE` (2), `escape_sparql_*` (2).
- `projection.rs::build_projection_where_patterns` is a second where-clause
  compiler; route it through `where_clause.rs`.
- `prolog_service/engine_pool.rs`: move the ~1,250 filtered-pool test lines to
  `filtered_pool.rs` (or `prolog_service/tests/`) and ~680 sdna-pool lines to
  `sdna_pool.rs`. Extract the 4× "static + sdna + data facts" assembly into one
  `fn facts_to_load(...)`.

### Item 9 — Service-file splits (M each)

| File | Split | Also |
|---|---|---|
| `languages/mod.rs` | `controller.rs` (runtime lifecycle), `install.rs`, `registry.rs`, `expressions.rs` | `create_neighbourhood*`/`get_neighbourhood` → `neighbourhoods.rs`; `read_and_template_holochain_dna` + `apply_template_data` → `holochain_service/dna_template.rs`; narrow the 13 tokio guards held across JS-eval awaits; add tests (currently zero) |
| `ai_service/mod.rs` | `models.rs`, `llm.rs`, `tasks.rs`, `embed.rs`, `transcription/` | `bill_*_if_authed` behind `CreditGate`; whisper name table → const table |
| `unyt_service.rs` | `unyt/{dna, client, payments}.rs` | `credit_and_complete` → `billing.rs`; add tests (currently zero) |
| `wallet.rs` | `wallet/{crypto, backend, local, shared}.rs` | delete legacy `WALLET` singleton |
| `agent/mod.rs` | `agent/{context, signing, service, users, persistence}.rs` | `init_global_test_instance` → `#[cfg(test)]`; `publish_agent_to_language` → caller in `api/agent_ws.rs` |

### Item 10 — Holochain service dispatch (S)

`holochain_service/mod.rs::init` inlines a 352-line `match` over
`HolochainServiceRequest` that mirrors `interface.rs` one-for-one. Extract into
`actor.rs` and generate request/response/method triples with a macro, so adding a
zome op is one edit. Move `pack_*`/`unpack_*` to `bundle.rs`. Replace the
`panic!("Holochain Conductor not started after 120s")` in `interface.rs:305` with
an error return.

### Item 11 — `AppContext` (L, incremental)

Introduce `AppContext { db, agent, ai, languages, runtime, pubsub, holochain,
config }` built once in `lib.rs` and stored in a `OnceCell`. Existing
`X::global_instance()` accessors become shims over it. Thread `Arc<AppContext>`
through `RequestContext` (WS), `Ad4mMcpHandler` (MCP) and `PerspectiveInstance::new`.
Tests construct their own `AppContext` with in-memory DB.

Success metric: delete `--test-threads=1` from `rust-executor/package.json`.

Also: `HolochainService::init` is currently called from HTTP handlers
(`api/agent_ws.rs:305,416`); move into the boot sequence behind a flag.

### Item 12 — Owned sub-structs in `PerspectiveInstance` (M, after items 3 and 11)

`SyncState` (5 fields, 3 loops), `SubscriptionRegistry` (3 fields; query
execution injected as a closure/trait), `PrologEngineHandle` (mode-dispatch trait
with `Simple`/`SdnaOnly`/`Pooled`/`Disabled` implementors replacing the ~15
pasted `match prolog_mode()` blocks), `ShapeCache` (drop `std::sync::RwLock`
`.unwrap()` poisoning).

## 4. Decisions needed from the team

- **D1 — Prolog config surface.** Config key + env var as in item 1, or config
  only? Proposal: both, env wins.
- **D2 — `perspectives/ordering`.** 1,118 lines of CRDT collection ordering,
  parsed into `ModelShape.ordering` and never read. Wire into `hydration.rs` and
  the collection setter/adder write path, or delete until needed.
- **D3 — `/api/v1/ws/events`.** Remove, or make RPC-socket event fan-out opt-in.
  Check `core/` SDK usage first.
- **D4 — Version string.** `globals.rs:7` and `package.json` both hardcode
  `0.13.0-test-interpretation-2`. Proposal: `env!("CARGO_PKG_VERSION")`, and
  `setVersion.js` writes Cargo.toml only.
- **D5 — Snapshot artifacts in git.** `CUSTOM_DENO_SNAPSHOT.bin` (2.1 MB) and
  generated `residual_*.rs` (4 MB) are committed though `residual_lazy.rs:15`
  says they are ignored. Ignore + build, or keep committed for CI speed.

## 5. Conventions going forward

### 5.1 Module layout
- One directory per bounded context. `mod.rs` = declarations, re-exports,
  ≤ 300 lines. No business logic in `mod.rs`.
- Flat files only for leaf concerns ≤ 800 production lines. A file that grows past
  that becomes a directory.
- Names: no `utils.rs`/`helpers.rs` unless the file is genuinely generic (path
  helpers, port finding). `src/helpers.rs` (perspective ACL + expression
  rendering) → `perspective_access.rs`. `perspectives/utils.rs` → see item 1.
- Service directories drop the `_service` suffix when they move (`unyt/`,
  `wallet/`); existing `*_service/` dirs keep their name until touched.

### 5.2 Globals and locking
- New code takes services from `AppContext` (item 11) or as parameters. No new
  `lazy_static` singletons.
- Import tokio locks as `TokioMutex`/`TokioRwLock`; std as `StdMutex`. Never a bare
  `Mutex` import in a file that uses both.
- Document lock order at the struct definition. `PerspectiveInstance`:
  `batch_store → persisted`; `prolog_update_mutex` write may be held across
  `PrologService` awaits only.
- Every `tokio::spawn`ed loop keeps its `JoinHandle` on the owning struct and is
  aborted in teardown/`Drop`.

### 5.3 Tests
- Unit tests: `#[cfg(test)] mod tests` in-file only while the file stays under the
  cap including tests. Otherwise `<module>/tests/<subject>.rs`.
- Real-LLM / real-network e2e: behind a cargo feature (`llm-e2e`), never in the
  default `cargo test` run.
- Integration (`tests/js`): required for any WS/MCP behaviour change.

### 5.4 Errors
- `thiserror` for every module error enum. `WsRpcError` gets `From` impls; no new
  `internal(format!("{}", e))` sites.
- `.expect()` only in boot (`lib.rs`) with a message naming the actual subsystem.

### 5.5 Docs
- `CLAUDE.md` per directory (see §0 rule 5). `AGENTS.md` = one line pointing at
  `CLAUDE.md`. Root `AGENTS.md` keeps the Holochain/testing gotchas.
- Design docs live in `planning/`, dated. Stale ones are deleted, not left.

## 6. Suggested schedule

| Week | Items |
|---|---|
| 1 | 1 (Prolog isolation + fixes), 2 (MCP dedup), 5 (types) |
| 2 | 3a, 3b (perspective_instance split), 4a (schema versioning) |
| 3 | 3c (dedup), 4b (db split), 6 (CapSpec) |
| 4 | 7 (agentic/), 8 (query-stack splits, start) |
| 5–6 | 8 (finish), 9 (service splits), 10 |
| 7+ | 11 (AppContext), 12 |

Items 2, 5, 10 are independent and good first tasks for a new contributor or an
agent session.
