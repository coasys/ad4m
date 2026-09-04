# rust-executor — agent guide

Crate `ad4m-executor` (lib `rust_executor`). The AD4M runtime. Refactoring plan and
conventions: `../planning/rust-executor-refactoring-spec-2026-09-04.md`. Read the
"Conventions" section there before adding files.

## Build and test

```bash
pnpm build                 # deno snapshot + cargo build --release (needed after any js_core/*.js change)
cargo build --release      # Rust-only change
pnpm test                  # = cargo test --release -- --test-threads=1  (serial: global singletons)
cargo test --release -- --test-threads=1 <name>   # one test
cd .. && pnpm run test-main                        # JS integration suites against built binary; pkill -9 ad4m-executor first
```

`--release` and `--test-threads=1` are required (see `.claude/skills/rust-executor-testing.md`).
Some tests in `perspectives/*_e2e.rs` and `flow_context/real_llm_e2e.rs` call real LLMs.

## Module map (what lives where)

| Module | Role | Doc |
|---|---|---|
| `lib.rs` | Boot sequence (`run()`): config → wallet/db backends → `Ad4mDb` → `AIService` → `AgentService` → runtime → V8 → `LanguageController` → perspectives → axum. Order is hand-maintained. | — |
| `config.rs` | `Ad4mConfig` + global config accessor | — |
| `api/` | axum WS RPC (`/api/v1/ws`), event stream, OpenAI-compatible `/v1` | `src/api/CLAUDE.md` |
| `mcp/` | MCP server + tools (static + SHACL-generated dynamic tools) | `src/mcp/CLAUDE.md` |
| `perspectives/` | Perspective registry, `PerspectiveInstance`, SPARQL store, SHACL/model queries, flows, interpretation, auto-processor | `src/perspectives/CLAUDE.md` |
| `ai_service/` | Local (candle) + remote LLM/embedding/whisper models, AI tasks; `harness/` = tool-calling loop for interpretation | `src/ai_service/CLAUDE.md` |
| `prolog_service/` | Scryer Prolog engine pools. **Disabled by default** (`PROLOG_MODE`), kept as an option | `src/prolog_service/CLAUDE.md` |
| `languages/` + `js_core/` | Language runtime: one Deno isolate per Language on its own thread; install, templates, expressions | `src/languages/CLAUDE.md` |
| `holochain_service/` | Embedded Holochain conductor behind an actor channel; signal fan-in | `src/holochain_service/CLAUDE.md` |
| `agent/` | Agent keys/DID, signing, multi-user (managed users by email), `capabilities/` (auth tokens, capability defs) | — |
| `db.rs` | All SQLite access (`rusqlite`), single `impl Ad4mDb`, accessed via `Ad4mDb::with_global_instance(|db| ...)`. Perspective **links** are not here (they are in `perspectives/sparql_store.rs`); `db` holds handles, diffs, agent data, AI models/tasks, users, billing, notifications. | — |
| `db_backend.rs` | `SharedDb` HTTP backend for stateless/hosted mode; only used when `config.db_backend == "shared"` | — |
| `pubsub.rs` | Global broadcast bus + topic constants; feeds `api/events_ws.rs` | — |
| `types/` | `core.rs` = domain types (Link, Expression, Perspective…), `domain.rs` = wire/input types; some duplicates, see spec item 5 | — |
| `runtime_service/`, `entanglement_service/`, `neighbourhoods.rs`, `billing.rs`, `email_service.rs`, `user_management.rs`, `unyt_service.rs`, `wallet.rs`, `perspective_snapshot.rs`, `logging.rs` | Smaller services; `unyt_service.rs` = Unyt/mHOT currency DNA + payments | — |

## Cross-cutting facts you need before editing

- **Services are global singletons** (`lazy_static`), e.g. `Ad4mDb::with_global_instance`,
  `AgentService::with_global_instance`, `LanguageController::global_instance()`,
  `AIService::global_instance().await`, `get_global_pubsub()`, `perspectives::get_perspective(uuid)`.
  Do not add new ones; spec item 11 introduces `AppContext`.
- **Perspective links live in the per-perspective `SparqlStore`** (oxigraph), not SQLite.
  `PerspectiveInstance` is `Clone` and cheap: all state is behind `Arc`. `get_perspective()` returns a clone.
- **Prolog is off.** `prolog_service::PROLOG_MODE` is `Disabled`; Prolog query paths return empty.
  Subject classes, flows and model queries run on SHACL + SPARQL (`perspectives/model_query`).
- **Tokio vs std locks** are both used; check the import before assuming `.lock()` is async.
- **JS ↔ Rust bridge**: `#[op2]` extensions in `js_core/*_extension.rs`, `holochain_service/`,
  `runtime_service/`, `entanglement_service/`; JS side in the sibling `.js` file and `js_core/host.js`.
  Languages are called by building JS source strings and evaluating them (`languages/mod.rs`).
- **Capabilities**: every WS handler checks `check_capability(&ctx.capabilities, &X_CAPABILITY)`
  itself (`agent/capabilities/defs.rs`). MCP tools check via `Ad4mMcpHandler::get_*_perspective`.
- **Multi-user**: `AgentContext` (`agent/mod.rs`) carries main-agent vs managed-user (by email).
  Any signing/DID/billing path takes it explicitly.

## Do / don't

- Don't add to `perspective_instance.rs`, `db.rs`, `languages/mod.rs`, `ai_service/mod.rs`: they are
  being split (spec items 3, 4, 9). Put new code in a new file with its own `impl` block.
- New directory ⇒ add a `CLAUDE.md` (≤ 40 lines) and `AGENTS.md` pointing to it.
- Mechanical moves and behaviour changes in separate commits.
- Stale docs in this dir: `LOGGING.md` is current. Anything named `PHASE1_*` is obsolete.
