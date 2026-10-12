# rust-executor — agent guide

Crate `ad4m-executor` (lib `rust_executor`). The AD4M runtime. Refactoring plan and
conventions: `../planning/rust-executor-refactoring-spec-2026-09-04.md`. Read the
"Conventions" section there before adding files.

`CLAUDE.md` next to this file is a Claude Code entrypoint (`@AGENTS.md`). Edit this
file; keep `CLAUDE.md` as `@AGENTS.md`.

## Build and test

```bash
pnpm build                 # deno snapshot + cargo build --release (needed after any js_core/*.js or
                           # extension-list change; with a stale snapshot, deno_core panics at runtime
                           # creation: "Extensions from snapshot loaded in wrong order")
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
| `config_file.rs` | The `run --config` JSON file shared with the launcher (`launcher-state.json` key names), secret resolution (`AD4M_*_FILE`), and the one mapping to `Ad4mConfig` | `docs-src/pages/developer-guides/executor-config.mdx` |
| `api/` | axum WS RPC (`/api/v1/ws`), event stream, OpenAI-compatible `/v1` | `src/api/AGENTS.md` |
| `mcp/` | MCP server + tools (static + SHACL-generated dynamic tools) | `src/mcp/AGENTS.md` |
| `perspectives/` | Perspective registry, `PerspectiveInstance`, SPARQL store, SHACL/model queries, flows, interpretation, auto-processor | `src/perspectives/AGENTS.md` |
| `ai_service/` | Local (candle) + remote LLM/embedding/whisper models, AI tasks; `harness/` = tool-calling loop for interpretation | `src/ai_service/AGENTS.md` |
| `prolog_service/` | Scryer Prolog engine pools. **Disabled by default** (`PROLOG_MODE`), kept as an option | `src/prolog_service/AGENTS.md` |
| `languages/` + `js_core/` | Language runtime: one Deno isolate per Language on its own thread; install, templates, expressions | `src/languages/AGENTS.md`, `src/js_core/AGENTS.md` |
| `holochain_service/` | Embedded Holochain conductor behind an actor channel; signal fan-in | `src/holochain_service/AGENTS.md` |
| `agent/` | Agent keys/DID, signing, multi-user (managed users by email), `capabilities/` (auth tokens, capability defs) | — |
| `db.rs` | All SQLite access (`rusqlite`), single `impl Ad4mDb`, accessed via `Ad4mDb::with_global_instance(closure)`. Perspective **links** are not here (they are in `perspectives/sparql_store.rs`); `db` holds handles, diffs, agent data, AI models/tasks, users, billing, notifications. | — |
| `db_backend.rs` | `SharedDb` HTTP backend for stateless/hosted mode; only used when `config.db_backend == "shared"` | — |
| `pubsub.rs` | Global broadcast bus: one channel of typed events (`events.rs`: `Topic`, `Audience`, `Event`) for every event, plus one string topic for live query updates until #1207; feeds `api/events_ws.rs` | `src/api/AGENTS.md` |
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
  `runtime_service/`; JS side in the sibling `.js` file and `js_core/host.js`. Every JS runtime is a
  language runtime, so third-party language code reaches every op: an op that signs must sign as
  `get_runtime_agent_context()`, never as the main agent directly.
  Languages are called by building JS source strings and evaluating them (`languages/mod.rs`).
- **Agents and sessions**: an executor hosts N agents. `AgentContext` (`agent/mod.rs`) names one:
  the main agent (agent #0, `user_email: None`, `is_main_agent()`), or a user's managed agent.
  `agent/session.rs` opens one `Session` per connection: `Principal::Operator` (the admin
  credential: the executor's administrator role, not an agent), `Principal::Agent(AgentContext)`
  (a grant on the main agent or on a user's agent), or `Principal::Anonymous`. Capabilities, the
  grant and the agent's DID are `Session` fields. Read identity with `acts_as()` /
  `acts_as_did()` / `user()` / `is_operator()`; never decode the token again. Any
  signing/DID/billing path takes the `AgentContext` explicitly.
- **Grants** (`agent/grants.rs`, table `grants` in `db/grants.rs`) are the one allowlist of
  tokens: a token (id = its `nonce`) opens a session only while its grant exists, is active and
  has not expired, and the session's capabilities are the grant's (tokens carry `iss, aud, iat,
  exp, nonce, sub` only). Both mint paths go through `grants::mint`: the app capability flow (a
  grant on the main agent) and a user login (an auto-approved grant on that user's agent). The
  admin credential is no token. Revoking or removing a grant cancels its `ended` token, which
  every socket of the session watches through `Session::ended()`. Start-up moves the old app
  registry (`apps_data.json`, renamed `.migrated`) into grant rows and drops expired rows. Tests
  mint with `test_utils::granted_token(auth)`; a token signed without a grant is refused.
- **Capabilities**: every WS handler checks `ctx.require(&X_CAPABILITY)` itself
  (`agent/capabilities/defs.rs`); `Session::may` answers the same question as a `bool`. MCP tools
  check via `Ad4mMcpHandler::get_*_perspective`.
- **Capability requests** (#1399, executor side): `agent.requestCapability(authInfo, agent?)`
  names a target agent (a managed user's email; absent = the main agent) and records a
  `PendingRequest` in memory only (`agent/capabilities/pending.rs`: never on disk, anonymous
  callers create them; 1024 at most, unapproved evicted first). The approval request event goes to
  the target agent's DID with `AGENT_PERMIT`. `agent.permitCapability` (the operator, or a
  session acting as the target managed agent) approves it, possibly with fewer capabilities, and
  gets a six-digit code (a `Secret`). `agent.generateJwt` redeems the code once, in one locked
  step: the grant is minted on the target agent with the approved capabilities capped at that
  agent's own (`grant_caps_for`); a failed mint leaves the request approved and redeemable. Five
  wrong codes delete a request; it lives 10 minutes (main agent) or 24 hours (managed agent),
  restarted at approval. No request is approved at the request: the app name and domain are
  whatever the (anonymous) caller declares, so they never grant anything. Only a six-digit code
  approves or redeems (`pending::is_code`); the empty code is refused before any comparison.
  The request flow is in `api/AGENTS.md`.
- **Secrets** (`secret.rs`): credentials are `Secret`s (no `Serialize`): the admin credential,
  AI provider keys (results carry `hasApiKey`), webhook credentials (`hasWebhookAuth`) and
  capability codes. `api/tests/secret_leak_tests.rs` calls every read method as each principal
  and fails if a seeded secret reaches a reply or an event; a new read method needing parameters
  gets them in its `params` table.
- **Notifications** post perspective data to any webhook URL. Their grant, update and delivery
  rules live in `runtime_service/notification_access.rs`: a notification fires only when granted
  and its owner may read the perspective. The owner is a DID (`owner_did`: a managed user's, or the
  main agent's), never an email; operator sessions act as the main agent. `runtime_ws.rs` refuses
  perspectives the owner cannot read; `events_ws.rs` sends `notification-triggered` only to sessions
  with the owner's DID. `Ad4mDb::migrate_notification_owners` (run at startup once the agent loads)
  moves rows of older executors from `user_email` to `owner_did`.
- **Keystore**: the `keystore` field of `agent.json` holds every local key, the main agent's and
  each managed user's (`wallet.rs`: Argon2id + XChaCha20-Poly1305, fresh salt and nonce per write).
  The first unlock of a legacy keystore rewrites the file and keeps `agent.json.legacy` until an
  unlock opens `agent.json` directly (`AgentService::unlock`). Meanwhile every save writes
  `keystoreFingerprint` (the saved keys) into `agent.json`, and the fallback to the backup runs
  only while the backup holds exactly those keys. Write the agent file only through
  `AgentService::save` / `try_save`.

## Do / don't

- Don't add to `perspective_instance.rs`, `db.rs`, `languages/mod.rs`, `ai_service/mod.rs`: they are
  being split (spec items 3, 4, 9). Put new code in a new file with its own `impl` block.
- New directory ⇒ add an `AGENTS.md` (≤ 40 lines: purpose, files, entry points, invariants,
  "do not") and a sibling `CLAUDE.md` containing only `@AGENTS.md`.
- Mechanical moves and behaviour changes in separate commits.
- Stale docs in this dir: `LOGGING.md` is current. Anything named `PHASE1_*` is obsolete.
