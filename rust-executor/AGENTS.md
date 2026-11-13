# rust-executor — agent guide

Crate `ad4m-executor` (lib `rust_executor`). The AD4M runtime. Refactoring plan and
conventions: `../planning/rust-executor-refactoring-spec-2026-09-04.md`. Read the
"Conventions" section there before adding files.

`CLAUDE.md` next to this file is a Claude Code entrypoint (`@AGENTS.md`). Edit this
file; keep `CLAUDE.md` as `@AGENTS.md`.

## Build and test

```bash
pnpm build                 # deno snapshot (scripts/build-deno-snapshot.sh). Run it after every pull and
                           # every js_core/ or extension change: it skips when the snapshot's inputs
                           # did not change (stamp CUSTOM_DENO_SNAPSHOT.inputs). FORCE_DENO_SNAPSHOT=1
                           # builds anyway. A stale snapshot can make deno_core panic at runtime creation
                           # ("Extensions from snapshot loaded in wrong order") or call the wrong op silently
cargo build --release      # Rust-only change
pnpm test                  # = cargo test --release -- --test-threads=1  (serial: global singletons)
cargo test --release -- --test-threads=1 <name>   # one test
cd .. && pnpm run test-main                        # JS integration suites against built binary; pkill -9 ad4m-executor first
```

`--release` and `--test-threads=1` are required (see `.claude/skills/rust-executor-testing.md`).

**Where a create or join spends its time:** `grep 'timing:'` in the executor log. Each line is
`timing: <step> [address] <ms>ms` (`logging::span`), logged when the step ends. `applyTemplate`,
`publish` and `join` are the totals of the three calls; `applyTemplate.*`, `publish.*`,
`installLanguage.*`, `loadLanguage.*` and `holochain.*` are their steps.
Some tests in `perspectives/*_e2e.rs` and `flow_context/real_llm_e2e.rs` call real LLMs.

## Module map (what lives where)

| Module | Role | Doc |
|---|---|---|
| `lib.rs` | Boot sequence (`run()`): config → data-folder lock → admin credential → storage secret out of the config → private dirs (0700) → `StorageGate` (sealed: the API starts early and `run` waits for `storage.unlock`) → wallet/db backends → `Ad4mDb` → `AIService` → `AgentService` → runtime → V8 → `LanguageController` → perspectives → axum. Order is hand-maintained. | — |
| `config.rs` | `Ad4mConfig` + global config accessor | — |
| `config_file.rs` | The `run --config` JSON file shared with the launcher (`launcher-state.json` key names), secret resolution (`AD4M_*_FILE`), and the one mapping to `Ad4mConfig` | `docs-src/pages/developer-guides/executor-config.mdx` |
| `config_rpc.rs` | The config file as RPC (`runtime.config`, `runtime.setConfig`, handlers in `api/config_ws.rs`): key classes (`restart`, `machine`), atomic 0600 writes, pinned keys, validators (`register_config_key`), `config-changed`. A value is written only when the file still maps to `Ad4mConfig` as the next start maps it (`to_ad4m_config`), else `invalid` | — |
| `exit.rs` | The run contract a supervisor reads: `ExitReason` (0, 75, 78, 79, 80), `StartRefused`, the graceful stop (`request_shutdown`), `--watch-stdin` (`stop_when_closed`, on the process's buffered `Stdin`, after the custody line when `--custody-channel` took one), the data-folder lock (`executor.lock`, taken before the log folder rotates, so a refused start leaves the running executor's log alone and reports to stdout), no core dumps on Linux | — |
| `at_rest/` | Data at rest: the operator secret (`secret_source.rs`: `AD4M_STORAGE_SECRET_FILE`, 64 hex, refused on the data folder's disk unless `allow_secret_on_data_disk`), the keyring file (`keyring_file.rs`: `keyring.json`, atomic 0600 writes), the data keys under a root derived from the secret (`data_keys.rs`), the languages' storage files sealed under their store's key (`lang_storage.rs`: `AD4MSEAL` files behind `LANGUAGE_CONTROLLER.readStorageFile`/`writeStorageFile`, the start step that seals plaintext ones; non-system language isolates get no Deno file permission), the agent passphrase (`agent_passphrase.rs`: derived from the root for an agent generated with a keyring; for an older agent, held sealed in the keyring after the first unlock that opens `agent.json` with it; `run` unlocks the agent from it, `AgentService::unlock_with_keys`), the executor's own secret files (`executor_secrets.rs`: `DataKeys::seal_executor_secret`/`open_executor_secret`, `AD4MSEAL` files beside `keyring.json`, and `seal_plaintext_executor_secret` to move a plaintext key file in and remove it), the rotations (`data_keys.rs`: `rotate_root` moves the keyring to a new secret in one keyring write, every root wrap re-wrapped and a derived agent passphrase held from then on; `rotate_person` gives a person's data key a new version; `rotation.rs`: a store's re-encryption, which makes a new store key version current, seals again every value and language file still under an older one, then retires the older ones; never the executor's own store, whose SQLCipher key is set once per process), and the `StorageGate` (sealed, unlocking, open) behind `storage.unlock`, `storage.keys`, `runtime.storageStatus`, `storage.rotateSecret`, `storage.rotatePersonKey`, `storage.reencrypt` and `storage-state-changed` | — |
| `log_files.rs` | `--log-dir`: one file per run (`executor-0.log`), five runs kept, 50 MiB cap per file, `tail` for `runtime.logTail` | — |
| `api/` | axum WS RPC (`/api/v1/ws`), event stream, OpenAI-compatible `/v1` | `src/api/AGENTS.md` |
| `mcp/` | MCP server + tools (static + SHACL-generated dynamic tools) | `src/mcp/AGENTS.md` |
| `perspectives/` | Perspective registry, `PerspectiveInstance`, SPARQL store, SHACL/model queries, flows, interpretation, auto-processor | `src/perspectives/AGENTS.md` |
| `ai_service/` | AI through model servers (endpoints): LLM, embedding, transcription and speech models, AI tasks; `harness/` = tool-calling loop for interpretation and agent sessions (`perspectives/agent_session.rs`) | `src/ai_service/AGENTS.md` |
| `prolog_service/` | Scryer Prolog engine pools. **Disabled by default** (`PROLOG_MODE`), kept as an option | `src/prolog_service/AGENTS.md` |
| `languages/` + `js_core/` | Language runtime: one Deno isolate per Language on its own thread; install, templates, expressions | `src/languages/AGENTS.md`, `src/js_core/AGENTS.md` |
| `tool_bundles/` | Tool bundles: an app's JS for its agents' tools, one bare `JsRuntime` per bundle with two ops (random bytes, a log line); time limit on load and every call, memory cap on heap and ArrayBuffers. `builtin.rs` compiles in WE's template tools (`template_read` / `template_update` in `mcp/`), downloaded from a WE release by `build.rs` (version and sha256 pinned there) | `src/tool_bundles/AGENTS.md` |
| `holochain_service/` | Embedded Holochain conductor behind an actor channel; signal fan-in | `src/holochain_service/AGENTS.md` |
| `agent/` | Agent keys/DID, signing, multi-user (managed agents by DID; a user's email is only the login name), `capabilities/` (auth tokens, capability defs) | — |
| `identity/` | `did:scid:coasys:1` on the executor: `KelCache` (key logs on `ad4m-hash-chain`'s `SqliteChainStore` in `ad4m_db.sqlite`), the agent-language backend, the operator record and first-link rule, `ClaimService`, the `identity.*` calls (handlers in `api/identity_ws.rs`), the device inbox, the private record and the request box, the `identity-fork`, `link-ticket` and `inbox-message` events | `src/identity/AGENTS.md` |
| `bots/` | Bots: managed agents whose key the owner's log marks as a bot's, the owner's grant (spaces, actions, pause, revoke) checked before the guard of every call a bot makes, the activity log with each write's reason, `bot.*` | `src/bots/AGENTS.md` |
| `db.rs`, `db/` | All SQLite access (`rusqlite`): `impl Ad4mDb` in `db.rs` plus child modules in `db/`. Executor code reaches it through `db_backend::db_backend()`, never `Ad4mDb` directly. Perspective **links** are not here (they are in `perspectives/sparql_store.rs`); `db` holds handles, diffs, agent data, AI models/tasks, users, billing, notifications. | `src/db/AGENTS.md` |
| `db_backend.rs` | `DbBackend`: the executor's database, one domain method per `Ad4mDb` method that executor code calls (`ad4m_db_methods!`), plus key-value methods for the shared records. `LocalDb` runs on SQLite; `SharedDb` (`config.db_backend == "shared"`) serves the key-value methods from the platform Worker over HTTP and every domain method from SQLite | `src/db/AGENTS.md` |
| `pubsub.rs` | Global broadcast bus: one channel of typed events (`events.rs`: `Topic`, `Audience`, `Event`) for every event (live query updates too, as `Event::for_connection`); feeds `api/events_ws.rs` | `src/api/AGENTS.md` |
| `types/` | `core.rs` = domain types (Link, Expression, Perspective…), `domain.rs` = wire/input types; some duplicates, see spec item 5 | — |
| `runtime_service/`, `entanglement_service/`, `neighbourhoods.rs`, `billing.rs`, `email_service.rs`, `user_management.rs`, `unyt_service.rs`, `wallet.rs`, `perspective_snapshot.rs`, `logging.rs` | Smaller services; `unyt_service.rs` = Unyt/mHOT currency DNA + payments | — |

## Cross-cutting facts you need before editing

- **Services are global singletons** (`lazy_static`), e.g. `db_backend::db_backend()`,
  `AgentService::with_global_instance`, `LanguageController::global_instance()`,
  `AIService::global_instance().await`, `get_global_pubsub()`, `perspectives::get_perspective(uuid)`.
  Do not add new ones; spec item 11 introduces `AppContext`.
- **Perspective links live in the per-perspective `SparqlStore`** (oxigraph), not SQLite.
  `PerspectiveInstance` is `Clone` and cheap: all state is behind `Arc`. `get_perspective()` returns a clone.
- **Open files.** Each perspective's RocksDB store holds about 70 descriptors and at most 274
  (see `perspectives/AGENTS.md`), so a 256 soft `RLIMIT_NOFILE` ran out at about a dozen
  perspectives ("Too many open files"). The executor raises its soft limit at startup in
  `fd_limit::raise_fd_limit_at_startup` (#1462); do not add another.
- **The run contract** (`exit.rs`), for a supervisor (the desktop app, systemd, a host). Exit
  codes: 0 stopped (`runtime.quit`, SIGTERM, SIGINT, stdin closed with `--watch-stdin true`),
  75 `runtime.restart`, 78 a refused config (the CLI's resolve, the admin-credential check,
  `--custody-channel` with multi-user mode, without a custody credential on stdin's first line,
  or with `AD4M_CUSTODY_CREDENTIAL` or `AD4M_CUSTODY_CREDENTIAL_FILE` set), 79 another
  executor holds
  `<app_data_path>/executor.lock`, 80 the API port does not bind; any other code is a crash.
  `run` returns a refused start through its handle as a `StartRefused`, and
  `exit_when_api_fails` exits with its code. Readiness: `GET /health` answers once the API
  listener is bound, the last step of `run`. The admin credential comes from the environment
  (`AD4M_ADMIN_CREDENTIAL`, or `AD4M_ADMIN_CREDENTIAL_FILE`), never from a file the executor
  writes; a supervisor gives a new one at each start. On Linux `run` sets `RLIMIT_CORE = 0`
  and `PR_SET_DUMPABLE = 0`; the `ad4m-executor` binary does so again first thing in `main`.
- **Sealed start.** No store opens before the operator secret. With `storage_secret` in the
  config (the CLI reads `AD4M_STORAGE_SECRET_FILE`; the launcher passes one from the OS
  keyring), `run` unlocks the keyring and goes on. Without it, or when that unlock fails, the
  executor comes up **sealed**: the API listens (`/health` answers), every RPC but
  `runtime.storageStatus` and `storage.unlock` gets 503 `storage-sealed`, a JWT session is
  refused, and `run` waits for `storage.unlock` (Operator, local only) or a stop. A secret
  file on the data folder's disk is refused (`secret-on-data-disk`) unless
  `allow_secret_on_data_disk` (`AD4M_ALLOW_SECRET_ON_DATA_DISK`), and the config file may
  never carry the secret. The data folder is 0700, its files 0600. The gate is a global
  (`at_rest::gate`); dispatch and admission check it (`HandlerMap::check_gate`).
- **Plaintext migration.** Right after the unlock, `at_rest::migration::run` moves an
  executor that ran before at-rest encryption under its keys, in a fixed order: the
  executor database (SQLCipher copy, verified, then renamed over the plaintext), the
  persons of its legacy users, then each language's storage files (sealed, read back,
  renamed over). The keyring file's `migration` record (started, steps done, finished)
  is the resume marker; it is written after each verified step, so a crash reruns at most
  one step, and each step skips what is already sealed. The gate stays `Migrating` (only
  `runtime.storageStatus` and `storage.unlock` answer) and publishes
  `storage-migration-progress` (`step`, `storesDone`, `storesTotal`, `store`; operator
  audience, at most one a second plus the first and last). A failed step keeps the
  plaintext, seals the gate with `migration-failed`, and the next `storage.unlock` resumes.
  A second run finds nothing to do. Holochain and lair files are never re-keyed.
- **Storage reset.** `ad4m-executor storage reset --data-path <dir> [--confirm <keyring id>]`
  (`at_rest::reset`) is offline only (it takes the data-folder lock) and never reaches
  outside the data dir. Without `--confirm` it only lists the moves. With the keyring id
  (`unreadable-keyring` for a keyring that does not parse) it moves the keyring, agent
  file, users, persons and their stores, perspectives, the executor database and every
  sealed language file into `<data>/lost-<time>/`. Language bundles, settings, the seed,
  logs and app data stay.
- **Local only.** A method registered `.local_only()` answers only the operator on this
  machine (`Session::is_operator_on_this_machine`: the admin credential, or no token on an
  executor without one, on a loopback listener). The method's guard answers first (403 "Admin
  credential required" for a caller without the credential); the admin credential on a
  network listener gets 403 `local-only`. It is on `agent.unlock`,
  `agent.signSessionStatement`, `identity.rekey`, `identity.custodyStatus`,
  `identity.takeMaster`, `identity.prepareDevice`, `identity.inbox`, `identity.inboxAck`,
  `identity.unwrapRecordKey`, `identity.acceptJoin`, `identity.cancelJoin`, `storage.unlock`,
  `runtime.quit` and `runtime.restart`, and `runtime.setConfig`
  refuses its `machine` keys the same way (the guard table in `api/AGENTS.md` marks them).
  An operator from a key login is never on this machine, so these refuse it too; it calls
  every other method as the operator, `agent.generate`, `agent.import`, `agent.lock`,
  `runtime.exportData` and `runtime.importData` included (a remote operator may move the
  whole data folder).
  An executor on `--localhost false` without TLS has no loopback listener, so these calls
  answer on no listener there.
- **Prolog is off.** `prolog_service::PROLOG_MODE` is `Disabled`; Prolog query paths return empty.
  Subject classes, flows and model queries run on SHACL + SPARQL (`perspectives/model_query`).
- **Tokio vs std locks** are both used; check the import before assuming `.lock()` is async.
- **JS ↔ Rust bridge**: `#[op2]` extensions in `js_core/*_extension.rs`, `holochain_service/`,
  `runtime_service/`; JS side in the sibling `.js` file and `js_core/host.js`. Every Language
  runtime gets every one of these extensions, so third-party language code reaches every op: an op
  that signs must sign as `get_runtime_agent_context()`, never as the main agent directly. The one
  other kind of JS runtime is a tool bundle's (`tool_bundles/sandbox.rs`), and it gets none of
  them: only its own `tool_bundle_host` extension. Never register a `js_core` extension there.
  Languages are called by building JS source strings and evaluating them (`languages/mod.rs`).
- **Agents and sessions**: an executor hosts N agents, and **an agent is its DID**.
  `AgentContext` (`agent/mod.rs`) is `Main` (agent #0, `is_main_agent()`) or `Managed { did }`
  (`managed_did()`). An email is only the login name of the user account that owns a managed
  agent: it resolves to the DID once, where a caller names an address (login, signup, email
  verification, `runtime.computeLog { userEmail }`, the language host's `*ForUser(email)` ops) via
  `AgentService::did_for_email` / `managed_agent_for_email`. Code that needs an address (SMTP,
  hosting info, the profile directory) looks it up with `AgentService::email_for_did`; nothing
  else carries an email. `users.did` is unique (partial index, empty DIDs excluded).
  `agent/session.rs` opens one `Session` per connection: `Principal::Operator` (the admin
  credential: the executor's administrator role, not an agent), `Principal::Agent(AgentContext)`
  (a grant on the main agent or on a managed agent, by DID), or `Principal::Anonymous`.
  Capabilities, the grant and the agent's DID are `Session` fields. Read identity with
  `acts_as()` / `acts_as_did()` / `managed_did()` / `is_operator()`; never decode the token
  again. Any signing/DID/billing path takes the `AgentContext` explicitly.
- **Grants** (`agent/grants.rs`, table `grants` in `db/grants.rs`) are the one allowlist of
  tokens: a token (id = its `nonce`) opens a session only while its grant exists, is active and
  has not expired, and the session's capabilities are the grant's (tokens carry `iss, aud, iat,
  exp, nonce, sub` only). A grant names its agent by `agent_did` (`NULL` = the main agent); a
  token's `sub` must name the same agent: none for the main agent, the DID for a managed agent.
  Both mint paths go through `grants::mint`: the app capability flow (a grant on the main agent)
  and a user login (an auto-approved grant on that user's agent). The admin credential is no
  token. Revoking or removing a grant cancels its `ended` token, which every socket of the
  session watches through `Session::ended()`. Start-up drops expired rows. The app registry of
  executors before grants (`apps_data.json`) is not read: no migration exists. Tests mint with
  `test_utils::granted_token(auth)`; a token signed without a grant is refused.
- **Key sessions** (`identity/key_login.rs`, `grants::mint_key_grant`): a login signed by a
  person's device key (`user.sessionChallenge`, `user.loginWithKey`; the statement names the
  origin it was signed for, accepted only from `public_origins` or, without it, this executor's
  own loopback origins: `src/identity/AGENTS.md`) mints a grant bound to that
  key (`KeyBinding`: the `did:scid`, the multikey, its key id; columns `scid`, `signing_key`,
  `key_id`, `head_seq`, `checked_at`) for one hour. Its role (`GrantRole`, column `role`) is
  `Operator` for the operator record's identity (`Session::open` opens `Principal::Operator`,
  `OperatorSource::Key`, within the grant) or `Agent` on the local agent that speaks for the
  identity. Its expiry is atomic: `user.renewSession` moves it and `Session::over` follows it,
  so open sockets stay open. `Grants::revoke_key` ends a key's grants on the key-log cache's
  `VerdictChange` and on the 5-minute re-check, which reads the store whatever the cache's
  age, so a revocation the change missed ends its sessions within 5 minutes (it also ends
  grants whose key could not be read for an hour). `user.logout` ends any grant; `user.endAllSessions` every grant of the
  caller's identity (and of a managed agent).
- **Managed agents' wallet keys** are named by the DID (`KEY_NAME_MAIN` for the main agent):
  `AgentService::create_managed_agent_key` creates one under its DID,
  `AgentService::wallet_key_for_did` finds it (cached). A shared wallet cannot name keys, so there
  the key sits under the user's email and counts only if it derives the DID. No migration from
  email-named keys exists: data from before D16 is not supported.
- **Billing** is per agent: `billing::bill_compute(agent_did, ..)`, `compute_log.agent_did`,
  `payment_requests.agent_did`. Credits stay on the user's `users` row, reached by
  DID (`get_agent_credits`, `deduct_credits_and_log`).
- **Capabilities**: every WS method declares a guard where it registers (`api/guards.rs`); the
  dispatcher checks its capability (`agent/capabilities/defs.rs`) and ownership rule before the
  handler runs. A handler calls `ctx.require(&X_CAPABILITY)` only for a check its guard does not
  make; `Session::may` answers the same question as a `bool`. MCP tools check via
  `Ad4mMcpHandler::get_*_perspective`.
- **Per-agent objects**: a session reaches only its agent's; the operator reaches all.
  Perspectives are owned by DID (`owners`; none = the main agent), AI tasks by DID
  (`tasks.agent_did`; `NULL` = the main agent), grants and capability requests by their agent's
  DID. A WS method that names one declares `Guard::Owns` (`api/AGENTS.md`); missing and not
  yours are both 404. The capability check alone does not
  separate users: user tokens hold wildcard perspective, neighbourhood and AI capabilities.
- **Capability requests** (#1399, executor side): `agent.requestCapability(authInfo, agent?)`
  names a target agent (a managed agent's DID, or its user's email, resolved to the DID at once; absent = the main agent) and records a
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
  with the owner's DID that may query the notification's perspective. `Ad4mDb::migrate_notification_owners` (run at startup once the agent loads)
  moves rows of older executors from `user_email` to `owner_did`.
- **Keystore**: the `keystore` field of `agent.json` holds every local key, the main agent's and
  each managed user's (`wallet.rs`: Argon2id + XChaCha20-Poly1305, fresh salt and nonce per write).
  The first unlock of a legacy keystore rewrites the file and keeps `agent.json.legacy` until an
  unlock opens `agent.json` directly (`AgentService::unlock`). Meanwhile every save writes
  `keystoreFingerprint` (the saved keys) into `agent.json`, and the fallback to the backup runs
  only while the backup holds exactly those keys. Write the agent file only through
  `AgentService::save` / `try_save`.
- **Identity** (`identity/`): an agent's device key (the wallet key) may speak
  for a `did:scid` whose log lists it; the 12 words never reach the executor and the master key
  rests in none (apps hold them through `ad4m-identity-client`; a master key sealed to the
  desktop app's computer opens only inside `identity.takeMaster`, for its custody service). The configured store's logs come through the
  agent language (`Capability::Identity*`, refused on any other language); a DID URL's foreign
  `src` is read by the executor itself (`identity/src_fetch.rs`, public addresses only at
  connect). The configured store's logs are kept after `fold` in `KelCache`; a foreign `src`
  log is folded in memory only, never stored, and a conflict it shows is reported in the
  resolution metadata (`coasys:skipped`), never recorded as a fork. A head counts only under a key of the
  config's `identity.storeKeys`, authorship reads a log only up to that head, and
  `identity.storeUrl` and `identity.storeKeys` become the agent language's `storeUrl` and
  `storeKeys` settings at every start (unset removes them; with neither set, the store and
  keys are the network seed's `identityStore` (the compiled-in `mainnet_seed.json` names
  `https://store.ad4m.dev`, today's staging network; a seed file without one, as an older
  `init` wrote, gets the compiled-in seed's), and without one there is no store; a
  `storeUrl` without `storeKeys` has no key; without a key the executor warns at start: no
  `did:scid` record verifies). A different event at a held seq that holds on the prefix
  records a fork, keeps the history and fires `identity-fork`; a store answering a shorter log
  is behind (or an unverified conflict), records nothing, and the cache answers.
  `ad4m-executor init --operator <did:scid>` records the operator (`operator_record`, never
  replaced by `init`); `identity.claimOffer` (operator only) claims with the main wallet key and
  refuses `already-linked` / `no-operator` / `first-link` inside
  `ClaimService::claim_for_main`. `agent.status` and `agent.get` gain `identity`; `did` stays the
  device `did:key`. `identity.resolve` (`identity/resolver.rs`) answers a DID Core resolution
  result for a `did:scid:coasys:1` DID URL or a device `did:key`; a URL's `src` must name a
  public HTTPS host unless `identity.allowPrivateSrc` is set. `identity.rekey` (local only, or
  `ad4m-executor rekey`) gives the main agent a new wallet key and moves the places that hold
  its DID (`identity/rekey.rs`); tokens signed with the old key stop working. A perspective
  that is not open at the time keeps the retired DID as owner until a later unlock moves it.
  `identity.custodyStatus` and `identity.takeMaster` are the custody channel
  (`identity/custody.rs`): only the custody credential (a session with
  `Principal::Custody`) on a loopback listener opens it, never the admin credential,
  which an app's window may hold. The custody credential is no operator: off a loopback
  listener `Session::open` refuses it, and it reaches only `custody::CUSTODY_METHODS`. With the
  channel on, `identity.inboxAck` is the custody service's alone too. The credential arrives as the first line of stdin
  (`config::read_custody_credential`, read by `ad4m-executor run` before anything else reads
  stdin; `--watch-stdin` then reads on from the same buffered `Stdin`), never from the
  environment or a flag: both are files another process, or the admin credential's host file
  reads, can open (`/proc/<pid>/environ`, `/proc/<pid>/cmdline`). `AD4M_CUSTODY_CREDENTIAL`
  (or `_FILE`) set refuses the start, exit 78. Do not add another way in. The device inbox (`identity/inbox.rs`): the main agent's joined
  inbox is read at start, unlock, binding and every 15 minutes; an executor with the custody
  channel keeps a master key message for `identity.takeMaster`, every other deletes it
  unopened. `identity.prepareDevice` (and `ad4m-executor link-code`) shows a device string and
  claims the ticket a holder pushes to its pre-join inbox (`link-ticket`); the private record
  and the request box pass through `identity/record.rs`.

## Do / don't

- Don't add to `perspective_instance.rs`, `db.rs`, `languages/mod.rs`, `ai_service/mod.rs`: they are
  being split (spec items 3, 4, 9). Put new code in a new file with its own `impl` block.
- New directory ⇒ add an `AGENTS.md` (≤ 40 lines: purpose, files, entry points, invariants,
  "do not") and a sibling `CLAUDE.md` containing only `@AGENTS.md`.
- Mechanical moves and behaviour changes in separate commits.
- Stale docs in this dir: `LOGGING.md` is current. Anything named `PHASE1_*` is obsolete.
