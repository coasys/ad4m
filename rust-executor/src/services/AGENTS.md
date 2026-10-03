# services/ — agent guide

Service Languages: typed, content-addressed service interfaces, the
implementations that provide them, and the host every caller reaches them
through.

## Files

| File | What it owns |
|---|---|
| `interface.rs` | Interface document types, structural rules, JCS hash (`content_hash`, the language hash function), module ID `service://<genesis hash>` (`module_uri` / `genesis_of`; the hash covers `author`, so successors must keep the genesis author, checked in `registry.rs`). Document fields `module` / `previous` and wire method prefixes stay bare hashes, author signature check, `standalone_schema` (`#/types/X` → `$defs`) |
| `semver.rs` | PATCH / MINOR / MAJOR checks of a version against its `previous`. MINOR uses a conservative subset checker: it proves additive edits and refuses the rest |
| `registry.rs` | Interfaces, module chains, implementations, preferences, resolution (`resolve`), event fan-out (`event_targets`) |
| `host.rs` | `ServiceHost`: dispatch (resolve → grant → params schema → call → result check), start/stop/health, event emit, `service-stream-end` |
| `builtin.rs` | `ServiceImplementation` trait, `CallContext` (grant layers), `EventEmitter`, `ServiceCaller` |
| `capability.rs` | Grants: `<moduleId>@<compat>` (`service://<hash>@<compat>`) × action; `allowed` needs every grant layer |
| `ws.rs` | Core RPC methods `services.describe`, `services.interface`, `services.setPreference` |
| `schema_export.rs` | `InterfaceBuilder`: builtin interface documents from Rust types (`schemars`) |
| `codegen.rs` | TS client module, MCP tool descriptors, Markdown; used by `ad4m service-gen` (`cli/src/service_gen.rs`) |
| `builtins/` | The executor's own services (below) |
| `tests.rs` | Test-only `echo` service: host dispatch, grants, events, and a round trip over the RPC socket |
| `fixtures/echo.interface.json` | Checked-in echo interface. Regenerate: `UPDATE_SERVICE_FIXTURES=1 cargo test --lib services::tests::echo_interface_is_current` |

## Built-in services (`builtins/`)

| Interface | Module | Over |
|---|---|---|
| `ai.inference` (prompt, embed, transcription, tasks) · `ai.models` | `ai.rs` | `AIService` |
| `billing.ledger` (credits, rates, free access, compute log, `check`) · `billing.settlement` (linked HoT wallet) | `billing.rs` | `Ad4mDb` billing tables |
| `unyt.wallet` | `unyt.rs` | `unyt_service` |
| `holochain.conductor` (agent infos, metrics, restart) | `holochain.rs` | `HolochainServiceInterface` |

- All built-ins share one author DID (`builtins::AUTHOR`) and are registered and started by `builtins::start_all`, called from `api::start_server`. Their event bridges (pubsub topic → service event) run on that runtime.
- `interfaces/*.json` are the checked-in contracts. Regenerate: `UPDATE_SERVICE_FIXTURES=1 cargo test --lib services::builtins::tests::interfaces_are_current`. `pnpm run generate:api-types` in `core/` also writes `core/src/generated/services/*.ts` and `rust-client/src/services.rs`; a test fails when either is stale.
- The host meters methods that declare `meter` by calling `billing.ledger.check` first (402 on refusal). Charging itself still happens inside `AIService`.
- Operator-only actions (host rates, Unyt DNA, Holochain restart, another user's compute log) check `CallContext::is_admin` (the admin credential) on top of the grant.
- `get_user_default_capabilities` grants users the built-in actions they need; operator actions are not in it.
- Entry points outside the host (OpenAI-compatible API, transcription feed) check grants with `builtins::capability(Builtin, action)`.

## Wire

- A method `<hash>.<method>` (interface version hash, or implementation hash to pin) is not in `HandlerMap`; `HandlerMap::dispatch` falls back to `services::host()` when the name parses as `<hash>.<method>`.
- An event goes out as `<interface hash>.<event>` once per registered compatible version, through `build_event_stream_for` (owner DID + grant filtered) and `events.watch` (scope field from the interface, `event_interest::event_scope`).
- A streaming method's reply can overtake its chunks. The host emits the core event `service-stream-end { streamId, method, ok }` after the call; it travels behind the chunks. The SDK's `ServiceClient.stream` waits for it.
- Errors: `WsRpcError { code, message, data }`. Declared method errors use their code and `data.name`. Protocol codes: 400 params, 403 grant, 404 unknown, 500, 502 (non-builtin result outside contract), 503 not running, 504 deadline.

## Rules and gotchas

- Interface params must be closed (`additionalProperties: false`). Otherwise a MINOR cannot add optional params safely, and unknown params would pass silently.
- A streaming method needs a required `streamId` param and a stream event scoped by `streamId`.
- The global host (`services::host()`) is shared by every test in the process. Tests that touch it register interfaces under a unique author DID so hashes never collide; host-only tests use `ServiceHost::new()`.
- `serde_json` here preserves key order (feature unification), so generated TS lists properties in document order.
- Builtin results are checked only in debug builds (as `HandlerMap::dispatch` does for core methods); non-builtin results always (→ 502).
- A new version is checked against its `previous` and against its registered neighbours in the line: a chain may branch, and resolution lets any higher version serve a lower one.
- The semver checker keeps annotation-named data (a property called `title`, `enum` / `const` values) and never sees through a `$ref` it cannot compare on both sides. Keep both rules when you extend it: MINOR must never accept a breaking change.
- `services.setPreference` needs `agent:UPDATE`. The executor default of an `executor`-selection interface needs the admin credential, also in single-user mode.
- Only `builtin` runtimes exist so far. `register_implementation` refuses other `runtime.kind`s.
