# api/ — agent guide

axum HTTP + WebSocket surface. Two protocols: AD4M WS RPC and an OpenAI-compatible
REST/WS shim. Split plan: spec item 6.

## Routes (`mod.rs`)

| Route | Handler | Notes |
|---|---|---|
| `GET /api/v1/ws` | `ws_rpc.rs` | JSON-RPC-ish: `{type, id, ...params}` → `HandlerMap::dispatch`. Auth once at upgrade (`auth.rs`). Per-request cancel token (`request.cancel`). **Also carries events** (`events_ws::build_event_stream`, filtered by `events.watch`) |
| `GET /api/v1/ws/events` | `events_ws.rs` | Standalone event stream (same content as above; candidate for removal, spec D3) |
| `GET /health`, `POST /internal/shutdown` | `internal.rs` | `INTERNAL_API_TOKEN` |
| `/v1/*`, `/api/v1/openai/v1/*` | `openai_compat/router.rs` | chat/completions, embeddings, audio, realtime WS |

## Handler modules

`*_ws.rs`, one per RPC namespace: `agent`, `ai`, `expressions`, `hosting`,
`languages`, `neighbourhoods`, `perspectives` (largest; also SHACL + interpretation
handlers), `runtime`, `users`. Each exposes `register_ws_handlers(&mut HandlerMap)`
called from `ws_handler::build_handler_map`.

Handler shape (`perspectives_ws.rs::add_link` is representative):

```rust
async fn add_link(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: AddLinkRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let mut perspective = guarded_perspective(&params).await?;     // the guard checked access
    let agent_context = ctx.acts_as();                              // the session's user, or the main agent
    ...
    Ok(serde_json::to_value(result)?)
}
// registered with .guard(Guard::perspective(perspective_update_capability))
```

A guard (next section) checks the capability and the access, so the handler does neither: a
perspective handler only fetches the perspective (`guarded_perspective`). **Every method
registers a guard**: the executor does not start with a method without one (a method open to
callers without any capability registers `Guard::Open`, with a comment saying why). A handler
takes `AgentContext` from the session
(`ctx.acts_as()`) for anything that signs, bills or writes. Never
re-derive it from the token: a token that stops decoding must not turn into the main agent.

## Guards (`guards.rs`)

A method declares who may call it where it registers:

```rust
map.method::<PerspectiveUuidParams, Option<PerspectiveHandle>>("perspective.get", handler)
    .read()
    .guard(Guard::perspective_or_null(perspective_query_capability));
map.method::<AgentRemoveAppParams, Vec<Apps>>("agent.removeApp", remove_app)
    .guard(Guard::Node(AGENT_UPDATE_CAPABILITY.clone()));
```

`Guard::perspective(cap)` and `Guard::perspective_or_null(cap)` build `Guard::Perspective` on
the `uuid` param (`hides_existence` false and true).

`HandlerMap::dispatch` runs, in order: find the method (404) → params deserialize into `P`
(400) → the guard's capability (403) → the guard's rule (403, or `null` without running the
handler when `hides_existence` is set and the perspective is missing or not the caller's) → the
handler. `HandlerMap::admit` runs the steps before the handler alone.

| Guard | Rule after the capability |
|---|---|
| `Open` | no capability and no rule: every caller, anonymous included, reaches the handler, which decides |
| `Operator` | no capability; `ctx.is_operator()` (the admin credential, or no token on a loopback listener of a node without one), else 403 "Admin credential required" (`guards::OPERATOR_ONLY`), whatever the caller's capabilities |
| `Anonymous(cap)` | none; `cap` must be one anonymous callers hold |
| `Node(cap)` | no user session: 403 naming the method, whatever the user's capabilities; the operator, apps and anonymous callers pass |
| `SelfAgent(cap)` | an authenticated session; the handler acts on `ctx.acts_as()` |
| `Perspective { .. }` | `get_perspective_with_access`: the operator reaches all; a user its own; an app the unowned and the main agent's; 404 when missing |
| `Task { .. }` | an authenticated session; a user reaches only its own AI tasks (`tasks.user_email`), and gets 404 for any other id, its own missing or not; the operator and apps reach every task |

Capabilities stay: every guard but `Open` and `Operator` checks one first, and the rule adds
whose data the call may touch. A method takes exactly one guard. A second panics where it
registers. A method without one makes `build_handler_map` panic, naming it
(`HandlerMap::require_guards`): the API thread builds the map before it serves, so the executor
binary exits at start. Dispatch also refuses (500) a method without a guard in a map built
without that check. A guard runs before the handler's own checks, so adding one changes nothing for callers the
handler already refuses with the same code. A handler check that is not part of the guard
(compute credits, param validation beyond the contract type) now runs after the access check:
a call that fails both gets the access answer.

Which guard each method has:

- `Open`: the methods that check no capability: `agent.isLocked`, `runtime.info`,
  `runtime.tlsDomain`, `runtime.freeHostingEnabled`, the `runtime.unyt*` stubs (501),
  `user.credits` (501), `user.logout` and `user.revokeAllSessions` (their handlers check the
  session's principal).
- `Operator`: `runtime.openLink`, `runtime.restartHolochain`, `runtime.setHostRates`,
  `runtime.setUnytMembraneProof`, `user.setMultiUserEnabled`, `user.freeAccess`.
- `Node`: the node-wide methods that refuse user sessions (agent keys, apps, trust list and
  entanglement writes, `agent.permitCapability`, AI models, hosting wallet, language
  publish/remove/settings, export/import, friends and outbox, link-language template writes,
  `runtime.addHcAgentInfos`, `runtime.setFreeHostingEnabled`, `runtime.quit`, `user.list`,
  `user.wallet`, `user.emailTest`). `runtime.importData` keeps its handler's
  `ctx.is_operator()` check: the guard admits apps. `runtime.exportData` of one perspective
  checks `get_perspective_with_access` in its handler (a `db` export names none), so an app
  exports only the unowned and the main agent's perspectives.
- `Anonymous`: `agent.requestCapability`, `agent.generateJwt`, `user.create`, `user.login`,
  `user.verifyEmail`, `user.requestVerification`, `user.multiUserEnabled`.
- `SelfAgent`: the methods any authenticated caller with the capability may call, users
  included. Most act as the caller (profile, sign, notifications, embeddings, expressions,
  `neighbourhood.join`, `ai.addTask`, which records the caller's user as the task's owner,
  `perspective.create`, which the caller's user owns, `hosting.setHotWallet`, which also
  refuses a session without a user). `perspective.all` lists the caller's own perspectives
  (every one to the operator).
  `runtime.grantNotification` keeps its handler's `ctx.is_operator()` check, so a user gets
  the handler's message.
  `ai.tasks` lists a user's own tasks, and every task to the operator and apps.
  Some read node-wide data that users read today: `agent.getApps`,
  `agent.getTrustedAgents`, `ai.models`, `language.*` reads, link-language templates,
  `runtime.hcAgentInfos`/`networkMetrics`/`hostRates`/`unytVersionInfo`. `Node` would refuse
  users there, a behaviour change.
- `Perspective`: every `perspective.*` method that names one perspective, and every
  `neighbourhood.*` method that names one (`publish` by `perspectiveUuid`, the others by
  `uuid`), on the neighbourhood capability: the perspective that holds a neighbourhood decides
  who may use it.
- `Task`: `ai.updateTask` (`task.taskId`, the row it rewrites), `ai.removeTask` (`id`) and
  `ai.prompt` (`taskId`).

`tests/method_matrix_tests.rs` walks every method in `build_handler_map()` (one without a
guard fails it; `every_registered_method_has_a_guard` says so on its own) and calls
`admit` as each kind of caller (operator, app with and without the capability, user, another
user, anonymous) on its own, others', unowned, the main agent's and missing perspectives, and
on the user's own, an operator's and a missing AI task. A new guard is covered without a new
test; a method whose params need more than `uuid` adds an entry to `EXTRA_PARAMS`, with
`TARGET` where it names the row's perspective or task in another param.

The MCP tools that wrap an RPC method dispatch through the same map
(`ws_handler::shared_handler_map`, `mcp::tools::call_method`), so the guards hold on MCP too.
`tests/method_matrix_mcp_tests.rs` runs each of them as every matrix caller and checks it
against `admit` of its methods (see `mcp/AGENTS.md`).

## Protocol

- Clients (core SDK, rust-client) ship with the executor from the same revision: no feature
  discovery and no compatibility modes.
- Events: a socket gets no events until it sends `events.watch { "<type>": null | [perspective
  uuids] }` (replaces the last watch; `events.unwatch` clears it). `event_interest.rs`, handled
  inline on both sockets (per-connection state, like `request.cancel`).
- Live query updates (`query-subscription-update`) pass `events.watch` untouched: every socket of
  the perspective owner gets them, and the SDK routes them by `subscriptionId`.
- One event path. A publisher calls `pubsub.publish_event(Event::new(Topic, Audience, payload))`
  (`src/events.rs`); there is no default audience. A socket's stream
  (`events_ws::build_event_stream_for`) delivers it when `Session::may_receive` (the audience) and
  `event_interest::wants` (the watch, keyed by `Topic`) hold, as `events_ws::wire_message`. No
  check takes a lock or parses the payload. The stream builder takes the socket's interest, so
  callers pass it (`Connection::interest()` on the RPC socket). Only authenticated sessions
  (`Session::is_authenticated`: not anonymous) get a stream on the RPC socket. The stream ends at
  the first event after `Session::check()` fails. `audience_matrix_tests` in `events_ws.rs` pins
  who receives each audience; a new topic or audience adds its cases there.
- One string topic remains: live query updates (`PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC`, sent as
  `query-subscription-update`). `events_ws::query_updates` waits for the perspective handle to
  check that the session owns the perspective (`matches_perspective_owner`, spawned so a parked
  stream cannot hold the handle). It stays until live queries belong to a connection (#1207); it
  then becomes a typed event to `Audience::Connection`. Language code cannot publish anything
  (`js_core/pubsub_extension.rs`). `notification-triggered` goes to the notification's `ownerDid`, never to
  co-owners. `signal` goes to its `recipient` (`perspectives::signal_event`), or to every
  session when it names none. `hosting-user-info-changed` goes to the user's DID (looked up by
  email at publish) and the main agent's (the operator and apps see every user's).
  `message-received` has no publisher; a new one uses `Audience::Authenticated`.
  `model-loading-status` goes to everyone; `transcription-text` to the stream owner's DID.
  `auto-processor-event` goes to `Audience::OperatorAndDids([pass owner])` when the owner may
  read the perspective, else to the operator; `auto-processor-neighbourhood-state` to the
  operator and the perspective's owners (every authenticated session for an unowned
  perspective). `auto_processor::events::emit` reads the owners from the handle when it
  publishes, so no emitter may hold the perspective's handle lock while it emits.
  `agent-status-changed` and `agent-updated` go to the DID the payload names
  (`Audience::agent`; a status without a DID, before `agent.generate()`, to everyone).
  `apps-changed` and the approval exceptions (`CapabilityRequested`,
  `InstallNotificationRequest`) go to `Audience::Approvers`: the sessions whose capabilities
  cover `AGENT_PERMIT` (the operator, and apps whose token holds it), not only the operator.
  Other exceptions go to everyone (`Audience::for_exception`). The perspective and link topics' audience is
  `perspectives::event_recipients` (the owners, or the main agent for an unowned perspective),
  read when the event is published. `sync-state-change` goes out once, to all owners.
  `perspective-added/-updated/-removed` and `link-added/-removed/-updated` go out once per owner
  (`perspectives::publish_to_each_owner`), each to that owner, with the owner's DID in the
  payload's `owner`.
- Every socket (RPC, `/events`, `/v1/realtime`) keeps the caller's `Arc<Session>` and calls
  `Session::check()` again on each request and each delivered event, so revocation and expiry end
  an open connection. Tests build sessions with `Session::for_test` / `for_test_expiring`.
- App and user sockets also register in `agent::session_registry` while open (an RAII guard).
  `revoked::revoke` and `revoked::revoke_all_sessions` cancel their token, and the socket closes
  at once with `ws_rpc::token_ended_frame()` (1008), the same frame a failed check sends. Only
  the socket's own task writes the close frame. The RPC socket first delivers in-flight replies
  for up to `REPLY_GRACE`, so `user.logout` gets its answer. A new socket type must register too.
- User revocation: `user.logout` revokes the caller's token id; `user.revokeAllSessions` sets a
  per-user cutoff (`users.sessions_valid_after`, cached in `revoked::SessionCutoffs`) that refuses
  every token with `iat` at or before it. `generate_user_jwt` issues later tokens after the cutoff.
- `ws_rpc::serve` runs one RPC connection over any text stream; `tests/connection_tests.rs` drives
  it through channels in place of a WebSocket.
- Contracts: each handler registers its params and result types (`map.method::<P, R>`), and
  `event_specs()` in `events_ws.rs` types every event payload. `tests/handler_table_tests.rs`
  writes `RpcMethods.ts` and `Events.ts` (ts-rs export dir) and fails if the copies in
  `core/src/generated/api/` or any type they import are stale — regenerate
  (`pnpm run generate:api-types` in `core/`) after changing a handler or an event.

## Types

- `types.rs`: request/response structs for WS (`ts-rs` exported for the SDK).
  `pnpm run generate:api-types` (in `core/`) writes one file per type into
  `core/src/generated/api/`, but not its `index.ts`: add the export line there by hand.
- `crate::types::core` (domain) vs `crate::types::domain` (wire/input). Some duplicates,
  see spec item 5. Prefer `crate::types::X` re-exports.
- `WsRpcError { code, message }` (`ws_handler.rs`): constructors `bad_request`,
  `forbidden`, `not_found`, `internal`. No `From` impls for domain errors yet.

## openai_compat/

Self-contained wire translation over `AIService`. Exception: `harness_bridge.rs` +
`tool_grammar.rs` are the interpretation harness's `CompletionSource` and tool
grammar, used by `perspectives/interpretation/run.rs`. They move to `agentic/`
(spec item 7); don't add more non-wire code here.

## Tests

`tests/` (`types_tests.rs`, `shacl_ws_tests.rs`) and `openai_compat/tests.rs`.
`tests/multi_user.rs` starts a multi-user node and opens sessions from tokens the node signs,
as the WS upgrade does; the `*_access_tests.rs` files drive `build_handler_map().dispatch` as
different users with it. Behaviour changes to any handler also need the JS integration suite (`pnpm run
test-main` at repo root).
