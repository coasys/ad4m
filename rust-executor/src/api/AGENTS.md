# api/ — agent guide

axum HTTP + WebSocket surface. Two protocols: AD4M WS RPC and an OpenAI-compatible
REST/WS shim. Split plan: spec item 6.

## Routes (`mod.rs`)

| Route | Handler | Notes |
|---|---|---|
| `GET /api/v1/ws` | `ws_rpc.rs` | JSON-RPC-ish: `{type, id, ...params}` → `HandlerMap::dispatch`. Auth once at upgrade (`auth.rs`). Per-request cancel token (`request.cancel`). **Also carries events** (`events_ws::build_event_stream`, filtered by `events.watch`) and the connection's live query updates; the only event socket |
| `GET /health`, `POST /internal/shutdown` | `internal.rs` | `INTERNAL_API_TOKEN` |
| `/v1/*`, `/api/v1/openai/v1/*` | `openai_compat/router.rs` | chat/completions, embeddings, audio, realtime WS |

## Handler modules

`*_ws.rs`, one per RPC namespace: `agent`, `ai`, `expressions`, `hosting`,
`languages`, `neighbourhoods`, `perspectives` (largest; also SHACL + interpretation
handlers), `runtime`, `users`. Each exposes `register_ws_handlers(&mut HandlerMap)`
called from `ws_handler::build_handler_map`.

Handler shape today (`perspectives_ws.rs::add_link` is representative):

```rust
async fn add_link(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;                       // ParamExt
    ctx.require(&perspective_update_capability(vec![uuid.clone()]))
        .map_err(|e| WsRpcError::forbidden(e))?;                  // repeated in every handler
    let body: AddLinkRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = ctx.acts_as();                              // the grant's agent, or the main agent
    ...
    Ok(serde_json::to_value(result)?)
}
```

Spec item 6 replaces the first block with `register_with(name, CapSpec, typed_handler)`.
Until then: **every new handler must check a capability** (or be registered with an
explicit comment saying why not) and take `AgentContext` from the session
(`ctx.acts_as()`) for anything that signs, bills or writes. Never
re-derive it from the token: a token that stops decoding must not turn into the main agent.

**An agent is its DID.** A user session is known by its managed agent's DID
(`ctx.user_did()`); there is no `ctx.user_email()`. Bill, own, filter and log by that DID. A
parameter that names a user by email (`runtime.computeLog { userEmail }`, the operator's
`user.*` calls) resolves to the DID at the handler with `AgentService::did_for_email`; a reply
field that shows an address (`hosting.info`'s `email`, the compute log's `userEmail`) reads it
back with `AgentService::email_for_did` or a join on `users.did`.

`RequestContext` (`types/domain.rs`) is the caller for one request: `session: Arc<Session>`,
the per-request `cancel_token`, and `auth_token` (read only by the interpretation pass; Plan 20
PR 6 removes it). The RPC socket opens one per connection (`RequestContext::open`); REST and the
OpenAI-compatible API take it as an axum extractor (`auth.rs`), which answers 401 for a token
that opens no session and 503 while the wallet is locked. `auto_permit_cap_requests` is read
from the global config where `agent.requestCapability` uses it.

## Protocol

- Clients (core SDK, rust-client) ship with the executor from the same revision: no feature
  discovery and no compatibility modes.
- Events: a socket gets no events until it sends `events.watch { "<type>": null | [perspective
  uuids] }` (replaces the last watch; `events.unwatch` clears it). `event_interest.rs`, handled
  inline on the RPC socket (per-connection state, like `request.cancel`). A list matches events
  whose `Event::perspective` (the payload's `perspectiveUuid`) it names; unscoped payloads carry
  none. There is no separate events socket (`/api/v1/ws/events` was removed).
- Live query updates (`query-subscription-update`) pass `events.watch` untouched. A live query
  belongs to the RPC connection that opened it (`RequestContext::connection_id`, #1207): its
  update is an `Event` with `Event::connection` set (`Event::for_connection`), and a socket's
  stream drops an event whose connection is not its own before `may_receive` runs, so the update
  needs both its connection and its audience. The key is not on the wire. When a socket's stream
  lags it sends `{ "type": "query-subscription-update", "lagged": true }` and `LiveQuery` resyncs.
  Closing the socket disposes its subscriptions (`ws_rpc::serve`).
- One event path. A publisher calls `pubsub.publish_event(Event::new(Topic, Audience, payload))`
  (`src/events.rs`); there is no default audience. Wire names live in `Topic::as_str` only;
  `events_ws::event_specs()` is keyed by `Topic`. A socket's stream
  (`events_ws::build_event_stream`) delivers an event when `Session::may_receive` and
  `event_interest::wants` (the watch, keyed by `Topic`) hold, as `events_ws::wire_message`.
- `Audience = Everyone | Dids(Vec<String>) | Operator`. `may_receive` needs both: the session is
  in the audience (everyone, `Session::acts_as_did` is one of the DIDs, or for `Operator` the
  operator principal only; a grant acts as its agent, the operator as the main agent it owns),
  and it holds `Event::capability`. `Event::new` fills
  the capability from `Topic::capability(perspective)`, the capability of the RPC method that
  reads the same data (#1284): per-perspective topics name the event's perspective
  (`perspective.queryLinks`' `perspective_query_capability([uuid])`; a signal: neighbourhood READ
  on it); an event without a perspective needs the grant on `*`. Approval requests carry
  AGENT_PERMIT instead (`Event::approval_request`). An app acts as the main agent, so without the
  capability an app approved for AI or one perspective got every perspective's events. A user
  token holds every topic's capability but `apps-changed`'s (AGENT_PERMIT) and the operator may
  do anything, so only narrower tokens lose events. A new topic picks its capability in the same
  exhaustive `match`. No check takes a lock or parses the payload. The stream builder takes the
  socket's interest (`Connection::interest()`). Only authenticated sessions
  (`Session::is_authenticated`: not anonymous) get a stream. The stream ends at the first event
  the session may receive after `Session::check()` fails. `audience_matrix_tests` in
  `events_ws.rs` pins who receives each audience; a new topic or audience adds its cases there.
- The operator has no view of other agents' events: it receives what the main agent receives,
  plus `Operator` events (what only the operator may read).
- Audiences. Every per-perspective topic reads its recipients from one helper,
  `perspectives::event_recipients`: the owners, or the main agent for an unowned perspective
  (or one whose owner list is empty), read when the event is published. No per-perspective event
  goes to `Everyone`. Each goes out once, to all recipients (`perspectives::publish_to_owners`),
  and no payload names an `owner`: `perspective-added/-updated/-removed`, `sync-state-change`,
  `link-added/-removed/-updated`. `query-subscription-update`
  (`PerspectiveInstance::send_delta_update`, which reads the recipients when it publishes)
  goes only to the subscriber (`SubscribedQuery::subscriber`, the main agent for `None`) when it
  is one of them: the query ran as the subscriber, so a co-owner must not get its results.
  `signal` goes to its `recipient` (`perspectives::signal_event`); one without a recipient goes
  once to each of `event_recipients`, each copy naming that DID as `recipient`
  (`perspectives::publish_telepresence_signal`, used by both link-language paths and by
  `send_signal` / `send_broadcast` for the executor's own agents: a neighbourhood's owners are
  only this executor's agents). `notification-triggered` goes to the notification's `ownerDid`,
  never to co-owners, and needs the query capability on its perspective (its `triggerMatch` is
  query results), not `runtime.notifications`' AGENT_UPDATE. `auto-processor-event` goes to the pass owner when it is one of
  `event_recipients`, else nobody (`auto_processor::events::pass_audience`);
  `auto-processor-neighbourhood-state` to `event_recipients` (empty for an unknown
  perspective). `flow-state-changed` goes to the agent whose bookkeeping moved when it is one of
  `event_recipients`; the main agent's state on a perspective it does not read (the operator
  voted there) to `Operator` only, as `perspective.flowStates` lets only the operator read it;
  any other agent's to nobody (`replica_state::state_changed_audience`). `auto_processor::events::emit` reads the handle when it publishes, so no emitter
  may hold the perspective's handle lock while it emits. `hosting-user-info-changed` goes out twice
  (`Event::hosting_user_info`): to the user's DID (looked up by email in the local users table
  at publish, never imported) with RUNTIME_HOSTING_READ, and to the main agent's with
  RUNTIME_USER_MANAGEMENT_READ (the operator and apps that may list users see every user's). `model-loading-status` goes to everyone; `transcription-text` to the stream
  owner's DID. `agent-status-changed` and `agent-updated` go to the DID the payload names
  (`Audience::agent`; a status without a DID, before `agent.generate()`, to everyone).
  `apps-changed` (`Event::apps_changed`) and the approval exceptions (`CapabilityRequested`,
  `InstallNotificationRequest`, `Event::approval_request`) go to the DID of the agent the
  approval is for (the main agent today; #1399 passes a managed agent) with capability
  AGENT_PERMIT: the operator and main-agent apps whose token holds it, whether or not they
  subscribe to exceptions. Other exceptions go to everyone (`Event::exception`) with the
  exception subscription's capability.
- Every socket (RPC, `/v1/realtime`) keeps the caller's `Arc<Session>` and calls
  `Session::check()` again on each request and each delivered event, so revocation and expiry end
  an open connection. Tests build sessions with `Session::for_test(principal)` plus
  `.with_caps(..)`, `.with_did(..)`, `.with_grant(..)`, `.expiring_at(..)`, and contexts with
  `RequestContext::{operator,app,user}_for_test` / `for_session` (`.on_connection(id)` for a
  context on an RPC connection).
- Each socket's "session over" token is `ctx.session.ended()`: a child of the grant's `ended`
  token (a token that never fires for the operator and anonymous callers). Revoking or removing
  the grant cancels it, and the socket closes at once with `ws_rpc::token_ended_frame()` (1008),
  the same frame a failed check sends. A grant revoked before the socket starts serving gives an
  already-cancelled child, so there is no race to check. Only the socket's own task writes the
  close frame. The RPC socket first delivers in-flight replies for up to `REPLY_GRACE`, so
  `user.logout` gets its answer; each call's `cancel_token` is a child of the socket's token, so
  the calls still running are cancelled (401). Expiry closes an idle socket the same way:
  every socket (and the SSE chat stream, and the RPC socket's event stream,
  `build_event_stream`) also waits on `session.over()`, which completes when
  the grant ends or the token expires (leeway included). A new socket type must watch
  `session.over()` too.
- Revocation is on grants (`agent::grants::Grants`): `agent.revokeToken` (by request id or by
  the token) and `agent.removeApp` act on grants of the caller's own agent, or any for the
  operator; another agent's grant answers as missing. `user.logout` revokes the caller's grant;
  `user.revokeAllSessions` revokes every grant on that user's agent. `agent.getApps` lists the
  grants of `acts_as()`.
- Live queries: `subscribeQuery` / `modelSubscribe` reply `{ subscriptionId, result, revision }`
  (revision 0, or later if a write landed while the first result was computed: the subscription is
  registered first);
  each `query-subscription-update` carries the change (models: `ids` + `upsert`; queries: `added` /
  `removed` rows, or the whole `result` when the rows would lose their order; see
  `perspectives/perspective_instance/subscriptions.rs`) and `revision` (+1 per update). On a gap,
  `perspective.resyncSubscription { uuid, subscriptionId }` → `{ revision, result }`. When the
  update topic lags, the socket gets one `{ type: 'query-subscription-update', lagged: true }` and
  the client resyncs every live query.
- A live query belongs to the RPC connection that opened it (`RequestContext::connection_id`, set
  by `ws_rpc::serve` next to the connection's `Session`): only that socket gets its updates (they
  bypass `events.watch`), and `ws_rpc::serve` ends them when the socket closes, and again as each
  call still in flight ends (a late subscribe). No keepalive; subscribing without a connection
  (REST) is a 400.
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
Behaviour changes to any handler also need the JS integration suite (`pnpm run
test-main` at repo root).
