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

Handler shape today (`perspectives_ws.rs::add_link` is representative):

```rust
async fn add_link(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let uuid = params.require_str("uuid")?;                       // ParamExt
    check_capability(&ctx.capabilities, &perspective_update_capability(vec![uuid.clone()]))
        .map_err(|e| WsRpcError::forbidden(e))?;                  // repeated in every handler
    let body: AddLinkRequest = serde_json::from_value(params.clone())
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let perspective = get_perspective_with_access(&uuid, &ctx).await?;
    let agent_context = ctx.acts_as();                              // the session's user, or the main agent
    ...
    Ok(serde_json::to_value(result)?)
}
```

Spec item 6 replaces the first block with `register_with(name, CapSpec, typed_handler)`.
Until then: **every new handler must check a capability** (or be registered with an
explicit comment saying why not) and take `AgentContext` from the session
(`ctx.acts_as()`) for anything that signs, bills or writes. Never
re-derive it from the token: a token that stops decoding must not turn into the main agent.

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
Behaviour changes to any handler also need the JS integration suite (`pnpm run
test-main` at repo root).
