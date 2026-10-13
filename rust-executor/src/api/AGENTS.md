# api/ — agent guide

axum HTTP + WebSocket surface. Two protocols: AD4M WS RPC and an OpenAI-compatible
REST/WS shim. Split plan: spec item 6.

## Routes (`mod.rs`)

| Route | Handler | Notes |
|---|---|---|
| `GET /api/v1/ws` | `ws_rpc.rs` | JSON-RPC-ish: `{type, id, ...params}` → `HandlerMap::dispatch`. Auth once at upgrade (`auth.rs`). Per-request cancel token (`request.cancel`). **Also carries events** (`events_ws::build_event_stream`, filtered by `events.watch`) and the connection's live query updates |
| `GET /api/v1/ws/events` | `events_ws.rs` | Standalone event stream (same events, no live query updates; candidate for removal, spec D3) |
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
  inline on both sockets (per-connection state, like `request.cancel`).
- Every socket (RPC, `/events`, `/v1/realtime`) keeps the caller's `Arc<Session>` and calls
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
  every socket (and the SSE chat stream) also waits on `session.over()`, which completes when
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
