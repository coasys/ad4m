# api/ — agent guide

axum HTTP + WebSocket surface. Two protocols: AD4M WS RPC and an OpenAI-compatible
REST/WS shim. Split plan: spec item 6.

## Routes (`mod.rs`)

| Route | Handler | Notes |
|---|---|---|
| `GET /api/v1/ws` | `ws_rpc.rs` | JSON-RPC-ish: `{type, id, ...params}` → `HandlerMap::dispatch`. Auth once at upgrade (`auth.rs`). Per-request cancel token (`request.cancel`). **Also inlines the full event stream** from `events_ws::build_event_stream` |
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
    let agent_context = AgentContext::from_auth_token(ctx.auth_token.clone());
    ...
    Ok(serde_json::to_value(result)?)
}
```

Spec item 6 replaces the first block with `register_with(name, CapSpec, typed_handler)`.
Until then: **every new handler must check a capability** (or be registered with an
explicit comment saying why not) and take `AgentContext` from the token for
anything that signs, bills or writes.

## Protocol v2 (opt-in features)

- Every v2 feature is opt-in per call (a new method, or a new parameter such as `delta: true`), so
  a client that does not use it sees v1 behaviour. There is no feature discovery: clients are built
  against the executor they talk to.
- `event_interest.rs`: `events.watch` / `events.unwatch`, handled inline on both sockets (per-connection
  state, like `request.cancel`). No watch = every event.
- `ws_rpc::serve` runs one RPC connection over any text stream: `Connection::handle_text` answers
  each client message, and the event task is aborted when the stream ends. `tests/connection_tests.rs`
  drives it through channels in place of a WebSocket.
- Subscriptions: `subscribeQuery` / `modelSubscribe` reply `{ subscriptionId, result, revision: 0 }`;
  each `query-subscription-update` carries the change (`added` / `removed` / `changed`) and
  `revision` (+1 per update). On a revision gap, call `perspective.resyncSubscription
  { uuid, subscriptionId }` → `{ revision, result }` and apply only the updates after that revision.
- A subscription belongs to the RPC connection that opened it (`RequestContext::connection_id`):
  only that socket gets its updates (`connectionId` on the pubsub payload, stripped by
  `events_ws::query_update_for`), and `ws_rpc::serve` ends them when the socket closes. No
  keepalive. Subscribing without a connection (REST) is a 400.
- Handler table: `HandlerMap::method_names()`; `tests/handler_table_tests.rs` writes `HandlerMethods.ts`
  (ts-rs export dir) and fails if `core/src/generated/api/HandlerMethods.ts` is stale — regenerate
  (`pnpm run generate:api-types` in `core/`) after adding a handler.
- Rule: a client that sends none of the new params gets byte-identical replies and events.

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
