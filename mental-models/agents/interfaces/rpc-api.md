# RPC API

The **RPC API** is one WebSocket per client at `GET /api/v1/ws?token=…`. The client sends `{ id, type, params }`; the executor answers `{ id, result }` or `{ id, error: { code, message } }`. The same socket carries the [events](events.md) that the client asked for and the updates of its [live queries](../../perspectives/querying/live-queries.md). One `HandlerMap` registers about 190 methods in ten namespaces: `agent`, `user`, `perspective`, `neighbourhood`, `language`, `expression`, `runtime`, `ai`, `hosting` and `sfu`.

**Why:** The socket replaced the earlier GraphQL API ([CHANGELOG](../../../CHANGELOG)). A client authenticates once, and the executor checks the session again on each call. Each method registers its params and result types (`map.method::<P, R>`), and a unit test writes the TypeScript contracts from them. A stale SDK therefore fails the build, not the call. There is no version negotiation: clients ship with the executor of the same revision.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

1. Upgrade: `ws_rpc::ws_rpc` opens a `RequestContext` from `?token=` ([Sessions](../access/sessions.md)). A refused token closes the socket (1008, or 1013 when locked). The executor sends a WebSocket ping every 20 s and ends a connection whose client sent no frame for 60 s, or longer while a large write can still be on its way to it (`Liveness` in `ws_rpc.rs`).
2. Each message: the executor answers inline control messages (`events.watch`, `events.unwatch`, `request.cancel`) at once. Any other message runs `Session::check()` (a failure answers 401 and closes the socket), then `HandlerMap::dispatch(type, params, ctx)`. Dispatch answers 404 for an unknown method and 400 for params that do not fit the method's contract.
3. Dispatch runs the method's guard: its capability, if it has one (403), then its rule (403, or 404 for a resource that is missing or not the caller's) ([Access checks](../access/checks.md)). Then the handler runs and returns JSON. Errors are `WsRpcError { code, message }` with HTTP-like codes (400, 401, 403, 404, 499 cancelled, 500, 501).
4. Method flags: `.read()` marks an idempotent method, which the SDK retries after a reconnect. `.long()` marks a method that runs for minutes; the SDK waits 20 minutes for it instead of 30 seconds. `.or_null()` answers `null` in place of 404. `.guard(..)` sets the method's guard; the executor does not start with a method that has none.
5. Events: the socket gets none until `events.watch`. Each delivered event runs `Session::check()` again and must pass `Session::may_receive`: the session is in the event's audience and holds its capability ([Events](events.md)).
6. Live queries (`perspective.subscribeQuery`, `perspective.modelSubscribe`) belong to the RPC connection that opened them and end when it closes. Closing also cancels the calls that still open one; every other call finishes. One connection runs at most 8 of these opens at once (`MAX_OPENING_LIVE_QUERIES`). The client repairs a gap with `perspective.resyncSubscription` ([Live queries](../../perspectives/querying/live-queries.md)).

Other routes in the same axum server:

| Route | Purpose |
|---|---|
| `POST /api/v1/ai/transcription/feed` | binary audio feed for a transcription stream |
| `/v1/*`, `/api/v1/openai/v1/*` | OpenAI-compatible chat, embeddings, audio, realtime over `AIService` |
| `GET /health` | open health check |
| `POST /internal/shutdown` | hosting service, `INTERNAL_API_TOKEN` |

Listeners: cleartext on `127.0.0.1:port` (or `0.0.0.0` with `localhost: false`). With TLS configured, HTTPS/WSS listens on `0.0.0.0:tls_port`, and the cleartext listener stays on `127.0.0.1`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/mod.rs](../../../rust-executor/src/api/mod.rs): routes, `listener_router`, `bind_api_listeners`, `start_server`.
- [rust-executor/src/api/ws_rpc.rs](../../../rust-executor/src/api/ws_rpc.rs): the socket, `serve`, cancellation.
- [rust-executor/src/api/ws_handler.rs](../../../rust-executor/src/api/ws_handler.rs): `HandlerMap`, `method`, `read`, `long`, `or_null`, `guard`, `dispatch`, `WsRpcError`, `build_handler_map`.
- [rust-executor/src/api/guards.rs](../../../rust-executor/src/api/guards.rs): `Guard`; the guard of every method is the generated table in [api/AGENTS.md](../../../rust-executor/src/api/AGENTS.md).
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs), [users_ws.rs](../../../rust-executor/src/api/users_ws.rs), [perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs), [runtime_ws.rs](../../../rust-executor/src/api/runtime_ws.rs) and the other `*_ws.rs`: one file per namespace, plus [agent_session_ws.rs](../../../rust-executor/src/api/agent_session_ws.rs) for `perspective.runAgent`.
- [rust-executor/src/api/events_ws.rs](../../../rust-executor/src/api/events_ws.rs), [event_interest.rs](../../../rust-executor/src/api/event_interest.rs): events ([Events](events.md)).
- [rust-executor/src/api/types.rs](../../../rust-executor/src/api/types.rs): request and response structs (ts-rs).
- [rust-executor/src/api/tests/handler_table_tests.rs](../../../rust-executor/src/api/tests/handler_table_tests.rs): writes `RpcMethods.ts` and `Events.ts`, fails when they are stale.
- [core/src/generated/api/](../../../core/src/generated/api): the generated contracts (`pnpm run generate:api-types` in `core/`).
- [rust-executor/src/api/openai_compat/](../../../rust-executor/src/api/openai_compat): the OpenAI-compatible routes, and the interpretation harness's bridge (`harness_bridge.rs`) and tool grammar (`tool_grammar.rs`).

</details>
