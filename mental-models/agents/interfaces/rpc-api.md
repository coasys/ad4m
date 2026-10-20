# RPC API

The **RPC API** is one WebSocket per client at `GET /api/v1/ws?token=…`. The client sends `{ id, type, params, meta? }`; the executor answers `{ id, result }` or `{ id, error: { code, message } }`. The same socket carries the [events](events.md) that the client asked for and the updates of its [live queries](../../perspectives/querying/live-queries.md). One `HandlerMap` registers every method, and each registration declares its params and result types, its [guard](../access/guards.md) and its flags. The namespaces are `agent`, `user`, `perspective`, `neighbourhood`, `language`, `expression`, `runtime`, `ai`, `hosting`, `sfu`, and, for identity and hosts, `identity`, `bot`, `bots`, `host`, `guest`, `spaces`, `storage` and `index`.

**Why:** A client authenticates once, and the executor checks the session again on each call. Each method's types and guard sit on its registration, and tests write the TypeScript contracts and the guard table from them, so a stale SDK or a method without a guard fails the build, not the call. There is no version negotiation: clients ship with the executor of the same revision, and an app shows an executor older than its minimum version as `outdated`. One socket route remains: the separate event socket went, because two entry checks for one stream drifted.

**Parent:** [Executor interfaces](README.md)

<details><summary>How it works</summary>

1. Upgrade: `ws_rpc::ws_rpc` opens a `RequestContext` from `?token=` ([Sessions](../access/sessions.md)). A refused token closes the socket (1008, or 1013 when the wallet is locked or the storage is sealed).
2. Each message: inline control messages (`events.watch`, `events.unwatch`, `request.cancel`) answer at once. Any other message runs `Session::check()`, then `HandlerMap::dispatch`: method (404), params (400), guard (403, or 404 for `Owns`), `.local_only()` (403 `local-only`), handler. A session that ends (revoke, expiry) closes the socket with 1008 and ends its calls with 401.
3. Errors are `WsRpcError { code, message }` with HTTP-like codes (400, 401, 403, 404, 409, 429, 499 cancelled, 500, 501, 503) and short reason words (`not-known`, `local-only`, `storage-sealed`, `bot-grant`, …).
4. Method flags: `.read()` (idempotent; the SDK retries after a reconnect), `.long()` (the SDK waits 20 minutes, not 30 s), `.or_null()` (404 answers `null`), `.local_only()`, `.guard(g)`.
5. `meta.reason`: an optional string on the request. The guard layer passes it to the bot activity log for a bot agent's session and ignores it for every other session ([Bot grants](../access/grants/bot-grants.md)).
6. While the storage is sealed, only `runtime.storageStatus`, `storage.unlock` and `/health` answer; every other method answers `storage-sealed`.

| Namespace (new) | What it does | Owner page |
|---|---|---|
| `user.*` key-login calls | `sessionChallenge`, `loginWithKey`, `redeemCode`, `renewSession`, `currentSession`, `endAllSessions` | [Key login](../access/key-login.md) |
| `identity.*` | the key log, offers and claims, the device inbox, the private record, `takeMaster`, `rekey` | [Key log](../identity/key-log/README.md), [Devices](../identity/devices/README.md) |
| `bot.*`, `bots.*` | bot grants, the bot activity log | [Bot grants](../access/grants/bot-grants.md) |
| `host.*`, `guest.*`, `index.*` | tenants and the join gate, guests, the hosting index | [Tenants](../executor/managed-agents/tenants.md), [Guests](../../perspectives/neighbourhoods/membership/guests.md), [Hosting-index client](../executor/services/hosting-index.md) |
| `storage.*` | the storage secret, keys and re-encryption | [Keyring and storage secret](../executor/data/keyring.md) |
| `spaces.*`, new `neighbourhood.*` calls | the neighbourhoods this executor holds, join mode, counts by person | [Neighbourhoods](../../perspectives/neighbourhoods/README.md), [Membership](../../perspectives/neighbourhoods/membership/README.md) |
| `runtime.*` additions | `operator`, `setOperator`, `localSession`, `config`, `setConfig`, `restart`, `storageStatus`, `lanStatus`, `linkServers` | [Operator and main agent](../access/operator-and-main-agent.md), [Executor config](../executor/config.md) |

Host calls sit in two namespaces: `hosting.*` holds credits, guests and the wallet (`hosting.info`, `hosting.removeGuest`); `host.*` holds the host's policy, invites, tenants and joins.

Listeners:

| Listener | Address | Reach | Admits |
|---|---|---|---|
| cleartext | `127.0.0.1:port` (or `0.0.0.0` with `localhost: false`) | `Loopback` (a forwarding header reads `Network`) | everything, including local-only calls |
| TLS | `0.0.0.0:tls_port`, when configured | `Network` | every non-local-only call |
| home-network listener | the desktop's private addresses, TLS 1.3 under a self-signed certificate, only with "Let my phone reach this computer on my network" on | `Network` | key login and key sessions; never the admin credential or a local-only call; key logins rate limited |

Other routes in the same axum server:

| Route | Purpose |
|---|---|
| `POST /api/v1/ai/transcription/feed` | binary audio feed for a transcription stream |
| `/v1/*`, `/api/v1/openai/v1/*` | OpenAI-compatible chat, embeddings, audio, realtime over `AIService` |
| `GET /health` | open health check; the app's reachability probe |
| `POST /internal/shutdown` | hosting service, `INTERNAL_API_TOKEN` |

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/api/mod.rs](../../../rust-executor/src/api/mod.rs): routes, `listener_router`, `bind_api_listeners`, `start_server`.
- [rust-executor/src/api/ws_rpc.rs](../../../rust-executor/src/api/ws_rpc.rs): the socket, `serve`, cancellation, close on `Session::over`.
- [rust-executor/src/api/ws_handler.rs](../../../rust-executor/src/api/ws_handler.rs): `HandlerMap`, `method`, `read`, `long`, `or_null`, `local_only`, `guard`, `require_guards`, `dispatch`, `WsRpcError`, `shared_handler_map`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs), [users_ws.rs](../../../rust-executor/src/api/users_ws.rs), [perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs), [runtime_ws.rs](../../../rust-executor/src/api/runtime_ws.rs) and the other `*_ws.rs`: one file per namespace.
- [rust-executor/src/lan_listener.rs](../../../rust-executor/src/lan_listener.rs) (new): the home-network listener, its bind rules and limits, `runtime.lanStatus`.
- [rust-executor/src/api/types.rs](../../../rust-executor/src/api/types.rs): request and response structs (ts-rs).
- [rust-executor/src/api/tests/handler_table_tests.rs](../../../rust-executor/src/api/tests/handler_table_tests.rs): writes `RpcMethods.ts`, `Events.ts` and the guard table, and fails when they are stale.
- [core/src/generated/api/](../../../core/src/generated/api): the generated contracts (`pnpm run generate:api-types` in `core/`).
- [rust-executor/src/api/openai_compat/](../../../rust-executor/src/api/openai_compat): the OpenAI-compatible routes, and the interpretation harness's bridge (`harness_bridge.rs`) and tool grammar (`tool_grammar.rs`).

</details>
