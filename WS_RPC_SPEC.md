# REST → WebSocket RPC Migration Spec

## Goal
Replace all REST HTTP endpoints with a single authenticated WebSocket RPC channel.
Auth happens once on WS upgrade. All operations become JSON messages over that channel.

## Branch
`feat/sse-to-websocket` based on `refactor/graphql-to-rest`

## Architecture

### Protocol
JSON-RPC-like messages over a single WebSocket connection at `/api/v1/ws`.

**Request:** `{ "id": "<correlation-id>", "type": "<operation>", ...params }`
**Response:** `{ "id": "<correlation-id>", "result": ...data }` or `{ "id": "<correlation-id>", "error": { "code": <int>, "message": "<text>" } }`
**Server push (events):** `{ "type": "<event-type>", ...payload }` (no `id` field — distinguishes events from responses)

The `id` field is a client-generated string (UUID or counter) for correlating requests to responses.
Events have no `id` — they're server-initiated pushes (same as current events_ws.rs output).

### Auth
- On WS upgrade: `token` query param or `Authorization` header (reuse existing AuthContext logic)
- Auth context cached for connection lifetime
- User email resolved once via `user_email_from_token()`
- No per-message auth

### Error Codes
- 400: Bad request (missing params, invalid JSON)
- 401: Unauthorized
- 403: Forbidden (capability check failed)
- 404: Not found
- 500: Internal error

## Server Changes (Rust)

### New: `ws_rpc.rs`
Single file implementing the WS RPC endpoint. Pattern:

```rust
pub async fn ws_rpc(
    ws: WebSocketUpgrade,
    State(state): State<AppState>,
    query_or_header_auth: AuthFromQueryOrHeader,
) -> impl IntoResponse {
    // Auth once on upgrade
    let auth_context = authenticate(query_or_header_auth)?;
    ws.on_upgrade(move |socket| handle_ws_rpc(socket, auth_context))
}

async fn handle_ws_rpc(socket: WebSocket, auth: AuthContext) {
    // Split into sender/receiver
    // Spawn event broadcast task (reuse events_ws.rs logic)
    // Message loop: parse JSON, dispatch to handlers, send response
}
```

### Handler extraction pattern
Each REST handler currently has this shape:
```rust
pub async fn some_handler(
    State(state): State<AppState>,
    Path(uuid): Path<String>,
    context: AuthContext,
    Json(body): Json<SomeBody>,
) -> Result<Json<SomeResponse>, ApiError> { ... }
```

For each handler, extract the business logic into an `_inner` function:
```rust
pub(crate) async fn some_handler_inner(
    uuid: &str,
    body: &SomeBody,
    auth: &AuthContext,
) -> Result<SomeResponse, ApiError> { ... }
```

The HTTP handler wraps this (backward compat), and the WS dispatcher calls it directly.

### Message type mapping
Format: `"<domain>.<operation>"` mapping to REST endpoints:

**Agent:**
- `agent.get` → GET /agent
- `agent.status` → GET /agent/status
- `agent.generate` → POST /agent/generate
- `agent.lock` → POST /agent/lock
- `agent.unlock` → POST /agent/unlock
- `agent.import` → POST /agent/import
- `agent.byDid` → GET /agent/by-did/:did
- `agent.updateProfile` → PATCH /agent/profile
- `agent.sign` → POST /agent/sign
- `agent.isLocked` → GET /agent/is-locked
- `agent.requestCapability` → POST /agent/auth/request
- `agent.permitCapability` → POST /agent/auth/permit
- `agent.generateJwt` → POST /agent/auth/jwt
- `agent.getApps` → GET /agent/apps
- `agent.removeApp` → DELETE /agent/apps/:id
- `agent.revokeToken` → DELETE /agent/auth/token/:token
- `agent.getTrustedAgents` → GET /agent/trusted
- `agent.addTrustedAgents` → PUT /agent/trusted
- `agent.deleteTrustedAgents` → DELETE /agent/trusted
- `agent.getEntanglementProofs` → GET /agent/entanglement
- `agent.addEntanglementProofs` → POST /agent/entanglement
- `agent.deleteEntanglementProofs` → DELETE /agent/entanglement
- `agent.entanglementProofPreflight` → POST /agent/entanglement-preflight

**Perspectives:**
- `perspective.all` → GET /perspectives
- `perspective.get` → GET /perspectives/:uuid
- `perspective.create` → POST /perspectives
- `perspective.update` → PUT /perspectives/:uuid
- `perspective.remove` → DELETE /perspectives/:uuid
- `perspective.snapshot` → GET /perspectives/:uuid/snapshot
- `perspective.publishSnapshot` → POST /perspectives/:uuid/publish-snapshot
- `perspective.queryLinks` → GET /perspectives/:uuid/links
- `perspective.addLink` → POST /perspectives/:uuid/links
- `perspective.addLinkExpression` → POST /perspectives/:uuid/links/expression
- `perspective.addLinks` → POST /perspectives/:uuid/links/bulk
- `perspective.updateLink` → PUT /perspectives/:uuid/links
- `perspective.removeLink` → DELETE /perspectives/:uuid/links
- `perspective.removeLinks` → POST /perspectives/:uuid/links/remove-bulk
- `perspective.linkMutations` → POST /perspectives/:uuid/links/mutations
- `perspective.queryProlog` → POST /perspectives/:uuid/query/prolog (or query with engine param)
- `perspective.querySparql` → POST /perspectives/:uuid/query/surreal
- `perspective.addSdna` → POST /perspectives/:uuid/sdna
- `perspective.executeCommands` → POST /perspectives/:uuid/execute-commands
- `perspective.createSubject` → POST /perspectives/:uuid/create-subject
- `perspective.getSubjectData` → POST /perspectives/:uuid/get-subject-data
- `perspective.createBatch` → POST /perspectives/:uuid/batch
- `perspective.commitBatch` → POST /perspectives/:uuid/batch/commit
- `perspective.subscribeQuery` → POST /perspectives/:uuid/subscribe-query
- `perspective.keepAliveQuery` → POST /perspectives/:uuid/keep-alive-query
- `perspective.disposeQuery` → POST /perspectives/:uuid/dispose-query

**Languages:**
- `language.all` → GET /languages
- `language.get` → GET /languages/:address
- `language.meta` → GET /languages/:address/meta
- `language.source` → GET /languages/:address/source
- `language.writeSettings` → PUT /languages/:address/settings
- `language.applyTemplate` → POST /languages/apply-template
- `language.publish` → POST /languages/publish
- `language.remove` → DELETE /languages/:address

**Neighbourhoods:**
- `neighbourhood.publish` → POST /neighbourhoods/publish
- `neighbourhood.join` → POST /neighbourhoods/join
- `neighbourhood.otherAgents` → GET /neighbourhoods/:uuid/other-agents
- `neighbourhood.hasTelepresence` → GET /neighbourhoods/:uuid/has-telepresence
- `neighbourhood.onlineAgents` → GET /neighbourhoods/:uuid/online-agents
- `neighbourhood.setOnlineStatus` → PUT /neighbourhoods/:uuid/online-status
- `neighbourhood.sendSignal` → POST /neighbourhoods/:uuid/signal
- `neighbourhood.sendBroadcast` → POST /neighbourhoods/:uuid/broadcast

**Expressions:**
- `expression.get` → GET /expressions/:url
- `expression.getMany` → GET/POST /expressions/many
- `expression.create` → POST /expressions
- `expression.interactions` → GET /expressions/:url/interactions
- `expression.interact` → POST /expressions/:url/interact

**Runtime:**
- `runtime.info` → GET /runtime/info
- `runtime.quit` → POST /runtime/quit
- `runtime.openLink` → POST /runtime/open-link
- `runtime.friends` → GET /runtime/friends
- `runtime.addFriends` → PUT /runtime/friends
- `runtime.removeFriends` → DELETE /runtime/friends
- `runtime.friendStatus` → GET /runtime/friends/:did
- `runtime.sendFriendMessage` → POST /runtime/friends/:did/message
- `runtime.inbox` → GET /runtime/messages/inbox
- `runtime.outbox` → GET /runtime/messages/outbox
- `runtime.notifications` → GET /runtime/notifications
- `runtime.createNotification` → POST /runtime/notifications
- `runtime.updateNotification` → PATCH /runtime/notifications/:id
- `runtime.grantNotification` → PATCH /runtime/notifications/:id/grant
- `runtime.deleteNotification` → DELETE /runtime/notifications/:id
- `runtime.setStatus` → PUT /runtime/status
- `runtime.linkLanguageTemplates` → GET /runtime/link-language-templates
- `runtime.addLinkLanguageTemplates` → PUT /runtime/link-language-templates
- `runtime.removeLinkLanguageTemplates` → DELETE /runtime/link-language-templates
- `runtime.hcAgentInfos` → GET /runtime/hc-agent-infos
- `runtime.addHcAgentInfos` → POST /runtime/hc-agent-infos
- `runtime.networkMetrics` → GET /runtime/network-metrics
- `runtime.restartHolochain` → POST /runtime/holochain/restart
- `runtime.verifySignature` → POST /runtime/verify-signature
- `runtime.tlsDomain` → GET /runtime/tls-domain
- `runtime.exportData` → POST /runtime/export
- `runtime.importData` → POST /runtime/import
- `runtime.freeHostingEnabled` → GET /runtime/free-hosting-enabled
- `runtime.setFreeHostingEnabled` → PUT /runtime/free-hosting-enabled

**AI:**
- `ai.models` → GET /ai/models
- `ai.addModel` → POST /ai/models
- `ai.updateModel` → PUT /ai/models/:id
- `ai.removeModel` → DELETE /ai/models/:id
- `ai.setDefaultModel` → PUT /ai/models/:id/default
- `ai.getDefaultModel` → GET /ai/models/default
- `ai.tasks` → GET /ai/tasks
- `ai.addTask` → POST /ai/tasks
- `ai.updateTask` → PUT /ai/tasks/:id
- `ai.removeTask` → DELETE /ai/tasks/:id
- `ai.prompt` → POST /ai/prompt
- `ai.embed` → POST /ai/embed
- `ai.modelLoadingStatus` → GET /ai/model-loading-status

**Users/Hosting:**
- `user.create` → POST /users
- `user.login` → POST /users/login
- `user.verifyEmail` → POST /users/verify-email
- `user.list` → GET /users
- `user.multiUserEnabled` → GET /users/multi-user-enabled
- `user.setMultiUserEnabled` → PUT /users/multi-user-enabled
- `hosting.info` → GET /hosting
- `hosting.wallet` → GET /hosting/wallet
- `hosting.requestPayment` → POST /hosting/request-payment

## Client Changes (TypeScript)

### `restClient.ts` → `wsClient.ts`
The `RestClient` class is renamed/replaced with `WsClient` that:
1. Opens a single WS connection to `/api/v1/ws`
2. Sends JSON-RPC-like requests with correlation IDs
3. Returns promises that resolve when the matching response arrives
4. Routes server-push events to subscriber callbacks (same as current)
5. Auto-reconnects with exponential backoff
6. Exposes same public API: `get()`, `post()`, `put()`, `delete()` map to message types
   OR expose `call(type, params)` directly and update each client

### Client modules (AgentClient, PerspectiveClient, etc.)
Replace `this.#restClient.get('/api/v1/perspectives')` with `this.#client.call('perspective.all')`.
Each module needs updating but the changes are mechanical.

### Subscriptions
Query subscriptions and events continue over the same WS connection.
No separate subscription lifecycle needed — `subscribe_query` returns a subscription_id,
updates arrive as events on the same connection.
`keep-alive-query` becomes unnecessary (WS ping/pong handles liveness).

## Backward Compatibility
- Keep REST endpoints for external/CLI tooling (curl, scripts)
- REST endpoints remain unchanged, just also callable over WS
- SSE endpoint kept as fallback

## Testing
- Update Ad4mClient.test.ts mock to use WS RPC messages
- Integration tests should work unchanged (SDK interface same)
- Browser verification with launch + auth scripts

## Non-goals
- Streaming responses (future consideration)
- Binary protocol (JSON is fine for now)
- Breaking the SDK's public API
