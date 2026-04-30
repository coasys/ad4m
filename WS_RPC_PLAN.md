# WS RPC Migration — Implementation Plan

## Phase 1: Rust server — ws_rpc.rs dispatcher + handler extraction

### Step 1: Create ws_rpc.rs with auth-once WebSocket endpoint
- Copy auth pattern from events_ws.rs
- Add message dispatch loop
- Support { id, type, ...params } → { id, result } / { id, error }
- Route events through same connection (merge events_ws functionality)

### Step 2: Extract handler business logic from axum signatures
For each REST module (agent.rs, perspectives.rs, etc.):
- Create `_inner` functions that take plain params + AuthContext
- HTTP handlers become thin wrappers calling `_inner`
- WS dispatcher calls `_inner` directly

### Step 3: Wire up dispatch table in ws_rpc.rs
Map each message type string to its handler_inner function.

### Step 4: Register route in mod.rs
`.route("/ws", get(ws_rpc::ws_rpc))` alongside existing routes.

## Phase 2: TypeScript client — WsClient

### Step 1: Replace RestClient with WsClient
- Single WS connection to /api/v1/ws
- Correlation ID tracking (Map<id, { resolve, reject }>)
- Event subscriber callbacks (same interface as current)
- Auto-reconnect with backoff
- `call(type: string, params?: object): Promise<T>` method

### Step 2: Update each client module
Replace `this.#restClient.get(path)` / `.post(path, body)` with
`this.#client.call('type', params)` in:
- AgentClient.ts
- PerspectiveClient.ts
- LanguageClient.ts
- NeighbourhoodClient.ts
- RuntimeClient.ts
- ExpressionClient.ts
- AIClient.ts

### Step 3: Update Ad4mClient.ts
Change RestClient references to WsClient.

### Step 4: Update tests
- Ad4mClient.test.ts — update mock to dispatch WS RPC messages
- NeighbourhoodProxy.test.ts — update mock

## Phase 3: Verify + Push
- TypeScript compiles clean
- Unit tests pass (core)
- Push to branch
- Build executor locally
- Launch with scripts
- Browser auth + verify Flux loads
- Update workspace scripts if needed
