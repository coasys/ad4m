# 6. WebSocket RPC API

## 6.1 Overview

The AD4M executor exposes a **WebSocket RPC** API via Axum. This is the sole client interface for all SDK operations and real-time event delivery.

- **RPC endpoint:** `ws://host:port/api/v1/ws` — all SDK operations (request/response) **and** server-push events on the same connection
- **Events endpoint:** `ws://host:port/api/v1/ws/events` — standalone event stream for clients that only need event delivery without RPC capabilities
- **HTTP:** Only `GET /api/v1/health` (health check) and `POST /api/v1/ai/transcription/feed` (binary audio upload) are exposed over HTTP

The RPC endpoint multiplexes both RPC responses and events on a single WebSocket connection. RPC responses are correlated by `id`; messages without an `id` (or with an `id` not matching any pending call) are server-push events. The SDK uses only the `/api/v1/ws` connection for both RPC and events. The `/api/v1/ws/events` endpoint is an alternative for clients that only need event consumption without sending RPC requests.

### Authentication

Authentication occurs once on WebSocket connection upgrade via a `token` query parameter:

```
ws://host:port/api/v1/ws?token=<value>
```

The token is either:
- The **admin credential** (pre-shared secret configured at executor startup)
- A **JWT capability token** (see [Agent Model §2.5](./02-agent-model.md#25-capability-tokens))

All subsequent messages on the authenticated connection share that auth context. No per-message authentication is required.

## 6.2 RPC Protocol

### Request Format

```json
{
  "id": "<correlation-id>",
  "type": "<operation>",
  "params": { ... }
}
```

- `id` — Client-generated correlation ID (string). Used to match responses to requests. MUST be unique per in-flight request on the connection.
- `type` — The RPC operation name (e.g., `"perspective.all"`, `"agent.get"`).
- `params` — Operation-specific parameters object. May be omitted or `{}` for parameterless operations.

### Response Format (Success)

```json
{
  "id": "<correlation-id>",
  "result": ...
}
```

### Response Format (Error)

```json
{
  "id": "<correlation-id>",
  "error": {
    "code": 500,
    "message": "Human-readable error description"
  }
}
```

### Error Codes

| Code | Meaning |
|------|---------|
| 400 | Bad request — missing parameters, invalid input |
| 401 | Unauthorized — invalid or missing token |
| 403 | Forbidden — insufficient capabilities for the requested operation |
| 404 | Not found — requested resource does not exist |
| 500 | Internal server error |
| 501 | Not implemented — operation not supported by this executor |

### Connection Health

Application-level ping/pong messages maintain connection health:

- Client sends: `{ "type": "ping" }`
- Server responds: `{ "type": "pong" }`

This is in addition to WebSocket-level ping/pong frames.

## 6.3 RPC Operations

All operations are grouped by domain. The `type` field in requests uses the format `<domain>.<operation>`.

### Agent

Handler: `agent_ws.rs`

| Operation | Description |
|-----------|-------------|
| `agent.get` | Get current agent |
| `agent.getApps` | List installed apps |
| `agent.byDid` | Get agent by DID |
| `agent.updateProfile` | Update agent profile |
| `agent.generate` | Generate new agent |
| `agent.import` | Import agent |
| `agent.lock` | Lock agent |
| `agent.unlock` | Unlock agent |
| `agent.sign` | Sign message |
| `agent.removeApp` | Remove app |
| `agent.requestCapability` | Request capability token |
| `agent.permitCapability` | Permit capability request |
| `agent.generateJwt` | Generate JWT |
| `agent.revokeToken` | Revoke token |
| `agent.status` | Get agent status |
| `agent.isLocked` | Check if locked |
| `agent.getTrustedAgents` | List trusted agents |
| `agent.addTrustedAgents` | Add trusted agents |
| `agent.deleteTrustedAgents` | Remove trusted agents |
| `agent.getEntanglementProofs` | Get entanglement proofs |
| `agent.addEntanglementProofs` | Add entanglement proofs |
| `agent.deleteEntanglementProofs` | Delete entanglement proofs |
| `agent.entanglementProofPreflight` | Preflight check |

**Example — Unlock agent:**

```json
// Request
{ "id": "req-1", "type": "agent.unlock", "params": { "passphrase": "my-secret" } }

// Response
{ "id": "req-1", "result": { "did": "did:key:z6Mk...", "isInitialized": true, "isUnlocked": true } }
```

### AI

Handler: `ai_ws.rs`

| Operation | Description |
|-----------|-------------|
| `ai.models` | List models |
| `ai.addModel` | Add model |
| `ai.updateModel` | Update model |
| `ai.removeModel` | Remove model |
| `ai.setDefaultModel` | Set default model |
| `ai.getDefaultModel` | Get default model |
| `ai.modelLoadingStatus` | Get model loading status |
| `ai.tasks` | List tasks |
| `ai.addTask` | Add task |
| `ai.updateTask` | Update task |
| `ai.removeTask` | Remove task |
| `ai.prompt` | AI prompt |
| `ai.embed` | AI embedding |
| `ai.transcriptionOpen` | Open transcription stream |
| `ai.transcriptionClose` | Close transcription stream |

### Expressions

Handler: `expressions_ws.rs`

| Operation | Description |
|-----------|-------------|
| `expression.get` | Get expression |
| `expression.getMany` | Get multiple expressions |
| `expression.create` | Create expression |
| `expression.interactions` | Get interactions |
| `expression.interact` | Execute interaction |

### Hosting

Handler: `hosting_ws.rs`

| Operation | Description |
|-----------|-------------|
| `hosting.info` | Hosting info |
| `hosting.wallet` | Wallet info |
| `hosting.walletHistory` | Wallet history |
| `hosting.requestPayment` | Request payment |
| `hosting.setHotWallet` | Set hot wallet |

### Languages

Handler: `languages_ws.rs`

| Operation | Description |
|-----------|-------------|
| `language.all` | List languages |
| `language.get` | Get language |
| `language.meta` | Get language meta |
| `language.source` | Get language source |
| `language.publish` | Publish language |
| `language.applyTemplate` | Apply template |
| `language.remove` | Remove language |
| `language.writeSettings` | Write settings |

### Neighbourhoods

Handler: `neighbourhoods_ws.rs`

| Operation | Description |
|-----------|-------------|
| `neighbourhood.join` | Join neighbourhood |
| `neighbourhood.publish` | Publish neighbourhood |
| `neighbourhood.sendBroadcast` | Send broadcast |
| `neighbourhood.sendSignal` | Send signal |
| `neighbourhood.setOnlineStatus` | Set online status |
| `neighbourhood.hasTelepresence` | Check telepresence |
| `neighbourhood.onlineAgents` | List online agents |
| `neighbourhood.otherAgents` | List other agents |

### Perspectives

Handler: `perspectives_ws.rs`

| Operation | Description |
|-----------|-------------|
| `perspective.all` | List perspectives |
| `perspective.get` | Get perspective |
| `perspective.create` | Create perspective |
| `perspective.update` | Update perspective |
| `perspective.remove` | Remove perspective |
| `perspective.snapshot` | Get snapshot |
| `perspective.publishSnapshot` | Publish snapshot |
| `perspective.queryLinks` | Query links |
| `perspective.addLink` | Add link |
| `perspective.addLinkExpression` | Add signed link expression |
| `perspective.addLinks` | Bulk add links |
| `perspective.updateLink` | Update link |
| `perspective.removeLink` | Remove link |
| `perspective.removeLinks` | Bulk remove links |
| `perspective.linkMutations` | Apply link mutations |
| `perspective.queryProlog` | Prolog query |
| `perspective.querySparql` | SPARQL query |
| `perspective.addSdna` | Add SDNA |
| `perspective.executeCommands` | Execute commands |
| `perspective.createSubject` | Create subject instance |
| `perspective.getSubjectData` | Get subject data |
| `perspective.createBatch` | Create batch |
| `perspective.commitBatch` | Commit batch |
| `perspective.subscribeQuery` | Subscribe to query |
| `perspective.keepAliveQuery` | Keep alive subscription |
| `perspective.disposeQuery` | Dispose subscription |
| `perspective.subscribeSparql` | Subscribe to SPARQL query |
| `perspective.keepAliveSparql` | Keep alive SPARQL subscription |
| `perspective.disposeSparql` | Dispose SPARQL subscription |
| `perspective.modelQuery` | Execute model query (Rust engine) |
| `perspective.modelSubscribe` | Subscribe to model query (Rust engine) |
| `perspective.evaluateGetters` | Evaluate property getters (Rust engine) |

**Example — Add a link:**

```json
// Request
{
  "id": "req-42",
  "type": "perspective.addLink",
  "params": {
    "uuid": "550e8400-e29b-41d4-a716-446655440000",
    "link": { "source": "ad4m://self", "predicate": "todo://state", "target": "todo://ready" }
  }
}

// Response
{
  "id": "req-42",
  "result": {
    "author": "did:key:z6Mk...",
    "timestamp": "2026-05-13T10:30:00.000Z",
    "data": { "source": "ad4m://self", "predicate": "todo://state", "target": "todo://ready" },
    "proof": { "key": "did:key:z6Mk...", "signature": "abcdef...", "valid": true },
    "status": "shared"
  }
}
```

**Example — SPARQL query:**

```json
// Request
{
  "id": "req-43",
  "type": "perspective.querySparql",
  "params": {
    "uuid": "550e8400-e29b-41d4-a716-446655440000",
    "query": "SELECT ?s ?o WHERE { ?s <todo://state> ?o }"
  }
}

// Response
{ "id": "req-43", "result": "{\"results\":{\"bindings\":[...]}}" }
```

**Example — Model query:**

```json
// Request
{
  "id": "req-44",
  "type": "perspective.modelQuery",
  "params": {
    "uuid": "550e8400-e29b-41d4-a716-446655440000",
    "class_name": "Todo",
    "query_json": "{\"where\":{\"state\":\"done\"},\"order\":{\"createdAt\":\"DESC\"},\"limit\":10}",
    "shape_json": "{\"id\":\"string\",\"state\":\"string\",\"title\":\"string\"}"
  }
}

// Response
{
  "id": "req-44",
  "result": "{\"instances\":[...],\"totalCount\":42}"
}
```

> **Note:** `query_json` and `shape_json` are JSON-serialized **strings**, not objects. The result is also a JSON-serialized string.

**Example — Evaluate getters:**

```json
// Request
{
  "id": "req-45",
  "type": "perspective.evaluateGetters",
  "params": {
    "uuid": "550e8400-e29b-41d4-a716-446655440000",
    "class_name": "Todo",
    "shape_json": "{\"id\":\"string\",\"title\":\"string\"}",
    "instance_ids": ["abc123", "def456"],
    "property_names": ["title", "state"]
  }
}

// Response
{ "id": "req-45", "result": "{...}" }
```

**Example — Model subscribe:**

```json
// Request
{
  "id": "req-46",
  "type": "perspective.modelSubscribe",
  "params": {
    "uuid": "550e8400-e29b-41d4-a716-446655440000",
    "class_name": "Todo",
    "query_json": "{\"where\":{\"state\":\"active\"}}",
    "shape_json": "{\"id\":\"string\",\"title\":\"string\"}"
  }
}

// Response
{ "id": "req-46", "result": { "subscription_id": "sub-789", "result": "{...}" } }
```

### Runtime

Handler: `runtime_ws.rs`

| Operation | Description |
|-----------|-------------|
| `runtime.info` | Runtime info |
| `runtime.quit` | Quit executor |
| `runtime.setStatus` | Set status |
| `runtime.openLink` | Open link |
| `runtime.exportData` | Export data |
| `runtime.importData` | Import data |
| `runtime.restartHolochain` | Restart Holochain |
| `runtime.verifySignature` | Verify signature |
| `runtime.tlsDomain` | Get TLS domain |
| `runtime.computeLog` | Get compute log |
| `runtime.friends` | List friends |
| `runtime.addFriends` | Add friends |
| `runtime.removeFriends` | Remove friends |
| `runtime.friendStatus` | Get friend status |
| `runtime.sendFriendMessage` | Send friend message |
| `runtime.inbox` | Get inbox |
| `runtime.outbox` | Get outbox |
| `runtime.notifications` | List notifications |
| `runtime.createNotification` | Create notification |
| `runtime.updateNotification` | Update notification |
| `runtime.grantNotification` | Grant notification |
| `runtime.deleteNotification` | Delete notification |
| `runtime.linkLanguageTemplates` | Get link language templates |
| `runtime.addLinkLanguageTemplates` | Add link language templates |
| `runtime.removeLinkLanguageTemplates` | Remove link language templates |
| `runtime.hcAgentInfos` | Get Holochain agent infos |
| `runtime.addHcAgentInfos` | Add Holochain agent infos |
| `runtime.networkMetrics` | Get network metrics |
| `runtime.freeHostingEnabled` | Check free hosting |
| `runtime.setFreeHostingEnabled` | Set free hosting |
| `runtime.hostRates` | Get host rates |
| `runtime.setHostRates` | Set host rates |

### Users

Handler: `users_ws.rs`

| Operation | Description |
|-----------|-------------|
| `user.create` | Create user |
| `user.login` | Login |
| `user.verifyEmail` | Verify email |
| `user.list` | List users |
| `user.multiUserEnabled` | Check multi-user mode |
| `user.setMultiUserEnabled` | Set multi-user mode |
| `user.freeAccess` | Set free access |
| `user.credits` | Get user credits |
| `user.wallet` | Get user wallet |
| `user.emailTest` | Test email |
| `user.requestVerification` | Request verification |

## 6.4 Events Protocol

Events are delivered as server-push messages on the RPC connection (`/api/v1/ws`) interleaved with RPC responses. A dedicated events-only endpoint is also available for clients that need event delivery without RPC:

### Dedicated Events Endpoint

```
ws://host:port/api/v1/ws/events?token=<value>
```

The events WebSocket connection uses the same token-based authentication as the RPC endpoint.

### Message Format

Server → Client event messages:

```json
{
  "type": "<event-type>",
  ...payload
}
```

### Connection Health

- Client → Server: `{ "type": "ping" }`
- Server → Client: `{ "type": "pong" }`

### Event Types

| Type | Filtering | Description |
|------|-----------|-------------|
| `agent-status-changed` | DID | Agent status changed |
| `agent-updated` | DID | Agent profile updated |
| `apps-changed` | user | Installed apps changed |
| `hosting-user-info-changed` | email | Hosting user info changed |
| `perspective-added` | owner DID | New perspective created |
| `perspective-removed` | owner DID | Perspective deleted |
| `perspective-updated` | owner DID | Perspective metadata updated |
| `sync-state-change` | broadcast | Neighbourhood sync state changed |
| `link-added` | owner DID | Link added to perspective |
| `link-removed` | owner DID | Link removed from perspective |
| `link-updated` | owner DID | Link updated in perspective |
| `signal` | recipient DID | Neighbourhood signal received |
| `message-received` | broadcast | Runtime message received |
| `notification-triggered` | perspective owner | Notification triggered |
| `exception-occurred` | broadcast | Exception occurred |
| `transcription-text` | userDid | AI transcription text |
| `model-loading-status` | broadcast | AI model loading status |
| `query-subscription-update` | perspective owner | Live query subscription update |

**Example — Link added event:**

```json
{
  "type": "link-added",
  "perspective": "550e8400-e29b-41d4-a716-446655440000",
  "link": {
    "author": "did:key:z6Mk...",
    "timestamp": "2026-05-13T10:30:00.000Z",
    "data": { "source": "ad4m://self", "predicate": "todo://state", "target": "todo://ready" },
    "proof": { "key": "did:key:z6Mk...", "signature": "abcdef...", "valid": true },
    "status": "shared"
  }
}
```

**Example — Query subscription update:**

```json
{
  "type": "query-subscription-update",
  "perspective": "550e8400-e29b-41d4-a716-446655440000",
  "subscriptionId": "sub-123",
  "result": {
    "instances": [...],
    "totalCount": 15
  }
}
```

## 6.5 HTTP Endpoints

Two HTTP endpoints remain for specific use cases where WebSocket is not appropriate:

### Health Check

```
GET /api/v1/health
```

Returns HTTP 200 if the executor is running and ready to accept connections. No authentication required.

### Audio Transcription Feed

```
POST /api/v1/ai/transcription/feed
Content-Type: application/octet-stream
```

Binary audio upload for real-time transcription. Constraints:
- Maximum 32 concurrent stream IDs
- Maximum 10 MB buffer per stream
- Requires authentication via `Authorization` header

## 6.6 Core Types

The following TypeScript type definitions describe the core data structures used across RPC operations and events:

```typescript
interface Agent {
  did: string;
  perspective?: Perspective;
}

interface AgentStatus {
  did?: string;
  didDocument?: string;
  error?: string;
  isInitialized: boolean;
  isUnlocked: boolean;
}

interface Link {
  source: string;
  target: string;
  predicate?: string;
}

interface LinkExpression {
  author: string;
  timestamp: string;
  data: Link;
  proof: ExpressionProof;
  status?: LinkStatus;
}

interface DecoratedLinkExpression {
  author: string;
  timestamp: string;
  data: Link;
  proof: DecoratedExpressionProof;
  status?: LinkStatus;
}

interface ExpressionProof {
  key: string;
  signature: string;
}

interface DecoratedExpressionProof {
  key: string;
  signature: string;
  valid?: boolean;
  invalid?: boolean;
}

enum LinkStatus {
  Shared = "shared",
  Local = "local"
}

interface Perspective {
  links: LinkExpression[];
}

interface PerspectiveHandle {
  uuid: string;
  name?: string;
  neighbourhood?: DecoratedNeighbourhoodExpression;
  sharedUrl?: string;
  state: PerspectiveState;
  owners?: string[];
}

enum PerspectiveState {
  Private = "Private",
  NeighbourhoodCreationInitiated = "NeighbourhoodCreationInitiated",
  NeighbourhoodJoinInitiated = "NeighbourhoodJoinInitiated",
  LinkLanguageFailedToInstall = "LinkLanguageFailedToInstall",
  LinkLanguageInstalledButNotSynced = "LinkLanguageInstalledButNotSynced",
  Synced = "Synced"
}

interface PerspectiveDiff {
  additions: LinkExpression[];
  removals: LinkExpression[];
}

interface LinkQuery {
  source?: string;
  target?: string;
  predicate?: string;
  fromDate?: string;
  untilDate?: string;
  limit?: number;
}

interface Neighbourhood {
  linkLanguage: string;
  meta: Perspective;
}

interface NeighbourhoodExpression {
  author: string;
  data: Neighbourhood;
  proof: ExpressionProof;
  timestamp: string;
}

interface DecoratedNeighbourhoodExpression {
  author: string;
  data: Neighbourhood;
  proof: DecoratedExpressionProof;
  timestamp: string;
}

interface RuntimeInfo {
  ad4mExecutorVersion: string;
  isInitialized: boolean;
  isUnlocked: boolean;
}

interface ExceptionInfo {
  title: string;
  message: string;
  type: ExceptionType;
  addon?: string;
}

enum ExceptionType {
  LanguageIsNotLoaded = "LanguageIsNotLoaded",
  ExpressionIsNotVerified = "ExpressionIsNotVerified",
  AgentIsUntrusted = "AgentIsUntrusted",
  CapabilityRequested = "CapabilityRequested",
  InstallNotificationRequest = "InstallNotificationRequest"
}

interface OnlineAgent {
  did: string;
  status: Perspective;
}

interface Apps {
  auth: string;
  requestId: string;
  revoked: boolean;
  token: string;
}

interface EntanglementProof {
  did: string;
  didSigningKeyId: string;
  deviceKey: string;
  deviceKeyType: string;
  deviceKeySignedByDid: string;
  didSignedByDeviceKey: string;
}
```
