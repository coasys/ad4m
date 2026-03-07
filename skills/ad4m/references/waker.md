# AD4M Waker Bridge

The AD4M Waker watches perspectives for data changes via GraphQL subscriptions and wakes your OpenClaw agent when relevant events occur. This enables reactive AI agents that respond to neighbourhood activity in real-time.

## Source

The waker is bundled with the AD4M skill at `skills/ad4m/waker/`. It's a standalone Node.js module.

## Installation

```bash
cd skills/ad4m/waker
npm install
```

## Configuration

Create `waker-config.json` (see also `waker-config.example.json`):

```json
{
  "executorUrl": "ws://localhost:12100/graphql",
  "token": "your-admin-credential",
  "mcpEndpoint": "http://localhost:3001/mcp",
  "agentDid": "did:key:z6Mk...",
  "wakeUrl": "http://localhost:18789/hooks/agent",
  "wakeToken": "your-openclaw-hooks-token",
  "debounceMs": 2000,
  "subscriptions": [
    {
      "id": "flux-messages",
      "type": "channel-messages",
      "perspective": "perspective-uuid",
      "channel": "literal://string:channel-id",
      "neighbourhood": "neighbourhood://Qm...",
      "query": "SELECT * FROM link WHERE source = 'literal://string:channel-id' AND predicate = 'ad4m://has_child'"
    }
  ]
}
```

### Configuration Fields

| Field | Description |
|-------|-------------|
| `executorUrl` | WebSocket URL for AD4M executor GraphQL |
| `token` | Admin credential for authentication |
| `mcpEndpoint` | (optional) MCP endpoint URL — included in wake messages |
| `agentDid` | (optional) Agent DID — included in wake messages |
| `wakeUrl` | OpenClaw webhook endpoint — use `/hooks/agent` for isolated agent runs |
| `wakeToken` | OpenClaw hooks authentication token |
| `debounceMs` | Debounce interval (prevents rapid-fire wakes, default 2000) |
| `subscriptions` | Array of subscription objects (see below) |

### Subscription Fields

| Field | Required | Description |
|-------|----------|-------------|
| `id` | yes | Unique identifier |
| `type` | yes | `"mention"` or `"channel-messages"` — determines the wake message |
| `perspective` | yes | AD4M perspective UUID |
| `channel` | yes | Channel address (where to read/post) |
| `neighbourhood` | no | Neighbourhood URL (for context in wake messages) |
| `query` | yes | SurrealQL subscription query |

## Running

```bash
# Direct
node ad4m-waker.js --config waker-config.json

# As a background service (recommended)
screen -dmS ad4m-waker bash -c 'node ad4m-waker.js --config waker-config.json 2>&1 | tee /tmp/ad4m-waker.log'
```

## How It Works

1. Connects to AD4M executor via GraphQL WebSocket
2. For each subscription, creates a `QuerySubscriptionProxy` with the SurrealQL query
3. When query results change (compared via JSON serialization), debounces and POSTs to the wake endpoint
4. OpenClaw agent wakes up and can process the new data via MCP

### Wake Message Format

The waker supports two endpoint formats:
- `/hooks/agent` — sends `{ message, name: "AD4M", wakeMode: "now" }` (recommended, creates isolated agent run)
- `/hooks/wake` — sends `{ text, mode: "now" }`

The wake message includes: event description, MCP endpoint, auth credential, agent DID, perspective UUID, channel address, subscription ID, and event type.

## Integration with OpenClaw

Add to your OpenClaw config's hooks section:
```json
{
  "hooks": {
    "enabled": true,
    "path": "/hooks",
    "token": "your-hooks-token"
  }
}
```
