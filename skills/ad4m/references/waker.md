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
  "wakeUrl": "http://localhost:18789/hooks/wake",
  "wakeToken": "your-openclaw-hooks-token",
  "debounceMs": 2000,
  "subscriptions": [
    {
      "id": "flux-messages",
      "type": "channel-messages",
      "perspective": "your-local-perspective-uuid",
      "channel": "literal://string:channel-id",
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
| `wakeUrl` | OpenClaw webhook endpoint — use `/hooks/wake` for isolated agent runs |
| `wakeToken` | OpenClaw hooks authentication token |
| `debounceMs` | Debounce interval (prevents rapid-fire wakes, default 2000) |
| `subscriptions` | Array of subscription objects (see below) |

### Subscription Fields

| Field | Required | Description |
|-------|----------|-------------|
| `id` | yes | Unique identifier |
| `type` | yes | `"mention"` or `"channel-messages"` — determines the wake message |
| `perspective` | yes | Your **local** perspective UUID (from `list_perspectives()`) |
| `channel` | yes | Channel address (where to read/post) |
| `query` | yes | SurrealQL subscription query |

> **Note:** Perspective UUIDs are local to your device. To find the local UUID for a neighbourhood, call `list_perspectives()` and match by the neighbourhood URL in the response. Store this mapping in your memory file (see SKILL.md rule 11).

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

**Use `/hooks/wake` (recommended).** It enqueues the event into the main agent session which has your skills loaded. Do NOT use `/hooks/agent` — that spawns an isolated sub-agent without your skills.

**`/hooks/wake` payload:**
```json
{
  "text": "New messages in an AD4M neighbourhood.\nRead the AD4M skill for instructions on how to handle this.\n\nMCP endpoint: http://localhost:3001/mcp\nAuth credential: your-admin-credential\nAgent DID: did:key:z6Mk...\nPerspective: cda8c4fc-...\nChannel: literal://string:channel-id\nSubscription: flux-messages\nEvent type: channel-messages",
  "mode": "now"
}
```

The `text` field contains key-value pairs, one per line:
- **Line 1** — Event description: "New messages in an AD4M neighbourhood." or "You were @mentioned in an AD4M neighbourhood."
- **Line 2** — "Read the AD4M skill for instructions on how to handle this."
- **MCP endpoint** — where to connect (e.g. `http://localhost:3001/mcp`)
- **Auth credential** — admin credential for the Authorization header
- **Agent DID** — the agent's own DID (to identify own messages)
- **Perspective** — local perspective UUID to operate on (look up your memory file for context about this space)
- **Channel** — channel address (where to read/post)
- **Subscription** — subscription ID
- **Event type** — `"mention"` or `"channel-messages"`

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
