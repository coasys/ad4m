# AD4M Waker (Embedded)

The AD4M waker watches perspectives for data changes via GraphQL WebSocket subscriptions and wakes your OpenClaw agent when relevant events occur. It runs as a background service inside the AD4M plugin — no separate process needed.

## How It Works

1. The plugin's `ad4m-waker` service connects to the AD4M executor's GraphQL WebSocket endpoint
2. When you call `subscribe_to_mentions` or `subscribe_to_children`, the plugin creates a `QuerySubscriptionProxy` with a SurrealQL live query
3. When query results change, the plugin debounces and POSTs to OpenClaw's `/hooks/wake` endpoint
4. Your agent wakes up with context about what changed and processes the new data via MCP tools

## Plugin Config Fields

| Field | Default | Description |
|-------|---------|-------------|
| `wakerEnabled` | `true` | Enable/disable the waker service |
| `executorWsUrl` | `ws://localhost:12100/graphql` | AD4M executor GraphQL WebSocket URL |
| `wakeUrl` | `http://localhost:18789/hooks/wake` | OpenClaw wake endpoint URL |
| `wakeToken` | — | Bearer token for the wake endpoint (required for waker to function) |
| `debounceMs` | `2000` | Debounce interval to prevent rapid-fire wakes (ms) |

## Subscription Tools

| Tool | Description |
|------|-------------|
| `subscribe_to_mentions(perspective_id)` | Watch for messages mentioning your name or DID |
| `subscribe_to_children(perspective_id, expression_address)` | Watch for new children under a parent (e.g., messages in a channel) |
| `unsubscribe_from_mentions(perspective_id)` | Stop watching mentions in a neighbourhood |
| `unsubscribe_from_children(perspective_id, expression_address)` | Stop watching a channel |
| `list_waker_subscriptions()` | List all active subscriptions |

The subscribe tools call the MCP tools `get_mention_waker_config` / `generate_waker_query` internally to build the SurrealQL queries — you don't need to construct queries manually.

## Wake Message Format

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

## OpenClaw Hooks Config

Add to your OpenClaw config:
```json
{
  "hooks": {
    "enabled": true,
    "path": "/hooks",
    "token": "your-hooks-token"
  }
}
```
