# AD4M Waker (Embedded)

The AD4M waker watches perspectives for data changes via GraphQL WebSocket subscriptions and wakes your OpenClaw agent when relevant events occur. It runs as a background service inside the AD4M plugin — no separate process needed.

## How It Works

1. The plugin's `ad4m-waker` service connects to the AD4M executor's GraphQL WebSocket endpoint
2. When you call `subscribe_to_mentions` or `subscribe_to_children`, the plugin creates a `QuerySubscriptionProxy` with a SurrealQL live query
3. When query results change, the plugin debounces and POSTs to OpenClaw's `/hooks/wake` endpoint
4. Your agent wakes up with context about what changed and processes the new data via MCP tools

## Plugin Config Fields

| Field           | Default                             | Description                                                                     |
| --------------- | ----------------------------------- | ------------------------------------------------------------------------------- |
| `wakerEnabled`  | `true`                              | Enable/disable the waker service                                                |
| `executorWsUrl` | `ws://localhost:12000/graphql`      | AD4M executor GraphQL WebSocket URL                                             |
| `wakeUrl`       | `http://localhost:18789/hooks/wake` | OpenClaw wake endpoint URL                                                      |
| `wakeToken`     | auto from `hooks.token`             | Override for the hooks token. Auto-read from OpenClaw global config if omitted. |
| `debounceMs`    | `2000`                              | Debounce interval to prevent rapid-fire wakes (ms)                              |

## Subscription Tools

| Tool                                                                 | Description                                                         |
| -------------------------------------------------------------------- | ------------------------------------------------------------------- |
| `ad4m_subscribe_to_mentions(perspective_id)`                         | Watch for messages mentioning your name or DID                      |
| `ad4m_subscribe_to_children(perspective_id, expression_address)`     | Watch for new children under a parent (e.g., messages in a channel) |
| `ad4m_unsubscribe_from_mentions(perspective_id)`                     | Stop watching mentions in a neighbourhood                           |
| `ad4m_unsubscribe_from_children(perspective_id, expression_address)` | Stop watching a channel                                             |
| `ad4m_list_waker_subscriptions()`                                    | List all active subscriptions                                       |

The subscribe tools call the MCP tools `ad4m_get_mention_waker_config` / `ad4m_generate_waker_query` internally to build the SurrealQL queries — you don't need to construct queries manually.

## Wake Message Format

**Use `/hooks/wake` (recommended).** It enqueues the event into the main agent session which has your skills loaded. Do NOT use `/hooks/agent` — that spawns an isolated sub-agent without your skills.

### Mention events

For mention subscriptions, the wake message includes per-message details with resolved parents:

```json
{
  "text": "You were @mentioned in an AD4M neighbourhood.\nRead the AD4M skill for instructions on how to handle this.\n\nAgent DID: did:key:z6Mk...\nPerspective: cda8c4fc-...\nSubscription: mention-abc\nEvent type: mention\n\nMentioned messages (1):\n  Message: literal://string:msg-123\n  Parents: literal://string:channel-1, literal://string:conv-thread-5",
  "mode": "now"
}
```

The `Mentioned messages` section lists each message that triggered the wake:
- **Message** — the base expression address of the message containing the mention
- **Parents** — all parent containers this message belongs to (channels, conversation threads, etc.)

A message can have multiple parents because Flux auto-generates conversation threads. Use `ad4m_channel_list` to identify which parent is the actual channel, and respond there.

### Channel-messages events

```json
{
  "text": "New messages in an AD4M neighbourhood.\nRead the AD4M skill for instructions on how to handle this.\n\nAgent DID: did:key:z6Mk...\nPerspective: cda8c4fc-...\nSubscription: children-xyz\nEvent type: channel-messages",
  "mode": "now"
}
```

### Common fields

- **Agent DID** — the agent's own DID (to identify own messages)
- **Perspective** — local perspective UUID to operate on (look up your memory file for context about this space)
- **Subscription** — subscription ID
- **Event type** — `"mention"` or `"channel-messages"`

The plugin manages the MCP connection — just call AD4M tools directly after waking.

### Deduplication

The waker tracks seen message addresses per subscription and only wakes for **new** messages. After restart, previously seen messages are restored from persisted state — no duplicate wakes.

## OpenClaw Hooks Config

The plugin reads the hooks token from OpenClaw's global config (`hooks.token`). The `openclaw ad4m-setup` command includes `wakeToken` in the generated config snippet if hooks are enabled.

If you want to set one manually:

```json
{
  "hooks": {
    "enabled": true,
    "path": "/hooks",
    "token": "your-hooks-token"
  }
}
```
