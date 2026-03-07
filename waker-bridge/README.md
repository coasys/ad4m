# AD4M Waker Bridge

Watches AD4M perspectives via `QuerySubscriptionProxy` (SurrealDB-backed, same mechanism as Flux UI) and POSTs to an OpenClaw `/hooks/agent` endpoint when matching links are detected.

The waker sends **actionable messages** with full context (perspective, channel, subscription type) so the agent knows exactly what happened and what to do.

Requires `@coasys/ad4m ^0.12.0`.

---

## Quick start

```bash
cd waker-bridge
npm install
node ad4m-waker.js --config waker-config.json
```

---

## Generating subscription configs

The waker is a config-driven runner — it executes whatever SurrealQL queries you give it. Query generation is handled by the AD4M MCP tools:

- **`get_mention_waker_config`** — generates a mention-tracking subscription for a neighbourhood (fires when messages contain the agent's name or DID)
- **`generate_waker_query`** — generates a subscription from a SHACL subject class definition

Typical flow:
1. Agent joins a neighbourhood via `neighbourhood_join_from_url`
2. Agent calls `get_mention_waker_config` with the perspective UUID
3. Agent appends the returned subscription entry to the waker config file (adding `type`, `channel`, and `neighbourhood` fields)
4. Agent restarts the waker

---

## Config file format

```json
{
  "executorUrl": "ws://localhost:12100/graphql",
  "token": "optional-ad4m-credential",
  "wakeUrl": "http://localhost:18789/hooks/agent",
  "wakeToken": "your-openclaw-wake-token",
  "debounceMs": 2000,
  "subscriptions": [
    {
      "id": "flux-all-messages",
      "type": "channel-messages",
      "perspective": "<neighbourhood-uuid>",
      "channel": "literal://string:<channel-id>",
      "neighbourhood": "neighbourhood://Qm...",
      "query": "SELECT * FROM link WHERE source = 'literal://string:<channel-id>' AND predicate = 'ad4m://has_child'"
    },
    {
      "id": "mention-<did-suffix>",
      "type": "mention",
      "perspective": "<neighbourhood-uuid>",
      "channel": "literal://string:<channel-id>",
      "neighbourhood": "neighbourhood://Qm...",
      "query": "SELECT * FROM link WHERE fn::contains(string::lowercase(fn::parse_literal(target)), 'agentname') OR fn::contains(string::lowercase(fn::parse_literal(target)), 'did:key:z6Mks...')"
    }
  ]
}
```

### Top-level fields

| Field | Required | Description |
|-------|----------|-------------|
| `executorUrl` | ✅ | AD4M executor WebSocket URL |
| `token` | | AD4M admin credential or JWT |
| `mcpEndpoint` | | MCP endpoint URL (e.g., `http://localhost:3001/mcp`) — included in wake messages so the agent knows where to connect |
| `agentDid` | | Agent DID — included in wake messages so the agent can identify itself |
| `wakeUrl` | ✅ | OpenClaw hooks endpoint (use `/hooks/agent` for actionable wakes) |
| `wakeToken` | ✅ | Bearer token for the hooks endpoint |
| `debounceMs` | | Debounce delay in ms (default: 2000) |
| `subscriptions` | ✅ | Array of subscription objects |

### Subscription fields

| Field | Required | Description |
|-------|----------|-------------|
| `id` | ✅ | Unique identifier for this subscription |
| `type` | ✅ | `"mention"` or `"channel-messages"` — determines the wake message content |
| `perspective` | ✅ | AD4M perspective UUID to subscribe to |
| `channel` | ✅ | Channel address (so the agent knows where to read/post messages) |
| `neighbourhood` | | Neighbourhood URL (for additional context) |
| `query` | ✅ | SurrealQL query — fires when the result set changes |

### Subscription types

- **`mention`**: Fires when the agent is mentioned by name or DID. Wake message tells the agent to find and respond to the mention.
- **`channel-messages`**: Fires when any new message appears in a channel. Wake message tells the agent to read new messages and respond if appropriate.

---

## How it works

1. Connects to the AD4M executor via GraphQL WebSocket
2. For each subscription, creates a `QuerySubscriptionProxy` with the given SurrealQL query
3. When the query result set changes, debounces and POSTs to `/hooks/agent` with:
   - An **actionable message** including perspective UUID, channel address, and subscription type
   - `name: "AD4M"` so the agent session is labelled
   - `wakeMode: "now"` for immediate processing
4. OpenClaw runs an isolated agent turn that reads new messages via MCP and responds

---

## Wake message examples

**Mention wake:**
```
You were mentioned in a Flux neighbourhood.
Read the latest messages in the channel, find the mention, and respond appropriately.

Perspective: 01409ead-3e13-4ca6-99ac-e1b623c18604
Channel: literal://string:gjgfascqbfhntekmtvhtbohu
Neighbourhood: neighbourhood://QmzSYwdhcjCcf726JkvGKKw7bszp3Jd2NsNN2ULkxJ8VYxdU9wv
Subscription: mention-guAacszuc2Jd
```

**Channel messages wake:**
```
New messages appeared in a Flux channel.
Read the latest messages and respond if appropriate (e.g. if addressed to you or relevant to your role).

Perspective: 01409ead-3e13-4ca6-99ac-e1b623c18604
Channel: literal://string:gjgfascqbfhntekmtvhtbohu
Subscription: e713df2e-3ea6-406e-b58e-e048a12f23ce
```

---

## Programmatic use

```js
const { startWaker } = require("./ad4m-waker");

const waker = await startWaker(config);
// Later:
waker.close();
```

---

## Testing

```bash
npm test
```
