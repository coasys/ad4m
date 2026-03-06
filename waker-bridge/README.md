# AD4M Waker Bridge

Watches AD4M perspectives via `QuerySubscriptionProxy` (SurrealDB-backed, same mechanism as Flux UI) and POSTs to an OpenClaw `/hooks/wake` endpoint when matching links are detected.

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
3. Agent appends the returned subscription entry to the waker config file
4. Agent restarts the waker

---

## Config file format

```json
{
  "executorUrl": "ws://localhost:12100/graphql",
  "token": "optional-ad4m-credential",
  "wakeUrl": "http://localhost:18789/hooks/wake",
  "wakeToken": "your-openclaw-wake-token",
  "debounceMs": 2000,
  "subscriptions": [
    {
      "id": "flux-all-messages",
      "perspective": "<neighbourhood-uuid>",
      "query": "SELECT * FROM link WHERE source = 'literal://string:<channel-id>' AND predicate = 'ad4m://has_child'"
    },
    {
      "id": "mention-<did-suffix>",
      "perspective": "<neighbourhood-uuid>",
      "query": "SELECT * FROM link WHERE target CONTAINS 'Marvin' OR target CONTAINS 'did:key:z6Mks...'"
    }
  ]
}
```

| Field | Required | Description |
|-------|----------|-------------|
| `executorUrl` | ✅ | AD4M executor WebSocket URL |
| `token` | | AD4M capability token |
| `wakeUrl` | ✅ | OpenClaw wake endpoint |
| `wakeToken` | ✅ | Bearer token for the wake endpoint |
| `debounceMs` | | Debounce delay in ms (default: 2000) |
| `subscriptions` | ✅ | Array of subscription objects |

Each subscription:

| Field | Description |
|-------|-------------|
| `id` | Unique identifier (included in wake POST body) |
| `perspective` | Perspective UUID to subscribe to |
| `query` | SurrealQL query — fires when the result set changes |

---

## How it works

1. Connects to the AD4M executor via GraphQL WebSocket
2. For each subscription, creates a `QuerySubscriptionProxy` with the given SurrealQL query
3. When the query result set changes, debounces and POSTs to the wake URL
4. The wake POST body contains the subscription `id` so the agent knows what changed

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
