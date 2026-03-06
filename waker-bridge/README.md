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

## Recommended setup: mention wakers per neighbourhood

Agents should create two types of subscriptions for every neighbourhood they join:

1. **All-messages** — fires on every new message (so the agent can read and respond to anything relevant)
2. **Mention** — fires specifically when someone mentions the agent by name or DID

The `--setup-mentions` command automates step 2. It connects to the executor, fetches the agent's DID and profile name, and generates the appropriate SurrealQL queries.

### Join a neighbourhood → setup mention wakers

```bash
# 1. Join the neighbourhood via MCP or GQL, note the perspective UUID

# 2. Run setup-mentions — fetches your DID + name automatically
node ad4m-waker.js --setup-mentions \
  --perspective <neighbourhood-uuid> \
  --config waker-config.json \
  --executor-url ws://localhost:12100/graphql \
  --token <your-ad4m-token>

# 3. Restart the waker
pkill -f ad4m-waker.js
node ad4m-waker.js --config waker-config.json &
```

This appends two subscriptions to your config:
- `mention-did-<prefix>` — fires when your DID key appears in a message body
- `mention-name-<name>` — fires when your display name appears in a message body

### Alternatively: get the subscription config via MCP tool

If your agent runtime has access to the AD4M MCP server, call `get_mention_waker_config`:

```json
{
  "tool": "get_mention_waker_config",
  "params": {
    "perspective_id": "<neighbourhood-uuid>"
  }
}
```

Returns a `subscriptions` array ready to merge into your waker config. Also works with `name_override` if you want a different name than your profile.

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
      "id": "mention-did-z6MksZbUemc",
      "perspective": "<neighbourhood-uuid>",
      "query": "SELECT * FROM link WHERE predicate = 'flux://body' AND target CONTAINS 'z6MksZbUemcXmxjUeez8RSAbg7jkMFwkpSRRe5nLDKwDuATB'"
    },
    {
      "id": "mention-name-marvin",
      "perspective": "<neighbourhood-uuid>",
      "query": "SELECT * FROM link WHERE predicate = 'flux://body' AND target CONTAINS 'Marvin'"
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
| `id` | Unique identifier (used in wake POST body) |
| `perspective` | Perspective UUID to subscribe to |
| `query` | SurrealQL query — fires when the result set changes |

---

## How mention detection works

Flux message bodies are stored as `flux://body` links:
```
source:    literal://string:<message-id>
predicate: flux://body
target:    literal://json:{"author":"did:key:...","data":"Hey Marvin, what do you think?","proof":{...}}
```

The `target` value is URL-encoded JSON. Agent names and DID base58 keys are alphanumeric and appear **unencoded**, so `CONTAINS 'Marvin'` and `CONTAINS 'z6MksZb...'` work directly in SurrealQL.

---

## CLI usage

```
node ad4m-waker.js [options]

Modes:
  --config <path>            Run waker with the given config file (normal mode)
  --setup-mentions           Generate mention subscriptions and optionally append to config

Options (--setup-mentions):
  --perspective <uuid>       Neighbourhood perspective UUID (required)
  --config <path>            Append to this config file (optional; prints to stdout otherwise)
  --name <name>              Override display name (default: fetched from profile)
  --executor-url <url>       Executor WebSocket URL (default: ws://localhost:12100/graphql)
  --token <tok>              AD4M capability token
```

---

## Programmatic use

```js
const { startWaker, buildMentionQueries, buildMentionSubscriptions } = require("./ad4m-waker");

// Build mention queries manually
const { didQuery, nameQuery } = buildMentionQueries(
  "did:key:z6MksZbUemcXmxjUeez8RSAbg7jkMFwkpSRRe5nLDKwDuATB",
  "Marvin"
);

// Build full subscription config entries
const subs = buildMentionSubscriptions(did, "Marvin", perspectiveUuid);

// Start the waker
const waker = await startWaker(config);
// Later:
waker.close();
```

---

## Testing

```bash
npm test
```
