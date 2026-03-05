# AD4M Waker Bridge

The AD4M Waker watches perspectives for data changes via GraphQL subscriptions and wakes your OpenClaw agent when relevant events occur. This enables reactive AI agents that respond to neighbourhood activity in real-time.

## Source

The waker is built from `waker-bridge/` in the [AD4M repository](https://github.com/coasys/ad4m/tree/dev/waker-bridge). It's a standalone Node.js module published as `@coasys/ad4m-waker`.

## Installation

```bash
# From npm (when published)
npm install -g @coasys/ad4m-waker

# Or from the AD4M repo
cd ad4m/waker-bridge
npm install
```

The skill includes a pre-built bundle at `bundle/ad4m-waker.js` for convenience.

## Configuration

Create `waker-config.json`:

```json
{
  "executor": {
    "graphqlUrl": "ws://localhost:12100/graphql",
    "adminCredential": "your-admin-credential"
  },
  "wake": {
    "url": "http://localhost:18789/hooks/wake",
    "token": "your-openclaw-hooks-token"
  },
  "subscriptions": [
    {
      "name": "flux-messages",
      "perspectiveUuid": "ab232819-5b0f-45d4-aa6c-d7f63c78bbf1",
      "query": "SELECT * FROM link WHERE predicate = 'rdf://type'",
      "debounceMs": 5000
    }
  ]
}
```

### Configuration Fields

| Field | Description |
|-------|-------------|
| `executor.graphqlUrl` | WebSocket URL for AD4M executor GraphQL |
| `executor.adminCredential` | Admin credential for authentication |
| `wake.url` | OpenClaw webhook endpoint to POST wake events |
| `wake.token` | OpenClaw hooks authentication token |
| `subscriptions[].name` | Human-readable subscription name |
| `subscriptions[].perspectiveUuid` | Perspective to watch |
| `subscriptions[].query` | SurrealDB query for filtering changes |
| `subscriptions[].debounceMs` | Debounce interval (prevents rapid-fire wakes) |

## Running

```bash
# Direct
node ad4m-waker.js --config waker-config.json

# Via npm script
npm start

# As a background service (recommended)
screen -dmS ad4m-waker bash -c 'node ad4m-waker.js --config waker-config.json 2>&1 | tee /tmp/ad4m-waker.log'
```

## How It Works

1. Connects to AD4M executor via GraphQL WebSocket
2. Subscribes to `subscribeSurrealDB` for each configured subscription
3. When new links match the query, debounces and POSTs to the OpenClaw wake endpoint
4. OpenClaw agent wakes up and can process the new data

## Building the Bundle

To create a fresh bundle from source:

```bash
cd ad4m/waker-bridge
npm install
# The module is a single file (ad4m-waker.js) with npm dependencies
# For deployment, copy the entire waker-bridge/ directory or use npm pack
npm pack  # Creates @coasys/ad4m-waker-0.3.0.tgz
```

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

The waker POSTs to `{gateway-url}/hooks/wake` with the configured token, triggering agent wake-up.
