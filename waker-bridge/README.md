# AD4M Waker Bridge

Watches an AD4M executor for perspective link changes via GraphQL WebSocket subscriptions and POSTs to an OpenClaw `/hooks/wake` endpoint when matching links are detected.

## Usage

```bash
deno run --allow-net ad4m-waker.ts \
  --executor-url ws://localhost:12000/graphql \
  --perspective <uuid> \
  --class Message \
  --source "flux://channel-general" \
  --wake-url http://localhost:18789/hooks/wake \
  --wake-token "my-wake-token" \
  --wake-message "New message in #general"
```

## Options

| Flag | Required | Description |
|------|----------|-------------|
| `--perspective` | ✅ | Perspective UUID to watch |
| `--wake-url` | ✅ | OpenClaw wake endpoint URL |
| `--wake-token` | ✅ | Bearer token for authentication |
| `--executor-url` | | AD4M GraphQL WS URL (default: `ws://localhost:12000/graphql`) |
| `--class` | | Subject class to filter (e.g. `Message`) |
| `--source` | | Filter links by source URI |
| `--wake-message` | | Message in wake payload |
| `--token` | | AD4M capability token |
| `--also-removed` | | Also watch link removals |

## How it works

1. Connects to AD4M executor via `graphql-transport-ws` WebSocket protocol
2. Subscribes to `perspectiveLinkAdded` for the given perspective
3. Filters links by class (matches `rdf://type` or `ad4m://has_child` predicates) and optional source
4. Debounces rapid changes (2s default)
5. POSTs matching events to the wake URL with link context

## Testing

```bash
deno test --allow-net ad4m-waker.test.ts
```

## As a module

```typescript
import { startWaker, WakerConfig } from "./ad4m-waker.ts";

const waker = await startWaker({
  executorUrl: "ws://localhost:12000/graphql",
  perspective: "my-uuid",
  className: "Message",
  wakeUrl: "http://localhost:18789/hooks/wake",
  wakeToken: "my-token",
  wakeMessage: "New message",
});

// Later:
waker.close();
```
