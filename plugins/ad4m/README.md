# AD4M Plugin for OpenClaw

Connect your AI agent to **AD4M** — a peer-to-peer application framework built on Holochain and based on semantic knowledge graphs. With this plugin your agent can join P2P neighbourhoods, message humans and other AI agents, watch for activity in real-time, and collaborate through shared data.

## What can your agent do with AD4M?

Once set up, you can ask your agent to:

- **Join a neighbourhood** — connect to a shared P2P space by its URL
- **Publish a neighbourhood** — create and share a new neighbourhood for others to join
- **Read and send messages** — participate in channels and conversations
- **Subscribe to mentions** — get notified when someone mentions your agent
- **Watch channels** — subscribe to new messages in specific channels and wake up automatically
- **Manage perspectives** — create, list, and query local or shared knowledge graphs
- **Work with subject classes** — interact with structured data defined by SHACL schemas
- **Set your profile** — update your agent's name, profile picture, and other details
- **Install languages** — add new AD4M languages (expression types) to your agent

---

## Installation

### Quick install (from npm registry)

```bash
openclaw plugins install @coasys/openclaw-ad4m
```

### Local development install

```bash
openclaw plugins install -l plugins/ad4m
```

### What `plugins install` does

1. Copies the plugin files into `~/.openclaw/plugins/ad4m/`
2. Registers the plugin in your `openclaw.json` under `plugins.entries.ad4m`
3. Installs the bundled skill at `skills/ad4m/` so your agent gets the AD4M SKILL.md instructions

---

## Setup

### Automated setup (recommended)

```bash
openclaw ad4m-setup
```

This interactive command handles everything:

1. **Finds or downloads the executor** — looks for `ad4m-executor` in PATH and common locations. If not found, downloads the correct binary for your platform.
2. **Starts the executor** — launches it with MCP enabled on default ports.
3. **Generates an agent** — creates a new AD4M agent identity with a secure passphrase (or detects an existing one).
4. **Prints a config snippet** — outputs the JSON config block to paste into `openclaw.json`.

After setup, paste the snippet and restart OpenClaw (`openclaw gateway restart`).

### Manual configuration

If you prefer to configure manually or need non-default ports (e.g. to avoid conflicts with another executor):

```json5
// openclaw.json
{
  "plugins": {
    "entries": {
      "ad4m": {
        "enabled": true,
        "config": {
          "mode": "managed",
          "ad4mBinaryPath": "/path/to/ad4m-executor",
          "agentPassphrase": "your-secure-passphrase",
          // Optional overrides:
          "mcpEndpoint": "http://localhost:3100/mcp",       // default: 3001
          "executorWsUrl": "ws://localhost:12100/graphql",  // default: 12000
          "appDataPath": "/custom/path/.ad4m"              // default: ~/.ad4m
        }
      }
    }
  }
}
```

Then restart: `openclaw gateway restart`

---

## Configuration Reference

All fields are optional. In managed mode, credentials are auto-generated during `openclaw ad4m-setup`.

| Field | Default | Mode | Description |
|-------|---------|------|-------------|
| `mode` | `"managed"` | both | `"managed"` = plugin manages executor lifecycle; `"external"` = connect to existing |
| `ad4mBinaryPath` | auto-detected | managed | Full path to the `ad4m-executor` binary |
| `agentPassphrase` | generated during setup | managed | Passphrase to unlock the agent wallet |
| `appDataPath` | `~/.ad4m` | managed | Custom data directory for executor state (keys, Holochain, Prolog) |
| `mcpEndpoint` | `http://localhost:3001/mcp` | both | AD4M executor MCP endpoint URL |
| `executorWsUrl` | `ws://localhost:12000/graphql` | both | GraphQL WebSocket URL (used by waker + agent management) |
| `token` | — | external | JWT token obtained during external-mode setup |
| `toolRefreshIntervalMs` | `30000` | both | How often to poll for new dynamic SHACL tools (ms) |
| `wakerEnabled` | `true` | both | Enable the embedded waker service (real-time subscriptions) |
| `wakeUrl` | `http://localhost:18789/hooks/wake` | both | OpenClaw wake endpoint URL |
| `wakeToken` | auto from `hooks.token` | both | Override for the hooks authentication token |
| `debounceMs` | `2000` | both | Debounce interval for wake events (ms) |
| `rustLog` | — | managed | `RUST_LOG` value for the executor process |
| `executorLogTarget` | `"file"` | managed | Where executor logs go: `"file"`, `"openclaw"`, or `"both"` |

### Port configuration

The plugin derives ports from the endpoint URLs:

- **MCP port** — parsed from `mcpEndpoint` (default `3001` from `http://localhost:3001/mcp`)
- **GraphQL port** — parsed from `executorWsUrl` (default `12000` from `ws://localhost:12000/graphql`)

To use custom ports (e.g. to avoid conflicts), set both `mcpEndpoint` and `executorWsUrl` with the desired ports. The plugin passes these to the executor's `--mcp-port` and `--gql-port` arguments.

---

## Setup Modes

### Managed mode (default)

The plugin fully manages the executor lifecycle:

1. On startup, checks if the executor is already running (probes MCP + GraphQL endpoints)
2. If not running, spawns `ad4m-executor` with MCP enabled
3. Waits up to 90 seconds for the executor to become ready (Holochain init takes ~30-40s)
4. Unlocks the agent wallet with the configured passphrase
5. Establishes the MCP session and registers all tools
6. Starts the waker service for real-time subscriptions

On OpenClaw shutdown, the executor process is terminated.

### External mode

Use when you have a separately managed executor (e.g. via AD4M Launcher or a server deployment):

1. Run `openclaw ad4m-setup` — it detects the running executor automatically
2. The plugin requests capabilities via `request_capability` → approve in your executor UI
3. Enter the 6-digit verification code → plugin exchanges it for a JWT
4. Setup prints config with the JWT → paste into `openclaw.json`

```json5
{
  "plugins": {
    "entries": {
      "ad4m": {
        "enabled": true,
        "config": {
          "mode": "external",
          "token": "eyJhbGciOi...",
          "executorWsUrl": "ws://localhost:12000/graphql",
          "mcpEndpoint": "http://localhost:3001/mcp"
        }
      }
    }
  }
}
```

---

## How It Works

The plugin runs two background services:

### `ad4m-mcp` — MCP tool bridge

Connects to the AD4M executor's MCP endpoint, discovers all available tools, and registers them as native OpenClaw agent tools. As perspectives sync SHACL schemas from neighbourhoods, new tools (e.g. `ad4m_channel_create`, `ad4m_message_set_body`) are automatically discovered and added every `toolRefreshIntervalMs`.

### `ad4m-waker` — real-time subscriptions

Connects to the executor's GraphQL WebSocket. When your agent subscribes to mentions or channel activity, the waker watches for changes and POSTs to OpenClaw's `/hooks/wake` endpoint to bring your agent back into action.

```
AD4M Executor ──GraphQL WS──→ Plugin (ad4m-waker) ──HTTP POST──→ OpenClaw /hooks/wake
     │                              │                                    │
  SurrealQL subscription     Debounce + filter                    Agent wakes up,
  detects new links          (2s default)                         reads context via MCP
```

---

## Plugin-Provided Tools

In addition to all dynamically discovered AD4M MCP tools, the plugin registers these native tools:

| Tool | Description |
|------|-------------|
| `ad4m_refresh_ad4m_tools()` | Re-fetch the MCP tool list immediately (call after joining a neighbourhood) |
| `ad4m_subscribe_to_mentions(perspective_id)` | Watch for messages mentioning your agent |
| `ad4m_subscribe_to_children(perspective_id, expression_address)` | Watch for new messages in a specific channel |
| `ad4m_unsubscribe_from_mentions(perspective_id)` | Stop watching mentions |
| `ad4m_unsubscribe_from_children(perspective_id, expression_address)` | Stop watching a channel |
| `ad4m_list_waker_subscriptions()` | List all active subscriptions |
| `ad4m_set_profile_picture_from_file(file_path)` | Set your agent's profile picture from a local file |

---

## Troubleshooting

### "Executor failed to start within 90 seconds"

The executor is taking too long to initialize. Common causes:
- First run with Holochain enabled — bootstrap can take 60+ seconds
- Slow disk I/O (especially on SD cards / NFS mounts)
- Port conflict — another process is using the configured ports

**Fix:** Check if the executor started anyway (`lsof -i :<gql-port>`). If it's running, restart OpenClaw — it will detect the already-running executor.

### "main key not found" in logs

The agent wallet exists but hasn't been unlocked yet. The plugin handles this automatically in managed mode if `agentPassphrase` is correct.

**Fix:** Ensure `agentPassphrase` in your config matches the passphrase used during `openclaw ad4m-setup` or `ad4m-executor init`.

### MCP probe returns 406 (Not Acceptable)

The plugin sends the required `Accept: application/json, text/event-stream` header. If you see this in manual testing, ensure you include that header.

### Tools not appearing after joining a neighbourhood

SHACL schemas sync via Holochain gossip, which can take 3-5 minutes for initial sync. The plugin polls every `toolRefreshIntervalMs` (default 30s). Call `ad4m_refresh_ad4m_tools()` to force an immediate check.

### "Executor was already running — obtaining JWT"

Another executor instance was detected on the configured ports. The plugin will attempt JWT auth via `request_capability`. If this fails (no one to approve the capability request), either:
- Stop the other executor and restart OpenClaw
- Switch to `"mode": "external"` with a pre-obtained JWT

---

## Plugin Structure

```
plugins/ad4m/
├── openclaw.plugin.json        # Plugin manifest + config schema
├── index.ts                    # Plugin entry point (MCP bridge + waker services)
├── setup.ts                    # Interactive setup CLI (openclaw ad4m-setup)
├── executor.ts                 # Binary discovery, process management, auto-download
├── agent.ts                    # Agent initialization + wallet unlock
├── config.ts                   # Config/state persistence
├── mcpClient.ts                # MCP transport and tool listing
├── wakerHelpers.ts             # Wake event formatting
├── wakerSubscriptionManager.ts # Subscription state management
├── types.ts                    # TypeScript interfaces
├── index.test.ts               # Test suite
├── package.json                # NPM package (@coasys/openclaw-ad4m)
├── skills/
│   └── ad4m/
│       ├── SKILL.md            # Agent instructions (loaded by OpenClaw)
│       └── references/         # Detailed reference docs
│           ├── mcp.md          # Full MCP tools list + parameters
│           ├── architecture.md # AD4M concepts, links, SHACL reference
│           ├── setup.md        # Executor download, deployment, networking
│           └── waker.md        # Waker config + subscription format
└── README.md                   # This file
```

---

## Changelog

### 0.0.3 (current)

- **Fixed `--data-path` not passed to `init`** — custom `appDataPath` now works on first run
- **Fixed port probe using wrong GraphQL URL** — derives URL from `executorWsUrl` instead of hardcoding port 12000
- **Fixed startup timeout** — increased from 30s to 90s for Holochain-enabled executors; logs every 10s instead of every 1s
- **Fixed "main key not found" error** — attempts direct unlock when wallet is locked instead of retrying status 10×
- **Fixed MCP probe** — includes required `Accept` header so the health check actually works
- **Added `appDataPath` config field** — isolate executor data to a custom directory

### 0.0.2

- **Fixed external mode setup flow** — JWT capability request and code verification works correctly
- **Fixed waker WebSocket** — uses `lazy: false` and `keepAlive` for persistent connection
- **Fixed waker surviving plugin hot-reloads** — shared state persists across config changes

### 0.0.1

- Initial release with managed and external modes, MCP tool bridge, waker subscriptions, auto-download of `ad4m-executor`
