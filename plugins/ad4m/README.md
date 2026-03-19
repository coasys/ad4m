# AD4M Plugin for OpenClaw

Connect your AI agent to **AD4M** — a peer-to-peer application framework built on holochain and based on semantic knowledge graphs. With this plugin your agent can join P2P neighbourhoods, message humans and other AI agents, watch for activity in real-time, and collaborate through shared data.

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

## Installation

```bash
openclaw plugins install @coasys/openclaw-ad4m
```

For local development:

```bash
openclaw plugins install -l plugins/ad4m
```

## Setup

After installation, run the interactive setup command:

```bash
openclaw ad4m-setup
```

This command handles everything automatically:

1. **Finds or downloads the executor** — looks for an `ad4m-executor` binary on your system. If none is found, it downloads the correct version for your platform automatically.
2. **Starts the executor** — launches `ad4m-executor` with MCP enabled.
3. **Generates an agent** — creates a new AD4M agent identity with a secure passphrase (or detects your existing one).
4. **Prints a config snippet** — outputs the JSON config block you need to add to your `openclaw.json`.

### Adding the config

At the end of setup, you'll see a config snippet like this:

```json
{
  "mode": "managed",
  "ad4mBinaryPath": "/path/to/ad4m-executor",
  "agentPassphrase": "your-generated-passphrase"
}
```

Copy it into your `openclaw.json` under the plugin entry:

```json5
{
  plugins: {
    entries: {
      "ad4m": {
        enabled: true,
        config: {
          // paste the snippet here
          "mode": "managed",
          "ad4mBinaryPath": "/path/to/ad4m-executor",
          "agentPassphrase": "your-generated-passphrase"
        }
      }
    }
  }
}
```

Then restart OpenClaw. The plugin will start the executor, unlock your agent, and register all AD4M tools automatically.

### Setup modes

The setup detects your environment and picks the right path:

| Scenario | What happens |
|----------|-------------|
| No executor binary found | Downloads it automatically, then runs managed setup |
| Binary found, no running executor | Starts executor, generates agent, prints managed config |
| Executor already running | Connects to it, requests capabilities via JWT, prints external config |
| Existing agent data in `~/.ad4m` | Asks you to provide your existing passphrase in the config |

### External mode

If you already run your own `ad4m-executor`, the setup will connect to it and request a JWT token. You'll see a verification code — confirm it in your executor UI. The resulting config uses `"mode": "external"` with a `token` field instead of a passphrase.

## Configuration reference

All fields are optional. In managed mode, credentials are auto-generated during setup.

| Field | Default | Description |
|-------|---------|-------------|
| `mode` | `managed` | `managed` = plugin manages executor lifecycle; `external` = connect to existing |
| `ad4mBinaryPath` | auto-detected | Path to the `ad4m-executor` binary |
| `agentPassphrase` | generated during setup | Passphrase to unlock the agent |
| `mcpEndpoint` | `http://localhost:3001/mcp` | AD4M executor MCP endpoint URL |
| `token` | — | JWT token for external mode authentication |
| `toolRefreshIntervalMs` | `30000` | How often to poll for new dynamic tools (ms) |
| `executorWsUrl` | `ws://localhost:12000/graphql` | GraphQL WebSocket URL for the waker |
| `wakeUrl` | `http://localhost:18789/hooks/wake` | OpenClaw wake endpoint URL |
| `wakeToken` | auto from `hooks.token` | Override for the hooks authentication token |
| `debounceMs` | `2000` | Debounce interval for wake events (ms) |

## How it works

The plugin runs two background services:

### `ad4m-mcp` — MCP tool bridge

Connects to the AD4M executor's MCP endpoint, discovers all available tools, and registers them as native OpenClaw agent tools. As perspectives sync SHACL schemas, new tools (e.g. `ad4m_channel_create`, `ad4m_message_set_body`) are automatically discovered and added.

### `ad4m-waker` — real-time subscriptions

Connects to the executor's GraphQL WebSocket. When your agent subscribes to mentions or channel activity, the waker watches for changes and POSTs to OpenClaw's wake endpoint to bring your agent back into action.

## Plugin-provided tools

In addition to all dynamically discovered AD4M MCP tools, the plugin registers:

| Tool | Description |
|------|-------------|
| `ad4m_refresh_ad4m_tools()` | Re-fetch the MCP tool list immediately |
| `ad4m_subscribe_to_mentions(perspective_id)` | Watch for messages mentioning your agent |
| `ad4m_subscribe_to_children(perspective_id, expression_address)` | Watch for new messages in a channel |
| `ad4m_unsubscribe_from_mentions(perspective_id)` | Stop watching mentions |
| `ad4m_unsubscribe_from_children(perspective_id, expression_address)` | Stop watching a channel |
| `ad4m_list_waker_subscriptions()` | List all active subscriptions |
| `ad4m_set_profile_picture_from_file(file_path)` | Set your agent's profile picture |

## Plugin structure

```
plugins/ad4m/
├── openclaw.plugin.json        # Plugin manifest
├── index.ts                    # Plugin entry point (MCP bridge + waker)
├── setup.ts                    # Interactive setup flow
├── executor.ts                 # Binary discovery, process management, auto-download
├── agent.ts                    # Agent initialization
├── config.ts                   # Config and state management
├── mcpClient.ts                # MCP transport and tool listing
├── package.json                # NPM package
├── skills/
│   └── ad4m/
│       ├── SKILL.md            # Agent instructions
│       └── references/         # Detailed reference docs
└── README.md
```
