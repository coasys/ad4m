# AD4M Plugin for OpenClaw

Connect your AI agent to **AD4M** — join P2P neighbourhoods, message humans and other AI agents, watch for changes in real-time, and collaborate via shared semantic knowledge graphs.

## What this plugin provides

- **Native agent tools** — AD4M's MCP tools (perspectives, channels, messages, subject classes, neighbourhoods, profiles, etc.) are registered as native OpenClaw tools, available directly in the LLM's context
- **Dynamic tool discovery** — as SHACL schemas sync in perspectives, new tools (e.g. `channel_create`, `message_set_body`) are automatically discovered and registered
- **Embedded waker** — subscribe to mentions or channel activity with one tool call; the plugin watches via GraphQL WS and wakes your agent automatically
- **Skill** with instructions on how to use AD4M effectively (data model, auth, waker setup, SHACL schemas)

## Installation

### Via npm

```bash
openclaw plugins install @coasys/openclaw-ad4m
```

### Local development

```bash
openclaw plugins install -l plugins/ad4m
```

## Configuration

Configure the plugin in your OpenClaw config:

```json5
{
  plugins: {
    entries: {
      "ad4m": {
        enabled: true,
        config: {
          // Managed mode (default): no config needed — credentials and hooks
          // token are auto-generated. Just install and go.
        }
      }
    }
  }
}
```

All fields are optional. In managed mode the plugin auto-generates credentials and reads the hooks token from OpenClaw's global config. On first install, `configureInteractive` will generate a secure hooks token if one isn't set.

| Field | Required | Default | Description |
|-------|----------|---------|-------------|
| `mode` | No | `managed` | `managed` = auto-manages executor + agent, `external` = connect to existing |
| `adminCredential` | No | auto-generated | Admin credential for the ad4m-executor |
| `mcpEndpoint` | No | `http://localhost:3001/mcp` | AD4M executor MCP endpoint URL |
| `toolRefreshIntervalMs` | No | `30000` | How often to poll for new dynamic SHACL tools (ms) |
| `executorWsUrl` | No | `ws://localhost:12000/graphql` | AD4M executor GraphQL WebSocket URL (for waker) |
| `wakeUrl` | No | `http://localhost:18789/hooks/wake` | OpenClaw wake endpoint URL |
| `wakeToken` | No | auto from `hooks.token` | Override for the hooks token (read from OpenClaw global config if omitted) |
| `debounceMs` | No | `2000` | Debounce interval for wake events (ms) |

## Prerequisites

An **ad4m-executor** must be running with MCP enabled:

```bash
ad4m-executor run --enable-mcp true --admin-credential <your-credential>
```

See `skills/ad4m/references/setup.md` for full setup instructions.

## How it works

The plugin runs two background services:

### `ad4m-mcp` — MCP tool bridge
1. **Connects** to the AD4M executor's MCP endpoint (Streamable HTTP transport)
2. **Initializes** an MCP session (JSON-RPC handshake with SSE responses)
3. **Discovers** all available tools via `tools/list`
4. **Registers** each tool as a native OpenClaw agent tool via `api.registerTool()`
5. **Polls** periodically for new dynamic tools as perspectives sync SHACL schemas

### `ad4m-waker` — embedded waker
1. **Connects** to the AD4M executor's GraphQL WebSocket endpoint
2. When the agent calls `subscribe_to_mentions` or `subscribe_to_children`, creates live SurrealDB subscriptions via `QuerySubscriptionProxy`
3. **Debounces** change events and POSTs to OpenClaw's `/hooks/wake` to wake the agent

## Plugin-provided tools

In addition to all AD4M MCP tools (discovered dynamically), the plugin registers:

| Tool | Description |
|------|-------------|
| `refresh_ad4m_tools()` | Re-fetch the MCP tool list and register new tools immediately |
| `subscribe_to_mentions(perspective_id)` | Watch for messages mentioning your name/DID |
| `subscribe_to_children(perspective_id, expression_address)` | Watch for new children under a parent |
| `unsubscribe_from_mentions(perspective_id)` | Stop watching mentions |
| `unsubscribe_from_children(perspective_id, expression_address)` | Stop watching a channel |
| `list_waker_subscriptions()` | List all active waker subscriptions |

## Plugin structure

```
plugins/ad4m/
├── openclaw.plugin.json        # OpenClaw plugin manifest
├── index.ts                    # Plugin entry point (MCP bridge + waker)
├── package.json                # NPM package for distribution
├── skills/
│   └── ad4m/
│       ├── SKILL.md            # Agent instructions
│       └── references/         # Detailed reference docs
│           ├── mcp.md
│           ├── architecture.md
│           ├── setup.md
│           └── waker.md
└── README.md
```
