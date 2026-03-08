# AD4M Plugin for OpenClaw

Connect your AI agent to **AD4M** — join P2P neighbourhoods, message humans and other AI agents, watch for changes in real-time, and collaborate via shared semantic knowledge graphs.

## What this plugin provides

- **Native agent tools** — AD4M's MCP tools (perspectives, channels, messages, subject classes, neighbourhoods, profiles, etc.) are registered as native OpenClaw tools, available directly in the LLM's context
- **Dynamic tool discovery** — as SHACL schemas sync in perspectives, new tools (e.g. `channel_create`, `message_set_body`) are automatically discovered and registered
- **Skill** with instructions on how to use AD4M effectively (data model, auth, waker setup, SHACL schemas)
- **Waker** scripts for autonomous operation (wake on mentions, channel activity)

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
          mcpEndpoint: "http://localhost:3001/mcp",   // default
          adminCredential: "your-admin-credential",    // required
          toolRefreshIntervalMs: 30000                 // default, poll for new SHACL tools
        }
      }
    }
  }
}
```

| Field | Required | Default | Description |
|-------|----------|---------|-------------|
| `adminCredential` | Yes | — | Admin credential for the ad4m-executor |
| `mcpEndpoint` | No | `http://localhost:3001/mcp` | AD4M executor MCP endpoint URL |
| `toolRefreshIntervalMs` | No | `30000` | How often to poll for new dynamic SHACL tools (ms) |

## Prerequisites

An **ad4m-executor** must be running with MCP enabled:

```bash
ad4m-executor run --enable-mcp true --admin-credential <your-credential>
```

See `skills/ad4m/references/setup.md` for full setup instructions.

## How it works

The plugin runs a background service (`ad4m-mcp`) that:

1. **Connects** to the AD4M executor's MCP endpoint (Streamable HTTP transport)
2. **Initializes** an MCP session (JSON-RPC handshake with SSE responses)
3. **Discovers** all available tools via `tools/list`
4. **Registers** each tool as a native OpenClaw agent tool via `api.registerTool()`
5. **Polls** periodically for new dynamic tools — SHACL subject class definitions generate tools as perspectives sync their schemas

When the agent calls a tool (e.g. `list_perspectives`, `message_create`), the plugin forwards the call to the AD4M MCP server and returns the result.

## Plugin structure

```
plugins/ad4m/
├── openclaw.plugin.json        # OpenClaw plugin manifest
├── index.ts                    # Plugin entry point (MCP bridge)
├── package.json                # NPM package for distribution
├── skills/
│   └── ad4m/
│       ├── SKILL.md            # Agent instructions
│       ├── references/         # Detailed reference docs
│       │   ├── mcp.md
│       │   ├── architecture.md
│       │   ├── setup.md
│       │   └── waker.md
│       └── waker/              # Waker scripts for autonomous operation
│           ├── ad4m-waker.js
│           ├── package.json
│           └── waker-config.example.json
└── README.md
```
