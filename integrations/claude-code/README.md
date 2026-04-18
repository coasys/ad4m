# AD4M × Claude Code Integration

Use AD4M as the **persistent semantic memory layer** for Claude Code agents. Instead of flat markdown files, your agent reads and writes `LinkExpressions` to AD4M `Perspectives` — cryptographically signed, author-attributed, and queryable across sessions.

## What this gives you

- `ad4m_write_memory` — store facts, rules, and project state as signed semantic links
- `ad4m_recall` — query memories by source, predicate, or target URI
- `ad4m_delete_memory` — remove outdated entries
- `ad4m_list_perspectives` / `ad4m_create_perspective` — manage named semantic graphs
- `ad4m_agent_status` — check executor state and lock status
- Auto-unlock on startup — no manual passphrase entry after reboot

## Requirements

- macOS (tested on 13.x ARM64 / M1)
- AD4M executor v0.12.0-rc2+ running locally
- Node.js 18+
- Claude Code CLI

## One-command setup

```bash
cd integrations/claude-code
./setup-claude-code.sh --passphrase YOUR_AD4M_PASSPHRASE
```

This will:
1. Build and install the MCP server
2. Store your passphrase securely in macOS Keychain
3. Install a launchd job that auto-unlocks the agent on every boot
4. Create a `ClaudeMemory` Perspective and save its UUID to `~/.ad4m/claude-memory-uuid`
5. Print the `settings.json` snippet to add to Claude Code

## Manual Claude Code settings

Add to `~/.claude/settings.json` under `mcpServers`:

```json
"ad4m": {
  "command": "node",
  "args": ["/path/to/integrations/claude-code/mcp-server/dist/index.js"],
  "env": { "AD4M_GQL_URL": "http://localhost:4000/graphql" }
}
```

## Memory URI conventions

```
source:    memory://{type}/{slug}         e.g. memory://feedback/mobile-first
predicate: ad4m://has-content             store full content
           ad4m://has-name                store display name
           ad4m://relates                 generic relationship
target:    literal://your content here    plain text content
```

## Executor not starting on port 4000?

The Launcher app defaults to port 12000. If you run the executor directly, pass `--gql-port 4000` or set `AD4M_GQL_URL` to match your port.

## Known issue: Launcher crashes on external executor connection

If you run the executor independently (not via the Launcher), the ADAM Launcher app will crash when you try to connect. This is tracked in [issue #798](https://github.com/coasys/ad4m/issues/798). The MCP server works independently of the Launcher.
