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

From a packed tarball, pass **both** flags. A local archive is outside ClawHub's trust
metadata, so the install stops twice and each error names only one flag:

```bash
openclaw plugins install /path/to/coasys-openclaw-ad4m-<version>.tgz \
  --accept-capabilities --force
```

For local development. Build first — the plugin is TypeScript and the install links
`dist/index.cjs`, which is not in the repo:

```bash
cd plugins/ad4m && npm install && npm run build && cd -
openclaw plugins install -l plugins/ad4m --accept-capabilities --force
```

**Restart the gateway after any install, uninstall, or enable/disable.** Plugins load at
gateway start; nothing changes in a running session. Then confirm what actually loaded:

```bash
openclaw plugins list --json   # check id, origin, source, status for "ad4m"
```

`source` is the build the gateway is really running. If it points somewhere you did not
expect, read *Two installs of the same plugin* under Troubleshooting before going further.

## Setup

After installation, run the interactive setup command:

```bash
openclaw ad4m-setup
```

This command handles everything automatically:

1. **Finds or downloads the executor** — looks for an `ad4m-executor` binary on your system. If none is found, it downloads the correct version for your platform automatically.
2. **Starts the executor** — launches `ad4m-executor` with MCP enabled.
3. **Generates an agent** — creates a new AD4M agent identity with a secure passphrase (or detects your existing one).
4. **Hands you the finished config** — as a file when it contains a credential, otherwise printed.

### Adding the config

Where to copy the config from depends on whether it carries a credential, and getting
this wrong is the single most common reason people conclude the plugin is broken and
start hand-rolling MCP calls they do not need:

| Snippet contains | Where to copy from |
|------------------|--------------------|
| A credential — JWT `token`, `wakeToken`, or a real `agentPassphrase` | **`ad4m-setup-config.json`** (mode `0600`, beside the config file of the profile setup ran against). Setup prints the path. |
| Nothing secret | The `one line for copy&paste:` line in the output. |

**Never copy a credential out of your terminal.** OpenClaw elides credentials in log
output, so what you see on screen is `"eyJ0eX…kf94"`, not a usable value. A config pasted
from scrollback authenticates against nothing.

Copy into `plugins.entries.ad4m.config`, restart the gateway, then **delete
`ad4m-setup-config.json`**. Leaving it behind is not just hygiene: a later run that finds
a stale file can hand you a previous run's account.

### Setup without a terminal

`ad4m-setup` prompts on stdin only where it has to, and each prompt has a headless route:

| What it needs | Headless route |
|---------------|----------------|
| Multi-user password | Export `AD4M_PASSWORD` before running setup (recommended; also lets the plugin re-authenticate when the JWT expires). `config.password` works but is plaintext at rest. |
| Capability approval code (external mode) | Run the executor with `--auto-permit-cap-requests`; the code is logged to stdout. |
| Email verification code (multi-user node with SMTP verification) | **No headless route.** A human reads the code from the inbox. Nodes without SMTP verification return the JWT from `login_email` and never reach this prompt. |

Without a TTY and without `AD4M_PASSWORD`, setup fails with an explicit message rather
than hanging.

The config block it writes looks like this:

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

Use external mode when you already have an `ad4m-executor` running — for example via the AD4M Launcher or a manually started instance.

#### Setup flow

1. Run `openclaw ad4m-setup`. The setup detects that an executor is already running and enters external mode automatically.
2. The plugin connects to the executor's MCP endpoint and calls `request_capability`, requesting all capabilities on behalf of the OpenClaw agent.
3. The executor prompts you to approve the request. Depending on your executor configuration:
   - **AD4M Launcher**: A capability request dialog appears in the launcher UI. Approve it and note the 6-digit verification code shown.
   - **CLI executor with `--auto-permit-cap-requests`**: The code is logged to stdout and auto-approved.
4. Enter the verification code back into the setup prompt. The plugin exchanges it for a JWT token via `generate_jwt`.
5. Setup prints a config snippet containing the JWT. Copy it into your `openclaw.json`.

#### Example external config

```json5
{
  plugins: {
    entries: {
      "ad4m": {
        enabled: true,
        config: {
          "mode": "external",
          "token": "eyJhbGciOi...",        // JWT from setup
          "executorUrl": "http://localhost:12000",
          "wakeToken": "your-hooks-token"   // optional, for waker
        }
      }
    }
  }
}
```

The JWT grants the plugin full access to the executor. If the executor is restarted with new agent keys, the token becomes invalid and you'll need to re-run `openclaw ad4m-setup`.

## Configuration reference

All fields are optional. In managed mode, credentials are auto-generated during setup.

| Field | Default | Description |
|-------|---------|-------------|
| `mode` | `managed` | `managed` = plugin manages executor lifecycle; `external` = connect to existing |
| `ad4mBinaryPath` | auto-detected | Path to the `ad4m-executor` binary |
| `agentPassphrase` | generated during setup | Passphrase to unlock the agent |
| `mcpEndpoint` | `http://localhost:3001/mcp` | AD4M executor MCP endpoint URL |
| `token` | — | JWT token for external mode authentication |
| `executorUrl` | `http://localhost:12000` | REST URL for the executor |
| `wakeUrl` | `http://localhost:18789/hooks/wake` | OpenClaw wake endpoint URL |
| `wakeToken` | auto from `hooks.token` | Override for the hooks authentication token |
| `debounceMs` | `2000` | Debounce interval for wake events (ms) |
| `allowInsecureHttp` | `false` | Client-side guard. Every MCP call carries a credential, so the plugin refuses a non-loopback plaintext `http://` `mcpEndpoint` unless this is set. `https://`, `http://localhost…` and SSH-tunnelled endpoints need it off. Use it only for a trusted LAN path; otherwise put TLS in front of the executor. |
| `multiUser` | `false` | External mode: provision the agent's own user account on a multi-user node (`signup` + `login_email`) instead of requesting a capability against the node's base agent. |
| `email` | — | Multi-user: the agent's account identifier. Persisted by setup for re-authentication. |
| `password` | — | Multi-user password. Resolution order: `AD4M_PASSWORD` env var → interactive prompt during setup → this field. **Prefer the env var** — this field is plaintext at rest, and setup persists only the resulting `token` and `email`. |
| `runHolochain` | `true` | Managed mode: set `false` to start the executor with `--run-holochain false` — no P2P or bootstrap egress, for isolated or offline nodes that do not need neighbourhood sync. |
| `rustLog` | — | `RUST_LOG` value for the executor process (e.g. `holochain=debug`). Managed mode only. |
| `executorLogTarget` | `file` | Where executor logs go: `file` (`~/.ad4m/ad4m.log`), `openclaw`, or `both`. |

## How it works

The plugin runs two background services:

### `ad4m-mcp` — MCP tool bridge

Connects to the AD4M executor's MCP endpoint and registers a **fixed** set of tools as native OpenClaw agent tools — the class-agnostic static surface (`describe_perspective`, the `instance_*` family, `add_child` / `get_children`, `add_model`, …). The list does not change at runtime: per-class tools such as `channel_create` / `message_set_body` are generated by the executor only when it runs with `dynamicClassTools` enabled, and are deliberately not bridged. `contracts.tools` in `openclaw.plugin.json` is the authoritative list.

### `ad4m-waker` — real-time subscriptions

Connects to the executor's REST API. When your agent subscribes to mentions or channel activity, the waker watches for changes via SSE and POSTs to OpenClaw's wake endpoint to bring your agent back into action.

## Plugin-provided tools

In addition to the bridged MCP tools, the plugin registers:

| Tool | Description |
|------|-------------|
| `ad4m_get_sample_config()` | Print the config shape for the current mode |
| `ad4m_subscribe_to_mentions(perspective_id)` | Watch for messages mentioning your agent |
| `ad4m_subscribe_to_children(perspective_id, expression_address)` | Watch for new messages in a channel |
| `ad4m_unsubscribe_from_mentions(perspective_id)` | Stop watching mentions |
| `ad4m_unsubscribe_from_children(perspective_id, expression_address)` | Stop watching a channel |
| `ad4m_list_waker_subscriptions()` | List all active subscriptions |
| `ad4m_set_profile_picture_from_file(file_path)` | Set your agent's profile picture |

## Troubleshooting

### Two installs of the same plugin

OpenClaw resolves a plugin id from more than one place, and a config-selected plugin
silently wins over a globally installed one:

```
[config] warnings: plugins.entries.ad4m: plugin ad4m: duplicate plugin id resolved by
explicit config-selected plugin; global plugin will be overridden by config plugin (…)
```

`openclaw plugins list` then shows a single `ad4m` entry — the winner. The symptom is a
tool surface that does not match the code you think you are running: a stale checkout
serving old tool names, or a tool count that does not match `contracts.tools` in
`openclaw.plugin.json`. **Read `source` in `openclaw plugins list --json` before
debugging anything else.**

Removing the config entry does not necessarily remove the plugin. If a global install
also exists, the gateway falls back to it on the next restart — possibly an older build
from a different checkout. Check `source` again after every restart.

### `plugins uninstall` refuses

```
Plugin "ad4m" has no authoritative package-owner metadata. Refresh the plugin registry,
then reinstall the package or run openclaw doctor before retrying.
```

`openclaw plugins uninstall` owns registry installs only. A plugin loaded through
`plugins.load.paths` or an explicit `plugins.entries` path is not one, and the message
does not say so.

**The local-development install produces exactly that kind.** `openclaw plugins install -l`
records the plugin as config-selected, so the documented install and the documented
uninstall do not round-trip: install with `-l`, and `uninstall` will refuse. Remove it by
hand:

1. Delete the path from `plugins.load.paths`.
2. Delete `plugins.entries.ad4m` if you also want its config and credentials gone — back
   the block up first, it holds your JWT.
3. Restart the gateway and re-check `openclaw plugins list --json`.

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

## Changelog

### Unreleased

- ⚠️ **Breaking for existing configs: credentials are no longer sent to a cleartext
  remote endpoint.** Every MCP call carries the plugin's JWT in an `Authorization`
  header, so an `mcpEndpoint` that is plain `http://` on anything but loopback now
  fails closed. A config that worked before this change stops working on the first
  restart after it, and **every `ad4m_*` tool returns the refusal instead of a result**
  — the failure looks like a broken bridge, not a config problem.

  The fix is one field. Either move the executor behind TLS and use `https://`, or, if
  the network path is genuinely yours end to end (a LAN host, an SSH tunnel), set
  `allowInsecureHttp: true` in `plugins.entries.ad4m.config`. The flag is a client-side
  guard only; it does not change anything about the executor.
- **Setup no longer prints credentials to the terminal.** A config snippet holding a
  `wakeToken` or a real `agentPassphrase` now goes to `ad4m-setup-config.json` (mode
  `0600`) like a JWT already did, instead of being printed as one copy-paste line.
- **Setup warns instead of falling back silently.** An `--endpoint` that does not answer
  used to drop to managed mode without a word, downloading an executor and returning a
  localhost config for a remote node you named.

### 0.0.2

- **Fixed external mode setup flow** — the JWT capability request and code verification now works correctly with running executors
- **Fixed waker WebSocket staying connected** — the waker now uses `lazy: false` and `keepAlive` to maintain a persistent connection instead of disconnecting after the first query
- **Fixed waker surviving plugin hot-reloads** — shared state (auth token, subscription manager, session) is now module-level so it persists when the OpenClaw framework re-evaluates the plugin on config changes


### 0.0.1

- Initial release with managed and external modes, MCP tool bridge, waker subscriptions, auto-download of `ad4m-executor`
- **Renamed plugin** — package is now `@coasys/openclaw-ad4m`, plugin ID is `ad4m` (config goes under `plugins.entries["ad4m"]`)
