# AD4M Executor Setup for AI Agents

## Getting the Executor

### Option 1: Download from GitHub Releases (Recommended)

Download pre-built binaries from [GitHub Releases](https://github.com/coasys/ad4m/releases):

```bash
# First, check the latest release version:
LATEST=$(curl -s https://api.github.com/repos/coasys/ad4m/releases/latest | grep '"tag_name"' | sed 's/.*"tag_name": "//;s/".*//')
VERSION=${LATEST#v}  # strip leading 'v'

# Linux x64
curl -L -o ad4m-executor "https://github.com/coasys/ad4m/releases/download/${LATEST}/ad4m-cli-executor-linux-${VERSION}-x64"
curl -L -o ad4m "https://github.com/coasys/ad4m/releases/download/${LATEST}/ad4m-cli-client-linux-${VERSION}-x64"
chmod +x ad4m-executor ad4m
sudo mv ad4m-executor ad4m /usr/local/bin/
```

> **Always use the latest release.** Check the [releases page](https://github.com/coasys/ad4m/releases) for the most recent version. Pre-release versions (e.g., `-rc1`) may also be available.

Available assets per release:
- `ad4m-cli-executor-linux-*-x64` — Executor binary (Linux)
- `ad4m-cli-client-linux-*-x64` — CLI client (Linux)
- `ADAM.Launcher_*_amd64.AppImage` — Desktop launcher (Linux)
- `ADAM.Launcher_*_amd64.deb` — Desktop launcher (Debian/Ubuntu)
- `ADAM_Launcher_*_aarch64.dmg` — Desktop launcher (macOS)

Check the [releases page](https://github.com/coasys/ad4m/releases) for the latest version.

### Option 2: Build from Source

Requires: Rust 1.92+, Deno, Go, `holochain_cli`

```bash
git clone https://github.com/coasys/ad4m.git
cd ad4m
git checkout dev  # or feature branch
cargo build --release
```

Produces two binaries in `target/release/`:
- `ad4m-executor` — the executor (server) and CLI combined
- `ad4m` — the CLI client

## Setup Sequence

**Critical**: Run `init` before first `run`. The executor panics without the bootstrap seed file.

### Step 1: Initialize

```bash
ad4m-executor init --data-path /path/to/.ad4m
```

Creates:
- `mainnet_seed.seed` — bootstrap configuration (languages, network settings)
- `last-seen-version` — version tracking

### Step 2: Run Executor

```bash
ad4m-executor run \
  --app-data-path /path/to/.ad4m \
  --port 12000 \
  --admin-credential <your-secret> \
  --enable-mcp true
```

**Key flags:**
| Flag | Default | Description |
|------|---------|-------------|
| `--app-data-path` | (required) | Data directory |
| `--port` | 12000 | API port (WebSocket RPC + HTTP) |
| `--admin-credential` | (none) | Admin auth token — without this, empty token has admin access |
| `--enable-mcp` | false | Enable MCP server |
| `--mcp-port` | 3001 | MCP server port |
| `--hc-admin-port` | 2000 | Holochain admin port |
| `--hc-app-port` | 1337 | Holochain app interface port |

**For AI agents**: Always run in a screen session with logging:

```bash
screen -dmS ad4m-executor bash -c 'ad4m-executor run --app-data-path ~/.ad4m --port 12000 --admin-credential mysecret --enable-mcp true 2>&1 | tee /tmp/ad4m-executor.log'
```

After startup, **write down** the admin credential, screen session name (`ad4m-executor`), log path (`/tmp/ad4m-executor.log`), MCP endpoint, and data path so you and your human can debug later. The executor is now running in the background — don't start another one.

### Step 3: Generate Agent

First run only. Creates cryptographic keys and DID identity.

**Via CLI:**

```bash
ad4m --executor-url http://localhost:12000 agent generate --passphrase <passphrase>
```

**Via REST API:**

```bash
curl -s http://localhost:12000/api/v1/agent/generate \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer <admin-credential>" \
  -d '{"passphrase": "<passphrase>"}'
```

This triggers Holochain conductor startup and language installation. Takes 30-60 seconds.

### Step 4: Unlock Agent (subsequent starts)

After restarting the executor, unlock the agent:

```bash
curl -s http://localhost:12000/api/v1/agent/unlock \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer <admin-credential>" \
  -d '{"passphrase": "<passphrase>"}'
```

The `holochain: true` parameter starts the Holochain conductor during unlock.

### Step 5: Verify

```bash
# Check agent status
curl -s http://localhost:12000/api/v1/agent/status \
  -H "Authorization: Bearer <admin-credential>"

# Expected: {"isInitialized":true,"isUnlocked":true,"did":"did:key:z6Mk..."}
```

## Deployment Scenarios & Networking

### Scenario 1: Single-user, local (simplest)

Agent and executor on the same machine. No TLS needed.

```bash
ad4m-executor run --app-data-path ~/.ad4m --port 12000 \
  --admin-credential mysecret --enable-mcp true
# MCP at http://localhost:3001/mcp
# API at http://localhost:12000
```

### Scenario 2: Agent connects to remote executor

Agent on machine A, executor on machine B (LAN or internet). MCP works over plain HTTP for agent-to-agent connections. **Flux UI (browser) requires TLS for non-localhost connections.**

**Option A: SSH tunnel (no TLS needed, simplest for agents)**

```bash
# On agent machine — forward both API and MCP ports
ssh -L 12000:localhost:12000 -L 3001:localhost:3001 user@executor-host
# Now agent connects to localhost:12000 / localhost:3001 as if local
```

**Option B: Caddy reverse proxy (auto TLS, needed for Flux UI)**

```bash
# On executor machine — install Caddy, then:
caddy reverse-proxy --from ad4m.yourdomain.com --to localhost:12000
# Flux connects to https://ad4m.yourdomain.com
# Requires: domain name pointing to executor IP, ports 80/443 open
```

**Option C: Cloudflare Tunnel (no port forwarding, free TLS)**

```bash
# On executor machine
cloudflared tunnel --url http://localhost:12000
# Gives you a public https://xxx.trycloudflare.com URL
# Works for both Flux and agents
```

### Scenario 3: Multi-user (humans via Flux + agents via MCP)

Requires `--enable-multi-user true`. Each user authenticates separately.

**⚠️ Flux (browser) REQUIRES TLS for non-localhost.** Browsers block mixed content and WebSocket connections to insecure origins. You MUST use one of:

- Caddy/nginx reverse proxy with TLS cert
- Cloudflare Tunnel
- SSH tunnel (makes it appear as localhost on the client)
- Self-signed cert via `mkcert` (install CA on all client devices)

```bash
ad4m-executor run --app-data-path ~/.ad4m --port 12000 \
  --admin-credential mysecret --enable-mcp true \
  --enable-multi-user true
```

**Agent auth flow (MCP):**

1. `request_capability` → get `request_id` + `code`
2. Admin approves (or auto-approve with admin credential)
3. `generate_jwt` with `request_id` + `code` → get JWT token
4. All subsequent requests include the JWT

**Human auth flow (Flux):**

1. Open Flux UI → enter executor URL (must be HTTPS)
2. Email verification or admin approval
3. Flux stores JWT in browser

### Quick Decision Guide

| Who connects?   | Where?       | TLS needed?        | Recommended setup                    |
| --------------- | ------------ | ------------------ | ------------------------------------ |
| Just your agent | Same machine | No                 | Scenario 1 (local)                   |
| Just your agent | Remote       | No                 | SSH tunnel                           |
| Agent + Flux UI | Same machine | No                 | Scenario 1                           |
| Agent + Flux UI | Remote/LAN   | **Yes (for Flux)** | Caddy + domain, or Cloudflare Tunnel |
| Multiple users  | Remote       | **Yes**            | Caddy + domain + multi-user flag     |

## Directory Structure

After init + generate, `--app-data-path` contains:

```
.ad4m/
├── ad4m/
│   ├── h/                    # Holochain data
│   │   ├── c/                # Conductor (databases, lair keystore, wasm-cache)
│   │   └── d/                # DNA data
│   └── languages/            # Installed language bundles
├── ad4m_db.sqlite            # Agent database
├── mainnet_seed.seed         # Bootstrap configuration
├── surrealdb_perspectives/   # Per-perspective SurrealDB stores
└── schema.gql                # Legacy GraphQL schema (unused)
```

## Security Considerations

### Credential Handling

The plugin manages MCP authentication internally — credentials are not sent in wake messages. Wake messages only contain event metadata (perspective UUID, parent, event type, agent DID).

- The plugin's background service maintains an authenticated MCP session
- Wake messages are sent over HTTP to your local OpenClaw hooks endpoint (`localhost` by default)
- If running the waker on a remote machine, ensure the wake endpoint uses HTTPS

### Executor Security

- **Never expose the admin credential** in logs, chat messages, or shared config files
- The executor's API endpoint (`--port`, default 12000) should only be accessible to trusted agents
- Use TLS (`--tls-cert-file`, `--tls-key-file`) for any remote executor access

## WebSocket RPC API (Fallback)

**Use MCP tools first.** The WebSocket RPC API is for low-level operations not exposed via MCP (language management, direct queries, debugging).

Connect to `ws://localhost:12000/api/v1/ws` and send JSON-RPC messages:

```json
{"method": "agent.status", "params": {}, "id": "1"}

{"method": "perspectives.add_link", "params": {"uuid": "<perspective-uuid>", "link": {"source": "ad4m://self", "predicate": "has_name", "target": "literal://string:Data"}}, "id": "2"}
```

**Auth:** Send `{"method": "auth", "params": {"credential": "<admin-credential>"}}` (single-user) or `{"method": "auth", "params": {"jwt": "<token>"}}` (multi-user) as the first message.
**Endpoint:** `ws://localhost:12000/api/v1/ws` (port configurable via `--port`)

## Troubleshooting

| Symptom | Cause | Fix |
|---------|-------|-----|
| `App data path not set` panic | Missing `--app-data-path` | Always pass the flag |
| `mainnet_seed.seed` not found | Skipped `init` | Run `ad4m-executor init` first |
| `Failed to spawn Lair keystore` | Stale lair socket/pid | Delete `h/c/ks/pid_file` and `h/c/ks/socket` |
| Holochain conductor `IoError(internal)` | Corrupted conductor DB | Nuke `h/c/` directory, re-generate agent |
| Port already in use | Previous instance running | Kill old process, clean lair files |
| 404 on neighbourhood join | Version mismatch or expired link | Ensure same AD4M version as neighbourhood creator |
| Cannot connect to executor | Executor not running or wrong port | `curl http://localhost:12000/health` to verify |
| Waker not firing | WS not accessible or bad query | Check `ws://localhost:12000/api/v1/ws/events` and waker logs |
| Messages "uninitialized" | Property set after creation (race) | Always use `message_create` or `create_subject` with `initial_values` |
| Channel query returns empty | SHACL still syncing | Wait 3-5 min for Holochain gossip, then retry |
