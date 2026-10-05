# Running an AD4M Executor — Node Operator Guide

> **Audience.** This file is for the person who stands up and maintains the executor process — downloading, initializing, running, unlocking, and verifying. If you were handed a running executor's address and just need to authenticate against it, see `references/setup.md` instead.

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
# Keep the secret in a mode-600 file and export it. Never pass it as a flag:
# `--admin-credential <value>` is visible to every user on the host via `ps`
# and stays in the shell history.
export AD4M_ADMIN_CREDENTIAL="$(cat /path/to/.ad4m/admin-credential)"
ad4m-executor run \
  --app-data-path /path/to/.ad4m \
  --port 12000 \
  --enable-mcp true
```

**Key flags:**
| Flag | Default | Description |
|------|---------|-------------|
| `--app-data-path` | (required) | Data directory |
| `--port` | 12000 | API port (WebSocket RPC + HTTP) |
| `AD4M_ADMIN_CREDENTIAL` (env) / `--admin-credential` | (none) | Admin auth token. Set it through the environment variable; the flag form leaks the secret into `ps` output and shell history. Without it, an empty token has admin access |
| `--enable-mcp` | false | Enable MCP server |
| `--mcp-port` | 3001 | MCP server port |
| `--hc-admin-port` | 2000 | Holochain admin port |
| `--hc-app-port` | 1337 | Holochain app interface port |

**For AI agents**: Always run in a screen session with logging:

```bash
export AD4M_ADMIN_CREDENTIAL="$(cat ~/.ad4m/admin-credential)"   # screen inherits the exported variable
screen -dmS ad4m-executor bash -c 'ad4m-executor run --app-data-path ~/.ad4m --port 12000 --enable-mcp true 2>&1 | tee /tmp/ad4m-executor.log'
```

After startup, **write down** where the admin credential lives (the file path — never the value itself), the screen session name (`ad4m-executor`), log path (`/tmp/ad4m-executor.log`), MCP endpoint, and data path so you and your human can debug later. The executor is now running in the background — don't start another one.

### Step 3: Generate Agent

First run only. Creates cryptographic keys and DID identity.

```bash
ad4m --executor-url http://localhost:12000 agent generate --passphrase <passphrase>
```

**There is no REST endpoint for this.** The executor's only HTTP routes are
`/`, `/health`, `/internal/shutdown` and the binary audio feed — everything
else, `agent.generate` included, is a WebSocket-RPC method (`api/mod.rs`).
The CLI above is the WS client; see `references/setup.md` → "WebSocket RPC API (Fallback)" to call it
directly.

This triggers Holochain conductor startup and language installation. Takes 30-60 seconds.

### Step 4: Unlock Agent (subsequent starts)

After restarting the executor, unlock the agent:

```bash
ad4m --executor-url http://localhost:12000 agent unlock --passphrase <passphrase> --holochain true
```

`--holochain true` starts the Holochain conductor during unlock. Same caveat
as Step 3: this is WS-RPC (`agent.unlock`), not a REST endpoint.

**Skipping this step is expected to break every other auth path — by design, not by bug.** An executor is only usable once its main operator has unlocked it. The wallet keeps signing keys in memory only; immediately after a restart, before `unlock` runs, the executor holds just the encrypted cipher — it can check that a password/credential is *structurally* valid but cannot actually sign anything, so it fails at key lookup instead. **What's actually wrong here is the error message, not the lockout itself:** a third party trying to authenticate against a not-yet-unlocked node should fail immediately with a clear "this executor hasn't been unlocked yet" message, not a confusing one that reads like a bad credential:

- `login_email` (multi-user) → `User key not found on executor` — reads like a wrong password. It's actually "nobody has unlocked this node yet."
- `request_capability` → `generate_jwt` (capability flow) → `main key not found` — the capability bootstrap is equally blocked until unlock.

If you're the executor's operator and don't have CLI access handy, the same unlock is available over the WebSocket RPC API (`references/setup.md` → "WebSocket RPC API (Fallback)"): `agent.unlock` with the agent's passphrase. If you're a third party hitting either error, this isn't something to retry your way around — someone with operator access needs to unlock the node first.

**Test-only mode, not a security bug:** on a node with no admin credential configured (neither `AD4M_ADMIN_CREDENTIAL` nor `--admin-credential`), an empty token resolves to full (`ALL_CAPABILITY`) access on the WS-RPC API, including `agent.unlock` — found live 2026-09-06 recovering a test executor. This is intentional, for local/test convenience, not a gap to fix. **Never run a node without an admin credential set except on loopback/local test setups** — on anything reachable by another user or over a network, this means anyone can unlock and fully control the node.

### Step 5: Verify

```bash
# Is the executor up at all? (this one really is an HTTP route)
curl -s http://localhost:12000/health          # {"status":"ok"}

# Agent status — WS-RPC `agent.status`, via the CLI
ad4m --executor-url http://localhost:12000 agent status

# Expected: initialized + unlocked, with the agent's did:key:z6Mk… DID
```

## Operator Networking for Remote Executors

The executor itself only speaks plain HTTP / WebSocket on its ports, and every request carries a secret — the admin credential, a JWT, or a password on login. **Anything that leaves the machine therefore goes through an SSH tunnel or a TLS proxy. Never point an agent at `http://<remote-host>:3001/mcp` or `http://<remote-host>:12000` across a network.** Flux UI (browser) additionally needs a real TLS certificate for non-localhost connections, because browsers block mixed content.

**Option A: SSH tunnel (encrypted by SSH, no certificate needed — simplest for agents)**

```bash
# On agent machine — forward both API and MCP ports
ssh -L 12000:localhost:12000 -L 3001:localhost:3001 user@executor-host
# Now agent connects to localhost:12000 / localhost:3001 as if local
```

**Option B: Caddy reverse proxy (auto TLS, needed for Flux UI)**

```bash
# On executor machine — install Caddy, then one TLS front for BOTH ports
# (the `caddy reverse-proxy` one-liner only fronts a single upstream):
cat > Caddyfile <<'EOF'
ad4m.yourdomain.com {
    reverse_proxy localhost:12000
}
mcp.yourdomain.com {
    reverse_proxy localhost:3001
}
EOF
caddy run --config Caddyfile
# Flux connects to https://ad4m.yourdomain.com
# MCP clients connect to https://mcp.yourdomain.com/mcp
# Requires: both names pointing to the executor IP, ports 80/443 open
```

**Option C: Cloudflare Tunnel (no port forwarding, free TLS)**

```bash
# On executor machine. A quick tunnel exposes ONE local port, so run one per port:
cloudflared tunnel --url http://localhost:12000   # API + Flux → https://xxx.trycloudflare.com
cloudflared tunnel --url http://localhost:3001    # MCP        → https://yyy.trycloudflare.com/mcp
# Each process prints its own https://….trycloudflare.com URL: point Flux at the
# first and MCP clients at the second (plus /mcp). Exposing only port 12000 gives
# you the API but no MCP endpoint. For a single hostname that routes both, use a
# named tunnel with an ingress config (host- or path-based rules) instead.
```

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
└── perspectives/             # Per-perspective SPARQL (Oxigraph) stores
```
