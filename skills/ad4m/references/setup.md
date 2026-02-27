# AD4M Executor Setup for AI Agents

## Getting the Executor

### Option 1: Download from GitHub Releases (Recommended)

Download pre-built binaries from [GitHub Releases](https://github.com/coasys/ad4m/releases):

```bash
# Linux x64
curl -L -o ad4m-executor https://github.com/coasys/ad4m/releases/download/v0.12.0-rc1/ad4m-cli-executor-linux-0.12.0-rc1-x64
curl -L -o ad4m https://github.com/coasys/ad4m/releases/download/v0.12.0-rc1/ad4m-cli-client-linux-0.12.0-rc1-x64
chmod +x ad4m-executor ad4m
sudo mv ad4m-executor ad4m /usr/local/bin/
```

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
  --gql-port 12100 \
  --admin-credential <your-secret> \
  --enable-mcp true
```

**Key flags:**
| Flag | Default | Description |
|------|---------|-------------|
| `--app-data-path` | (required) | Data directory |
| `--gql-port` | 12000 | GraphQL API port |
| `--admin-credential` | (none) | Admin auth token — without this, empty token has admin access |
| `--enable-mcp` | false | Enable MCP server |
| `--mcp-port` | 3001 | MCP server port |
| `--hc-admin-port` | 2000 | Holochain admin port |
| `--hc-app-port` | 1337 | Holochain app interface port |

**For AI agents**: Run in a screen/tmux session for persistence:
```bash
screen -dmS ad4m-run bash -c 'ad4m-executor run --app-data-path ~/.ad4m --gql-port 12100 --admin-credential mysecret --enable-mcp true 2>&1 | tee /tmp/ad4m-run.log'
```

### Step 3: Generate Agent

First run only. Creates cryptographic keys and DID identity.

**Via CLI:**
```bash
ad4m --executor-url http://localhost:12100/graphql agent generate --passphrase <passphrase>
```

**Via GraphQL:**
```bash
curl -s http://localhost:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { agentGenerate(passphrase: \"<passphrase>\") { did } }"}'
```

This triggers Holochain conductor startup and language installation. Takes 30-60 seconds.

### Step 4: Unlock Agent (subsequent starts)

After restarting the executor, unlock the agent:

```bash
curl -s http://localhost:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { agentUnlock(passphrase: \"<passphrase>\", holochain: true) { isInitialized isUnlocked did } }"}'
```

The `holochain: true` parameter starts the Holochain conductor during unlock.

### Step 5: Verify

```bash
# Check agent status
curl -s http://localhost:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"{ agentStatus { isInitialized isUnlocked did } }"}'

# Expected: {"data":{"agentStatus":{"isInitialized":true,"isUnlocked":true,"did":"did:key:z6Mk..."}}}
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
├── surrealdb_perspectives/   # Per-perspective SurrealDB stores
└── schema.gql                # GraphQL schema
```

## Troubleshooting

| Symptom | Cause | Fix |
|---------|-------|-----|
| `App data path not set` panic | Missing `--app-data-path` | Always pass the flag |
| `mainnet_seed.seed` not found | Skipped `init` | Run `ad4m-executor init` first |
| `Failed to spawn Lair keystore` | Stale lair socket/pid | Delete `h/c/ks/pid_file` and `h/c/ks/socket` |
| Holochain conductor `IoError(internal)` | Corrupted conductor DB | Nuke `h/c/` directory, re-generate agent |
| Port already in use | Previous instance running | Kill old process, clean lair files |
| 404 on neighbourhood join | Version mismatch or expired link | Ensure same AD4M version as neighbourhood creator |
