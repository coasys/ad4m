---
name: ad4m-executor
description: Build, configure, and run the AD4M executor from source. Use when setting up a development environment, troubleshooting build failures, configuring bootstrap seeds, enabling MCP or TLS, or debugging language loading issues.
---

# AD4M Executor

## Prerequisites

### Required toolchain
- **Rust 1.84.0+** with `wasm32-unknown-unknown` target
- **Go 1.22.0+**
- **Node.js 20+** with **pnpm**
- **protobuf compiler** (`protoc`)
- **cmake**

### Platform packages

**macOS:**
```bash
brew install protobuf cmake
```

**Ubuntu/Debian:**
```bash
sudo apt-get install -y libgtk-3-dev webkit2gtk-4.0 libappindicator3-dev \
  librsvg2-dev patchelf protobuf-compiler cmake fuse libfuse2 \
  mesa-utils mesa-vulkan-drivers libsoup-3.0-dev \
  javascriptcoregtk-4.1-dev webkit2gtk-4.1-dev librust-alsa-sys-dev
```

## Build from Source

The executor requires a **two-step build** in strict order.

```bash
git clone https://github.com/coasys/ad4m.git && cd ad4m
pnpm install

# Step 1: Build Deno snapshot + rust-executor library
cd rust-executor && pnpm build
# This runs: cargo run --release --features generate_snapshot --bin generate_snapshot
# Then:      cargo build --release
# First run: ~20-40 min. Subsequent: ~2 min.

# Step 2: Build CLI (produces the actual ad4m-executor and ad4m binaries)
cd ../cli && pnpm build
# This runs: cargo build --release
```

### ⚠️ Binary collision

Both crates produce a binary named `ad4m-executor` in `target/release/`. The CLI crate's version has all runtime flags (MCP, TLS, multi-user). **Always build CLI last**, or target it explicitly:

```bash
cargo build --release -p ad4m
```

### Rebuild after changes

Only need to repeat the steps that changed:
- **Deno/JS changes** → `cd rust-executor && pnpm build` then `cd ../cli && pnpm build`
- **Rust executor changes** → `cd rust-executor && pnpm build` then `cd ../cli && pnpm build`
- **CLI-only changes** → `cd cli && pnpm build`

## Bootstrap Seed

The bootstrap seed determines how languages are distributed. **This is the most common source of failures.**

| Seed | Location | Transport | Use for |
|------|----------|-----------|---------|
| **Mainnet** | `cli/mainnet_seed.json` | Cloudflare proxy (`bootstrap-store-gateway.perspect3vism.workers.dev`) | Development, standalone operation |
| **Test** | `tests/js/bootstrapSeed.json` | Local filesystem (`./tst-tmp/languages/`) | Test pipeline only (requires `prepare-test`) |

**Always use the mainnet seed for development:**
```bash
ad4m-executor init --data-path /tmp/ad4m-data \
  --network-bootstrap-seed ./cli/mainnet_seed.json
```

Without `--network-bootstrap-seed`, the executor looks for `mainnet_seed.seed` in the data path — a file that doesn't exist after init. This fails silently: the executor starts, agent generates, but all languages fail to resolve.

### Diagnosing language failures

If logs show:
```
Did not find language source for given address: QmzSYwd...
Did not find meta file for given address: QmzSYwd...
```
→ Wrong bootstrap seed. Switch to `cli/mainnet_seed.json`.

## Run the Executor

### Minimal (development)
```bash
ad4m-executor init --data-path /tmp/ad4m-data \
  --network-bootstrap-seed ./cli/mainnet_seed.json

ad4m-executor run --app-data-path /tmp/ad4m-data
```

### Full flags
```bash
ad4m-executor run \
  --app-data-path /tmp/ad4m-data \
  --gql-port 12100 \
  --hc-admin-port 2100 \
  --hc-app-port 1400 \
  --hc-use-bootstrap true \
  --hc-use-mdns true \
  --admin-credential <password> \
  --language-language-only false \
  --run-dapp-server false
```

### Port table

| Port | Service | Default | Required |
|------|---------|---------|----------|
| 12100 | GraphQL HTTP | Yes | Yes |
| 2100 | Holochain admin | Yes | Yes |
| 1400 | Holochain app | Yes | Yes |
| 3001 | MCP server | No | Only with `--enable-mcp` |
| 12001 | HTTPS/TLS | No | Only with `--tls-cert-file` |

## Agent Initialisation

After the executor starts, generate an agent via GraphQL:

```bash
curl -s http://127.0.0.1:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { agentGenerate(passphrase: \"<passphrase>\") { isInitialized did } }"}'
```

**Wait for init to complete.** After `agentGenerate`, the executor takes 30-60 seconds to download and load bootstrap languages. Watch logs for `"AD4M init complete"` before using perspectives or neighbourhoods. There is no GraphQL readiness endpoint.

## Enable MCP Server

```bash
ad4m-executor run \
  --app-data-path /tmp/ad4m-data \
  --admin-credential <password> \
  --enable-mcp true \
  --mcp-port 3001
```

**Transport:** Streamable HTTP at `POST /mcp` (not SSE, not stdio). Uses `rmcp` crate.

### MCP authentication flow

MCP requires a session handshake before tool calls work:

```bash
# 1. Initialize — capture Mcp-Session-Id from response header
curl -si POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"my-app","version":"1.0"}}}'

# 2. Send initialized notification (REQUIRED — without this, tool calls return empty)
curl -s POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","method":"notifications/initialized"}'

# 3. Request capability
curl -s POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"request_capability","arguments":{"app_name":"my-app","app_desc":"Description","app_url":"http://localhost"}}}'
# → Returns request_id and code

# 4. Generate JWT
curl -s POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"generate_jwt","arguments":{"request_id":"<id>","code":"<code>"}}}'
# → Authenticated. All subsequent calls in this session use the token.
```

### MCP tools (37 available)

Perspectives: `add_perspective`, `list_perspectives`, `query_links`, `add_link`, `query_subjects`, `create_subject`, `get_subject_data`, `set_subject_property`, `delete_subject`, `get_subject_children`, `get_subject_collection`, `add_to_collection`, `remove_from_collection`

Neighbourhoods: `neighbourhood_join_from_url`, `neighbourhood_publish_from_perspective`

Agent: `get_agent_profile`, `set_agent_profile`, `set_agent_profile_picture`, `get_agent_public_perspective`, `set_agent_public_perspective`

Auth: `request_capability`, `generate_jwt`, `auth_status`, `signup`, `login_email`, `request_login_verification`, `verify_email_code`

AI/Flows: `get_models`, `add_model`, `infer`, `get_flows`, `add_flow`, `flow_start`, `flow_state`, `flow_actions`, `flow_run_action`

Utility: `execute_commands`, `generate_waker_query`

## Enable TLS (Remote Access)

```bash
# Generate self-signed cert (dev only)
openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 365 -nodes \
  -subj "/CN=<your-ip>" -addext "subjectAltName=IP:<your-ip>,IP:127.0.0.1"

ad4m-executor run \
  --app-data-path /tmp/ad4m-data \
  --admin-credential <password> \
  --tls-cert-file cert.pem \
  --tls-key-file key.pem \
  --tls-port 12001
```

**Dual-server mode:** With TLS, the executor runs two servers:
- HTTP on `127.0.0.1:12100` (localhost only)
- HTTPS on `0.0.0.0:12001` (all interfaces, for remote clients)

**Self-signed cert gotcha:** Browsers must visit `https://<ip>:12001` directly and accept the cert warning before WebSocket connections (e.g., from Flux) will work.

## Project Structure

```
ad4m/
├── cli/                    # CLI crate → ad4m + ad4m-executor binaries
│   └── mainnet_seed.json   # Production bootstrap seed
├── rust-executor/          # Core executor library + Deno snapshot
├── core/                   # TypeScript types + Ad4mClient (@coasys/ad4m)
├── connect/                # Connection library (@coasys/ad4m-connect)
├── ui/                     # AD4M Launcher / Flux desktop (Tauri)
├── dapp/                   # Dapp web interface (@coasys/dapp)
├── bootstrap-languages/    # Core Holochain languages
└── tests/js/               # Integration tests (uses test-only bootstrap seed)
```

## Common Issues

| Symptom | Cause | Fix |
|---------|-------|-----|
| Languages fail to resolve | Test bootstrap seed | Use `cli/mainnet_seed.json` |
| `ad4m-executor` missing MCP flags | Built rust-executor last (wrong binary) | Rebuild with `cd cli && pnpm build` |
| Empty MCP responses | Missing `notifications/initialized` | Send notification after initialize |
| Port bind error | Previous executor still running | `lsof -ti:12100 \| xargs kill -9` |
| Agent generates but nothing works | Init still in progress | Wait for "AD4M init complete" in logs |
| `mainnet_seed.seed` not found | No seed provided to init | Use `--network-bootstrap-seed cli/mainnet_seed.json` |
