---
name: ad4m-executor
description: Build, configure, and run the AD4M executor from source. Use when setting up a development environment, troubleshooting build failures, configuring bootstrap seeds, enabling MCP or TLS, multi-user mode, Flux connections, GraphQL subscriptions, or debugging language loading issues.
---

# AD4M Executor

## Prerequisites

### Required toolchain
- **Rust 1.92+** with `wasm32-unknown-unknown` target (see `rust-toolchain.toml` and `.circleci/Dockerfile` for normative versions)
- **Go 1.22.0+**
- **Node.js 18+** with **pnpm**
- **protobuf compiler** (`protoc`)
- **cmake**

### Platform packages

**macOS:**
```bash
brew install protobuf cmake
```

**Ubuntu/Debian:**
```bash
sudo apt-get install -y libgtk-3-dev webkit2gtk-4.0 libayatana-appindicator3-dev \
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

### ⚠️ Binary note

Only the CLI crate produces binaries (`ad4m-executor` and `ad4m` in `target/release/`). The `rust-executor` crate is a library — it doesn't create standalone binaries, but its build step generates the Deno snapshot required by the CLI. **Always build `rust-executor` before `cli`.**

### ⚠️ `--data-path` vs `--app-data-path`

Different flag names for init vs run:
- `ad4m-executor init --data-path /tmp/ad4m-data`
- `ad4m-executor run --app-data-path /tmp/ad4m-data`

Using the wrong flag silently creates a default data directory instead of the one you intended.

### Rebuild after changes

Only need to repeat the steps that changed:
- **JS changes in `executor/`** → `cd executor && pnpm build` then `cd ../rust-executor && pnpm build` then `cd ../cli && pnpm build`
- **Deno/snapshot changes in `rust-executor/`** → `cd rust-executor && pnpm build` then `cd ../cli && pnpm build`
- **Rust executor changes** → `cd rust-executor && pnpm build` then `cd ../cli && pnpm build`
- **CLI-only changes** → `cd cli && pnpm build`

### Branch awareness

When switching branches to test different features, **always rebuild both crates** — the running binary is from `target/release/ad4m-executor` which is shared across branches. If you checkout `feature/mcp-server` but the binary was last built from `docs/some-branch`, you'll get the wrong binary.

## Bootstrap Seed

The bootstrap seed determines how languages are distributed. **This is the most common source of failures.**

| Seed | Location | Transport | Use for |
|------|----------|-----------|---------|
| **Mainnet** | `cli/mainnet_seed.json` | Cloudflare proxy (`bootstrap-store-gateway.perspect3vism.workers.dev`) | Development, standalone operation |
| **Test** | `tests/js/bootstrapSeed.json` | Local filesystem (`./tst-tmp/languages/`) | Test pipeline only (requires `prepare-test`) |

**The mainnet seed is embedded in the binary.** Running `ad4m-executor init` creates `~/.ad4m` (or the specified `--data-path`) and writes the included mainnet seed as `mainnet_seed.seed`. No extra flags needed for standard development:

```bash
ad4m-executor init --data-path /tmp/ad4m-data
```

Use `--network-bootstrap-seed` only to override with a different seed (e.g., for testing):
```bash
ad4m-executor init --data-path /tmp/ad4m-data \
  --network-bootstrap-seed ./tests/js/bootstrapSeed.json
```

### Diagnosing language failures

If logs show:
```text
Did not find language source for given address: QmzSYwd...
Did not find meta file for given address: QmzSYwd...
```
→ The data directory has a corrupted or wrong seed. Re-run `ad4m-executor init` to regenerate `mainnet_seed.seed`.

## Run the Executor

### Minimal (development)
```bash
ad4m-executor init --data-path /tmp/ad4m-data

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

| Port | Service | Default | Bind | Required |
|------|---------|---------|------|----------|
| 12100 | GraphQL HTTP | Yes | `127.0.0.1` | Yes |
| 2100 | Holochain admin | Yes | `127.0.0.1` | Yes |
| 1400 | Holochain app | Yes | `127.0.0.1` | Yes |
| 3001 | MCP server | No | `127.0.0.1` | Only with `--enable-mcp` |
| 12001 | HTTPS/TLS | No | `0.0.0.0` | Only with `--tls-cert-file` |

## Agent Initialisation

After the executor starts, generate an agent via GraphQL:

```bash
curl -s http://127.0.0.1:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { agentGenerate(passphrase: \"<passphrase>\") { isInitialized did } }"}'
```

### ⚠️ agentGenerate creates the JWT signing key

`agentGenerate` does two things: creates the agent DID **and** creates the wallet main key used for JWT token signing. If you skip this step and only use MCP's `request_capability`/`generate_jwt`, the multi-user login flow will fail with:
```text
Failed to generate token: main key not found. call createMainKey() first
```
**Always run `agentGenerate` once** after init, even if you plan to use MCP auth.

### ⚠️ Wallet must be unlocked after every restart

After restarting the executor, the agent wallet is **locked**. The encrypted keystore loads from `agent.json` but keys are not decrypted until you call `agentUnlock`:

```bash
curl -s http://127.0.0.1:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { agentUnlock(passphrase: \"<passphrase>\", holochain: true) { isUnlocked did } }"}'
```

Without unlocking, MCP tools and multi-user JWT generation will fail with `"Wallet is locked"`. The `holochain: true` flag ensures Holochain services also reinitialise with the agent identity.

**Wait for init to complete.** After `agentGenerate`, the executor takes 30-60 seconds to download and load bootstrap languages. Watch logs for `"AD4M init complete"` before using perspectives or neighbourhoods. There is no GraphQL readiness endpoint.

## Multi-User Mode

Multi-user mode allows multiple users to connect to a single executor, each with their own DID and scoped capabilities. **Required for Flux web client connections.**

### Enable multi-user mode

```bash
curl -s http://127.0.0.1:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { runtimeSetMultiUserEnabled(enabled: true) }"}'
```

### Create a user

```bash
curl -s http://127.0.0.1:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { runtimeCreateUser(email: \"user@example.com\", password: \"password\") { did success error } }"}'
```

Without SMTP configured, the response will say email verification was not sent — this is fine for development. The user can login immediately.

### Login

```bash
curl -s http://127.0.0.1:12100/graphql \
  -H "Content-Type: application/json" \
  -d '{"query":"mutation { runtimeLoginUser(email: \"user@example.com\", password: \"password\") }"}'
# → Returns JWT token
```

### Capability flow in multi-user mode

When multi-user is **disabled**, unauthenticated requests only get `AUTHENTICATE` + `READ_ENABLED` capabilities — nothing else. When **enabled**, unauthenticated requests additionally get `CREATE`, `LOGIN`, and `VERIFY` capabilities for user management, but still no `agent READ` until authenticated.

**Implication:** Flux (and any client) needs multi-user mode enabled to show the login screen. Without it, even the login form fails because the client tries to query agent info pre-login and gets a capability error.

### Full setup sequence for Flux connections

```bash
# 1. Start executor with TLS + admin credential
ad4m-executor run --app-data-path /tmp/ad4m-data \
  --admin-credential mypassword \
  --tls-cert-file cert.pem --tls-key-file key.pem

# 2. Generate agent (creates main key for JWT signing)
curl -sk https://127.0.0.1:12001/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: mypassword" \
  -d '{"query":"mutation { agentGenerate(passphrase: \"my-passphrase\") { did } }"}'

# 3. Enable multi-user mode
curl -sk https://127.0.0.1:12001/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: mypassword" \
  -d '{"query":"mutation { runtimeSetMultiUserEnabled(enabled: true) }"}'

# 4. Create user account
curl -sk https://127.0.0.1:12001/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: mypassword" \
  -d '{"query":"mutation { runtimeCreateUser(email: \"user@example.com\", password: \"pass123\") { did success error } }"}'

# 5. User connects via Flux at https://<ip>:12001
```

## Trusted Agents

Trusted agents can install and run languages on the executor without proof verification. Add trusted agents to allow languages published by specific DIDs:

```bash
curl -s http://127.0.0.1:12100/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: <admin-credential>" \
  -d '{"query":"mutation { addTrustedAgents(agents: [\"did:key:z6Mk...\"] ) }"}'
```

The default AD4M language author (`did:key:z6MkvPpWxwXAnLtMcoc9sX7GEoJ96oNnQ3VcQJRLspNJfpE7`) should be added if language proof verification fails after Holochain version updates.

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
curl -si -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"my-app","version":"1.0"}}}'

# 2. Send initialized notification (REQUIRED — without this, tool calls return empty)
curl -s -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","method":"notifications/initialized"}'

# 3. Request capability
curl -s -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"request_capability","arguments":{"app_name":"my-app","app_desc":"Description","app_url":"http://localhost"}}}'
# → Returns request_id and code (code is also printed to executor stdout)

# 4. Generate JWT
curl -s -X POST http://127.0.0.1:3001/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -H "Mcp-Session-Id: <session-id>" \
  -d '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"generate_jwt","arguments":{"request_id":"<id>","code":"<code>"}}}'
# → Authenticated. All subsequent calls in this session use the token.
```

### MCP tools

Perspectives: `add_perspective`, `list_perspectives`, `query_links`, `add_link`, `query_subjects`, `create_subject`, `get_subject_data`, `set_subject_property`, `delete_subject`, `get_subject_children`, `get_subject_collection`, `add_to_collection`, `remove_from_collection`

Neighbourhoods: `neighbourhood_join_from_url`, `neighbourhood_publish_from_perspective`

Agent: `get_agent_profile`, `set_agent_profile`, `set_agent_profile_picture`, `get_agent_public_perspective`, `set_agent_public_perspective`

Auth: `request_capability`, `generate_jwt`, `auth_status`, `signup`, `login_email`, `request_login_verification`, `verify_email_code`

AI/Flows: `get_models`, `add_model`, `infer`, `get_flows`, `add_flow`, `flow_start`, `flow_state`, `flow_actions`, `flow_run_action`

Utility: `execute_commands`, `generate_waker_query`

**Additional SDNA-derived tools:** When you join a neighbourhood with SHACL-defined subject classes (e.g., Flux communities), the MCP server dynamically generates CRUD tools for each class (e.g., `message_create`, `message_query`, `channel_get`). A Flux neighbourhood typically exposes ~248 tools total.

### MCP session lifecycle

Each MCP session is tied to the executor process. **Restarting the executor invalidates all sessions** — you must re-authenticate.

## Enable TLS (Remote Access)

```bash
# Generate self-signed cert (dev only)
openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 365 -nodes \
  -subj "/CN=<your-ip>" -addext "subjectAltName=IP:<your-ip>,IP:127.0.0.1"

ad4m-executor run \
  --app-data-path /tmp/ad4m-data \
  --admin-credential <password> \
  --tls-cert-file cert.pem \
  --tls-key-file key.pem
```

**Dual-server mode:** With TLS, the executor runs two servers:
- HTTP on `127.0.0.1:12100` (localhost only)
- HTTPS on `0.0.0.0:12001` (all interfaces, for remote clients)

**Self-signed cert gotcha:** Browsers must visit `https://<ip>:12001` directly and accept the cert warning before WebSocket connections (e.g., from Flux) will work.

## Connecting Flux (Web Client)

Flux is the web UI for AD4M neighbourhoods. The current development deployment is:

```text
https://deploy-preview-548--fluxsocial-dev.netlify.app/
```

### Connection requirements

1. **TLS must be enabled** — Flux connects over WebSocket which requires HTTPS for remote executors
2. **Multi-user mode must be enabled** — Flux uses email/password login
3. **`agentGenerate` must have been called** — creates the JWT signing key needed for login tokens
4. **A user account must exist** — created via `runtimeCreateUser` mutation
5. **Self-signed cert must be accepted** — visit `https://<ip>:12001` directly in the browser first

### Troubleshooting Flux connections

| Error | Cause | Fix |
|-------|-------|-----|
| `Capability not matched... expected LOGIN` | Multi-user mode disabled | `runtimeSetMultiUserEnabled(enabled: true)` |
| `Capability not matched... expected agent READ` | Unauthenticated requests lack READ; multi-user was just enabled but client not authenticated | Ensure user account exists, try login again |
| `main key not found. call createMainKey()` | `agentGenerate` was never called | Run `agentGenerate` mutation |
| WebSocket connection fails silently | Self-signed cert not accepted | Visit `https://<ip>:12001` directly first |
| Login form doesn't appear | Multi-user mode disabled | Enable it via mutation |

## GraphQL Subscriptions (WebSocket)

The executor supports GraphQL subscriptions over WebSocket at `/graphql` using `graphql-transport-ws` protocol.

### Connection

```javascript
const ws = new WebSocket('ws://127.0.0.1:12100/graphql', 'graphql-transport-ws');
// Auth goes inside connection_init payload:
ws.send(JSON.stringify({
  type: 'connection_init',
  payload: { headers: { authorization: '<admin-credential>' } }
}));
```

**⚠️ Auth format matters:** The authorization must be inside `payload.headers.authorization`, not `payload.authorization`. Wrong format results in connection closing after ~15 seconds (code 1006).

### Available subscriptions

| Subscription | Use |
|-------------|-----|
| `perspectiveLinkAdded(uuid)` | New links added to a perspective (fires for every synced link including historical on first subscribe) |
| `perspectiveLinkRemoved(uuid)` | Links removed |
| `perspectiveLinkUpdated(uuid)` | Links modified |
| `perspectiveQuerySubscription(subscriptionId)` | SurrealQL query change notifications (for waker pattern) |
| `perspectiveSyncStateChange(uuid)` | Sync status updates |
| `agentStatusChanged` | Agent online/offline changes |
| `neighbourhoodSignal(uuid)` | Neighbourhood signals |
| `runtimeMessageReceived` | Runtime messages |
| `exceptionOccurred` | Error events |

### Waker pattern (recommended for change detection)

Instead of `perspectiveLinkAdded` (which replays all links on initial subscribe), use the SurrealQL subscription pattern:

```bash
# 1. Register a SurrealQL subscription
mutation {
  perspectiveSubscribeSurrealQuery(
    uuid: "<perspective-id>",
    query: "SELECT * FROM link WHERE predicate = 'ad4m://has_child' AND source = '<channel-address>'"
  ) { subscriptionId result }
}
# result contains current matching links (use to populate "seen" set)

# 2. Subscribe to changes via WebSocket
subscription {
  perspectiveQuerySubscription(subscriptionId: "<subscription-id>")
}
# Fires with updated query results when matching links change
# Results prefixed with "#init#" on initial delivery

# 3. Keep alive (every 30s) — subscription expires without this
mutation {
  perspectiveKeepAliveSurrealQuery(
    uuid: "<perspective-id>",
    subscriptionId: "<subscription-id>"
  )
}
```

The `generate_waker_query` MCP tool can generate the SurrealQL query for a given subject class, but **beware double-encoding** — it URL-encodes the source address inside the query. Verify the query manually against `perspectiveQueryLinks` results.

### Subscription polling interval

SurrealQL subscriptions have an internal polling interval (~5-60 seconds). Changes are not instant — expect latency between link creation and subscription notification.

## Project Structure

```text
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
| Languages fail to resolve | Corrupted or missing bootstrap seed | Re-run `ad4m-executor init` to regenerate `mainnet_seed.seed` |
| `ad4m-executor` missing MCP flags | Built rust-executor last (wrong binary) | Rebuild with `cd cli && pnpm build` |
| Empty MCP responses | Missing `notifications/initialized` | Send notification after initialize |
| Port bind error | Previous executor still running | <code>lsof -ti:12100 &#124; xargs -r kill</code> (use `kill -9` only if needed) |
| Agent generates but nothing works | Init still in progress | Wait for "AD4M init complete" in logs |
| `mainnet_seed.seed` not found | Data directory corrupted or init not run | Re-run `ad4m-executor init` |
| `main key not found` on login | `agentGenerate` never called | Run `agentGenerate` mutation once |
| `Wallet is locked` on MCP/JWT | Executor restarted, wallet not unlocked | Run `agentUnlock` mutation with passphrase |
| `Capability not matched... LOGIN` | Multi-user mode disabled | `runtimeSetMultiUserEnabled(enabled: true)` |
| WebSocket closes after 15s | Wrong auth format in `connection_init` | Use `payload: { headers: { authorization: '...' } }` |
| Binary has wrong features after branch switch | Stale build from previous branch | Rebuild both `rust-executor` and `cli` |
| `rust-client/schema.gql` broken symlink | Target deleted in 2023 | `cp tests/js/schema.gql rust-client/schema.gql` (known issue) |
| MCP session stops working | Executor restarted | Re-authenticate (init → notify → request_capability → generate_jwt) |
| Flux can't connect remotely | TLS not enabled or cert not accepted | Enable TLS flags + accept cert in browser |
| `generate_waker_query` returns no results | Double-encoded source address in SurrealQL | Write query manually with correct source address |
