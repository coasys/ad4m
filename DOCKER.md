# AD4M Docker Deployment

Run a headless AD4M executor as a Docker container. No Tauri, no GUI, no external dependencies.

## Quick Start

```bash
ADMIN_CREDENTIAL=your-secret AGENT_PASSPHRASE=your-passphrase docker compose up -d
```

The container initialises on first boot, generates an agent, and exposes the API.

## Building the Image

### Default (standalone, with WE frontend)

```bash
docker compose build
```

### Without WE frontend

```bash
INCLUDE_WE=false docker compose build
```

### With pre-cached AI models (~1.8 GB)

```bash
INCLUDE_MODELS=true docker compose build
```

Pre-downloads the default Kalosm models into the image so the executor can load them without network access at runtime. On first boot, the entrypoint auto-registers the transcription and LLM models with the executor and sets the LLM as default — Flux transcription and summarisation work out of the box.

- **Embedding:** BAAI/bge-small-en-v1.5 (auto-loaded on startup)
- **Transcription:** openai/whisper-small (registered as "Whisper")
- **LLM:** TinyLlama-1.1B-Chat Q4_K_M (registered as "TinyLlama", set as default)

### With Holochain enabled

```bash
RUN_HOLOCHAIN=true docker compose up -d
```

### Build from scratch (no compose)

```bash
docker build -t ad4m-executor .
docker build -t ad4m-executor --build-arg INCLUDE_WE=false .
docker build -t ad4m-executor --build-arg INCLUDE_MODELS=true .
```

## Port Map

| Service | Container | Host (compose) | Description |
|---|---|---|---|
| WE + API proxy | 8080 | 8180 | Caddy reverse proxy: WE frontend, executor API, and WebSocket on one origin |
| WS-RPC API | 12000 | 13000 | Direct executor access for non-browser clients (CLI, scripts) |
| MCP server | 3001 | 3101 | Model Context Protocol for AI agents |

Host ports offset by +1000 to avoid collisions with a local dev executor.

## Build Arguments

| Argument | Default | Description |
|---|---|---|
| `INCLUDE_WE` | `true` | Bundle the WE web frontend |
| `INCLUDE_MODELS` | `false` | Pre-cache default Kalosm AI models (~1.8 GB) |

## Environment Variables

| Variable | Default | Description |
|---|---|---|
| `ADMIN_CREDENTIAL` | _(none)_ | Shared secret for admin-level API access |
| `AGENT_PASSPHRASE` | _(none)_ | Passphrase for agent key generation and auto-unlock |
| `ENABLE_MULTI_USER` | `true` | Allow multiple users via signup/login flow |
| `ENABLE_MCP` | `true` | Start the MCP server |
| `MCP_PORT` | `3001` | MCP server port inside the container |
| `RUN_HOLOCHAIN` | `false` | Start the Holochain conductor (`true` for P2P mode) |
| `WE_PORT` | `8080` | Caddy reverse proxy port inside the container |
| `NETWORK_BOOTSTRAP_SEED` | _(docker seed)_ | Path to a custom bootstrap seed JSON file |

## Modes of Operation

### Standalone (default)

Holochain disabled. All bootstrap languages use local storage. The executor functions as a self-contained server: perspectives, agents, languages, and neighbourhoods all persist locally. No external network calls.

```bash
RUN_HOLOCHAIN=false docker compose up -d
```

### P2P (Holochain enabled)

The Holochain conductor starts alongside the executor. Bootstrap languages connect to the AD4M network for peer discovery, link sync, and language distribution. Requires network access to `bootstrap.ad4m.dev`.

```bash
RUN_HOLOCHAIN=true NETWORK_BOOTSTRAP_SEED=/path/to/mainnet_seed.json docker compose up -d
```

## Data Persistence

All state lives in the `/data` volume:

```text
/data/
  mainnet_seed.seed    — bootstrap seed (written on first init)
  ad4m/
    agent.json         — agent identity and keys
    languages/         — installed language bundles
    perspectives/      — perspective data
    ...
```

The `ad4m-data` named volume persists across container restarts. Back it up with:

```bash
docker run --rm -v ad4m-data:/data -v $(pwd):/backup busybox tar czf /backup/ad4m-backup.tar.gz /data
```

## WE Frontend

When built with `INCLUDE_WE=true` (the default), a Caddy reverse proxy serves the WE web application and proxies API requests to the executor — all on a single port (8080, host 8180). This single-origin design avoids CORS issues when WE runs behind an authentication proxy (e.g. Cloudflare Access).

Caddy routes:

| Path | Target | Description |
|---|---|---|
| `/health` | `localhost:12000` | Executor health check |
| `/api/*` | `localhost:12000` | HTTP API passthrough (incl. WebSocket) |
| `/apps/flux/*` | Flux static files | Flux SPA (iframe-embeddable, no X-Frame-Options) |
| `/*` | WE static files | SPA with fallback to `index.html` |

Flux (the social toolkit) builds alongside WE and loads inside WE as an iframe at `/apps/flux/`. Embedded apps connect to the executor directly through the same-origin reverse proxy — no postMessage proxy mode required.

In ad4m-connect, set the executor URL to the same origin as the WE page (e.g. `https://we.example.com`). No separate API subdomain required.

## API Access

### With admin credential

```bash
# WS-RPC (WebSocket)
wscat -c ws://localhost:13000/ws -H "Authorization: your-secret"

# CLI
ad4m --executor-url http://localhost:13000 --admin-credential your-secret agent status
```

### Multi-user flow

1. Create a user: call `user.create` via WS-RPC
2. Login: call `user.login` to receive a JWT
3. Use the JWT as the Authorization header for subsequent requests

### MCP (AI agents)

The MCP server exposes ~42 tools for AI agents. Connect any MCP-compatible client to `http://localhost:3101`.

## systemd Integration

Create a user unit for auto-start:

```ini
# ~/.config/systemd/user/ad4m-docker.service
[Unit]
Description=AD4M Docker Executor
After=docker.service

[Service]
Type=simple
WorkingDirectory=/path/to/ad4m
ExecStart=/usr/bin/docker compose up --no-build
ExecStop=/usr/bin/docker compose down
Restart=on-failure
RestartSec=10

[Install]
WantedBy=default.target
```

```bash
systemctl --user daemon-reload
systemctl --user enable --now ad4m-docker.service
```

## Troubleshooting

### Container exits immediately

Check logs: `docker compose logs ad4m`. Common causes:

- Missing `AGENT_PASSPHRASE` on first boot — the container requires it for agent generation
- Port conflict — another process already binds to the host ports

### Agent unlock fails

The entrypoint auto-unlocks on boot if `AGENT_PASSPHRASE` matches the passphrase used during generation. If the passphrase changed or was lost, the volume must start fresh:

```bash
docker compose down -v
AGENT_PASSPHRASE=new-passphrase docker compose up -d
```

### Executor ready but languages fail to load

In standalone mode, bootstrap languages load from pre-seeded bundles on disk. Verify they exist:

```bash
docker compose exec ad4m ls /data/ad4m/languages/
```

Each hash directory should contain a `bundle.js` file.

### WE frontend not loading

Verify the image was built with `INCLUDE_WE=true`:

```bash
docker compose exec ad4m ls /opt/ad4m/we-dist/index.html
```

If missing, rebuild: `INCLUDE_WE=true docker compose build`

## Architecture

```text
┌──────────────────────────────────────────────────┐
│ Docker Container                                 │
│                                                  │
│  ┌──────────────────┐    ┌─────────────────────┐ │
│  │  Caddy Proxy      │    │  AD4M Executor      │ │
│  │                    │    │  (Rust binary)      │ │
│  │  :8080 ─┬─ /*     │    │                     │ │
│  │         │  static  │    │  :12000 WS-RPC      │ │
│  │         ├─ /ws ────┼───>│  :3001  MCP         │ │
│  │         ├─ /health─┼───>│                     │ │
│  │         └─ /api/* ─┼───>│  ┌───────────────┐  │ │
│  │                    │    │  │ Holochain     │  │ │
│  └──────────────────┘    │  │ (optional)    │  │ │
│                           │  └───────────────┘  │ │
│                           └─────────────────────┘ │
│                                                  │
│  /data (persistent volume)                       │
└──────────────────────────────────────────────────┘
```
