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

Pre-downloads the default Kalosm models into the image so the executor can load them without network access at runtime:

- **Embedding:** BAAI/bge-small-en-v1.5 (auto-loaded on startup)
- **Transcription:** openai/whisper-small (default whisper model)
- **LLM:** TinyLlama-1.1B-Chat Q4_K_M (smallest supported chat model)

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
| WS-RPC API | 12000 | 13000 | AD4M's primary API (WebSocket + HTTP) |
| MCP server | 3001 | 3101 | Model Context Protocol for AI agents |
| Dapp server | 8080 | 8180 | Entanglement proof mini-app |
| WE frontend | 8081 | 8181 | WE collaborative web app |

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
| `WE_PORT` | `8081` | WE frontend server port inside the container |
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

When built with `INCLUDE_WE=true` (the default), the WE web application serves on port 8081 (host 8181). WE connects to the AD4M executor via WebSocket. Point the connection at the executor's WS-RPC port (container-internal: `ws://localhost:12000`, or from the host: `ws://localhost:13000`).

WE provides a collaborative interface for working with AD4M perspectives, neighbourhoods, and social DNA.

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
┌─────────────────────────────────────────────┐
│ Docker Container                            │
│                                             │
│  ┌─────────────────┐  ┌──────────────────┐  │
│  │  AD4M Executor   │  │  WE Frontend     │  │
│  │  (Rust binary)   │  │  (Python httpd)  │  │
│  │                  │  │                  │  │
│  │  :12000 WS-RPC   │  │  :8081 HTTP      │  │
│  │  :8080  Dapp     │  │                  │  │
│  │  :3001  MCP      │  └──────────────────┘  │
│  │                  │                        │
│  │  ┌────────────┐  │                        │
│  │  │ Holochain  │  │  (only if              │
│  │  │ Conductor  │  │   RUN_HOLOCHAIN=true)  │
│  │  └────────────┘  │                        │
│  └─────────────────┘                        │
│                                             │
│  /data (persistent volume)                  │
└─────────────────────────────────────────────┘
```
