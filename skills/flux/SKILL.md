---
name: flux
description: Build and run the Flux UI (AD4M Launcher) for development. Use when setting up the Flux frontend, connecting it to an AD4M executor, troubleshooting UI builds, or working with the Tauri desktop shell.
---

# Flux (AD4M Launcher)

Flux is the user-facing application for AD4M. In the repo it lives at `ui/` (package name: `ad4m-launcher`). It's a Vite + React app that connects to a running AD4M executor via GraphQL/WebSocket. It can run as:

- **Web app** (development) — `pnpm dev` in `ui/`
- **Desktop app** (Tauri) — `pnpm tauri dev` or `pnpm run package-ad4m`

## Prerequisites

- **Node.js 20+** with **pnpm**
- A **running AD4M executor** (see the `ad4m-executor` skill or docs)
- For desktop builds: Tauri prerequisites (Rust + platform libs — same as executor)

## Quick Start (Web Dev Mode)

```bash
cd ad4m
pnpm install

# Build dependencies first (core types + connect library)
cd core && pnpm build && cd ..
cd connect && pnpm build && cd ..

# Start Flux dev server
cd ui && pnpm dev
# → http://127.0.0.1:5173
```

Flux will prompt for an executor URL on first load. Point it at your running executor's GraphQL endpoint (default: `http://127.0.0.1:12100`).

## Connecting to the Executor

Flux uses `@coasys/ad4m-connect` to handle:
- Executor discovery and connection
- Capability-based authentication (requests capabilities, gets JWT)
- Token storage and reconnection

### Local executor
Default: `http://127.0.0.1:12100` — works out of the box.

### Remote executor (TLS)
When connecting to a remote executor with self-signed TLS:

1. The executor must run with `--tls-cert-file` and `--tls-key-file` (serves HTTPS on port 12001)
2. **First:** Open `https://<executor-ip>:12001` in the browser and accept the self-signed cert
3. **Then:** Point Flux at `https://<executor-ip>:12001`

Without step 2, WebSocket connections silently fail.

### Authentication
Flux handles auth automatically via `@coasys/ad4m-connect`. When using `--admin-credential` on the executor, Flux prompts for it on connection. The connect library then:
1. Requests capabilities from the executor
2. Receives a random code (auto-permitted in single-user mode)
3. Generates a JWT
4. Stores the token for reconnection

## Building for Desktop (Tauri)

```bash
cd ui

# macOS (with Metal GPU support)
pnpm run package-ad4m:macos

# Linux
pnpm run package-ad4m:linux

# Windows
pnpm run package-ad4m:windows
```

Output in `target/release/bundle/`.

The Tauri desktop build embeds the executor — it's a standalone app that manages its own `ad4m-executor` process. For development, running the web dev server against a separate executor is faster.

## Key Packages

| Package | Location | npm | Purpose |
|---------|----------|-----|---------|
| `@coasys/ad4m` | `core/` | Published | TypeScript types, `Ad4mClient`, GraphQL schema |
| `@coasys/ad4m-connect` | `connect/` | Published | Connection + auth library |
| `@coasys/flux-ui` | External | See `ui/package.json` | Flux UI components (imported by `ui/`) |
| `ad4m-launcher` | `ui/` | Not published | Desktop/web shell |
| `@coasys/dapp` | `dapp/` | Not published | Separate dapp web interface |

### Build order for local development

If modifying core types or connect library:
```bash
cd core && pnpm build       # TypeScript types
cd ../connect && pnpm build # Connection library
cd ../ui && pnpm dev        # Flux dev server picks up changes
```

## Common Issues

| Symptom | Cause | Fix |
|---------|-------|-----|
| Flux loads but can't connect | Executor not running or wrong URL | Check executor is at `http://127.0.0.1:12100` |
| WebSocket fails to remote executor | Self-signed cert not accepted | Visit `https://<ip>:12001` in browser first |
| Auth prompt appears repeatedly | Admin credential mismatch | Use same `--admin-credential` value |
| Blank page after build | Stale build cache | `cd ui && pnpm dev -- --force` (clears Vite dep cache) |
| `@coasys/ad4m` types missing | Core not built | `cd core && pnpm build` |
| Neighbourhood appears empty | Languages still loading | Wait for "AD4M init complete" in executor logs |
| Flux connects but no communities visible | Agent not initialised | Run `agentGenerate` mutation on executor first |
