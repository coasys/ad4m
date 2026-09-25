# syntax=docker/dockerfile:1
#
# Headless Sovereign server image for wind-tunnel integration testing.
#
# The build CONTEXT is the Sovereign repo root (not this directory). Docker
# reads the sibling `Dockerfile.dockerignore` (BuildKit per-Dockerfile ignore),
# which strips node_modules/dist/.git from the context so deps and the native
# node-pty binding build FRESH in-image — carrying the host installs in would
# drag in host-ABI binaries and absolute-path pnpm symlinks that break the build.
#
# Build (run from anywhere):
#   docker build -f interop/agents/sovereign/Dockerfile \
#     -t swwt-sovereign:latest <path-to>/sovereign
#
# Config is NOT baked. The server reads host/port/tls from config.json in
# SOVEREIGN_CONFIG_DIR. Supply it at run time and boot with no host mounts:
#   docker create --name sv -p 127.0.0.1:8877:8080 swwt-sovereign:latest
#   docker cp config.json sv:/data/config/config.json
#   docker start sv
# Anthropic provider is read from ANTHROPIC_BASE_URL / ANTHROPIC_API_KEY /
# ANTHROPIC_AUTH_TOKEN passed at `docker run` (the bundled claude CLI inherits
# process.env). Health: GET /health -> {"status":"ok","message":"sovereign"}

FROM node:26-bookworm

# node-pty is the ONLY native addon in the tree; node-gyp needs python3 + a
# C/C++ toolchain to compile its linux binding.
RUN apt-get update \
 && apt-get install -y --no-install-recommends python3 make g++ ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# devDeps (tsdown, typescript, vite, rollup, ...) are required to BUILD, so keep
# NODE_ENV=development for the install. playwright-core must not fetch browsers;
# the browser service is satisfied at run time by CHROME_PATH instead.
ENV NODE_ENV=development \
    PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1

WORKDIR /app

# node:26 dropped the bundled corepack, so install the repo's pinned pnpm
# (packageManager: pnpm@10.16.1) directly rather than via `corepack enable`.
RUN npm install -g pnpm@10.16.1

# Repo source only (see Dockerfile.dockerignore).
COPY . .

# pnpm 10 blocks dependency build scripts by default (it prints "Ignored build
# scripts: ... node-pty"), and `.pnpm-approve-builds.json` is NOT honoured by
# pnpm 10.16. node-pty ships no linux prebuild and terminal.ts imports it at
# module load, so the binding MUST compile or boot crashes before listen();
# esbuild backs the client's vite build. Approve both (a workspace-wide root
# setting) so the normal `pnpm install` runs their build scripts.
RUN node -e "const fs=require('fs'),p=JSON.parse(fs.readFileSync('package.json','utf8'));const s=new Set([...((p.pnpm&&p.pnpm.onlyBuiltDependencies)||[]),'node-pty','esbuild']);p.pnpm={...(p.pnpm||{}),onlyBuiltDependencies:[...s]};fs.writeFileSync('package.json',JSON.stringify(p,null,2))"

# Install every workspace dep (incl. the vendor/coasys/ad4m/core member) and
# compile the approved native build scripts, then assert node-pty's linux
# binding exists (fail the build early rather than crash at boot).
RUN pnpm install \
 && find node_modules -path '*node-pty*/build/Release/pty.node' | grep -q pty.node

# Emit the ad4m/core .d.ts (build:vendor), then build every workspace package
# EXCEPT the browser client. core's own build (tsc + rollup) produces
# lib/index.js|cjs, which the server imports at run time (Ad4mClient et al.);
# build:vendor alone emits only .d.ts. The client is a browser UI — not needed
# headless, nothing the server requires depends on it, and its vite.config
# reads a dev TLS keypair at load, so building it would force host secrets into
# the image. The server itself reads .certs only when server.tls.enabled=true.
RUN pnpm run build:vendor \
 && pnpm -r --if-present --filter '!@sovereign/client' build \
 && test -f packages/server/dist/index.js

# ── Baked runtime env ───────────────────────────────────────────────────
ENV SOVEREIGN_CONFIG_DIR=/data/config \
    SOVEREIGN_DATA_DIR=/data/data \
    HOME=/data/home \
    SEMBLE_MCP=off \
    CHROME_PATH=/bin/true

# Config dir (config.json lands here via docker cp), runtime data dir, HOME +
# the workspace + agent dirs the sample config points at.
RUN mkdir -p /data/config /data/data /data/home/ws /data/home/.claude

EXPOSE 8080

# Runs from the repo WORKDIR; reads config.json from SOVEREIGN_CONFIG_DIR.
CMD ["node", "packages/server/dist/index.js"]
