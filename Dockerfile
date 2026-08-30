# =============================================================================
# AD4M Executor Docker Image
# Multi-stage build: full toolchain → minimal runtime
#
# Build args:
#   INCLUDE_WE=true       — bundle WE web frontend (served on port 8081)
#   INCLUDE_MODELS=false  — pre-cache default Kalosm AI models (~1.8 GB)
#   RUN_HOLOCHAIN=true    — include Holochain conductor (false = standalone mode)
# =============================================================================

ARG RUST_VERSION=1.92
ARG NODE_MAJOR=24
ARG GO_VERSION=1.24.6
ARG INCLUDE_WE=true
ARG INCLUDE_MODELS=false

# =============================================================================
# Stage 1: Builder
# =============================================================================
FROM ubuntu:24.04 AS builder

ARG RUST_VERSION
ARG NODE_MAJOR
ARG GO_VERSION

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    curl \
    wget \
    git \
    unzip \
    pkg-config \
    cmake \
    clang \
    protobuf-compiler \
    libgtk-3-dev \
    libwebkit2gtk-4.1-dev \
    libjavascriptcoregtk-4.1-dev \
    libappindicator3-dev \
    librsvg2-dev \
    libasound2-dev \
    libssl-dev \
    patchelf \
    && rm -rf /var/lib/apt/lists/*

# Go
RUN wget -q https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz -O /tmp/go.tar.gz \
    && tar -C /usr/local -xzf /tmp/go.tar.gz \
    && rm /tmp/go.tar.gz
ENV PATH="/usr/local/go/bin:${PATH}"

# Node
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_MAJOR}.x | bash - \
    && apt-get install -y --no-install-recommends nodejs \
    && rm -rf /var/lib/apt/lists/*

# pnpm (match version pinned in package.json)
RUN npm install -g pnpm@9.15.0

# Non-root build user
RUN useradd -m builder
USER builder
WORKDIR /home/builder

# Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y \
    --default-toolchain ${RUST_VERSION}
ENV PATH="/home/builder/.cargo/bin:${PATH}"
RUN rustup target add wasm32-unknown-unknown

ENV CARGO_INCREMENTAL=0
ENV CARGO_NET_GIT_FETCH_WITH_CLI=true

# Git wrapper: intercepts submodule fetches to stale/huge repos (wpt 3GB,
# chromium buildtools) while passing everything else through.
RUN git config --global advice.detachedHead false \
    && git config --global http.postBuffer 524288000 \
    && mkdir -p /home/builder/bin \
    && printf '#!/bin/bash\nif [ "$1" = "fetch" ]; then\n  for arg in "$@"; do\n    case "$arg" in\n      *web-platform-tests*|*nicolo-ribaudo*|*nicol*test262*|*/user/nicol*|*chromium.googlesource.com*) exit 0 ;;\n    esac\n  done\nfi\nif [ "$1" = "submodule" ]; then exit 0; fi\nexec /usr/bin/git "$@"\n' > /home/builder/bin/git \
    && chmod +x /home/builder/bin/git
ENV PATH="/home/builder/bin:${PATH}"

# Deno
RUN curl -fsSL https://deno.land/install.sh | sh
ENV DENO_INSTALL="/home/builder/.deno"
ENV PATH="${DENO_INSTALL}/bin:${PATH}"

# ── Pre-clone Deno monorepo (bypass submodule hell) ───────────────────
# v2.9.5-coasys-4 folds deno_core, deno_v8 (rusty_v8), and all ext crates
# into one repo. Clone without submodules and strip .gitmodules so
# libgit2 can't chase stale WPT/chromium refs.
RUN for i in 1 2 3; do \
      git clone --depth 1 --single-branch --branch v2.9.5-coasys-4 \
        --no-recurse-submodules \
        https://github.com/coasys/deno.git /home/builder/deno-local && break; \
      rm -rf /home/builder/deno-local && sleep 10; \
    done && \
    cd /home/builder/deno-local && \
    git rm --cached tests/wpt/suite tests/util/std tests/node_compat/runner/suite 2>/dev/null || true && \
    rm -f .gitmodules && \
    git add -A && \
    git -c user.email="build@docker" -c user.name="docker" commit --allow-empty -m "strip submodules"

# ── Pre-clone rusty_v8 (bypass chromium submodule hell) ──────────────
# deno_v8 depends on v8 crate from coasys/rusty_v8.git. That repo has
# 10+ submodules (chromium buildtools, V8 source, etc.) with revspecs
# force-pushed out of upstream. libgit2 tries to fetch them and fails.
# Pre-clone without submodules, strip .gitmodules, patch in locally.
RUN for i in 1 2 3; do \
      git clone --depth 1 --single-branch --branch v150.4.0-coasys \
        --no-recurse-submodules \
        https://github.com/coasys/rusty_v8.git /home/builder/rusty_v8-local && break; \
      rm -rf /home/builder/rusty_v8-local && sleep 10; \
    done && \
    cd /home/builder/rusty_v8-local && \
    rm -f .gitmodules && \
    git add -A && \
    git -c user.email="build@docker" -c user.name="docker" commit --allow-empty -m "strip submodules"

# Pre-cache floneum in cargo git DB (GitHub rejects fetch-by-SHA on forks)
RUN mkdir -p /home/builder/.cargo/git/db && \
    for i in 1 2 3; do \
      git clone --bare --single-branch --branch coasys-2-without-pdf --depth 20 \
        https://github.com/coasys/floneum.git \
        /home/builder/.cargo/git/db/floneum-bfbb720c433546c9 && break; \
      rm -rf /home/builder/.cargo/git/db/floneum-bfbb720c433546c9; sleep 10; \
    done && \
    cd /home/builder/.cargo/git/db/floneum-bfbb720c433546c9 && \
    git fetch --depth 1 origin 427cfdf3c07f5502ea085f281f6a362adb046312 || true && \
    mkdir -p /home/builder/.cargo/git/checkouts/floneum-bfbb720c433546c9 && \
    git clone --shared /home/builder/.cargo/git/db/floneum-bfbb720c433546c9 \
      /home/builder/.cargo/git/checkouts/floneum-bfbb720c433546c9/427cfdf && \
    cd /home/builder/.cargo/git/checkouts/floneum-bfbb720c433546c9/427cfdf && \
    git checkout 427cfdf3c07f5502ea085f281f6a362adb046312

# ── Copy source (selective — keeps docker-entrypoint.sh out of the
#    builder so entrypoint-only edits don't bust the Rust cache) ─────────
WORKDIR /home/builder/ad4m
COPY --chown=builder:builder Cargo.toml Cargo.lock package.json pnpm-lock.yaml pnpm-workspace.yaml rust-toolchain.toml deno.lock turbo.json ./
COPY --chown=builder:builder .cargo/ ./.cargo/
COPY --chown=builder:builder rust-executor/ ./rust-executor/
COPY --chown=builder:builder rust-client/ ./rust-client/
COPY --chown=builder:builder cli/ ./cli/
COPY --chown=builder:builder core/ ./core/
COPY --chown=builder:builder connect/ ./connect/
COPY --chown=builder:builder ui/ ./ui/
COPY --chown=builder:builder dapp/ ./dapp/
COPY --chown=builder:builder bootstrap-languages/ ./bootstrap-languages/
COPY --chown=builder:builder ad4m-ldk/ ./ad4m-ldk/
COPY --chown=builder:builder ad4m-hooks/ ./ad4m-hooks/
COPY --chown=builder:builder tests/ ./tests/
COPY --chown=builder:builder patches/ ./patches/
COPY --chown=builder:builder hooks/ ./hooks/
COPY --chown=builder:builder docs-src/ ./docs-src/

# Skip Electron/Playwright binary downloads (headless build)
ENV ELECTRON_SKIP_BINARY_DOWNLOAD=1
ENV PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1

# ── JS deps + dapp assets ──────────────────────────────────────────────
RUN pnpm install --no-frozen-lockfile
RUN pnpm build-dapp

WORKDIR /home/builder/ad4m/core
RUN pnpm install --no-frozen-lockfile || \
    (echo ">>> Retrying core pnpm install..." && sleep 5 && pnpm install --no-frozen-lockfile)
WORKDIR /home/builder/ad4m

# Patch rust-executor/Cargo.toml to use local deno monorepo clone.
# All 7 deno crates now come from a single repo (v2.9.5-coasys-4).
RUN sed -i 's|deno_v8 = { version = "0.2.0", git = "https://github.com/coasys/deno.git", tag = "v2.9.5-coasys-4"|deno_v8 = { version = "0.2.0", path = "/home/builder/deno-local/libs/deno_v8"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_core = { version = "0.410.0", git = "https://github.com/coasys/deno.git", tag = "v2.9.5-coasys-4"|deno_core = { version = "0.410.0", path = "/home/builder/deno-local/libs/core"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_runtime = { version = "0.265.0", git = "https://github.com/coasys/deno.git", tag = "v2.9.5-coasys-4"|deno_runtime = { version = "0.265.0", path = "/home/builder/deno-local/runtime"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_resolver = { version = "0.88.0", git = "https://github.com/coasys/deno.git", tag = "v2.9.5-coasys-4"|deno_resolver = { version = "0.88.0", path = "/home/builder/deno-local/libs/resolver"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_fs = { version = "0.167.0", git = "https://github.com/coasys/deno.git", tag = "v2.9.5-coasys-4"|deno_fs = { version = "0.167.0", path = "/home/builder/deno-local/ext/fs"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_lib = { version = "0.75.0", git = "https://github.com/coasys/deno.git", tag = "v2.9.5-coasys-4"|deno_lib = { version = "0.75.0", path = "/home/builder/deno-local/cli/lib"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_snapshots = { version = "0.72.0", git = "https://github.com/coasys/deno.git", tag = "v2.9.5-coasys-4"|deno_snapshots = { version = "0.72.0", path = "/home/builder/deno-local/cli/snapshot"|' rust-executor/Cargo.toml

# Workspace-level [patch] so transitive deps also resolve from the local clone.
# v2.9.5-coasys-4 monorepo: all deno crates from one git URL.
# rusty_v8: v8 crate from coasys/rusty_v8.git (deno_v8's transitive dep).
RUN if ! grep -q '\[patch."https://github.com/coasys/deno.git"\]' Cargo.toml; then \
      printf '\n[patch."https://github.com/coasys/deno.git"]\ndeno_v8 = { path = "/home/builder/deno-local/libs/deno_v8" }\ndeno_core = { path = "/home/builder/deno-local/libs/core" }\ndeno_runtime = { path = "/home/builder/deno-local/runtime" }\ndeno_resolver = { path = "/home/builder/deno-local/libs/resolver" }\ndeno_fs = { path = "/home/builder/deno-local/ext/fs" }\ndeno_lib = { path = "/home/builder/deno-local/cli/lib" }\ndeno_snapshots = { path = "/home/builder/deno-local/cli/snapshot" }\n' >> Cargo.toml; \
    fi && \
    if ! grep -q '\[patch."https://github.com/coasys/rusty_v8.git"\]' Cargo.toml; then \
      printf '\n[patch."https://github.com/coasys/rusty_v8.git"]\nv8 = { path = "/home/builder/rusty_v8-local" }\n' >> Cargo.toml; \
    fi

# Also patch deno_v8's own Cargo.toml to use the local rusty_v8 clone
# (it declares rusty_v8 as a git dep — redirect to local path).
RUN sed -i 's|rusty_v8 = { package = "v8", version = "150.4.0", optional = true, default-features = false, git = "https://github.com/coasys/rusty_v8.git", tag = "v150.4.0-coasys"|rusty_v8 = { package = "v8", version = "150.4.0", optional = true, default-features = false, path = "/home/builder/rusty_v8-local"|' /home/builder/deno-local/libs/deno_v8/Cargo.toml

# Strip git source lines from Cargo.lock so Cargo resolves from
# local paths (avoids libgit2 submodule fetch attempts).
RUN sed -i '/^source = "git+https:\/\/github\.com\/coasys\/deno\.git/d' Cargo.lock && \
    sed -i '/^source = "git+https:\/\/github\.com\/coasys\/rusty_v8\.git/d' Cargo.lock

# ── Build (cargo target cached across rebuilds) ───────────────────────
# If the v8 binding file is absent (fresh source tree after layer
# invalidation), clear stale v8 fingerprints so build.rs re-runs and
# regenerates it.  Only fires when the COPY layers actually changed.
RUN --mount=type=cache,id=ad4m-cargo,target=/home/builder/ad4m/target,uid=1001,gid=1001 \
    if [ ! -f /home/builder/deno-local/libs/deno_v8/gen/src_binding_release_x86_64-unknown-linux-gnu.rs ]; then \
      echo ">>> v8 binding missing — clearing stale build cache"; \
      rm -rf target/release/build/v8-* target/release/build/deno_v8-* \
             target/release/.fingerprint/v8-* target/release/.fingerprint/deno_v8-* 2>/dev/null || true; \
    fi && \
    pnpm run build-deno-snapshot

RUN --mount=type=cache,id=ad4m-cargo,target=/home/builder/ad4m/target,uid=1001,gid=1001 \
    pnpm run build-libs \
    && cp target/release/ad4m /home/builder/ad4m-bin \
    && cp target/release/ad4m-executor /home/builder/ad4m-executor-bin

# Free disk space
RUN rm -rf /home/builder/deno-local /home/builder/rusty_v8-local \
    && rm -rf /tmp/rustc* \
    && rm -rf /home/builder/.cargo/registry/cache

# ── Generate Docker bootstrap seed ────────────────────────────────────
# Local bootstrap languages replace Holochain-backed ones for standalone mode.
# docker/ is copied here (not with the source tree above) so that changes
# to generate-seed.mjs or download-models.sh don't bust the Rust cache.
COPY --chown=builder:builder docker/ ./docker/
RUN node docker/generate-seed.mjs docker/bootstrap-languages docker/seed-output

# =============================================================================
# Stage 2a: WE web frontend (conditional)
# =============================================================================
FROM ubuntu:24.04 AS we-builder

ARG NODE_MAJOR
ARG INCLUDE_WE

ENV DEBIAN_FRONTEND=noninteractive

RUN if [ "${INCLUDE_WE}" != "true" ]; then \
      mkdir -p /we-dist && echo "WE build skipped" > /we-dist/SKIPPED && exit 0; \
    fi && \
    apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl git nodejs && \
    curl -fsSL https://deb.nodesource.com/setup_${NODE_MAJOR}.x | bash - && \
    apt-get install -y --no-install-recommends nodejs && \
    npm install -g pnpm@9.15.0 && \
    rm -rf /var/lib/apt/lists/*

RUN if [ "${INCLUDE_WE}" != "true" ]; then exit 0; fi && \
    git clone --depth 1 --single-branch --branch dev \
      https://github.com/coasys/we.git /we

WORKDIR /we

RUN if [ "${INCLUDE_WE}" != "true" ]; then exit 0; fi && \
    HOSTING_FILE=$(find apps/we-web/src/platform -name 'ad4mConnector.ts' -o -name 'webAdapter.ts' 2>/dev/null | head -1) && \
    if [ -n "${HOSTING_FILE}" ]; then \
      sed -i 's/hosting: true,/hosting: true,\n      remoteUrl: window.location.origin,/' "${HOSTING_FILE}"; \
      sed -i 's/const { core, client }/const { core, client: clientP }/' "${HOSTING_FILE}"; \
      sed -i '/return {/i\    const client = await clientP;' "${HOSTING_FILE}"; \
    fi && \
    sed -i 's|"globalSpaceUrl": "neighbourhood://[^"]*"|"globalSpaceUrl": ""|' we-seed.json && \
    sed -i 's|"marketplaceUrl": "neighbourhood://[^"]*"|"marketplaceUrl": ""|' we-seed.json && \
    sed -i 's|"webUrl": "https://fluxsocial-dev.netlify.app"|"webUrl": "/apps/flux/"|' we-seed.json && \
    BRIDGE_FILE=$(find packages/app-shell/src -name 'appBridge.ts' 2>/dev/null | head -1) && \
    if [ -n "${BRIDGE_FILE}" ]; then \
      sed -i 's/= deps\.isDesktop$/= deps.isDesktop || url/' "${BRIDGE_FILE}"; \
    fi && \
    if [ -f apps/we-web/src/index.tsx ]; then \
      sed -i '/import weSeed from/a\if (typeof window !== "undefined" && (window as any).__WE_HOSTING_CONFIG?.globalSpaceUrl) { (weSeed as any).globalSpaceUrl = (window as any).__WE_HOSTING_CONFIG.globalSpaceUrl; }' apps/we-web/src/index.tsx; \
    elif [ -f packages/app-framework/src/frameworks/solid/stores/AdamStore.tsx ]; then \
      sed -i '/import weSeedFile from/a\if (typeof window !== "undefined" && (window as any).__WE_HOSTING_CONFIG?.globalSpaceUrl) { (weSeedFile as any).globalSpaceUrl = (window as any).__WE_HOSTING_CONFIG.globalSpaceUrl; }' packages/app-framework/src/frameworks/solid/stores/AdamStore.tsx; \
    fi && \
    pnpm install --no-frozen-lockfile && \
    pnpm --filter "@we/app-web..." --workspace-concurrency=1 run build

# Patch the built WE bundle: replace the hardcoded centralized
# file-storage language hash with the Docker-local version.
COPY --from=builder /home/builder/ad4m/docker/seed-output/file-storage-hash.txt /tmp/file-storage-hash.txt
RUN if [ "${INCLUDE_WE}" = "true" ] && [ -d /we/apps/we-web/dist ]; then \
      LOCAL_FS_HASH=$(cat /tmp/file-storage-hash.txt) && \
      find /we/apps/we-web/dist -name '*.js' -exec \
        sed -i "s/QmzSYwddqhm49PrRMzSrJf3AvmmreXMKtr1u56nbTjBFVmCzS8N/${LOCAL_FS_HASH}/g" {} + && \
      echo "Patched FILE_STORAGE_LANGUAGE → ${LOCAL_FS_HASH}"; \
    fi

RUN mkdir -p /we-dist && \
    if [ "${INCLUDE_WE}" = "true" ] && [ -d /we/apps/we-web/dist ]; then \
      cp -r /we/apps/we-web/dist/* /we-dist/; \
    fi

# ── Flux app build (served alongside WE at /apps/flux/) ──────────────────────
RUN if [ "${INCLUDE_WE}" != "true" ]; then \
      mkdir -p /flux-dist && exit 0; \
    fi && \
    git clone --depth 1 --single-branch --branch dev \
      https://github.com/coasys/flux.git /flux

RUN if [ "${INCLUDE_WE}" != "true" ]; then exit 0; fi && \
    cd /flux && \
    LOCAL_FS_HASH=$(cat /tmp/file-storage-hash.txt) && \
    sed -i "s/QmzSYwddqhm49PrRMzSrJf3AvmmreXMKtr1u56nbTjBFVmCzS8N/${LOCAL_FS_HASH}/g" \
      packages/constants/src/languages.ts && \
    echo "Patched Flux FILE_STORAGE_LANGUAGE source → ${LOCAL_FS_HASH}" && \
    pnpm install --no-frozen-lockfile && \
    VITE_ALLOWED_ORIGINS="" VITE_BASE=/apps/flux/ NODE_OPTIONS='--max-old-space-size=4096' pnpm run build

RUN mkdir -p /flux-dist && \
    if [ "${INCLUDE_WE}" = "true" ] && [ -d /flux/app/dist ]; then \
      cp -r /flux/app/dist/* /flux-dist/; \
    fi

# =============================================================================
# Stage 2b: Kalosm model pre-cache (conditional)
# =============================================================================
FROM ubuntu:24.04 AS model-fetcher

ARG INCLUDE_MODELS

RUN if [ "${INCLUDE_MODELS}" != "true" ]; then \
      mkdir -p /models && exit 0; \
    fi && \
    apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl && \
    rm -rf /var/lib/apt/lists/*

COPY docker/download-models.sh /tmp/download-models.sh
RUN chmod +x /tmp/download-models.sh && \
    if [ "${INCLUDE_MODELS}" = "true" ]; then \
      /tmp/download-models.sh /models; \
    else \
      mkdir -p /models; \
    fi

# =============================================================================
# Stage 3: Runtime
# =============================================================================
FROM ubuntu:24.04 AS runtime

ENV DEBIAN_FRONTEND=noninteractive

ARG CADDY_VERSION=2.9.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    jq \
    libssl3 \
    libgtk-3-0 \
    libwebkit2gtk-4.1-0 \
    libjavascriptcoregtk-4.1-0 \
    libasound2t64 \
    gosu \
    && rm -rf /var/lib/apt/lists/*

RUN ARCH=$(dpkg --print-architecture) && \
    curl -fsSL "https://github.com/caddyserver/caddy/releases/download/v${CADDY_VERSION}/caddy_${CADDY_VERSION}_linux_${ARCH}.tar.gz" \
    | tar -xz -C /usr/local/bin caddy && \
    chmod +x /usr/local/bin/caddy

# websocat: minimal WebSocket CLI for AI model registration via WS-RPC
ARG WEBSOCAT_VERSION=1.14.1
RUN ARCH=$(dpkg --print-architecture) && \
    if [ "${ARCH}" = "amd64" ]; then WS_BIN="websocat.x86_64-unknown-linux-musl"; \
    elif [ "${ARCH}" = "arm64" ]; then WS_BIN="websocat.aarch64-unknown-linux-musl"; \
    else echo "Unsupported arch for websocat: ${ARCH}" >&2; exit 0; fi && \
    curl -fsSL "https://github.com/vi/websocat/releases/download/v${WEBSOCAT_VERSION}/${WS_BIN}" \
        -o /usr/local/bin/websocat && \
    chmod +x /usr/local/bin/websocat

COPY --from=builder /home/builder/ad4m-bin /usr/local/bin/ad4m
COPY --from=builder /home/builder/ad4m-executor-bin /usr/local/bin/ad4m-executor

# Docker bootstrap seed, language bundles, and language-language KV store
COPY --from=builder /home/builder/ad4m/docker/seed-output/docker_seed.json /opt/ad4m/docker_seed.json
COPY --from=builder /home/builder/ad4m/docker/seed-output/languages/ /opt/ad4m/bootstrap-languages/
COPY --from=builder /home/builder/ad4m/docker/seed-output/language-language-kv/ /opt/ad4m/language-language-kv/

# WE web frontend (empty dir if INCLUDE_WE=false)
COPY --from=we-builder /we-dist/ /opt/ad4m/we-dist/

# Flux app (served at /apps/flux/ alongside WE; empty dir if INCLUDE_WE=false)
COPY --from=we-builder /flux-dist/ /opt/ad4m/flux-dist/

# Pre-cached Kalosm models (empty dir if INCLUDE_MODELS=false)
COPY --from=model-fetcher /models/ /opt/ad4m/models/

RUN useradd -m -s /bin/bash ad4m && mkdir -p /data && chown ad4m:ad4m /data \
    && chown -R ad4m:ad4m /opt/ad4m/we-dist/ \
    && chown -R ad4m:ad4m /opt/ad4m/flux-dist/

COPY --chmod=755 docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh

WORKDIR /data

# Caddy reverse proxy (WE frontend + executor API)
EXPOSE 8080
# WS-RPC API (direct, for non-browser clients)
EXPOSE 12000
# MCP server
EXPOSE 3001

HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 \
    CMD curl -sf http://localhost:12000/ || exit 1

ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
