# =============================================================================
# AD4M Executor Docker Image
# Multi-stage build: full toolchain → minimal runtime
#
# Build args:
#   INCLUDE_WE=true     — bundle WE web frontend (served on port 8081)
#   RUN_HOLOCHAIN=true  — include Holochain conductor (false = standalone mode)
# =============================================================================

ARG RUST_VERSION=1.92
ARG NODE_MAJOR=24
ARG GO_VERSION=1.24.6
ARG INCLUDE_WE=true

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

# ── Copy source ─────────────────────────────────────────────────────────
COPY --chown=builder:builder . /home/builder/ad4m
WORKDIR /home/builder/ad4m

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

# ── Prepare Deno/rusty_v8 local clones (bypass submodule hell) ─────────
RUN for i in 1 2 3; do \
      git clone --depth 1 --single-branch --branch new-v8-dylib-hickory-update \
        --no-recurse-submodules \
        https://github.com/coasys/deno.git /home/builder/deno-local && break; \
      rm -rf /home/builder/deno-local && sleep 10; \
    done && \
    cd /home/builder/deno-local && \
    git rm --cached tests/wpt/suite 2>/dev/null || true && \
    rm -f .gitmodules && \
    sed -i 's|deno_core = { version = "0.347.0", git = "https://github.com/coasys/deno_core.git", branch = "new-v8-dylib" }|deno_core = { version = "0.347.0", path = "/home/builder/deno_core-local/core" }|' Cargo.toml && \
    git add -A && \
    git -c user.email="build@docker" -c user.name="docker" commit --allow-empty -m "strip submodules"

RUN for i in 1 2 3; do \
      git clone --depth 1 --single-branch --branch new-v8-dylib \
        --no-recurse-submodules \
        https://github.com/coasys/deno_core.git /home/builder/deno_core-local && break; \
      rm -rf /home/builder/deno_core-local && sleep 10; \
    done && \
    sed -i 's|v8 = { version = "137.1.1", default-features = false, git = "https://github.com/coasys/rusty_v8.git", tag = "v137.1.1" }|v8 = { version = "137.1.1", default-features = false, path = "/home/builder/rusty_v8-local" }|' /home/builder/deno_core-local/Cargo.toml

RUN for i in 1 2 3; do \
      git clone --depth 1 --single-branch --branch v137.1.1 \
        --no-recurse-submodules \
        https://github.com/coasys/rusty_v8.git /home/builder/rusty_v8-local && break; \
      rm -rf /home/builder/rusty_v8-local && sleep 10; \
    done && \
    cd /home/builder/rusty_v8-local && \
    git rm --cached buildtools 2>/dev/null || true && \
    rm -f .gitmodules && \
    git add -A && \
    git -c user.email="build@docker" -c user.name="docker" commit --allow-empty -m "strip submodules"

# Patch Cargo.toml to use local clones
RUN sed -i 's|deno_runtime = {version = "0.212.0", git = "https://github.com/coasys/deno.git", branch = "new-v8-dylib-hickory-update"|deno_runtime = {version = "0.212.0", path = "/home/builder/deno-local/runtime"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_resolver = {version = "0.35.0", git = "https://github.com/coasys/deno.git", branch = "new-v8-dylib-hickory-update"|deno_resolver = {version = "0.35.0", path = "/home/builder/deno-local/resolvers/deno"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_fs = {version = "0.114.0", git = "https://github.com/coasys/deno.git", branch = "new-v8-dylib-hickory-update"|deno_fs = {version = "0.114.0", path = "/home/builder/deno-local/ext/fs"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_lib = {version = "0.20.0", git = "https://github.com/coasys/deno.git", branch = "new-v8-dylib-hickory-update"|deno_lib = {version = "0.20.0", path = "/home/builder/deno-local/cli/lib"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_snapshots = {version = "0.19.0", git = "https://github.com/coasys/deno.git", branch = "new-v8-dylib-hickory-update"|deno_snapshots = {version = "0.19.0", path = "/home/builder/deno-local/cli/snapshot"|' rust-executor/Cargo.toml && \
    sed -i 's|deno_core = {version = "0.347.0", git = "https://github.com/coasys/deno_core.git", branch = "new-v8-dylib"|deno_core = {version = "0.347.0", path = "/home/builder/deno_core-local/core"|' rust-executor/Cargo.toml

# Workspace-level [patch] to prevent duplicate crate versions
RUN if ! grep -q '\[patch."https://github.com/coasys/deno_core.git"\]' Cargo.toml; then \
      printf '\n[patch."https://github.com/coasys/deno_core.git"]\ndeno_core = { path = "/home/builder/deno_core-local/core" }\n' >> Cargo.toml; \
    fi && \
    if ! grep -q '\[patch."https://github.com/coasys/rusty_v8.git"\]' Cargo.toml; then \
      printf '\n[patch."https://github.com/coasys/rusty_v8.git"]\nv8 = { path = "/home/builder/rusty_v8-local" }\n' >> Cargo.toml; \
    fi

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

# Strip the rusty_v8 git source from Cargo.lock so Cargo resolves v8
# from the [patch] local path instead of fetching from GitHub (which
# fails on submodule resolution for chromium buildtools).
RUN sed -i '/^source = "git+https:\/\/github\.com\/coasys\/rusty_v8/d' Cargo.lock

# ── Build (cargo target cached across rebuilds) ───────────────────────
RUN --mount=type=cache,target=/home/builder/ad4m/target,uid=1001,gid=1001 \
    pnpm run build-deno-snapshot

RUN --mount=type=cache,target=/home/builder/ad4m/target,uid=1001,gid=1001 \
    pnpm run build-libs \
    && cp target/release/ad4m /home/builder/ad4m-bin \
    && cp target/release/ad4m-executor /home/builder/ad4m-executor-bin

# Free disk space
RUN rm -rf /home/builder/deno-local /home/builder/deno_core-local /home/builder/rusty_v8-local \
    && rm -rf /tmp/rustc* \
    && rm -rf /home/builder/.cargo/registry/cache

# ── Generate Docker bootstrap seed ────────────────────────────────────
# Local bootstrap languages replace Holochain-backed ones for standalone mode.
RUN node docker/generate-seed.mjs bootstrap-languages/docker docker/seed-output

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
    pnpm install --no-frozen-lockfile && \
    pnpm build:web

RUN mkdir -p /we-dist && \
    if [ "${INCLUDE_WE}" = "true" ] && [ -d /we/apps/we-web/dist ]; then \
      cp -r /we/apps/we-web/dist/* /we-dist/; \
    fi

# =============================================================================
# Stage 3: Runtime
# =============================================================================
FROM ubuntu:24.04 AS runtime

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    python3-minimal \
    libssl3 \
    libgtk-3-0 \
    libwebkit2gtk-4.1-0 \
    libjavascriptcoregtk-4.1-0 \
    libasound2t64 \
    gosu \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /home/builder/ad4m-bin /usr/local/bin/ad4m
COPY --from=builder /home/builder/ad4m-executor-bin /usr/local/bin/ad4m-executor

# Docker bootstrap seed and pre-populated language bundles
COPY --from=builder /home/builder/ad4m/docker/seed-output/docker_seed.json /opt/ad4m/docker_seed.json
COPY --from=builder /home/builder/ad4m/docker/seed-output/languages/ /opt/ad4m/bootstrap-languages/

# WE web frontend (empty dir if INCLUDE_WE=false)
COPY --from=we-builder /we-dist/ /opt/ad4m/we-dist/

RUN useradd -m -s /bin/bash ad4m && mkdir -p /data && chown ad4m:ad4m /data

COPY --chmod=755 docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh

WORKDIR /data

# WS-RPC API
EXPOSE 12000
# MCP server
EXPOSE 3001
# Dapp server
EXPOSE 8080
# WE web frontend
EXPOSE 8081

HEALTHCHECK --interval=30s --timeout=5s --start-period=30s --retries=3 \
    CMD curl -sf http://localhost:12000/ || exit 1

ENTRYPOINT ["docker-entrypoint.sh"]
