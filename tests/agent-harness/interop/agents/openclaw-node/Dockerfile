# Combined image for the A2 managed path.
#
# OpenClaw managed mode spawns the AD4M executor as a child process inside the
# OpenClaw container, so that container must be able to run the executor. The
# executor binary needs a newer glibc than the OpenClaw base image ships, so we
# base on the harness executor image (interop/executor) and add Node + the
# OpenClaw CLI, at the version the pinned OpenClaw image runs.
#
# The ad4m plugin is installed at pod-setup time from a tarball packed from this
# repo's plugins/ad4m, so it is not baked here.
ARG BASE=ad4m-agent-harness-executor:local
FROM ${BASE}
USER root
ENV PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1 \
    NODE_ENV=development

ARG NODE_VERSION=24.19.0
RUN arch="$(dpkg --print-architecture)"; case "$arch" in amd64) n=x64 ;; arm64) n=arm64 ;; *) echo "unsupported $arch"; exit 1 ;; esac \
 && apt-get update && apt-get install -y --no-install-recommends xz-utils && rm -rf /var/lib/apt/lists/* \
 && curl -fsSL "https://nodejs.org/dist/v${NODE_VERSION}/node-v${NODE_VERSION}-linux-${n}.tar.xz" \
    | tar -xJ -C /usr/local --strip-components=1 \
 && npm install -g openclaw@2026.7.1 \
 && npm cache clean --force

# The base image's entrypoint starts the executor; for the managed pod we drive
# OpenClaw instead and let its plugin manage the executor, so callers override
# the entrypoint (e.g. `--entrypoint sleep ... infinity`).
