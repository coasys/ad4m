# Combined image for the A2 managed path.
#
# OpenClaw managed mode spawns the AD4M executor as a child process inside the
# OpenClaw container, so that container must be able to run the executor. The
# host-built binary needs a newer glibc than the OpenClaw base image ships, so we
# base on the AD4M executor image instead (Ubuntu 24.04, with an in-image
# /usr/local/bin/ad4m-executor built against a matching glibc + all runtime
# deps) and add Node + the OpenClaw CLI on top.
#
# The ad4m plugin is installed at pod-setup time from a locally-built tarball
# (the npm package fix is not published yet), so it is not baked here.
ARG BASE=ad4m-test:latest
FROM ${BASE}
USER root
ENV PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1 \
    NODE_ENV=development

RUN apt-get update \
 && apt-get install -y --no-install-recommends curl ca-certificates gnupg \
 && curl -fsSL https://deb.nodesource.com/setup_24.x | bash - \
 && apt-get install -y --no-install-recommends nodejs \
 && npm install -g openclaw@2026.7.1 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

# The base image's entrypoint starts the executor; for the managed pod we drive
# OpenClaw instead and let its plugin manage the executor, so callers override
# the entrypoint (e.g. `--entrypoint sleep ... infinity`).
