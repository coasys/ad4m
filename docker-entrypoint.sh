#!/bin/bash
set -e

# If running as root, fix bind-mount ownership and re-exec as ad4m user
if [ "$(id -u)" = "0" ]; then
    chown -R ad4m:ad4m /data
    exec gosu ad4m "$0" "$@"
fi

# First-run init
if [ ! -f /data/mainnet_seed.seed ]; then
    echo "Initializing AD4M data directory..."
    ad4m-executor init --data-path /data
fi

EXTRA_ARGS=()

if [ -n "${ADMIN_CREDENTIAL:-}" ]; then
    EXTRA_ARGS+=(--admin-credential "$ADMIN_CREDENTIAL")
fi

if [ "${ENABLE_MULTI_USER:-}" = "true" ]; then
    EXTRA_ARGS+=(--enable-multi-user true)
fi

if [ "${ENABLE_MCP:-}" = "true" ]; then
    EXTRA_ARGS+=(--enable-mcp true --mcp-port "${MCP_PORT:-3001}")
fi

exec ad4m-executor run \
    --app-data-path /data \
    --localhost false \
    --run-dapp-server true \
    "${EXTRA_ARGS[@]}" \
    "$@"
