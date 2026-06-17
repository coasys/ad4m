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

# ── Agent auto-generation ────────────────────────────────────────────────────
# If AGENT_PASSPHRASE is set, auto-generate the agent on first boot after the
# executor is ready.  If it is NOT set and the agent is not yet initialised,
# the container will fail fast so the operator knows they forgot to supply it.

AD4M_CLI_URL="http://localhost:12000"

# Build the common CLI prefix (executor URL; no cap-token needed for
# agent.status / agent.generate).
AD4M_CLI_BASE="ad4m --executor-url ${AD4M_CLI_URL} --no-capability"
if [ -n "${ADMIN_CREDENTIAL:-}" ]; then
    # Use the admin credential so the CLI can reach a secured executor.
    AD4M_CLI_BASE="ad4m --executor-url ${AD4M_CLI_URL} --admin-credential ${ADMIN_CREDENTIAL}"
fi

wait_for_executor() {
    echo "Waiting for AD4M executor to be ready..."
    local attempt=0
    local max_attempts=120  # 2 minutes
    until curl -sf "http://localhost:12000/" >/dev/null 2>&1; do
        attempt=$(( attempt + 1 ))
        if [ "${attempt}" -ge "${max_attempts}" ]; then
            echo "ERROR: AD4M executor did not become ready within 120 seconds." >&2
            exit 1
        fi
        sleep 1
    done
    echo "AD4M executor is ready."
}

maybe_generate_agent() {
    # Check whether the agent is already initialised.
    local status_output
    if ! status_output=$(${AD4M_CLI_BASE} agent status 2>&1); then
        echo "WARNING: could not check agent status: ${status_output}" >&2
        return
    fi

    if echo "${status_output}" | grep -q "is_initiliazed: true"; then
        echo "Agent already initialised — skipping generate."
        return
    fi

    # Agent is NOT initialised.  AGENT_PASSPHRASE is required.
    if [ -z "${AGENT_PASSPHRASE:-}" ]; then
        echo "ERROR: Agent is not initialised and AGENT_PASSPHRASE is not set." >&2
        echo "       Set the AGENT_PASSPHRASE environment variable and restart the container." >&2
        # Kill the background executor so the container exits cleanly.
        kill "${EXECUTOR_PID}" 2>/dev/null || true
        exit 1
    fi

    echo "Generating AD4M agent..."
    if ! ${AD4M_CLI_BASE} agent generate --passphrase "${AGENT_PASSPHRASE}" 2>&1; then
        echo "ERROR: agent generate failed." >&2
        kill "${EXECUTOR_PID}" 2>/dev/null || true
        exit 1
    fi
    echo "AD4M agent generated successfully."
}

# ── Start executor in background, auto-generate agent, then wait ────────────
ad4m-executor run \
    --app-data-path /data \
    --localhost false \
    --run-dapp-server true \
    "${EXTRA_ARGS[@]}" \
    "$@" &

EXECUTOR_PID=$!

wait_for_executor
maybe_generate_agent

# Forward SIGTERM/SIGINT to the executor process so Docker can stop it cleanly.
trap 'kill -TERM "${EXECUTOR_PID}" 2>/dev/null' TERM INT

# Wait for executor to exit; propagate its exit code.
wait "${EXECUTOR_PID}"
