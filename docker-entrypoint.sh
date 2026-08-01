#!/bin/bash
set -e

# If running as root, fix bind-mount ownership and re-exec as ad4m user
if [ "$(id -u)" = "0" ]; then
    chown -R ad4m:ad4m /data
    exec gosu ad4m "$0" "$@"
fi

# ── First-run init ──────────────────────────────────────────────────────────
# Use the Docker seed (local bootstrap languages, no external dependencies)
# unless the operator supplies a custom seed via NETWORK_BOOTSTRAP_SEED.
SEED_FILE="${NETWORK_BOOTSTRAP_SEED:-/opt/ad4m/docker_seed.json}"

if [ ! -f /data/mainnet_seed.seed ]; then
    echo "Initializing AD4M data directory..."
    ad4m-executor init --data-path /data --network-bootstrap-seed "${SEED_FILE}"

    # Pre-populate language bundles on disk so the executor finds them
    # without needing to fetch from any external language-language store.
    if [ -d /opt/ad4m/bootstrap-languages ]; then
        echo "Pre-seeding bootstrap language bundles..."
        cp -r /opt/ad4m/bootstrap-languages/* /data/ad4m/languages/ 2>/dev/null || true
    fi
fi

# ── Pre-populate Kalosm model cache ────────────────────────────────────────
# If the Docker image includes pre-cached models (INCLUDE_MODELS=true at
# build time), copy them into the kalosm cache directory so the executor
# finds them on disk and skips network downloads.
KALOSM_CACHE="${HOME}/.local/share/kalosm/cache"
if [ -d /opt/ad4m/models ] && [ "$(ls -A /opt/ad4m/models 2>/dev/null)" ]; then
    if [ ! -d "${KALOSM_CACHE}" ]; then
        echo "Pre-populating Kalosm model cache..."
        mkdir -p "${KALOSM_CACHE}"
        cp -r /opt/ad4m/models/* "${KALOSM_CACHE}/"
    fi
fi

# ── Build executor args ─────────────────────────────────────────────────────
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

# No-Holochain mode: skip conductor startup entirely.
# Default to false (no holochain) for Docker standalone deployments.
if [ "${RUN_HOLOCHAIN:-false}" = "true" ]; then
    EXTRA_ARGS+=(--run-holochain true)
else
    EXTRA_ARGS+=(--run-holochain false)
fi

# ── Agent auto-generation + auto-unlock ─────────────────────────────────────
# On first boot, if AGENT_PASSPHRASE is set, auto-generate the agent.
# On subsequent boots (agent already initialised), auto-unlock with the same
# passphrase so multi-user signup/login (which requires the wallet's `main`
# secret key to sign JWTs) works without a manual `agent unlock` step.
# If AGENT_PASSPHRASE is not set and the agent is not yet initialised, the
# container fails fast so the operator knows they forgot to supply it.

AD4M_CLI_URL="http://localhost:12000"

run_ad4m_cli() {
    if [ -n "${ADMIN_CREDENTIAL:-}" ]; then
        ad4m --executor-url "${AD4M_CLI_URL}" --admin-credential "${ADMIN_CREDENTIAL}" "$@"
    else
        ad4m --executor-url "${AD4M_CLI_URL}" --no-capability "$@"
    fi
}

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

# Extract the raw JSON blob the CLI prints on deserialization failure.
# The current CLI's response schema is out of sync with the executor's
# `agent.status` and `agent.unlock` responses, but the raw payload is
# emitted at the tail of the error message as `raw: {...}`. Parsing it
# directly is more robust than depending on the CLI's parsed output.
extract_agent_flag() {
    # $1 = raw output, $2 = flag name (e.g. isInitialized, isUnlocked)
    local raw json
    raw=$(printf '%s' "$1" | grep -oE 'raw: \{.*\}' | head -1 | sed 's/^raw: //')
    if [ -z "${raw}" ]; then
        # No `raw:` prefix — the CLI parsed the response cleanly.
        # Fall back to searching the whole output.
        raw="$1"
    fi
    printf '%s' "${raw}" | grep -oE "\"$2\":(true|false)" | head -1 | sed "s/\"$2\"://"
}

maybe_setup_agent() {
    local status_output is_initialized is_unlocked
    status_output=$(run_ad4m_cli agent status 2>&1 || true)
    is_initialized=$(extract_agent_flag "${status_output}" isInitialized)
    is_unlocked=$(extract_agent_flag "${status_output}" isUnlocked)

    if [ -z "${is_initialized}" ]; then
        echo "WARNING: could not parse agent status. Raw output follows:" >&2
        echo "${status_output}" >&2
        return
    fi

    if [ "${is_initialized}" != "true" ]; then
        # Agent needs first-run generation.
        if [ -z "${AGENT_PASSPHRASE:-}" ]; then
            echo "ERROR: Agent is not initialised and AGENT_PASSPHRASE is not set." >&2
            echo "       Set the AGENT_PASSPHRASE environment variable and restart the container." >&2
            kill "${EXECUTOR_PID}" 2>/dev/null || true
            exit 1
        fi
        echo "Generating AD4M agent..."
        run_ad4m_cli agent generate --passphrase "${AGENT_PASSPHRASE}" 2>&1 || true
        local post_output post_initialized
        post_output=$(run_ad4m_cli agent status 2>&1 || true)
        post_initialized=$(extract_agent_flag "${post_output}" isInitialized)
        if [ "${post_initialized}" != "true" ]; then
            echo "ERROR: agent generate did not initialise the agent. Raw status:" >&2
            echo "${post_output}" >&2
            kill "${EXECUTOR_PID}" 2>/dev/null || true
            exit 1
        fi
        echo "AD4M agent generated."
        return
    fi

    # Agent is initialised. Unlock the wallet if a passphrase was provided
    # and it's not already unlocked.
    if [ "${is_unlocked}" = "true" ]; then
        echo "Agent already initialised and unlocked."
        return
    fi

    if [ -z "${AGENT_PASSPHRASE:-}" ]; then
        echo "WARNING: Agent is initialised but locked, and AGENT_PASSPHRASE is not set." >&2
        echo "         Set AGENT_PASSPHRASE to auto-unlock on boot, or run 'ad4m agent unlock' manually." >&2
        return
    fi

    echo "Unlocking AD4M agent..."
    local unlock_output unlock_status
    unlock_output=$(run_ad4m_cli agent unlock --passphrase "${AGENT_PASSPHRASE}" 2>&1 || true)
    unlock_status=$(extract_agent_flag "${unlock_output}" isUnlocked)
    if [ "${unlock_status}" = "true" ]; then
        echo "AD4M agent unlocked."
    else
        echo "WARNING: agent unlock did not report isUnlocked:true. Raw output:" >&2
        echo "${unlock_output}" >&2
    fi
}

# ── WE web frontend (static file server) ───────────────────────────────────
WE_PID=""
start_we_server() {
    local we_dist="/opt/ad4m/we-dist"
    local we_port="${WE_PORT:-8081}"

    if [ -f "${we_dist}/SKIPPED" ] || [ ! -f "${we_dist}/index.html" ]; then
        echo "WE frontend not bundled — skipping static server."
        return
    fi

    echo "Starting WE web frontend on port ${we_port}..."
    # Use Python's built-in HTTP server (available on Ubuntu 24.04)
    python3 -m http.server "${we_port}" --directory "${we_dist}" --bind 0.0.0.0 &
    WE_PID=$!
    echo "WE frontend serving at http://0.0.0.0:${we_port}/"
}

# ── Start executor in background, auto-generate agent, then wait ────────────
ad4m-executor run \
    --app-data-path /data \
    --localhost false \
    --run-dapp-server true \
    "${EXTRA_ARGS[@]}" \
    "$@" &

EXECUTOR_PID=$!

start_we_server
wait_for_executor
maybe_setup_agent

# Forward SIGTERM/SIGINT to all child processes so Docker can stop cleanly.
cleanup() {
    kill -TERM "${EXECUTOR_PID}" 2>/dev/null
    [ -n "${WE_PID}" ] && kill -TERM "${WE_PID}" 2>/dev/null
}
trap cleanup TERM INT

# Wait for executor to exit; propagate its exit code.
wait "${EXECUTOR_PID}"
