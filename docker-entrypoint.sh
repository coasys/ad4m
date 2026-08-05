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
fi

# Pre-populate language bundles on disk so the executor finds them
# without needing to fetch from any external language-language store.
# Runs on every boot but only copies bundles that don't already exist,
# so language upgrades in a new image get picked up while runtime state
# in existing language directories stays untouched.
if [ -d /opt/ad4m/bootstrap-languages ]; then
    seeded=0
    for lang_dir in /opt/ad4m/bootstrap-languages/*/; do
        lang_hash=$(basename "${lang_dir}")
        target="/data/ad4m/languages/${lang_hash}"
        if [ ! -f "${target}/bundle.js" ]; then
            mkdir -p "${target}"
            cp "${lang_dir}bundle.js" "${target}/bundle.js"
            seeded=$((seeded + 1))
        fi
    done
    [ "${seeded}" -gt 0 ] && echo "Pre-seeded ${seeded} bootstrap language bundle(s)."
fi

# Pre-populate the language-language's KV store so it can serve
# bootstrap language bundles and metadata via storageGet().
# Merges bootstrap entries into any existing KV file (preserving
# runtime-published languages while ensuring bootstrap meta/bundle
# entries exist for applyTemplateAndPublish).
if [ -f /opt/ad4m/language-language-kv/address.txt ]; then
    LL_ADDR=$(cat /opt/ad4m/language-language-kv/address.txt)
    LL_DIR="/data/ad4m/languages/${LL_ADDR}"
    mkdir -p "${LL_DIR}"
    if [ ! -f "${LL_DIR}/ad4m-language-kv.json" ]; then
        cp /opt/ad4m/language-language-kv/ad4m-language-kv.json "${LL_DIR}/ad4m-language-kv.json"
        echo "Pre-seeded language-language KV store for ${LL_ADDR}"
    else
        jq -s '.[0] * .[1]' "${LL_DIR}/ad4m-language-kv.json" /opt/ad4m/language-language-kv/ad4m-language-kv.json \
            > "${LL_DIR}/ad4m-language-kv.json.tmp" \
            && mv "${LL_DIR}/ad4m-language-kv.json.tmp" "${LL_DIR}/ad4m-language-kv.json"
        echo "Merged bootstrap entries into language-language KV store."
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

# Only override run_holochain when explicitly set.
# Executor default: true (Holochain conductor starts).
if [ "${RUN_HOLOCHAIN:-}" = "true" ]; then
    EXTRA_ARGS+=(--run-holochain true)
elif [ "${RUN_HOLOCHAIN:-}" = "false" ]; then
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

# Extract a boolean flag from the CLI's agent status output.
# The CLI has a deserialization bug that wraps the raw JSON in an error
# message, so all fields land on ONE line. We must extract the boolean
# value anchored to the specific flag name, not just any true/false.
extract_agent_flag() {
    # $1 = raw output, $2 = flag name (e.g. isInitialized, isUnlocked)
    local cleaned flag_name="$2"
    # Strip ANSI escape codes
    cleaned=$(printf '%s' "$1" | sed $'s/\x1b\\[[0-9;]*m//g')

    # 1) JSON format: "flagName":true  or  "flagName": false
    local val
    val=$(printf '%s' "${cleaned}" \
        | grep -oE "\"${flag_name}\"[[:space:]]*:[[:space:]]*(true|false)" \
        | grep -oE '(true|false)' | head -1)
    if [ -n "${val}" ]; then
        echo "${val}"
        return
    fi

    # 2) snake_case CLI table:  is_unlocked: true  (+ known typo)
    local snake
    snake=$(printf '%s' "${flag_name}" | sed 's/\([A-Z]\)/_\L\1/g')
    local pattern="${snake}"
    [ "${flag_name}" = "isInitialized" ] && pattern="${pattern}|is_initiliazed|initiliazed"

    val=$(printf '%s' "${cleaned}" \
        | grep -oE "(${pattern})[[:space:]]*:[[:space:]]*(true|false)" \
        | grep -oE '(true|false)' | head -1)
    echo "${val}"
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
    local unlock_args=(agent unlock --passphrase "${AGENT_PASSPHRASE}")
    if [ "${RUN_HOLOCHAIN:-}" = "false" ]; then
        unlock_args+=(--holochain false)
    fi
    local unlock_output
    unlock_output=$(run_ad4m_cli "${unlock_args[@]}" 2>&1 || true)

    # The CLI has a deserialization bug that wraps the JSON response in
    # an error message. Check the actual isUnlocked field rather than
    # looking for "Agent unlocked" text output.
    local post_unlock post_unlocked
    post_unlock=$(run_ad4m_cli agent status 2>&1 || true)
    post_unlocked=$(extract_agent_flag "${post_unlock}" isUnlocked)
    if [ "${post_unlocked}" = "true" ]; then
        echo "AD4M agent unlocked."
    else
        echo "WARNING: agent unlock may have failed (isUnlocked=${post_unlocked:-<empty>}). Raw output:" >&2
        echo "${unlock_output}" >&2
    fi
}

# ── AI model auto-registration ─────────────────────────────────────────────
# When pre-cached models exist (INCLUDE_MODELS=true at build time), register
# them with the executor via WebSocket RPC so Flux transcription and
# summarisation work out of the box.
ws_rpc() {
    local msg_type="$1" params="$2" msg_id="${3:-1}"
    printf '{"id":"%s","type":"%s","params":%s}\n' "${msg_id}" "${msg_type}" "${params}" \
        | websocat -n1 "ws://localhost:12000/api/v1/ws?token=${ADMIN_CREDENTIAL}" 2>/dev/null
}

setup_ai_models() {
    if [ -z "$(ls -A "${KALOSM_CACHE}" 2>/dev/null)" ]; then
        return
    fi
    if [ -z "${ADMIN_CREDENTIAL:-}" ]; then
        return
    fi
    if ! command -v websocat >/dev/null 2>&1; then
        echo "WARNING: websocat not found, skipping AI model registration." >&2
        return
    fi

    echo "Registering AI models..."

    local models_response
    models_response=$(ws_rpc "ai.models" "{}" "list-models")
    if [ -z "${models_response}" ]; then
        echo "WARNING: could not query AI models." >&2
        return
    fi

    # Register Whisper (transcription) if not already present
    if ! echo "${models_response}" | jq -e '.result[] | select(.name == "Whisper")' >/dev/null 2>&1; then
        local whisper_result whisper_id
        whisper_result=$(ws_rpc "ai.addModel" \
            '{"model":{"name":"Whisper","type":"TRANSCRIPTION","local":{"fileName":"whisper_small"}}}' \
            "add-whisper")
        whisper_id=$(echo "${whisper_result}" | jq -r '.result // empty')
        if [ -n "${whisper_id}" ]; then
            echo "  Registered transcription model: Whisper (${whisper_id})"
        else
            echo "  WARNING: failed to register Whisper model." >&2
        fi
    else
        echo "  Whisper model already registered."
    fi

    # Register TinyLlama (LLM) if no LLM model exists
    local llm_id=""
    if echo "${models_response}" | jq -e '.result[] | select(.modelType == "LLM")' >/dev/null 2>&1; then
        llm_id=$(echo "${models_response}" | jq -r '[.result[] | select(.modelType == "LLM")][0].id')
        echo "  LLM model already registered (${llm_id})."
    else
        local llm_result
        llm_result=$(ws_rpc "ai.addModel" \
            '{"model":{"name":"TinyLlama","type":"LLM","local":{"fileName":"tinyllama-1.1b-chat-v1.0.Q4_K_M.gguf","huggingfaceRepo":"TheBloke/TinyLlama-1.1B-Chat-v1.0-GGUF","tokenizerSource":{"repo":"hf-internal-testing/llama-tokenizer","revision":"main","fileName":"tokenizer.json"}}}}' \
            "add-llm")
        llm_id=$(echo "${llm_result}" | jq -r '.result // empty')
        if [ -n "${llm_id}" ]; then
            echo "  Registered LLM model: TinyLlama (${llm_id})"
        else
            echo "  WARNING: failed to register TinyLlama model." >&2
        fi
    fi

    # Set default LLM if none configured
    if [ -n "${llm_id}" ]; then
        local default_response
        default_response=$(ws_rpc "ai.getDefaultModel" '{"modelType":"LLM"}' "get-default")
        if echo "${default_response}" | jq -e '.result == null' >/dev/null 2>&1; then
            ws_rpc "ai.setDefaultModel" "{\"id\":\"${llm_id}\",\"modelType\":\"LLM\"}" "set-default" >/dev/null
            echo "  Set TinyLlama as default LLM."
        fi
    fi
}

# ── Global discovery space (create on first boot, inject URL into WE) ───
setup_global_space() {
    local we_dist="/opt/ad4m/we-dist"

    # Skip if WE not bundled
    if [ -f "${we_dist}/SKIPPED" ] || [ ! -f "${we_dist}/index.html" ]; then
        return
    fi

    local url_file="/data/global_space_url"
    local neighbourhood_url

    if [ -f "${url_file}" ]; then
        neighbourhood_url=$(cat "${url_file}")
        echo "Using existing global space: ${neighbourhood_url}"
    else
        echo "Creating global discovery space..."

        # Extract link language hash from the Docker seed
        local link_lang
        link_lang=$(grep -A1 'knownLinkLanguages' /opt/ad4m/docker_seed.json | grep -oE 'Qm[A-Za-z0-9]+' | head -1 || true)

        if [ -z "${link_lang}" ]; then
            echo "WARNING: could not extract link language from docker_seed.json" >&2
            return
        fi

        # Create a perspective
        local perspective_output perspective_uuid
        perspective_output=$(run_ad4m_cli perspectives add "Global Space" 2>&1 || true)
        perspective_uuid=$(printf '%s' "${perspective_output}" | sed $'s/\x1b\\[[0-9;]*m//g' | grep -oE '[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}' | head -1)

        if [ -z "${perspective_uuid}" ]; then
            echo "WARNING: could not create perspective for global space. Raw output:" >&2
            echo "${perspective_output}" >&2
            return
        fi

        # Publish as neighbourhood using the Docker bootstrap link language
        local neighbourhood_output
        neighbourhood_output=$(run_ad4m_cli neighbourhoods create "${perspective_uuid}" "${link_lang}" 2>&1 || true)
        neighbourhood_url=$(printf '%s' "${neighbourhood_output}" | sed $'s/\x1b\\[[0-9;]*m//g' | grep -oE 'neighbourhood://[A-Za-z0-9]+' | head -1)

        if [ -z "${neighbourhood_url}" ]; then
            echo "WARNING: could not publish global space. Raw output:" >&2
            echo "${neighbourhood_output}" >&2
            return
        fi

        # Persist for subsequent boots
        echo "${neighbourhood_url}" > "${url_file}"
        echo "Global space created: ${neighbourhood_url}"
    fi

    # Inject hosting config into WE index.html (idempotent)
    if [ -n "${neighbourhood_url}" ]; then
        sed -i '/__WE_HOSTING_CONFIG/d' "${we_dist}/index.html"
        sed -i "s|</head>|<script>window.__WE_HOSTING_CONFIG={globalSpaceUrl:\"${neighbourhood_url}\"};</script></head>|" \
          "${we_dist}/index.html"
    fi
}

# ── Auto-auth: inject ad4m-connect credentials into WE so it skips interactive login ──
inject_we_auth() {
    local we_dist="/opt/ad4m/we-dist"

    if [ -f "${we_dist}/SKIPPED" ] || [ ! -f "${we_dist}/index.html" ]; then
        return
    fi

    if [ -z "${ADMIN_CREDENTIAL:-}" ]; then
        return
    fi

    # Remove previous injection (idempotent on restart — strip the tag, keep the rest of the line)
    sed -i 's|<script id="__AD4M_AUTO_AUTH">[^<]*</script>||g' "${we_dist}/index.html"

    # Two-stage escaping:
    #   1. JavaScript string literal: \ → \\  and  ' → \'
    #   2. sed replacement string:    \ → \\  and  & → \&  and  | → \|
    local safe_cred
    safe_cred=$(printf '%s' "${ADMIN_CREDENTIAL}" | sed "s/\\\\/\\\\\\\\/g; s/'/\\\\'/g" | sed 's/[\\&|]/\\&/g')

    sed -i "s|</head>|<script id=\"__AD4M_AUTO_AUTH\">localStorage.setItem('ad4m-token','${safe_cred}');localStorage.setItem('ad4m-url',window.location.origin);</script></head>|" \
      "${we_dist}/index.html"
    echo "Injected auto-auth credentials into WE frontend."
}

# ── WE reverse proxy (Caddy: static files + executor API on one origin) ───
WE_PID=""
start_we_server() {
    local we_dist="/opt/ad4m/we-dist"
    local we_port="${WE_PORT:-8080}"

    if [ -f "${we_dist}/SKIPPED" ] || [ ! -f "${we_dist}/index.html" ]; then
        echo "WE frontend not bundled — skipping reverse proxy."
        return
    fi

    local caddyfile="/tmp/Caddyfile"
    cat > "${caddyfile}" <<CADDYEOF
{
    admin off
}

:${we_port} {
    handle /health {
        reverse_proxy localhost:12000
    }

    handle /api/* {
        reverse_proxy localhost:12000
    }

    handle_path /apps/flux/* {
        root * /opt/ad4m/flux-dist
        try_files {path} /index.html
        file_server
        header -X-Frame-Options
    }

    handle {
        root * ${we_dist}
        try_files {path} /index.html
        file_server
    }
}
CADDYEOF

    echo "Starting Caddy reverse proxy on port ${we_port}..."
    caddy run --config "${caddyfile}" --adapter caddyfile &
    WE_PID=$!
    echo "WE frontend + API proxy serving at http://0.0.0.0:${we_port}/"
}

# ── Start executor in background, auto-generate agent, then wait ────────────
ad4m-executor run \
    --app-data-path /data \
    --localhost false \
    --run-dapp-server false \
    "${EXTRA_ARGS[@]}" \
    "$@" &

EXECUTOR_PID=$!

wait_for_executor
maybe_setup_agent
setup_ai_models || true
setup_global_space
inject_we_auth
start_we_server

# Forward SIGTERM/SIGINT to all child processes so Docker can stop cleanly.
cleanup() {
    kill -TERM "${EXECUTOR_PID}" 2>/dev/null
    [ -n "${WE_PID}" ] && kill -TERM "${WE_PID}" 2>/dev/null
}
trap cleanup TERM INT

# Wait for executor to exit; propagate its exit code.
wait "${EXECUTOR_PID}"
