# AI endpoints from the environment. Sourced by docker-entrypoint.sh, which defines
# `ws_rpc TYPE PARAMS ID` (one WS-RPC call as the operator); tested by
# docker/ai-env.test.sh with a recording `ws_rpc`.
#
# The executor runs no models itself: its AI goes to model servers. When the
# variables below are set, register those servers as endpoints and their models,
# and make each the default of its type when the type has none. Idempotent by
# name, so a restart registers nothing twice. The servers are registered in the
# order below, so the first one that names a model of a type supplies its default.
# See docker/compose.ai.yml.
#
#   AD4M_AI_OLLAMA_URL                       Ollama, for chat and embeddings (http://ollama:11434)
#   AD4M_AI_LLM_MODEL                        chat model on it, by its Ollama name
#   AD4M_AI_EMBEDDING_MODEL                  embedding model on it, by its Ollama name
#   AD4M_AI_LLAMA_SERVER_URL                 llama-server (http://llama-server:8080)
#   AD4M_AI_LLAMA_SERVER_API_KEY             its --api-key, when it was started with one
#   AD4M_AI_LLAMA_SERVER_LLM_MODEL           chat model on it, by the id /v1/models lists
#   AD4M_AI_LLAMA_SERVER_EMBEDDING_MODEL     embedding model on it (a server started with --embeddings)
#   AD4M_AI_OPENAI_COMPATIBLE_URL            any other OpenAI-shaped server (vLLM, LocalAI, a gateway)
#   AD4M_AI_OPENAI_COMPATIBLE_API_KEY        its key (sent over https or to loopback only)
#   AD4M_AI_OPENAI_COMPATIBLE_LLM_MODEL      chat model on it
#   AD4M_AI_OPENAI_COMPATIBLE_EMBEDDING_MODEL  embedding model on it
#   AD4M_AI_TRANSCRIPTION_URL                OpenAI-shaped transcription server (http://speaches:8000)
#   AD4M_AI_TRANSCRIPTION_API_KEY            its key, when it takes one
#   AD4M_AI_TRANSCRIPTION_MODEL              model on it, by the id the server knows it by

# Answers the id of the endpoint named $1, registering it (kind $2, URL $3, key $4
# when not empty) first when there is none. Empty on failure.
ensure_ai_endpoint() {
    local name="$1" kind="$2" url="$3" key="${4:-}" id
    id=$(ws_rpc "ai.endpoints" "{}" "list-endpoints" \
        | jq -r --arg n "${name}" '.result[]? | select(.name == $n) | .id')
    if [ -z "${id}" ]; then
        id=$(ws_rpc "ai.addEndpoint" \
            "$(jq -cn --arg n "${name}" --arg k "${kind}" --arg u "${url}" --arg key "${key}" \
                '{name: $n, kind: $k, baseUrl: $u} + (if $key == "" then {} else {apiKey: $key} end)')" \
            "add-endpoint" \
            | jq -r '.result.id // empty')
        [ -n "${id}" ] && echo "  Registered AI endpoint ${name} (${url})" >&2
    fi
    echo "${id}"
}

# Registers model $1 of type $2 as $4 on endpoint $3, unless a model has that
# name, and makes it the default of its type when there is none.
ensure_ai_model() {
    local name="$1" type="$2" endpoint_id="$3" remote="$4" id
    id=$(ws_rpc "ai.models" "{}" "list-models" \
        | jq -r --arg n "${name}" '.result[]? | select(.name == $n) | .id')
    if [ -z "${id}" ]; then
        id=$(ws_rpc "ai.addModel" \
            "$(jq -cn --arg n "${name}" --arg t "${type}" --arg e "${endpoint_id}" --arg r "${remote}" \
                '{model: {name: $n, modelType: $t, endpointId: $e, remoteModel: $r}}')" "add-model" \
            | jq -r '.result // empty')
        if [ -z "${id}" ]; then
            echo "  WARNING: failed to register AI model ${name}." >&2
            return
        fi
        echo "  Registered ${type} model ${name} (${remote})"
    fi
    if ws_rpc "ai.getDefaultModel" "{\"modelType\":\"${type}\"}" "get-default" \
        | jq -e '.result == null' >/dev/null 2>&1; then
        ws_rpc "ai.setDefaultModel" "{\"id\":\"${id}\",\"modelType\":\"${type}\"}" "set-default" >/dev/null
        echo "  ${name} is the default ${type} model."
    fi
}

# One server: endpoint $1 of kind $2 at $3 with key $4, and its chat model $5,
# embedding model $6 and transcription model $7 (each skipped when empty).
register_ai_server() {
    local name="$1" kind="$2" url="$3" key="$4" llm="$5" embedding="$6" transcription="$7" id
    id=$(ensure_ai_endpoint "${name}" "${kind}" "${url}" "${key}")
    if [ -z "${id}" ]; then
        echo "  WARNING: failed to register the ${name} endpoint." >&2
        return
    fi
    [ -n "${llm}" ] && ensure_ai_model "${llm}" "LLM" "${id}" "${llm}"
    [ -n "${embedding}" ] && ensure_ai_model "${embedding}" "EMBEDDING" "${id}" "${embedding}"
    [ -n "${transcription}" ] && ensure_ai_model "${transcription}" "TRANSCRIPTION" "${id}" "${transcription}"
    return 0
}

setup_ai_models() {
    if [ -z "${AD4M_AI_OLLAMA_URL:-}${AD4M_AI_LLAMA_SERVER_URL:-}${AD4M_AI_OPENAI_COMPATIBLE_URL:-}${AD4M_AI_TRANSCRIPTION_URL:-}" ]; then
        return 0
    fi
    if ! command -v websocat >/dev/null 2>&1; then
        echo "WARNING: websocat not found, skipping AI endpoint registration." >&2
        return 0
    fi

    echo "Registering AI endpoints..."
    if [ -n "${AD4M_AI_OLLAMA_URL:-}" ]; then
        register_ai_server "ollama" "OLLAMA" "${AD4M_AI_OLLAMA_URL}" "" \
            "${AD4M_AI_LLM_MODEL:-}" "${AD4M_AI_EMBEDDING_MODEL:-}" ""
    fi
    if [ -n "${AD4M_AI_LLAMA_SERVER_URL:-}" ]; then
        register_ai_server "llama-server" "LLAMA_SERVER" "${AD4M_AI_LLAMA_SERVER_URL}" \
            "${AD4M_AI_LLAMA_SERVER_API_KEY:-}" "${AD4M_AI_LLAMA_SERVER_LLM_MODEL:-}" \
            "${AD4M_AI_LLAMA_SERVER_EMBEDDING_MODEL:-}" ""
    fi
    if [ -n "${AD4M_AI_OPENAI_COMPATIBLE_URL:-}" ]; then
        register_ai_server "openai-compatible" "OPEN_AI_COMPATIBLE" "${AD4M_AI_OPENAI_COMPATIBLE_URL}" \
            "${AD4M_AI_OPENAI_COMPATIBLE_API_KEY:-}" "${AD4M_AI_OPENAI_COMPATIBLE_LLM_MODEL:-}" \
            "${AD4M_AI_OPENAI_COMPATIBLE_EMBEDDING_MODEL:-}" ""
    fi
    if [ -n "${AD4M_AI_TRANSCRIPTION_URL:-}" ]; then
        register_ai_server "transcription" "OPEN_AI_AUDIO" "${AD4M_AI_TRANSCRIPTION_URL}" \
            "${AD4M_AI_TRANSCRIPTION_API_KEY:-}" "" "" "${AD4M_AI_TRANSCRIPTION_MODEL:-}"
    fi
    return 0
}

# Local AI the image ships (INCLUDE_MODELS=true): the store $2 and the selection the
# image build kept in $1. Points the executor at the store (unless AD4M_LOCAL_AI_DIR
# names another) and gives the executor's state $3 the selection once, so local AI
# starts with the executor (local_ai_autostart) with no download. A selection the
# executor has already (a later ai.localInstall) is kept.
seed_local_ai() {
    local image_state="$1" image_store="$2" state="$3"
    [ -f "${image_state}/selection.json" ] || return 0
    export AD4M_LOCAL_AI_DIR="${AD4M_LOCAL_AI_DIR:-${image_store}}"
    if [ ! -f "${state}/selection.json" ]; then
        mkdir -p "${state}"
        cp "${image_state}/selection.json" "${state}/selection.json"
        echo "Local AI from the image: its runtimes and models start with the executor."
    fi
    return 0
}
