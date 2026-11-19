#!/bin/bash
# Each case sets its variables in a subshell of its own, on purpose.
# shellcheck disable=SC2030,SC2031
# Tests of docker/ai-env.sh (AI endpoints from the environment): bash docker/ai-env.test.sh
# `ws_rpc` here is a fake executor that keeps endpoints, models and defaults in files and
# records every call, so each case reads what the entrypoint would have asked for.
set -u

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="${AI_ENV_SCRIPT:-${here}/ai-env.sh}"
failures=0

state=""
fresh_executor() {
    state=$(mktemp -d)
    echo '[]' > "${state}/endpoints"
    echo '[]' > "${state}/models"
    echo '{}' > "${state}/defaults"
    : > "${state}/calls"
    # setup_ai_models only checks that websocat exists; ws_rpc below never runs it.
    mkdir -p "${state}/bin"
    printf '#!/bin/sh\nexit 0\n' > "${state}/bin/websocat"
    chmod +x "${state}/bin/websocat"
    PATH="${state}/bin:${PATH}"
}

ws_rpc() {
    local type="$1" params="$2" n
    echo "${type} ${params}" >> "${state}/calls"
    case "${type}" in
        ai.endpoints) jq -c '{result: .}' "${state}/endpoints" ;;
        ai.models) jq -c '{result: .}' "${state}/models" ;;
        ai.addEndpoint)
            n=$(jq length "${state}/endpoints")
            jq -c --argjson p "${params}" --arg id "endpoint-${n}" '. + [$p + {id: $id}]' \
                "${state}/endpoints" > "${state}/tmp" && mv "${state}/tmp" "${state}/endpoints"
            echo "{\"result\":{\"id\":\"endpoint-${n}\"}}"
            ;;
        ai.addModel)
            n=$(jq length "${state}/models")
            jq -c --argjson p "${params}" --arg id "model-${n}" '. + [$p.model + {id: $id}]' \
                "${state}/models" > "${state}/tmp" && mv "${state}/tmp" "${state}/models"
            echo "{\"result\":\"model-${n}\"}"
            ;;
        ai.getDefaultModel)
            jq -c --argjson p "${params}" '{result: .[$p.modelType]}' "${state}/defaults"
            ;;
        ai.setDefaultModel)
            jq -c --argjson p "${params}" '. + {($p.modelType): $p.id}' \
                "${state}/defaults" > "${state}/tmp" && mv "${state}/tmp" "${state}/defaults"
            echo '{"result":true}'
            ;;
    esac
}

check() {
    local what="$1" expected="$2" actual="$3"
    if [ "${expected}" = "${actual}" ]; then
        echo "ok - ${what}"
    else
        echo "not ok - ${what}"
        echo "  expected: ${expected}"
        echo "  actual:   ${actual}"
        failures=$((failures + 1))
    fi
}

endpoints() { jq -c '[.[] | {name, kind, baseUrl, apiKey}]' "${state}/endpoints"; }
models() { jq -c '[.[] | {name, modelType, endpointId, remoteModel}]' "${state}/models"; }
defaults() { jq -c -S . "${state}/defaults"; }

run_setup() {
    (
        # shellcheck source=/dev/null
        . "${script}"
        setup_ai_models
    ) > "${state}/out" 2>&1
}

unset_ai_env() {
    local var
    for var in $(compgen -e | grep '^AD4M_AI_'); do unset "${var}"; done
}

# ── llama-server and an OpenAI-compatible server, with keys ──────────────────
unset_ai_env
fresh_executor
export AD4M_AI_LLAMA_SERVER_URL=http://llama-server:8080
export AD4M_AI_LLAMA_SERVER_API_KEY=sk-llama
export AD4M_AI_LLAMA_SERVER_LLM_MODEL=Qwen3.5-4B-Q4_K_M
export AD4M_AI_LLAMA_SERVER_EMBEDDING_MODEL=nomic-embed-text-v1.5
export AD4M_AI_OPENAI_COMPATIBLE_URL=https://gateway.example/v1
export AD4M_AI_OPENAI_COMPATIBLE_API_KEY=sk-gateway
export AD4M_AI_OPENAI_COMPATIBLE_LLM_MODEL=gpt-oss-120b
run_setup
check "llama-server and the OpenAI-compatible server are endpoints, with their keys" \
    '[{"name":"llama-server","kind":"LLAMA_SERVER","baseUrl":"http://llama-server:8080","apiKey":"sk-llama"},{"name":"openai-compatible","kind":"OPEN_AI_COMPATIBLE","baseUrl":"https://gateway.example/v1","apiKey":"sk-gateway"}]' \
    "$(endpoints)"
check "their models are registered on them" \
    '[{"name":"Qwen3.5-4B-Q4_K_M","modelType":"LLM","endpointId":"endpoint-0","remoteModel":"Qwen3.5-4B-Q4_K_M"},{"name":"nomic-embed-text-v1.5","modelType":"EMBEDDING","endpointId":"endpoint-0","remoteModel":"nomic-embed-text-v1.5"},{"name":"gpt-oss-120b","modelType":"LLM","endpointId":"endpoint-1","remoteModel":"gpt-oss-120b"}]' \
    "$(models)"
check "the first server to name a type supplies its default" \
    '{"EMBEDDING":"model-1","LLM":"model-0"}' "$(defaults)"
check "no key is printed" "" "$(grep -E 'sk-llama|sk-gateway' "${state}/out")"

# A restart registers nothing twice.
calls_before=$(grep -c '^ai.add' "${state}/calls")
run_setup
check "a restart adds nothing" "${calls_before}" "$(grep -c '^ai.add' "${state}/calls")"

# ── Ollama and a transcription server (the variables that were there before) ─
unset_ai_env
fresh_executor
export AD4M_AI_OLLAMA_URL=http://ollama:11434
export AD4M_AI_LLM_MODEL=qwen3:8b
export AD4M_AI_EMBEDDING_MODEL=nomic-embed-text
export AD4M_AI_TRANSCRIPTION_URL=http://speaches:8000
export AD4M_AI_TRANSCRIPTION_MODEL=Systran/faster-whisper-small
run_setup
check "Ollama and the transcription server are endpoints, without a key" \
    '[{"name":"ollama","kind":"OLLAMA","baseUrl":"http://ollama:11434","apiKey":null},{"name":"transcription","kind":"OPEN_AI_AUDIO","baseUrl":"http://speaches:8000","apiKey":null}]' \
    "$(endpoints)"
check "their models are registered and are the defaults" \
    '{"EMBEDDING":"model-1","LLM":"model-0","TRANSCRIPTION":"model-2"}' "$(defaults)"

# A transcription key (OpenAI's own transcription) goes with its endpoint.
unset_ai_env
fresh_executor
export AD4M_AI_TRANSCRIPTION_URL=https://api.openai.com
export AD4M_AI_TRANSCRIPTION_API_KEY=sk-openai
export AD4M_AI_TRANSCRIPTION_MODEL=gpt-4o-transcribe
run_setup
check "a transcription key goes with its endpoint" \
    '[{"name":"transcription","kind":"OPEN_AI_AUDIO","baseUrl":"https://api.openai.com","apiKey":"sk-openai"}]' \
    "$(endpoints)"

# ── Local AI the image ships (INCLUDE_MODELS=true) ──────────────────────────
seed_dir=$(mktemp -d)
mkdir -p "${seed_dir}/image-state" "${seed_dir}/store"
echo '{"runtimes":[],"models":[]}' > "${seed_dir}/image-state/selection.json"
seeded=$(
    unset AD4M_LOCAL_AI_DIR
    # shellcheck source=/dev/null
    . "${script}"
    seed_local_ai "${seed_dir}/image-state" "${seed_dir}/store" "${seed_dir}/data/local-ai" >/dev/null
    echo "${AD4M_LOCAL_AI_DIR}"
)
check "the image's store is the executor's local AI store" "${seed_dir}/store" "${seeded}"
check "the image's selection is the executor's" \
    '{"runtimes":[],"models":[]}' "$(cat "${seed_dir}/data/local-ai/selection.json")"
echo '{"runtimes":[{"x":1}],"models":[]}' > "${seed_dir}/data/local-ai/selection.json"
(
    # shellcheck source=/dev/null
    . "${script}"
    seed_local_ai "${seed_dir}/image-state" "${seed_dir}/store" "${seed_dir}/data/local-ai" >/dev/null
)
check "a selection the executor has already is kept" \
    '{"runtimes":[{"x":1}],"models":[]}' "$(cat "${seed_dir}/data/local-ai/selection.json")"
other=$(
    export AD4M_LOCAL_AI_DIR=/elsewhere
    # shellcheck source=/dev/null
    . "${script}"
    seed_local_ai "${seed_dir}/image-state" "${seed_dir}/store" "${seed_dir}/data2" >/dev/null
    echo "${AD4M_LOCAL_AI_DIR}"
)
check "an operator's AD4M_LOCAL_AI_DIR wins" "/elsewhere" "${other}"
empty=$(
    unset AD4M_LOCAL_AI_DIR
    # shellcheck source=/dev/null
    . "${script}"
    seed_local_ai "${seed_dir}/none" "${seed_dir}/store" "${seed_dir}/data3"
    echo "dir=${AD4M_LOCAL_AI_DIR:-}"
)
check "an image without local AI keeps the store in the data volume" \
    "dir=${seed_dir}/data3/store" "${empty}"
check "  and makes no state" "no" "$([ -e "${seed_dir}/data3" ] && echo yes || echo no)"
kept=$(
    export AD4M_LOCAL_AI_DIR=/elsewhere
    # shellcheck source=/dev/null
    . "${script}"
    seed_local_ai "${seed_dir}/none" "${seed_dir}/store" "${seed_dir}/data4"
    echo "dir=${AD4M_LOCAL_AI_DIR:-}"
)
check "  unless AD4M_LOCAL_AI_DIR names another" "dir=/elsewhere" "${kept}"

# ── Nothing set: nothing is asked ───────────────────────────────────────────
unset_ai_env
fresh_executor
run_setup
check "without AI variables the executor is not asked anything" "" "$(cat "${state}/calls")"

if [ "${failures}" -gt 0 ]; then
    echo "${failures} failed"
    exit 1
fi
echo "all passed"
