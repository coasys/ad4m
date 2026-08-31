#!/bin/bash
set -euo pipefail
#
# Download default Kalosm models into the kalosm cache directory structure.
# Usage: download-models.sh <cache-root>
#
# Produces:
#   <cache-root>/<repo>/<revision>/<filename>
#
# The cache layout matches kalosm-common's Cache::get() expectations so the
# executor finds the files on disk and skips network downloads at runtime.

CACHE_ROOT="${1:?Usage: download-models.sh <cache-root>}"

fetch() {
    local repo="$1" revision="$2" filename="$3"
    local dir="${CACHE_ROOT}/${repo}/${revision}"
    local dest="${dir}/${filename}"
    local url="https://huggingface.co/${repo}/resolve/${revision}/${filename}"

    if [ -f "${dest}" ]; then
        echo "  [cached] ${repo}/${filename}"
        return
    fi

    mkdir -p "${dir}"
    echo "  [download] ${repo}/${filename}"
    # NOTE: no post-download integrity check (sha256/md5). The HF API
    # supports ETag-based caching but not content hashes in the URL.
    # For reproducible builds, pin `revision` to a commit SHA rather than
    # "main" and verify the downloaded file sizes in CI.
    curl -fSL --retry 3 --retry-delay 5 \
        -o "${dest}.partial" "${url}"
    mv "${dest}.partial" "${dest}"
}

echo "=== Embedding: BAAI/bge-small-en-v1.5 ==="
fetch "BAAI/bge-small-en-v1.5" "main" "model.safetensors"
fetch "BAAI/bge-small-en-v1.5" "main" "tokenizer.json"
fetch "BAAI/bge-small-en-v1.5" "main" "config.json"

echo "=== Transcription: openai/whisper-small ==="
fetch "openai/whisper-small" "main" "model.safetensors"
fetch "openai/whisper-small" "main" "tokenizer.json"
fetch "openai/whisper-small" "main" "config.json"

echo "=== Transcription: Whisper tiny quantized (lmz/candle-whisper) ==="
fetch "lmz/candle-whisper" "main" "model-tiny-q80.gguf"
fetch "lmz/candle-whisper" "main" "tokenizer-tiny.json"
fetch "lmz/candle-whisper" "main" "config-tiny.json"

echo "=== LLM: TinyLlama-1.1B-Chat Q4_K_M ==="
fetch "TheBloke/TinyLlama-1.1B-Chat-v1.0-GGUF" "main" "tinyllama-1.1b-chat-v1.0.Q4_K_M.gguf"
fetch "hf-internal-testing/llama-tokenizer" "main" "tokenizer.json"

echo "=== Done ==="
du -sh "${CACHE_ROOT}" 2>/dev/null || true
