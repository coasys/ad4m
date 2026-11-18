#!/usr/bin/env bash
# Run the full LLM-E2E suite: Rust `#[ignore = "llm-e2e"]` tests + JS suites
# gated by `LLM_E2E=1`.
#
# What runs where (both hit the same OpenAI-compatible endpoint):
#   Rust — perspectives::interpretation_e2e,
#          perspectives::interpretation_harness_e2e,
#          perspectives::flow_context::real_llm_e2e,
#          (any other perspectives::* modules with `#[ignore = "llm-e2e"]`)
#   JS   — tests/js/tests/model/{run-interpretation,interpretation-models,
#          auto-processor,auto-processor-observability}.test.ts,
#          tests/js/tests/auto-processor-multi-user.test.ts,
#          tests/js/tests/auto-processor-neighbourhood.ts (via integration)
#
# Endpoint + model — env-overridable. Defaults are staging's llama-server
# (systemd user unit llama-qwen27b.service). This script does not start it:
#   INTERPRETATION_E2E_BASE_URL      (default http://127.0.0.1:12460/v1)
#   INTERPRETATION_E2E_MODEL         (default qwen3.8-27b-ud-q4_k_m)
#   INTERPRETATION_E2E_ENDPOINT_KIND (default LLAMA_SERVER)
#   INTERPRETATION_E2E_MAX_NUM_CTX   (default 57344, under the 65536 slot)
#   INTERPRETATION_E2E_REQUEST_TIMEOUT_SECS (default 900)
#   INTERPRETATION_E2E_API_KEY       (unset: llama-server takes no key)
# Embeddings stay on Ollama's nomic-embed-text:
#   INTERPRETATION_EMBED_BASE_URL    (default http://127.0.0.1:11434/v1)
#   INTERPRETATION_EMBED_MODEL       (default nomic-embed-text)
#
# The Rust leg below is `cargo test --release`, matching the nightly job.
# A debug run on this host omits `--release` and keeps CARGO_TARGET_DIR on
# the worktree's existing debug target.
#
# Skip one leg with SKIP_RUST=1 or SKIP_JS=1.
set -euo pipefail

REPO_ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$REPO_ROOT"

failed=0

if [[ -z "${SKIP_RUST:-}" ]]; then
  echo "=== Rust LLM-E2E ==="
  # `--ignored` runs ONLY the ignored tests; that's exactly the set we want.
  # `--test-threads=1` because these share one llama-server slot.
  if ! cargo test --release --lib \
      perspectives::interpretation_e2e \
      perspectives::interpretation_harness_e2e \
      perspectives::flow_context::real_llm_e2e \
      -- --ignored --test-threads=1 --nocapture; then
    failed=1
    echo "!!! Rust LLM-E2E failed"
  fi
else
  echo "=== Rust LLM-E2E — SKIPPED (SKIP_RUST=1) ==="
fi

if [[ -z "${SKIP_JS:-}" ]]; then
  echo "=== JS LLM-E2E ==="
  export LLM_E2E=1
  # prepare-test publishes perspective-diff-sync. The multi-user suite
  # joins that neighbourhood (a private perspective is owner-scoped), and
  # the integration leg needs the same publish. The multi-user file is
  # not part of `test-model`.
  if ! (cd tests/js && pnpm run test-model \
      && pnpm run prepare-test \
      && pnpm exec ts-mocha -p tsconfig.json --timeout 1200000 --exit \
           tests/auto-processor-multi-user.test.ts \
      && pnpm run test-integration); then
    failed=1
    echo "!!! JS LLM-E2E failed"
  fi
else
  echo "=== JS LLM-E2E — SKIPPED (SKIP_JS=1) ==="
fi

exit "$failed"
