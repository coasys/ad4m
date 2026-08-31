#!/usr/bin/env bash
# Run the full LLM-E2E suite: Rust `#[ignore = "llm-e2e"]` tests + JS suites
# gated by `LLM_E2E=1`.
#
# What runs where (both hit the same OpenAI-compatible endpoint):
#   Rust — perspectives::interpretation_e2e,
#          perspectives::interpretation_harness_e2e,
#          (any other perspectives::* modules with `#[ignore = "llm-e2e"]`)
#   JS   — tests/js/tests/model/{run-interpretation,interpretation-models,
#          auto-processor,auto-processor-observability}.test.ts,
#          tests/js/tests/auto-processor-multi-user.test.ts,
#          tests/js/tests/auto-processor-neighbourhood.ts (via integration)
#
# Endpoint + model — env-overridable, defaults hit Ollama at localhost:
#   INTERPRETATION_E2E_BASE_URL (default http://localhost:11434/v1)
#   INTERPRETATION_E2E_MODEL    (default gemma3:12b)
#   INTERPRETATION_E2E_API_KEY  (only if the endpoint requires it)
#
# From a dev box, tunnel Marvin's Ollama first:
#   ssh -L 11434:localhost:11434 marvin
#
# Skip one leg with SKIP_RUST=1 or SKIP_JS=1.
set -euo pipefail

REPO_ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")"/.. && pwd)"
cd "$REPO_ROOT"

failed=0

if [[ -z "${SKIP_RUST:-}" ]]; then
  echo "=== Rust LLM-E2E ==="
  # `--ignored` runs ONLY the ignored tests; that's exactly the set we want.
  # `--test-threads=1` because these hit a single Ollama endpoint.
  if ! cargo test --release --lib \
      perspectives::interpretation_e2e \
      perspectives::interpretation_harness_e2e \
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
  # tests/js expects `pnpm run prepare-test` to have run at least once.
  if ! (cd tests/js && pnpm run test-model && pnpm run test-integration); then
    failed=1
    echo "!!! JS LLM-E2E failed"
  fi
else
  echo "=== JS LLM-E2E — SKIPPED (SKIP_JS=1) ==="
fi

exit "$failed"
