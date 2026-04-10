#!/usr/bin/env bash
# Build all Rust WASM test languages against the wasm32 target.
#
# Requires:
#   rustup target add wasm32-unknown-unknown
#
# Optionally (for post-processing to JS glue):
#   cargo install wasm-bindgen-cli
#
# The raw .wasm output lands in:
#   tests/rust-languages/<lang>/target/wasm32-unknown-unknown/release/<lang>.wasm
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

LANGUAGES=(
    "test-wasm-language"
)

for lang in "${LANGUAGES[@]}"; do
    echo "==> Building $lang (wasm32-unknown-unknown, release)"
    pushd "$SCRIPT_DIR/$lang" >/dev/null
    cargo build --target wasm32-unknown-unknown --release
    popd >/dev/null
done

echo "==> Done."
