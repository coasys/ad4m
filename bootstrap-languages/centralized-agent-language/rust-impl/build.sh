#!/usr/bin/env bash
# Build the centralized-agent-language Rust ALDK crate into a single-file
# WASM ES module and copy the resulting bundle.js into ../build/ so the
# bootstrap loader picks it up like any other language bundle.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Reuse the known-good wasm-bindgen version lockfile from the agent-language
# crate so the schema versions match the installed wasm-bindgen-cli.
if [ ! -f Cargo.lock ]; then
    LOCKSRC="$SCRIPT_DIR/../../agent-language/rust-impl/Cargo.lock"
    if [ -f "$LOCKSRC" ]; then
        cp "$LOCKSRC" Cargo.lock
    fi
fi

echo "==> cargo build --target wasm32-unknown-unknown --release"
cargo build --target wasm32-unknown-unknown --release

WASM_IN="target/wasm32-unknown-unknown/release/centralized_agent_language.wasm"

if ! command -v wasm-bindgen >/dev/null 2>&1; then
    echo "!! wasm-bindgen not installed. Install with:" >&2
    echo "     cargo install wasm-bindgen-cli --version 0.2.117" >&2
    exit 1
fi

echo "==> wasm-bindgen --target deno"
rm -rf build-deno
wasm-bindgen --target deno --out-dir build-deno "$WASM_IN"

echo "==> inlining wasm into single-file bundle"
node "$SCRIPT_DIR/inline.mjs"

echo "==> copying bundle.js into ../build/"
mkdir -p "$SCRIPT_DIR/../build"
cp "$SCRIPT_DIR/build/bundle.js" "$SCRIPT_DIR/../build/bundle.js"

echo "==> centralized-agent-language (Rust ALDK) built:"
ls -lh "$SCRIPT_DIR/../build/bundle.js"
