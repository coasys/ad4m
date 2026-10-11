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

    crate_name="${lang//-/_}"
    wasm_input="$SCRIPT_DIR/$lang/target/wasm32-unknown-unknown/release/${crate_name}.wasm"
    deno_out="$SCRIPT_DIR/$lang/build-deno"

    if command -v wasm-bindgen >/dev/null 2>&1; then
        echo "==> wasm-bindgen --target deno → $deno_out"
        rm -rf "$deno_out"
        wasm-bindgen --target deno --out-dir "$deno_out" "$wasm_input"

        if command -v node >/dev/null 2>&1; then
            echo "==> inlining wasm into single bundle"
            node "$SCRIPT_DIR/inline-wasm.mjs" "$SCRIPT_DIR/$lang"
        else
            echo "==> node not found; skipping bundle inlining" >&2
        fi
    else
        echo "==> wasm-bindgen not installed; skipping JS glue generation." >&2
        echo "    Install with: cargo install wasm-bindgen-cli --version 0.2.117" >&2
    fi
done

echo "==> Done."
