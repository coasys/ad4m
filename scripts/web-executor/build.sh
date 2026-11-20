#!/usr/bin/env bash
# Builds the web executor (S9): the `web-executor` crate for wasm32 in release, then its JS
# glue with the wasm-bindgen CLI. From the repo root:
#   bash scripts/web-executor/build.sh            # Node glue in web-executor/pkg/node
#   bash scripts/web-executor/build.sh web        # browser/Worker glue in web-executor/pkg/web
#
# The CLI must be the version of the `wasm-bindgen` crate in Cargo.lock (the glue and the
# Wasm speak one schema): `cargo install wasm-bindgen-cli --version <that version>`.
# Then, if `wasm-opt` is on the PATH, `wasm-opt -Oz` runs over the bindgen output (set
# WASM_OPT=0 to skip it). Prints the size of each stage, raw and gzip -9.
# Uses CARGO_TARGET_DIR when it is set.

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"
JS_TARGET="${1:-nodejs}"
case "$JS_TARGET" in
nodejs | node) JS_TARGET=nodejs OUT="web-executor/pkg/node" ;;
web) OUT="web-executor/pkg/web" ;;
*) echo "usage: $0 [node|web]" >&2; exit 2 ;;
esac

LOCKED=$(awk '/^name = "wasm-bindgen"$/ { getline; gsub(/version = |"/, ""); print; exit }' Cargo.lock)
CLI=$(wasm-bindgen --version 2>/dev/null | awk '{ print $2 }') || true
if [ "$CLI" != "$LOCKED" ]; then
    echo "wasm-bindgen CLI is ${CLI:-missing}, Cargo.lock has $LOCKED:" >&2
    echo "  cargo install wasm-bindgen-cli --version $LOCKED" >&2
    exit 1
fi

cargo build --release -p web-executor --target wasm32-unknown-unknown
TARGET_DIR="${CARGO_TARGET_DIR:-$ROOT/target}"
WASM="$TARGET_DIR/wasm32-unknown-unknown/release/web_executor.wasm"

size() {
    local raw gz
    raw=$(wc -c <"$1")
    gz=$(gzip -9 -c "$1" | wc -c)
    awk -v n="$2" -v r="$raw" -v g="$gz" \
        'BEGIN { printf "web-executor size (%s): raw %d bytes (%.1f MiB), gzip %d bytes (%.1f MiB)\n", n, r, r / 1048576, g, g / 1048576 }'
}

size "$WASM" cargo
wasm-bindgen --target "$JS_TARGET" --out-dir "$OUT" "$WASM"
BG="$OUT/web_executor_bg.wasm"
size "$BG" wasm-bindgen
if [ "${WASM_OPT:-1}" != 0 ] && command -v wasm-opt >/dev/null 2>&1; then
    # The features rustc's wasm32 target turns on by default.
    wasm-opt -Oz --enable-bulk-memory --enable-nontrapping-float-to-int --enable-sign-ext \
        --enable-mutable-globals --enable-reference-types --enable-multivalue \
        "$BG" -o "$BG.opt"
    mv "$BG.opt" "$BG"
    size "$BG" "wasm-opt -Oz"
fi
echo "web-executor glue: $OUT"
