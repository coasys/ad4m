#!/usr/bin/env bash
# Browser (wasm32) smoke test for ad4m-client: starts a scratch
# ad4m-executor and runs rust-client/tests/wasm_browser.rs in a real
# browser through wasm-bindgen-test-runner.
#
#   bash scripts/test-wasm-client.sh                    # headless Chromium
#   DRIVER=firefox bash scripts/test-wasm-client.sh     # headless Firefox
#   EXECUTOR_PATH=/path/ad4m-executor bash scripts/test-wasm-client.sh
#   KEEP=1 ...                                          # keep the executor up
#
# Requires: wasm-bindgen-test-runner matching Cargo.lock's wasm-bindgen
#   (cargo install wasm-bindgen-cli --version $(grep -A1 'name = "wasm-bindgen"'
#   Cargo.lock | grep version | tr -d ' "version=') ), a chromedriver or
#   geckodriver on PATH or via CHROMEDRIVER/GECKODRIVER.
set -euo pipefail
cd "$(dirname "$0")/.."

# --- executor binary ---------------------------------------------------------
EXEC="${EXECUTOR_PATH:-}"
if [ -z "$EXEC" ]; then
    for c in "${CARGO_TARGET_DIR:-}/release/ad4m-executor" \
             "target/release/ad4m-executor"; do
        [ -n "$c" ] && [ -x "$c" ] && EXEC="$c" && break
    done
fi
[ -n "$EXEC" ] && [ -x "$EXEC" ] || {
    echo "no ad4m-executor binary found — set EXECUTOR_PATH" >&2; exit 1; }
echo "executor: $EXEC"

# --- scratch data dir ---------------------------------------------------------
DATA="$(mktemp -d)"
"$EXEC" init --data-path "$DATA" \
    --network-bootstrap-seed tests/js/bootstrapSeed.json >/dev/null

# --- free port + run ----------------------------------------------------------
PORT="${WASM_EXEC_PORT:-}"
if [ -z "$PORT" ]; then
    PORT="$(python3 -c 'import socket;s=socket.socket();s.bind(("127.0.0.1",0));print(s.getsockname()[1]);s.close()' 2>/dev/null || echo 12499)"
fi
"$EXEC" run \
    --app-data-path "$DATA" \
    --port "$PORT" \
    --run-holochain false \
    --language-language-only true \
    --run-dapp-server false \
    --insecure-no-admin-credential \
    >/dev/null 2>&1 &
EPID=$!
cleanup() { kill "$EPID" 2>/dev/null || true; rm -rf "$DATA"; }
[ "${KEEP:-0}" = "1" ] || trap cleanup EXIT

echo "waiting for executor on :$PORT ..."
up=""
for i in $(seq 1 240); do
    if curl -sf "http://127.0.0.1:$PORT/health" >/dev/null 2>&1; then up=1; break; fi
    sleep 0.5
done
[ -n "$up" ] || { echo "executor never came up on :$PORT" >&2; exit 1; }

# --- wasm test ----------------------------------------------------------------
export EXECUTOR_URL="http://127.0.0.1:$PORT"
export CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER="${WASM_RUNNER:-wasm-bindgen-test-runner}"

DRIVER="${DRIVER:-chromium}"
case "$DRIVER" in
    chromium|chrome) export CHROMEDRIVER="${CHROMEDRIVER:-/snap/bin/chromium.chromedriver}" ;;
    firefox)         export GECKODRIVER="${GECKODRIVER:-/snap/bin/geckodriver}" ;;
    *)               echo "DRIVER must be chromium or firefox" >&2; exit 1 ;;
esac

echo "wasm test in $DRIVER against $EXECUTOR_URL ..."
# -p is required: run from the workspace root, cargo would otherwise build
# every member for wasm32 (rust-executor's tokio/mio can't compile there).
cargo test -p ad4m-client --target wasm32-unknown-unknown \
    --test wasm_browser
