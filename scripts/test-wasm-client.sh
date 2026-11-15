#!/usr/bin/env bash
# Browser (wasm32) tests for ad4m-client: starts a scratch ad4m-executor
# WITH an admin credential (a real token, one that needs URL encoding) and
# the scripted peer scripts/wasm-test-server.py (fake WebSocket endpoints
# plus a connection-counting proxy to the executor), then runs the crate's
# in-browser unit tests (`--lib`, transport/browser.rs) and
# rust-client/tests/wasm_browser.rs in a real browser through
# wasm-bindgen-test-runner.
#
#   bash scripts/test-wasm-client.sh                    # headless Chromium
#   DRIVER=firefox bash scripts/test-wasm-client.sh     # headless Firefox
#   EXECUTOR_PATH=/path/ad4m-executor bash scripts/test-wasm-client.sh
#   KEEP=1 ...                                          # keep the executor up
#   WASM_TESTS=--lib bash scripts/test-wasm-client.sh [filter]  # one test crate
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
free_port() {
    python3 -c 'import socket;s=socket.socket();s.bind(("127.0.0.1",0));print(s.getsockname()[1]);s.close()'
}
# A real credential: random, with characters the query string must encode.
ADMIN_TOKEN="wasm admin+$(python3 -c 'import secrets;print(secrets.token_hex(12))')/=&?%"
AD4M_ADMIN_CREDENTIAL="$ADMIN_TOKEN" "$EXEC" run \
    --app-data-path "$DATA" \
    --port "$PORT" \
    --run-holochain false \
    --language-language-only true \
    --run-dapp-server false \
    >"$DATA.executor.log" 2>&1 &
EPID=$!
CONTROL_PORT="$(free_port)"
PROXY_PORT="$(free_port)"
python3 -I scripts/wasm-test-server.py --control "$CONTROL_PORT" \
    --proxy "$PROXY_PORT" --target "$PORT" &
SPID=$!
# Only the processes this script started; never by binary name.
cleanup() {
    kill "$EPID" "$SPID" 2>/dev/null || true
    rm -rf "$DATA" "$DATA.executor.log"
}
[ "${KEEP:-0}" = "1" ] || trap cleanup EXIT

echo "waiting for executor on :$PORT ..."
up=""
for i in $(seq 1 240); do
    if curl -sf "http://127.0.0.1:$PORT/health" >/dev/null 2>&1; then up=1; break; fi
    sleep 0.5
done
[ -n "$up" ] || {
    echo "executor never came up on :$PORT" >&2; tail -20 "$DATA.executor.log" >&2; exit 1; }

# --- wasm test ----------------------------------------------------------------
export EXECUTOR_URL="http://127.0.0.1:$PORT"
export EXECUTOR_PROXY_URL="http://127.0.0.1:$PROXY_PORT"
export FAKE_PEER_URL="http://127.0.0.1:$CONTROL_PORT"
export ADMIN_TOKEN
# A port nothing listens on: a refused TCP connect.
CLOSED_PORT="$(free_port)"
export CLOSED_PORT
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
# The env above is read at compile time (`option_env!`): it changes per run,
# so these test crates rebuild each time.
# WASM_TESTS picks the test crates (default both); arguments filter tests.
# shellcheck disable=SC2086
cargo test -p ad4m-client --target wasm32-unknown-unknown \
    ${WASM_TESTS:---lib --test wasm_browser} "$@"
