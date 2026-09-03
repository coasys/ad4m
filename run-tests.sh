#!/bin/bash
set -o pipefail
export PATH="$HOME/.deno/bin:$HOME/.local/go/bin:$HOME/.cargo/bin:$PATH"
LOG=/tmp/wt-932/test-run.log
exec > "$LOG" 2>&1
echo "=== wasm-bindgen-cli 0.2.118 sidecar ==="
if [ ! -x /tmp/wbg118/bin/wasm-bindgen ]; then
  cargo install wasm-bindgen-cli --version 0.2.118 --root /tmp/wbg118 || { echo "WBG_INSTALL_FAILED"; exit 1; }
fi
export PATH="/tmp/wbg118/bin:$PATH"
wasm-bindgen --version
cd /tmp/wt-932 || exit 1
echo "=== pnpm build ==="
pnpm build || { echo "BUILD_FAILED"; exit 1; }
echo "=== fmt check ==="
cargo fmt --all --check && echo FMT_OK || echo FMT_DIRTY
echo "=== flow tests ==="
cd rust-executor || exit 1
cargo test --release flow -- --test-threads=1 || { echo "FLOW_TESTS_FAILED"; exit 1; }
echo "=== interpretation tests ==="
cargo test --release interpretation -- --test-threads=1 || { echo "INTERP_TESTS_FAILED"; exit 1; }
echo "=== ALL_GREEN $(date -Iseconds) ==="
