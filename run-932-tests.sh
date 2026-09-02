#!/bin/bash
export PATH="$HOME/.deno/bin:$HOME/.local/go/bin:$HOME/.cargo/bin:$PATH"
cd /tmp/wt-932
{
echo "=== pnpm install ==="
pnpm install 2>&1 | tail -3
echo "=== pnpm build ==="
pnpm build 2>&1 | tail -10
echo "BUILD_EXIT=$?"
echo "=== fmt ==="
cargo fmt --all --check && echo FMT_OK || echo FMT_FAIL
cd rust-executor
echo "=== flow tests ==="
cargo test --release flow -- --test-threads=1 2>&1 | tail -20
echo "=== interpretation tests ==="
cargo test --release interpretation -- --test-threads=1 2>&1 | tail -10
echo "=== DONE $(date -Is) ==="
} > /tmp/wt-932/test-run.log 2>&1
