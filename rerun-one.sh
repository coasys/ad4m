#!/bin/bash
export PATH="/tmp/wbg118/bin:$HOME/.deno/bin:$HOME/.local/go/bin:$HOME/.cargo/bin:$PATH"
cd /tmp/wt-932/rust-executor || exit 1
exec > /tmp/wt-932/rerun-one.log 2>&1
cargo test --release auto_processor_two_users_one_executor_no_double_processing -- --test-threads=1 && echo "=== RERUN_GREEN ===" || echo "=== RERUN_FAILED ==="
