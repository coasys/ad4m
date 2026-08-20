export PATH="$HOME/.deno/bin:$HOME/.local/go/bin:$HOME/.cargo/bin:$PATH"
cd /home/data/code/ad4m/.worktrees/extraction-ws-ts
echo "=== building executor (incremental) ==="
cargo build --release -p ad4m-executor 2>&1 | tail -3
echo "=== running test-mcp (no LLM needed) ==="
cd tests/js
pnpm run test-mcp 2>&1
echo "=== __MCP_REPRO_DONE__ exit $? ==="
