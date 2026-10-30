# Shared by every verify-*.sh script: image pins, paths, the hardened executor
# container, and pod teardown. Source it after setting TAG (the log prefix, e.g.
# "a2h") and NET (the pod's private network); then call `pod_init <containers…>`.
#
# Images (override per run):
#   AD4M_EXEC_IMAGE  executor image built from this checkout by
#                    interop/executor/build-image.sh
#   OC_IMG           OpenClaw, pinned by digest
#   HERMES_IMAGE     Hermes, pinned by digest
#   SOVEREIGN_IMAGE  a locally built Sovereign image (interop/agents/sovereign/Dockerfile)
# AD4M_PLUGIN_DIR    the OpenClaw ad4m plugin (default: this repo's plugins/ad4m)
# AGENT_HARNESS_LOG_DIR  when set, a failing script saves each container's log there
# KEEP=1             leave the pod running (no teardown)

AH_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)" # tests/agent-harness
REPO_ROOT="$(cd "$AH_ROOT/../.." && pwd)"
AGENTS="$AH_ROOT/interop/agents"
TSX="$AH_ROOT/node_modules/.bin/tsx"

EXEC_IMG="${AD4M_EXEC_IMAGE:-ad4m-agent-harness-executor:local}"
OC_IMG="${OC_IMG:-ghcr.io/openclaw/openclaw@sha256:2f5ce8848a1a69b3c460622e566cb9395da9fd18d7ef7b038cd8e2c4f195decf}"
HER_IMG="${HERMES_IMAGE:-nousresearch/hermes-agent@sha256:e418b2648b6e5bbf3195c4e30c14b939c9d455ac5dc04faada1ff4c96580bc2e}"
SV_IMG="${SOVEREIGN_IMAGE:-swwt-sovereign:latest}"
MOCK_IMG=ad4m-agent-harness-mockllm:local
PLUGIN_DIR="${AD4M_PLUGIN_DIR:-$REPO_ROOT/plugins/ad4m}"
ADMIN=windtunnel-admin
KEEP="${KEEP:-}"

[ -x "$TSX" ] || { echo "[$TAG] FAIL — $TSX missing: run pnpm install --ignore-workspace in tests/agent-harness"; exit 1; }
docker image inspect "$EXEC_IMG" >/dev/null 2>&1 || {
  echo "[$TAG] FAIL — executor image $EXEC_IMG missing: build it with interop/executor/build-image.sh"
  exit 1
}

# Pull a third-party image unless present; SKIP the route when it cannot be had.
need_image() {
  docker image inspect "$1" >/dev/null 2>&1 && return 0
  echo "[$TAG] pull $1"
  timeout 900 docker pull -q "$1" >/dev/null 2>&1 || { echo "[$TAG] SKIP — image $1 unavailable"; exit 0; }
}

POD_CONTAINERS=()
pod_down() {
  docker rm -f "${POD_CONTAINERS[@]}" >/dev/null 2>&1 || true
  docker network rm "$NET" >/dev/null 2>&1 || true
}
pod_teardown() {
  local rc=$?
  if [ "$rc" -ne 0 ] && [ -n "${AGENT_HARNESS_LOG_DIR:-}" ]; then
    mkdir -p "$AGENT_HARNESS_LOG_DIR"
    for c in "${POD_CONTAINERS[@]}"; do
      docker logs "$c" >"$AGENT_HARNESS_LOG_DIR/$TAG-$c.log" 2>&1 || true
    done
  fi
  if [ -n "$KEEP" ]; then echo "[$TAG] KEEP set — leaving pod up"; return; fi
  pod_down
}
# pod_init <container…>: clean slate, teardown trap, fresh private network.
pod_init() {
  POD_CONTAINERS=("$@")
  pod_down
  trap pod_teardown EXIT
  trap 'exit 143' TERM INT # the runner's timeout: still tear the pod down
  docker network create "$NET" >/dev/null
}

build_mock() { docker build -q -t "$MOCK_IMG" "$AGENTS/mock-llm" >/dev/null; }

# Model calls the mock container <name> has served (it must run with MOCK_LLM_LOG=1).
mock_calls() { docker logs "$1" 2>&1 | grep -cE '^=== POST .*(chat/completions|/messages)' || true; }
# Wait until <name>'s call count has not moved for 6 s (at most 60 s), so turns
# started before (e.g. by a readiness probe) are counted before a baseline.
settle_mock_calls() {
  local last=-1 now same=0
  for _ in $(seq 1 30); do
    now=$(mock_calls "$1")
    if [ "$now" = "$last" ]; then same=$((same + 1)); [ "$same" -ge 3 ] && return 0; else same=0; fi
    last=$now
    sleep 2
  done
}

# Install and build the plugin checkout (dist + node_modules). The plugin is an
# npm package with its own lockfile and CI job; NODE_ENV=development keeps devDeps.
build_plugin() {
  [ -f "$PLUGIN_DIR/package.json" ] || { echo "[$TAG] SKIP — no ad4m plugin at $PLUGIN_DIR"; exit 0; }
  ( cd "$PLUGIN_DIR" && NODE_ENV=development npm install --include=dev >/dev/null 2>&1 && NODE_ENV=development npm run build >/dev/null 2>&1 ) \
    || { echo "[$TAG] FAIL — could not build the ad4m plugin in $PLUGIN_DIR"; exit 1; }
}

# start_node <name> <host MCP port> [<host WS port>] [<memory>]: a hardened
# multi-user executor (MCP on, Holochain off) on $NET; waits until MCP answers.
start_node() {
  local name="$1" mcp="$2" ws="${3:-}" mem="${4:-2g}"
  local ports=(-p "127.0.0.1:${mcp}:3001")
  [ -n "$ws" ] && ports+=(-p "127.0.0.1:${ws}:12000")
  docker run -d --name "$name" --network "$NET" \
    -e ADMIN_CREDENTIAL="$ADMIN" -e ENABLE_MULTI_USER=true -e ENABLE_MCP=true -e MCP_PORT=3001 \
    -e RUN_HOLOCHAIN=false -e AGENT_PASSPHRASE=windtunnel-pass \
    "${ports[@]}" \
    --cap-drop ALL --cap-add CHOWN --cap-add SETUID --cap-add SETGID --cap-add DAC_OVERRIDE --cap-add FOWNER \
    --security-opt no-new-privileges:true --memory "$mem" --cpus 1.5 --pids-limit 512 \
    "$EXEC_IMG" >/dev/null
  local code
  for _ in $(seq 1 60); do
    code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 -X POST "http://127.0.0.1:${mcp}/mcp" \
      -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' -H "Authorization: Bearer $ADMIN" \
      -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"w","version":"1"}}}' 2>/dev/null || true)
    if [ "$code" = "200" ]; then echo "[$TAG] node MCP ready"; return 0; fi
    [ "$(docker inspect -f '{{.State.Status}}' "$name" 2>/dev/null)" = exited ] && break
    sleep 2
  done
  echo "[$TAG] FAIL — executor $name did not serve MCP"
  docker logs --tail 30 "$name" 2>&1 | sed 's/^/    /'
  exit 1
}
