#!/usr/bin/env bash
#
# A4 — Waker. Proves the @coasys/openclaw-ad4m waker integration surface: a live
# AD4M perspective subscription detects new channel messages and wakes the agent
# through OpenClaw's REAL /hooks/wake endpoint.
#
# The driver (a4-waker-driver.ts) imports the plugin's OWN WakerSubscriptionManager
# + postWake and @coasys/ad4m's Ad4mClient + QuerySubscriptionProxy, so the entire
# subscribe -> detect -> debounce -> wake -> POST path is production plugin code
# (driven exactly as the plugin's unit tests drive it). The wake is delivered to a
# real OpenClaw gateway hook (real Bearer-token auth, action=agent) — no stub sink.
#
# Asserts: 1 msg -> 1 wake; 3 rapid msgs -> coalesced to 1 wake (debounce dedup);
# a later msg -> 1 more wake; every wake reaches the real hook (2xx), zero failures.
#
# Hardened + host-isolated (cap-drop, no-new-privileges, resource limits,
# loopback-only published ports, Holochain off), full teardown. Needs a dist-built
# plugin (set AD4M_PLUGIN_DIR); SKIPs otherwise.
#
# Env: KEEP=1 leaves the pod up for debugging.
#
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)" # worktree root (for `npx tsx`)
OC_IMG="${OC_IMG:-ghcr.io/openclaw/openclaw:latest}"
EXE_IMG="${A4_EXEC_IMAGE:-ad4m-test:latest}"
MOCK_IMG="${A4_MOCK_IMAGE:-a2wt-mockllm}"
NET=a4-net
EXE=a4-exec
OC=a4-oc
MOCK=a4-mock
ADMIN=windtunnel-admin
WAKE_TOKEN=a4-wake-tok
GW_TOKEN=a4-gw-tok
MCP_PORT=14410
WS_PORT=14411
HOOK_PORT=18790
KEEP="${KEEP:-}"

PLUGIN_DIR="${AD4M_PLUGIN_DIR:-${AD4M_REPO:+$AD4M_REPO/plugins/ad4m}}"
if [ -z "${PLUGIN_DIR:-}" ] || [ ! -f "$PLUGIN_DIR/package.json" ] || ! grep -q '"build"' "$PLUGIN_DIR/package.json" 2>/dev/null; then
  echo "[a4] SKIP — no ad4m plugin with a dist build available (set AD4M_PLUGIN_DIR to a plugins/ad4m checkout carrying coasys/ad4m#880)"
  exit 0
fi

teardown() { [ -n "$KEEP" ] && { echo "[a4] KEEP set — leaving pod up"; return; }; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true; }
trap teardown EXIT

echo "[a4] clean slate"; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true
docker network create "$NET" >/dev/null

if ! docker image inspect "$MOCK_IMG" >/dev/null 2>&1; then
  echo "[a4] build mock-llm image ($MOCK_IMG)"
  docker build -q -t "$MOCK_IMG" "$HERE/mock-llm" >/dev/null
fi

echo "[a4] build ad4m plugin (dist + node_modules) from $PLUGIN_DIR"
( cd "$PLUGIN_DIR" && NODE_ENV=development npm install --include=dev >/dev/null 2>&1 && NODE_ENV=development npm run build >/dev/null 2>&1 )

echo "[a4] start multi-user node (MCP + WS, Holochain off, hardened)"
docker run -d --name "$EXE" --network "$NET" \
  -e ADMIN_CREDENTIAL="$ADMIN" -e ENABLE_MULTI_USER=true -e ENABLE_MCP=true -e MCP_PORT=3001 \
  -e RUN_HOLOCHAIN=false -e AGENT_PASSPHRASE=windtunnel-pass \
  -p 127.0.0.1:${MCP_PORT}:3001 -p 127.0.0.1:${WS_PORT}:12000 \
  --cap-drop ALL --cap-add CHOWN --cap-add SETUID --cap-add SETGID --cap-add DAC_OVERRIDE --cap-add FOWNER \
  --security-opt no-new-privileges:true --memory 2g --cpus 1.5 --pids-limit 512 \
  "$EXE_IMG" >/dev/null
for i in $(seq 1 60); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 -X POST http://127.0.0.1:${MCP_PORT}/mcp \
    -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' -H "Authorization: Bearer $ADMIN" \
    -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"w","version":"1"}}}' 2>/dev/null || echo 000)
  [ "$code" = "200" ] && { echo "[a4] node MCP ready"; break; }
  sleep 2
done

echo "[a4] start mock LLM (for the wake-triggered agent turn)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_SCRIPT='[{"text":"ack"}]' "$MOCK_IMG" >/dev/null

echo "[a4] start OpenClaw gateway (token auth, bind auto, real /hooks/wake -> agent)"
docker run -d --name "$OC" --network "$NET" --entrypoint sleep \
  -p 127.0.0.1:${HOOK_PORT}:18789 \
  --cap-drop ALL --security-opt no-new-privileges:true --memory 2g --cpus 2 --pids-limit 1024 \
  "$OC_IMG" infinity >/dev/null
cat >/tmp/a4-cfg.json5 <<JSON
{
  gateway: { mode: "local" },
  models: { providers: { mock: { api: "openai-completions", baseUrl: "http://$MOCK:8080/v1", apiKey: "mock", auth: "api-key", contextWindow: 8192, maxTokens: 256, models: [ { id: "mock-model", name: "mock-model" } ] } } },
  hooks: { enabled: true, token: "$WAKE_TOKEN", mappings: [ { id: "wake", match: { path: "wake" }, action: "agent", deliver: false } ] }
}
JSON
docker cp /tmp/a4-cfg.json5 "$OC":/tmp/cfg.json5 >/dev/null
docker exec "$OC" sh -c 'openclaw config patch --file /tmp/cfg.json5 >/dev/null 2>&1 && openclaw models set mock/mock-model >/dev/null 2>&1'
docker exec -d "$OC" sh -c "openclaw gateway run --auth token --token $GW_TOKEN --bind auto --allow-unconfigured --force >/tmp/gw.log 2>&1"
for i in $(seq 1 30); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 4 -X POST http://127.0.0.1:${HOOK_PORT}/hooks/wake \
    -H "Authorization: Bearer $WAKE_TOKEN" -H 'Content-Type: application/json' -d '{"text":"probe","mode":"now"}' 2>/dev/null || echo 000)
  [ "$code" = "200" ] && { echo "[a4] gateway hook ready"; break; }
  sleep 2
done

echo "[a4] run waker driver (real WakerSubscriptionManager -> real hook)"
cd "$ROOT"
AD4M_PLUGIN_DIR="$PLUGIN_DIR" \
  A4_WS="http://127.0.0.1:${WS_PORT}" A4_MCP="http://127.0.0.1:${MCP_PORT}" \
  A4_HOOK="http://127.0.0.1:${HOOK_PORT}/hooks/wake" A4_WAKE_TOKEN="$WAKE_TOKEN" A4_ADMIN="$ADMIN" \
  npx tsx "$HERE/a4-waker-driver.ts"
