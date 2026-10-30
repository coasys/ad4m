#!/usr/bin/env bash
#
# A4 — Waker (OpenClaw). A live AD4M perspective subscription detects new channel
# messages and wakes the agent through OpenClaw's REAL /hooks/wake endpoint.
#
# The driver (a4-waker-driver.ts) runs core's WakerSubscriptionManager (the class
# the @coasys/openclaw-ad4m plugin re-exports) on the executor's
# generate_waker_query channel query, and delivers each wake with the plugin's own
# postWake to a real OpenClaw gateway hook (real Bearer-token auth,
# action=agent) — no stub sink.
#
# Asserts: 1 msg -> 1 wake; 3 rapid msgs -> coalesced to 1 wake (debounce dedup);
# a later msg -> 1 more wake; every wake reaches the real hook (2xx), zero
# failures; and the hook ran an agent turn (the mock model was called).
#
# Hardened + host-isolated (cap-drop, no-new-privileges, resource limits,
# loopback-only published ports, Holochain off), full teardown.
#
# Env: see lib.sh. KEEP=1 leaves the pod up for debugging.
#
set -euo pipefail
TAG=a4
NET=a4-net
source "$(dirname "$0")/lib.sh"
EXE=a4-exec
OC=a4-oc
MOCK=a4-mock
WAKE_TOKEN=a4-wake-tok
GW_TOKEN=a4-gw-tok
MCP_PORT=14410
WS_PORT=14411
HOOK_PORT=18790

need_image "$OC_IMG"
echo "[a4] clean slate"; pod_init "$EXE" "$OC" "$MOCK"
build_mock
echo "[a4] build ad4m plugin (postWake) from $PLUGIN_DIR"
build_plugin

echo "[a4] start multi-user node (MCP + WS, Holochain off, hardened)"
start_node "$EXE" "$MCP_PORT" "$WS_PORT"

echo "[a4] start mock LLM (for the wake-triggered agent turn)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 -e MOCK_LLM_SCRIPT='[{"text":"ack"}]' "$MOCK_IMG" >/dev/null

echo "[a4] start OpenClaw gateway (token auth, bind auto, real /hooks/wake -> agent)"
docker run -d --name "$OC" --network "$NET" --entrypoint sleep \
  -p 127.0.0.1:${HOOK_PORT}:18789 \
  --cap-drop ALL --security-opt no-new-privileges:true --memory 2g --cpus 2 --pids-limit 1024 \
  "$OC_IMG" infinity >/dev/null
CFG="$(mktemp)"
cat >"$CFG" <<JSON
{
  gateway: { mode: "local" },
  models: { providers: { mock: { api: "openai-completions", baseUrl: "http://$MOCK:8080/v1", apiKey: "mock", auth: "api-key", contextWindow: 8192, maxTokens: 256, models: [ { id: "mock-model", name: "mock-model" } ] } } },
  hooks: { enabled: true, token: "$WAKE_TOKEN", mappings: [ { id: "wake", match: { path: "wake" }, action: "agent", deliver: false } ] }
}
JSON
docker cp "$CFG" "$OC":/tmp/cfg.json5 >/dev/null; rm -f "$CFG"
docker exec "$OC" sh -c 'openclaw config patch --file /tmp/cfg.json5 >/dev/null 2>&1 && openclaw models set mock/mock-model >/dev/null 2>&1'
docker exec -d "$OC" sh -c "openclaw gateway run --auth token --token $GW_TOKEN --bind auto --allow-unconfigured --force >/tmp/gw.log 2>&1"
READY=""
for i in $(seq 1 30); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 4 -X POST http://127.0.0.1:${HOOK_PORT}/hooks/wake \
    -H "Authorization: Bearer $WAKE_TOKEN" -H 'Content-Type: application/json' -d '{"text":"probe","mode":"now"}' 2>/dev/null || true)
  [ "$code" = "200" ] && { READY=1; echo "[a4] gateway hook ready"; break; }
  sleep 2
done
[ -n "$READY" ] || { echo "[a4] FAIL — OpenClaw gateway hook never answered"; docker exec "$OC" tail -20 /tmp/gw.log | sed 's/^/    /'; exit 1; }
# The readiness probe itself starts a turn; count model calls from after it.
settle_mock_calls "$MOCK"
CALLS0=$(mock_calls "$MOCK")

echo "[a4] run waker driver (real WakerSubscriptionManager -> real hook)"
set +e
OUT=$(A4_TAG=a4 A4_TARGET=openclaw AD4M_PLUGIN_DIR="$PLUGIN_DIR" \
  A4_WS="http://127.0.0.1:${WS_PORT}" A4_MCP="http://127.0.0.1:${MCP_PORT}" A4_ADMIN="$ADMIN" \
  A4_INGRESS="http://127.0.0.1:${HOOK_PORT}/hooks/wake" A4_WAKE_TOKEN="$WAKE_TOKEN" \
  timeout 120 "$TSX" "$AGENTS/a4-waker-driver.ts" 2>&1)
RC=$?
set -e
echo "$OUT" | sed 's/^/    /'
if [ $RC -ne 0 ]; then echo "[a4] FAIL — waker driver did not pass"; exit 1; fi

echo "[a4] confirm the wakes ran agent turns (mock model called)"
CALLS=0
for i in $(seq 1 15); do
  CALLS=$(( $(mock_calls "$MOCK") - CALLS0 ))
  [ "$CALLS" -ge 1 ] && break
  sleep 2
done
echo "    mock model calls after the wakes: $CALLS"
if [ "$CALLS" -lt 1 ]; then echo "[a4] FAIL — the hook accepted the wakes but no agent turn ran"; exit 1; fi

echo "[a4] PASS — real ad4m waker delivered to OpenClaw's /hooks/wake; agent turns ran (mock calls: $CALLS)"
