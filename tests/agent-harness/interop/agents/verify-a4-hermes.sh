#!/usr/bin/env bash
#
# A4 (Hermes) — Waker. A neighbourhood event wakes Hermes and it acts.
#
# The driver (a4-waker-driver.ts) runs core's WakerSubscriptionManager (the class
# the @coasys/openclaw-ad4m plugin re-exports) on a live channel subscription
# against a containerised node, and delivers each detected change to Hermes's REAL
# webhook wake ingress (`hermes gateway run` + a `/webhooks/wake` route,
# HMAC-SHA256 signed via GitHub's X-Hub-Signature-256 scheme). Each wake fires a
# Hermes agent turn (against the mock model) — the harness acts.
#
# Asserts: 1 msg -> 1 wake; 3 rapid msgs coalesce to 1 (debounce dedup); a later
# msg -> 1 more wake; every wake reaches the webhook (2xx); and at least one
# agent turn actually ran (mock model called).
#
# Hardened + host-isolated: private network, no host mounts, loopback-only
# published ports, Holochain off, full teardown.
#
# Env: see lib.sh. KEEP=1 leaves the pod up for debugging.
#
set -euo pipefail
TAG=a4h
NET=a4h-net
source "$(dirname "$0")/lib.sh"
EXE=a4h-exec
OC=a4h-hermes
MOCK=a4h-mock
SECRET=a4-wake-secret
MCP_PORT=14421
WS_PORT=14422
HOOK_PORT=18644

need_image "$HER_IMG"
echo "[a4h] clean slate"; pod_init "$EXE" "$OC" "$MOCK"
build_mock

echo "[a4h] start multi-user node (MCP + WS, Holochain off, hardened)"
start_node "$EXE" "$MCP_PORT" "$WS_PORT"

echo "[a4h] start mock LLM + Hermes gateway (webhook ingress)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 "$MOCK_IMG" >/dev/null
CFG="$(mktemp)"
cat >"$CFG" <<YAML
_config_version: 33
model:
  provider: custom
  base_url: "http://$MOCK:8080/v1"
  api_key: "sk-mock"
  default: "mock/m"
  context_length: 131072
tools:
  tool_search:
    enabled: "off"
platforms:
  webhook:
    enabled: true
    port: 8644
YAML
docker create --name "$OC" --network "$NET" -e WEBHOOK_ENABLED=true -e WEBHOOK_PORT=8644 \
  --memory 3g --cpus 2 --pids-limit 1024 -p 127.0.0.1:${HOOK_PORT}:8644 \
  "$HER_IMG" gateway run >/dev/null
docker cp "$CFG" "$OC":/opt/data/config.yaml >/dev/null; rm -f "$CFG"
docker start "$OC" >/dev/null
UP=""
for i in $(seq 1 45); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 http://127.0.0.1:${HOOK_PORT}/ 2>/dev/null || true)
  [ -n "$code" ] && [ "$code" != "000" ] && { UP=1; echo "[a4h] webhook ingress up (HTTP $code)"; break; }
  sleep 2
done
[ -n "$UP" ] || { echo "[a4h] FAIL — Hermes webhook ingress never came up"; exit 1; }
echo "[a4h] register signed webhook route"
docker exec "$OC" hermes webhook subscribe wake --secret "$SECRET" --prompt "AD4M event: {text}. Acknowledge it." >/dev/null 2>&1 \
  || { echo "[a4h] FAIL — could not register the Hermes webhook route"; exit 1; }
sleep 4

echo "[a4h] run waker driver (real ad4m subscription -> signed Hermes webhook)"
set +e
OUT=$(A4_TAG=a4h A4_TARGET=hermes \
  A4_WS="http://127.0.0.1:${WS_PORT}" A4_MCP="http://127.0.0.1:${MCP_PORT}" A4_ADMIN="$ADMIN" \
  A4_INGRESS="http://127.0.0.1:${HOOK_PORT}/webhooks/wake" A4_SECRET="$SECRET" \
  timeout 120 "$TSX" "$AGENTS/a4-waker-driver.ts" 2>&1)
RC=$?
set -e
echo "$OUT" | sed 's/^/    /'
if [ $RC -ne 0 ]; then echo "[a4h] FAIL — waker driver did not pass"; exit 1; fi

echo "[a4h] confirm at least one agent turn actually ran (mock model called)"
CALLS=0
for i in $(seq 1 15); do
  CALLS=$(mock_calls "$MOCK")
  [ "$CALLS" -ge 1 ] && break
  sleep 2
done
echo "    mock model calls: $CALLS"
if [ "$CALLS" -lt 1 ]; then echo "[a4h] FAIL — webhook accepted but no agent turn ran"; exit 1; fi

echo "[a4h] PASS — real ad4m waker delivered to Hermes's webhook; agent turns ran (mock calls: $CALLS)"
