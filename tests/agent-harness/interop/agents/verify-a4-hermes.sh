#!/usr/bin/env bash
#
# A4 (Hermes) — Waker. A neighbourhood event wakes Hermes and it acts.
#
# The driver (a4-hermes-driver.ts) runs the REAL @coasys/openclaw-ad4m waker
# component (WakerSubscriptionManager + a live perspective SPARQL subscription)
# against a containerised node, and delivers each detected channel message to
# Hermes's REAL webhook wake ingress (`hermes gateway run` + a `/webhooks/wake`
# route, HMAC-SHA256 signed via GitHub's X-Hub-Signature-256 scheme). Each wake
# fires a Hermes agent turn (against the mock model) — the harness acts.
#
# Asserts: 1 msg -> 1 wake; 3 rapid msgs coalesce to 1 (debounce dedup); a later
# msg -> 1 more wake; every wake reaches the webhook (2xx); and at least one
# agent turn actually ran (mock model called).
#
# Hardened + host-isolated: private network, no host mounts, loopback-only
# published ports, Holochain off, full teardown. Needs a dist-built plugin
# (AD4M_PLUGIN_DIR) + the Hermes image; SKIPs otherwise.
#
# Env: KEEP=1 leaves the pod up for debugging.
#
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
HER_IMG="${HERMES_IMAGE:-nousresearch/hermes-agent:latest}"
EXE_IMG="${A4H_EXEC_IMAGE:-ad4m-test:latest}"
MOCK_IMG="${A4H_MOCK_IMAGE:-a2wt-mockllm}"
NET=a4h-net
EXE=a4h-exec
OC=a4h-hermes
MOCK=a4h-mock
ADMIN=windtunnel-admin
SECRET=a4-wake-secret
MCP_PORT=14421
WS_PORT=14422
HOOK_PORT=18644
KEEP="${KEEP:-}"

PLUGIN_DIR="${AD4M_PLUGIN_DIR:-${AD4M_REPO:+$AD4M_REPO/plugins/ad4m}}"
if [ -z "${PLUGIN_DIR:-}" ] || [ ! -f "$PLUGIN_DIR/package.json" ]; then
  echo "[a4h] SKIP — no ad4m plugin (set AD4M_PLUGIN_DIR to a plugins/ad4m checkout carrying coasys/ad4m#880)"
  exit 0
fi
if ! docker image inspect "$HER_IMG" >/dev/null 2>&1; then
  echo "[a4h] pull $HER_IMG (large, first run only)"
  timeout 600 docker pull "$HER_IMG" >/dev/null 2>&1 || { echo "[a4h] SKIP — Hermes image unavailable"; exit 0; }
fi

teardown() { [ -n "$KEEP" ] && { echo "[a4h] KEEP set — leaving pod up"; return; }; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true; }
trap teardown EXIT

echo "[a4h] clean slate"; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true
docker network create "$NET" >/dev/null

if ! docker image inspect "$MOCK_IMG" >/dev/null 2>&1; then
  echo "[a4h] build mock-llm image ($MOCK_IMG)"; docker build -q -t "$MOCK_IMG" "$HERE/mock-llm" >/dev/null
fi
echo "[a4h] build ad4m plugin (for the waker component + @coasys/ad4m)"
( cd "$PLUGIN_DIR" && NODE_ENV=development npm install --include=dev >/dev/null 2>&1 && NODE_ENV=development npm run build >/dev/null 2>&1 )

echo "[a4h] start multi-user node (MCP + WS, Holochain off, hardened)"
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
  [ "$code" = "200" ] && { echo "[a4h] node MCP ready"; break; }
  sleep 2
done

echo "[a4h] start mock LLM + Hermes gateway (webhook ingress)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 "$MOCK_IMG" >/dev/null
cat >/tmp/a4h-config.yaml <<YAML
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
docker cp /tmp/a4h-config.yaml "$OC":/opt/data/config.yaml >/dev/null
docker start "$OC" >/dev/null
for i in $(seq 1 45); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 http://127.0.0.1:${HOOK_PORT}/ 2>/dev/null || echo 000)
  [ "$code" != "000" ] && { echo "[a4h] webhook ingress up (HTTP $code)"; break; }
  sleep 2
done
echo "[a4h] register signed webhook route"
docker exec "$OC" hermes webhook subscribe wake --secret "$SECRET" --prompt "AD4M event: {payload.text}. Acknowledge it." >/dev/null 2>&1 || true
sleep 4

echo "[a4h] run waker driver (real ad4m subscription -> signed Hermes webhook)"
cd "$ROOT"
set +e
OUT=$(AD4M_PLUGIN_DIR="$PLUGIN_DIR" \
  A4_WS="http://127.0.0.1:${WS_PORT}" A4_MCP="http://127.0.0.1:${MCP_PORT}" A4_ADMIN="$ADMIN" \
  A4H_WEBHOOK="http://127.0.0.1:${HOOK_PORT}/webhooks/wake" A4H_SECRET="$SECRET" \
  timeout 120 npx tsx "$HERE/a4-hermes-driver.ts" 2>&1)
RC=$?
set -e
echo "$OUT" | grep -E '\[a4h\]' | sed 's/^/    /'
if [ $RC -ne 0 ]; then echo "[a4h] FAIL — waker driver did not pass"; exit 1; fi

echo "[a4h] confirm at least one agent turn actually ran (mock model called)"
CALLS=0
for i in $(seq 1 10); do
  CALLS=$(docker logs "$MOCK" 2>&1 | grep -cE 'chat/completions|/messages' || true)
  [ "$CALLS" -ge 1 ] && break
  sleep 2
done
echo "    mock model calls: $CALLS"
if [ "$CALLS" -lt 1 ]; then echo "[a4h] FAIL — webhook accepted but no agent turn ran"; exit 1; fi

echo "[a4h] PASS — real ad4m waker delivered to Hermes's webhook; agent turns ran (mock calls: $CALLS)"
