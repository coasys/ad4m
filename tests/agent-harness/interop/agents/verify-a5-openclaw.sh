#!/usr/bin/env bash
#
# A5 (OpenClaw) — A/V action loop (mocked), WAKE half. OpenClaw is woken by a
# mocked call. Media transport is mocked; presence + transcript are real AD4M
# perspective links / Messages.
#
# The driver (a5-av-driver.ts) runs core's WakerSubscriptionManager on the
# executor's mention subscription against a containerised node, seeds a
# call-presence entry + a caller's transcript naming the agent in free speech
# (mock-av fixture), and delivers the wake to OpenClaw's REAL /hooks/wake
# ingress (Bearer wake token, action=agent).
#
# Asserts the WAKE half end-to-end:
#   - the call-presence entry alone does NOT wake (negative control);
#   - the transcript's spoken name wakes exactly once (verified caller link);
#   - the wake reaches OpenClaw's real /hooks/wake ingress (2xx), and the hook
#     runs an agent turn (mock model called);
#   - the call-presence entry reads back over MCP.
# It does not drive the perceive->act-via-MCP half: OpenClaw lists MCP tools in
# the system prompt and drives them through its own text / code-bridge tool
# protocol (not an OpenAI `tools` array), and the `action:agent` hook turn
# surfaces no model-visible user message, so the deterministic OpenAI-format mock
# cannot make OpenClaw execute the transcript read + reply. Hermes and Sovereign
# prove the full perceive->act loop on the mock lane.
#
# Hardened + host-isolated: private network, no host mounts (config via docker
# cp), loopback-only ports, Holochain off, full teardown.
#
# Env: see lib.sh. KEEP=1 leaves the pod up.
#
set -euo pipefail
TAG=a5oc
NET=a5oc-net
source "$(dirname "$0")/lib.sh"
EXE=a5oc-exec
OC=a5oc-oc
MOCK=a5oc-mock
WAKE_TOKEN=a5-wake-tok
GW_TOKEN=a5-gw-tok
CHAN="a5oc://channel"
TRANSCRIPT="On the call someone said: hey Aria, can you summarise the last point about the soil trial?"
MCP_PORT=14471
WS_PORT=14472
HOOK_PORT=18791

need_image "$OC_IMG"
echo "[a5oc] clean slate"; pod_init "$EXE" "$OC" "$MOCK"
build_mock

echo "[a5oc] start multi-user node (MCP + WS, Holochain off, hardened)"
start_node "$EXE" "$MCP_PORT" "$WS_PORT" 4g

echo "[a5oc] mint a channel perspective"
UUID=$("$TSX" "$AGENTS/sovereign/waker-ad4m.ts" mint "http://127.0.0.1:${MCP_PORT}" "$ADMIN" "a5oc-channel" | grep '^UUID=' | cut -d= -f2-)
[ -z "$UUID" ] && { echo "[a5oc] FAIL — could not mint perspective"; exit 1; }
echo "[a5oc] perspective=$UUID"

echo "[a5oc] start mock LLM + OpenClaw gateway (ad4m MCP + real /hooks/wake -> agent)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 -e MOCK_LLM_SCRIPT='[{"text":"ack"}]' "$MOCK_IMG" >/dev/null
docker run -d --name "$OC" --network "$NET" --entrypoint sleep \
  -p 127.0.0.1:${HOOK_PORT}:18789 \
  --cap-drop ALL --security-opt no-new-privileges:true --memory 2g --cpus 2 --pids-limit 1024 \
  "$OC_IMG" infinity >/dev/null
CFG="$(mktemp)"
cat >"$CFG" <<JSON
{
  gateway: { mode: "local" },
  models: { providers: { mock: { api: "openai-completions", baseUrl: "http://$MOCK:8080/v1", apiKey: "mock", auth: "api-key", contextWindow: 8192, maxTokens: 256, models: [ { id: "mock-model", name: "mock-model" } ] } } },
  mcp: { servers: { ad4m: { url: "http://$EXE:3001/mcp", transport: "streamable-http", headers: { Authorization: "Bearer $ADMIN" } } } },
  hooks: { enabled: true, token: "$WAKE_TOKEN", mappings: [ { id: "wake", match: { path: "wake" }, action: "agent", deliver: false, allowUnsafeExternalContent: true, messageTemplate: "AD4M call transcript wake — read the channel transcript with your ad4m tools, then reply into the channel." } ] }
}
JSON
docker cp "$CFG" "$OC":/tmp/cfg.json5 >/dev/null; rm -f "$CFG"
docker exec "$OC" sh -c 'openclaw config patch --file /tmp/cfg.json5 >/dev/null 2>&1 && openclaw models set mock/mock-model >/dev/null 2>&1'
docker exec -d "$OC" sh -c "openclaw gateway run --auth token --token $GW_TOKEN --bind auto --allow-unconfigured --force >/tmp/gw.log 2>&1"
READY=""
for i in $(seq 1 30); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 4 -X POST http://127.0.0.1:${HOOK_PORT}/hooks/wake \
    -H "Authorization: Bearer $WAKE_TOKEN" -H 'Content-Type: application/json' -d '{"text":"probe","mode":"now"}' 2>/dev/null || true)
  [ "$code" = "200" ] && { READY=1; echo "[a5oc] gateway hook ready"; break; }
  sleep 2
done
[ -n "$READY" ] || { echo "[a5oc] FAIL — OpenClaw gateway hook never answered"; docker exec "$OC" tail -20 /tmp/gw.log | sed 's/^/    /'; exit 1; }
# The readiness probe itself starts a turn; count model calls from after it.
settle_mock_calls "$MOCK"
CALLS0=$(mock_calls "$MOCK")

echo "[a5oc] run A5 driver (mention waker -> real /hooks/wake ingress; wake half)"
set +e
OUT=$(A5_TAG=a5oc A5_TARGET=openclaw \
  A5_WS="http://127.0.0.1:${WS_PORT}" A5_MCP="http://127.0.0.1:${MCP_PORT}" A5_ADMIN="$ADMIN" \
  A5_INGRESS="http://127.0.0.1:${HOOK_PORT}/hooks/wake" A5_WAKE_TOKEN="$WAKE_TOKEN" \
  A5_UUID="$UUID" A5_CHAN="$CHAN" A5_TRANSCRIPT="$TRANSCRIPT" A5_NAME="Aria" \
  timeout 180 "$TSX" "$AGENTS/a5-av-driver.ts" 2>&1)
RC=$?
set -e
echo "$OUT" | sed 's/^/    /'
if [ $RC -ne 0 ]; then
  echo "[a5oc] FAIL — the mocked-call WAKE loop did not pass (negative control / wake / real hook ingress)"
  exit 1
fi

echo "[a5oc] confirm the wake ran an agent turn (mock model called)"
CALLS=0
for i in $(seq 1 15); do
  CALLS=$(( $(mock_calls "$MOCK") - CALLS0 ))
  [ "$CALLS" -ge 1 ] && break
  sleep 2
done
echo "    mock model calls after the wake: $CALLS"
if [ "$CALLS" -lt 1 ]; then echo "[a5oc] FAIL — the hook accepted the wake but no agent turn ran"; exit 1; fi

echo "[a5oc] PASS (wake half) — call-presence alone did not wake; the caller's transcript naming the agent woke OpenClaw once through /hooks/wake, which ran a turn. Perceive->act via MCP is not driven on the mock lane for OpenClaw."
