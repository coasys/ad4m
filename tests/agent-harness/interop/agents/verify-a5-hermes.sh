#!/usr/bin/env bash
#
# A5 (Hermes) — A/V action loop (mocked). Hermes wakes on a mocked call, reads
# the transcript, and replies into the channel. Media transport is mocked;
# presence + transcript + reply are all real AD4M perspective links / Messages.
#
# The driver (a5-av-driver.ts) runs core's WakerSubscriptionManager on the
# executor's mention subscription against a containerised node, seeds a
# call-presence entry + a caller's transcript naming the agent in free speech
# (mock-av fixture), and delivers the wake to Hermes's REAL signed webhook
# ingress (X-Hub-Signature-256). The woken Hermes turn — mock model +
# `mcp_servers.ad4m` — reads the transcript with query_links (PERCEIVE) then
# writes a reply message with add_link (ACT).
#
# Asserts: (a) WAKE — the call-presence entry alone does NOT wake (negative
# control); the transcript's spoken name wakes exactly once; the wake reaches
# the webhook (2xx). (b) PERCEIVE — the agent's read tool returned the transcript
# body (has_mark in the mock log), before it replied. (c) REPLY — the reply
# Message lands as a fresh channel child, written by the Hermes turn.
#
# Cross-user visibility needs neighbourhood sync (OUT of scope) — the reply is
# asserted to land + be readable, not to sync to a second agent.
#
# Hardened + host-isolated: private network, no host mounts (config via docker
# cp), loopback-only ports, Holochain off, full teardown.
#
# Env: see lib.sh. KEEP=1 leaves the pod up.
#
set -euo pipefail
TAG=a5h
NET=a5h-net
source "$(dirname "$0")/lib.sh"
EXE=a5h-exec
OC=a5h-hermes
MOCK=a5h-mock
SECRET=a5-wake-secret
CHAN="a5h://channel"
MARKER="A5H_REPLY_OK"
REPLY="a5h://channel/reply/1"
TRANSCRIPT="On the call someone said: hey Aria, can you summarise the last point about the soil trial?"
# A URL-encoding-safe marker word that appears ONLY in the transcript body — so
# has_mark proves the agent's read tool returned the transcript content (the raw
# body literal is URL-encoded, so a phrase with spaces would not match).
MARK="soil"
MCP_PORT=14461
WS_PORT=14462
HOOK_PORT=18645

need_image "$HER_IMG"
echo "[a5h] clean slate"; pod_init "$EXE" "$OC" "$MOCK"
build_mock

echo "[a5h] start multi-user node (MCP + WS, Holochain off, hardened)"
start_node "$EXE" "$MCP_PORT" "$WS_PORT" 4g

echo "[a5h] mint a channel perspective (shared by mock, driver, assertions)"
UUID=$("$TSX" "$AGENTS/sovereign/waker-ad4m.ts" mint "http://127.0.0.1:${MCP_PORT}" "$ADMIN" "a5h-channel" | grep '^UUID=' | cut -d= -f2-)
[ -z "$UUID" ] && { echo "[a5h] FAIL — could not mint perspective"; exit 1; }
echo "[a5h] perspective=$UUID"

echo "[a5h] start mock LLM (perceive: read the transcript -> act: reply add_link)"
TRANSCRIPT_ADDR="$CHAN/transcript/1"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 \
  -e MOCK_LLM_TRIGGER="AD4M call transcript" \
  -e MOCK_LLM_MARK="$MARK" \
  -e MOCK_LLM_SCRIPT="[{\"tool_calls\":[{\"name\":\"mcp__ad4m__query_links\",\"arguments\":{\"perspective_id\":\"$UUID\",\"source\":\"$TRANSCRIPT_ADDR\"}}]},{\"tool_calls\":[{\"name\":\"mcp__ad4m__add_link\",\"arguments\":{\"perspective_id\":\"$UUID\",\"source\":\"$CHAN\",\"predicate\":\"ad4m://has_child\",\"target\":\"$REPLY\"}},{\"name\":\"mcp__ad4m__add_link\",\"arguments\":{\"perspective_id\":\"$UUID\",\"source\":\"$REPLY\",\"predicate\":\"ad4m://message_body\",\"target\":\"literal:string:$MARKER\"}}]},{\"text\":\"replied\"}]" \
  "$MOCK_IMG" >/dev/null

echo "[a5h] start Hermes gateway (mock model + mcp_servers.ad4m + webhook ingress)"
CFG="$(mktemp)"
cat >"$CFG" <<YAML
_config_version: 33
model:
  provider: custom
  base_url: "http://$MOCK:8080/v1"
  api_key: "sk-mock"
  default: "mock/m"
  context_length: 131072
mcp_servers:
  ad4m:
    url: "http://$EXE:3001/mcp"
    headers:
      Authorization: "Bearer $ADMIN"
    connect_timeout: 15
    timeout: 30
tools:
  tool_search:
    enabled: "off"
platforms:
  webhook:
    enabled: true
    port: 8645
YAML
docker create --name "$OC" --network "$NET" -e WEBHOOK_ENABLED=true -e WEBHOOK_PORT=8645 \
  --memory 3g --cpus 2 --pids-limit 1024 -p 127.0.0.1:${HOOK_PORT}:8645 \
  "$HER_IMG" gateway run >/dev/null
docker cp "$CFG" "$OC":/opt/data/config.yaml >/dev/null; rm -f "$CFG"
docker start "$OC" >/dev/null
UP=""
for i in $(seq 1 45); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 http://127.0.0.1:${HOOK_PORT}/ 2>/dev/null || true)
  [ -n "$code" ] && [ "$code" != "000" ] && { UP=1; echo "[a5h] webhook ingress up (HTTP $code)"; break; }
  sleep 2
done
[ -n "$UP" ] || { echo "[a5h] FAIL — Hermes webhook ingress never came up"; exit 1; }
echo "[a5h] register signed webhook route"
docker exec "$OC" hermes webhook subscribe wake --secret "$SECRET" --prompt "{text}" >/dev/null 2>&1 \
  || { echo "[a5h] FAIL — could not register the Hermes webhook route"; exit 1; }
sleep 4

echo "[a5h] run A5 driver (mention waker -> signed webhook -> Hermes read + reply)"
set +e
OUT=$(A5_TAG=a5h A5_TARGET=hermes A5_EXPECT_REPLY=1 \
  A5_WS="http://127.0.0.1:${WS_PORT}" A5_MCP="http://127.0.0.1:${MCP_PORT}" A5_ADMIN="$ADMIN" \
  A5_INGRESS="http://127.0.0.1:${HOOK_PORT}/webhooks/wake" A5_SECRET="$SECRET" \
  A5_UUID="$UUID" A5_CHAN="$CHAN" A5_TRANSCRIPT="$TRANSCRIPT" A5_MARKER="$MARKER" A5_NAME="Aria" \
  timeout 240 "$TSX" "$AGENTS/a5-av-driver.ts" 2>&1)
RC=$?
set -e
echo "$OUT" | sed 's/^/    /'
if [ $RC -ne 0 ]; then
  echo "[a5h] FAIL — A5 driver did not pass (wake / webhook / reply)"
  docker logs "$MOCK" 2>&1 | grep -E 'advance step|has_mark' | tail -8 | sed 's/^/    mock: /' || true
  exit 1
fi

echo "[a5h] assert PERCEIVE — the agent read the transcript before replying"
# has_mark: the transcript's body (marker word) returned to the model after the
# read tool ran. read-before-reply: the read tool advanced before the reply tool.
PERCEIVED=$(docker logs "$MOCK" 2>&1 | grep -c 'has_mark=true' || true)
READ_STEP=$(docker logs "$MOCK" 2>&1 | grep -n 'advance step.*query_links' | head -1 | cut -d: -f1 || true)
REPLY_STEP=$(docker logs "$MOCK" 2>&1 | grep -n 'advance step.*add_link' | head -1 | cut -d: -f1 || true)
echo "[a5h] perceiveMarks=$PERCEIVED readStepLine=${READ_STEP:-none} replyStepLine=${REPLY_STEP:-none}"
if [ "$PERCEIVED" -lt 1 ]; then echo "[a5h] FAIL — the transcript body never returned to the model (no perceive)"; exit 1; fi
if [ -z "$READ_STEP" ] || [ -z "$REPLY_STEP" ] || [ "$READ_STEP" -ge "$REPLY_STEP" ]; then
  echo "[a5h] FAIL — the agent did not read the transcript before it replied"
  exit 1
fi

echo "[a5h] PASS — mocked-A/V loop: call-presence alone did not wake; the caller's transcript naming the agent woke Hermes once; it read the transcript (perceive) and its reply landed as a fresh channel child"
