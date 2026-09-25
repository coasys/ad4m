#!/usr/bin/env bash
#
# A5 (Hermes) — A/V action loop (mocked). Hermes wakes on a mocked call, reads
# the transcript, and replies into the channel. Media transport is mocked;
# presence + transcript + reply are all real AD4M perspective links / Messages.
#
# The driver (a5-hermes-driver.ts) runs the REAL @coasys/openclaw-ad4m mention
# waker against a containerised node, seeds a call-presence entry + a transcript
# naming the agent in free speech (mock-av fixture), and delivers the wake to
# Hermes's REAL signed webhook ingress (X-Hub-Signature-256). The woken Hermes
# turn — mock model + `mcp_servers.ad4m` — reads the transcript with
# get_children_body_parsed (PERCEIVE) then writes a reply message (ACT).
#
# Asserts: (a) WAKE — the call-presence entry alone does NOT wake (negative
# control); the transcript's spoken name wakes exactly once; every wake reaches
# the webhook (2xx). (b) PERCEIVE — the agent's read tool returned the transcript
# body (has_mark in the mock log). (c) REPLY — the reply Message lands as a fresh
# channel child (child-has marker), written by the Hermes turn.
#
# Cross-user visibility needs neighbourhood sync (OUT of scope) — the reply is
# asserted to land + be readable, not to sync to a second agent.
#
# Hardened + host-isolated: private network, no host mounts (config via docker
# cp), loopback-only ports, Holochain off, full teardown. Needs a plugin
# (AD4M_PLUGIN_DIR) + the Hermes image; SKIPs otherwise. KEEP=1 leaves the pod.
#
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
HER_IMG="${HERMES_IMAGE:-nousresearch/hermes-agent:latest}"
EXE_IMG="${A5H_EXEC_IMAGE:-ad4m-test:latest}"
MOCK_IMG="${A5H_MOCK_IMAGE:-a2wt-mockllm}"
NET=a5h-net
EXE=a5h-exec
OC=a5h-hermes
MOCK=a5h-mock
ADMIN=windtunnel-admin
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
KEEP="${KEEP:-}"

PLUGIN_DIR="${AD4M_PLUGIN_DIR:-${AD4M_REPO:+$AD4M_REPO/plugins/ad4m}}"
if [ -z "${PLUGIN_DIR:-}" ] || [ ! -f "$PLUGIN_DIR/package.json" ]; then
  echo "[a5h] SKIP — no ad4m plugin (set AD4M_PLUGIN_DIR to a plugins/ad4m checkout carrying coasys/ad4m#880)"
  exit 0
fi
if ! docker image inspect "$HER_IMG" >/dev/null 2>&1; then
  echo "[a5h] pull $HER_IMG (large, first run only)"
  timeout 600 docker pull "$HER_IMG" >/dev/null 2>&1 || { echo "[a5h] SKIP — Hermes image unavailable"; exit 0; }
fi

teardown() { [ -n "$KEEP" ] && { echo "[a5h] KEEP set — leaving pod up"; return; }; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true; }
trap teardown EXIT

echo "[a5h] clean slate"; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true
docker network create "$NET" >/dev/null
# A5 needs the current server.mjs (MOCK_LLM_MARK); cheap rebuild.
docker build -q -t "$MOCK_IMG" "$HERE/mock-llm" >/dev/null
# The driver imports the plugin's waker .ts sources (via tsx) + @coasys/ad4m from
# node_modules — it does NOT need the plugin dist build, so only ensure deps are
# installed (avoids coupling to the plugin's separately-owned build state).
echo "[a5h] ensure ad4m plugin deps (@coasys/ad4m for the waker component)"
[ -d "$PLUGIN_DIR/node_modules/@coasys/ad4m" ] || ( cd "$PLUGIN_DIR" && NODE_ENV=development npm install --include=dev >/dev/null 2>&1 )

echo "[a5h] start multi-user node (MCP + WS, Holochain off, hardened)"
docker run -d --name "$EXE" --network "$NET" \
  -e ADMIN_CREDENTIAL="$ADMIN" -e ENABLE_MULTI_USER=true -e ENABLE_MCP=true -e MCP_PORT=3001 \
  -e RUN_HOLOCHAIN=false -e AGENT_PASSPHRASE=windtunnel-pass \
  -p 127.0.0.1:${MCP_PORT}:3001 -p 127.0.0.1:${WS_PORT}:12000 \
  --cap-drop ALL --cap-add CHOWN --cap-add SETUID --cap-add SETGID --cap-add DAC_OVERRIDE --cap-add FOWNER \
  --security-opt no-new-privileges:true --memory 4g --cpus 1.5 --pids-limit 512 \
  "$EXE_IMG" >/dev/null
for i in $(seq 1 60); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 -X POST http://127.0.0.1:${MCP_PORT}/mcp \
    -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' -H "Authorization: Bearer $ADMIN" \
    -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"w","version":"1"}}}' 2>/dev/null || echo 000)
  [ "$code" = "200" ] && { echo "[a5h] node MCP ready"; break; }
  sleep 2
done

echo "[a5h] mint a channel perspective (shared by mock, driver, assertions)"
UUID=$(cd "$ROOT" && npx tsx "$HERE/sovereign/waker-ad4m.ts" mint "http://127.0.0.1:${MCP_PORT}" "$ADMIN" "a5h-channel" | grep '^UUID=' | cut -d= -f2-)
[ -z "$UUID" ] && { echo "[a5h] FAIL — could not mint perspective"; exit 1; }
echo "[a5h] perspective=$UUID"

echo "[a5h] start mock LLM (perceive: read the transcript -> act: reply add_link)"
TRANSCRIPT_ADDR="$CHAN/transcript/1"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 \
  -e MOCK_LLM_TRIGGER="AD4M call transcript" \
  -e MOCK_LLM_MARK="$MARK" \
  -e MOCK_LLM_SCRIPT="[{\"tool_calls\":[{\"name\":\"mcp__ad4m__query_links\",\"arguments\":{\"perspective_id\":\"$UUID\",\"source\":\"$TRANSCRIPT_ADDR\"}}]},{\"tool_calls\":[{\"name\":\"mcp__ad4m__add_link\",\"arguments\":{\"perspective_id\":\"$UUID\",\"source\":\"$CHAN\",\"predicate\":\"ad4m://has_child\",\"target\":\"$REPLY\"}},{\"name\":\"mcp__ad4m__add_link\",\"arguments\":{\"perspective_id\":\"$UUID\",\"source\":\"$REPLY\",\"predicate\":\"ad4m://message_body\",\"target\":\"literal://string:$MARKER\"}}]},{\"text\":\"replied\"}]" \
  "$MOCK_IMG" >/dev/null

echo "[a5h] start Hermes gateway (mock model + mcp_servers.ad4m + webhook ingress)"
cat >/tmp/a5h-config.yaml <<YAML
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
docker cp /tmp/a5h-config.yaml "$OC":/opt/data/config.yaml >/dev/null
docker start "$OC" >/dev/null
for i in $(seq 1 45); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 http://127.0.0.1:${HOOK_PORT}/ 2>/dev/null || echo 000)
  [ "$code" != "000" ] && { echo "[a5h] webhook ingress up (HTTP $code)"; break; }
  sleep 2
done
echo "[a5h] register signed webhook route"
docker exec "$OC" hermes webhook subscribe wake --secret "$SECRET" --prompt "{text}" >/dev/null 2>&1 || true
sleep 4

echo "[a5h] run A5 driver (mention waker -> signed webhook -> Hermes read + reply)"
cd "$ROOT"
set +e
OUT=$(AD4M_PLUGIN_DIR="$PLUGIN_DIR" \
  A5_WS="http://127.0.0.1:${WS_PORT}" A5_MCP="http://127.0.0.1:${MCP_PORT}" A5_ADMIN="$ADMIN" \
  A5H_WEBHOOK="http://127.0.0.1:${HOOK_PORT}/webhooks/wake" A5H_SECRET="$SECRET" \
  A5_UUID="$UUID" A5_CHAN="$CHAN" A5_TRANSCRIPT="$TRANSCRIPT" A5_MARKER="$MARKER" A5_NAME="Aria" \
  timeout 180 npx tsx "$HERE/a5-hermes-driver.ts" 2>&1)
RC=$?
set -e
echo "$OUT" | grep -E '\[a5h\]' | sed 's/^/    /'
if [ $RC -ne 0 ]; then
  echo "[a5h] FAIL — A5 driver did not pass (wake / webhook / reply)"
  docker logs "$MOCK" 2>&1 | grep -E 'advance step|has_mark' | tail -8 | sed 's/^/    mock: /'
  exit 1
fi

echo "[a5h] assert PERCEIVE — the agent read the transcript before replying"
# has_mark: the transcript's body (marker word) returned to the model after the
# read tool ran. read-before-reply: the read tool advanced before the reply tool.
PERCEIVED=$(docker logs "$MOCK" 2>&1 | grep -c 'has_mark=true' || true)
READ_STEP=$(docker logs "$MOCK" 2>&1 | grep -n 'advance step.*query_links' | head -1 | cut -d: -f1)
REPLY_STEP=$(docker logs "$MOCK" 2>&1 | grep -n 'advance step.*add_link' | head -1 | cut -d: -f1)
echo "[a5h] perceiveMarks=$PERCEIVED readStepLine=${READ_STEP:-none} replyStepLine=${REPLY_STEP:-none}"
if [ "$PERCEIVED" -lt 1 ]; then echo "[a5h] FAIL — the transcript body never returned to the model (no perceive)"; exit 1; fi
if [ -z "$READ_STEP" ] || [ -z "$REPLY_STEP" ] || [ "$READ_STEP" -ge "$REPLY_STEP" ]; then
  echo "[a5h] FAIL — the agent did not read the transcript before it replied"
  exit 1
fi

echo "[a5h] PASS — mocked-A/V loop: call-presence alone did not wake; the transcript's spoken name woke Hermes once; it read the transcript (perceive) and its reply landed as a fresh channel child"
