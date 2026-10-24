#!/usr/bin/env bash
#
# A5 (OpenClaw) — A/V action loop (mocked). OpenClaw wakes on a mocked call, reads
# the transcript, and replies into the channel. Media transport is mocked;
# presence + transcript + reply are all real AD4M perspective links / Messages.
#
# The driver (a5-openclaw-driver.ts) runs the REAL @coasys/openclaw-ad4m mention
# waker against a containerised node, seeds a call-presence entry + a transcript
# naming the agent in free speech (mock-av fixture), and delivers the wake to
# OpenClaw's REAL /hooks/wake ingress (Bearer wake token, action=agent).
#
# HONEST PARTIAL (mock lane). This proves the WAKE half end-to-end:
#   - the call-presence entry alone does NOT wake (negative control);
#   - the transcript's spoken name wakes exactly once;
#   - the wake reaches OpenClaw's real /hooks/wake ingress (2xx);
#   - the call-presence entry reads back over MCP.
# It SKIPs the perceive->act-via-MCP half: OpenClaw lists MCP tools in the system
# prompt and drives them through its own text / code-bridge tool protocol (not an
# OpenAI `tools` array — the model compat.supportsTools flag does not flip its hook
# turn to native tool_calls), and the `action:agent` hook turn surfaces no
# model-visible user message. So the deterministic OpenAI-format mock cannot make
# OpenClaw execute the transcript read + reply. That half rides the real-model lane
# (the reference plugin registers ad4m tools natively). Sovereign + Hermes prove
# the full perceive->act loop on the mock lane; see the PR finding.
#
# Hardened + host-isolated: private network, no host mounts (config via docker
# cp), loopback-only ports, Holochain off, full teardown. Needs a plugin
# (AD4M_PLUGIN_DIR) + the OpenClaw image; SKIPs otherwise. KEEP=1 leaves the pod.
#
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
OC_IMG="${OC_IMG:-ghcr.io/openclaw/openclaw:latest}"
EXE_IMG="${A5OC_EXEC_IMAGE:-ad4m-test:latest}"
MOCK_IMG="${A5OC_MOCK_IMAGE:-a2wt-mockllm}"
NET=a5oc-net
EXE=a5oc-exec
OC=a5oc-oc
MOCK=a5oc-mock
ADMIN=windtunnel-admin
WAKE_TOKEN=a5-wake-tok
GW_TOKEN=a5-gw-tok
CHAN="a5oc://channel"
MARKER="A5OC_REPLY_OK"
REPLY="a5oc://channel/reply/1"
TRANSCRIPT="On the call someone said: hey Aria, can you summarise the last point about the soil trial?"
MARK="soil"
MCP_PORT=14471
WS_PORT=14472
HOOK_PORT=18791
KEEP="${KEEP:-}"

PLUGIN_DIR="${AD4M_PLUGIN_DIR:-${AD4M_REPO:+$AD4M_REPO/plugins/ad4m}}"
if [ -z "${PLUGIN_DIR:-}" ] || [ ! -f "$PLUGIN_DIR/package.json" ]; then
  echo "[a5oc] SKIP — no ad4m plugin (set AD4M_PLUGIN_DIR to a plugins/ad4m checkout carrying coasys/ad4m#880)"
  exit 0
fi
if ! docker image inspect "$OC_IMG" >/dev/null 2>&1; then
  echo "[a5oc] SKIP — OpenClaw image $OC_IMG unavailable"
  exit 0
fi

teardown() { [ -n "$KEEP" ] && { echo "[a5oc] KEEP set — leaving pod up"; return; }; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true; }
trap teardown EXIT

echo "[a5oc] clean slate"; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true
docker network create "$NET" >/dev/null
# A5 needs the current server.mjs (MOCK_LLM_MARK); cheap rebuild.
docker build -q -t "$MOCK_IMG" "$HERE/mock-llm" >/dev/null
# The driver imports the plugin's waker .ts sources (via tsx) + @coasys/ad4m from
# node_modules — no plugin dist build needed (avoids coupling to the plugin's
# separately-owned build state).
echo "[a5oc] ensure ad4m plugin deps (@coasys/ad4m for the waker component)"
[ -d "$PLUGIN_DIR/node_modules/@coasys/ad4m" ] || ( cd "$PLUGIN_DIR" && NODE_ENV=development npm install --include=dev >/dev/null 2>&1 )

echo "[a5oc] start multi-user node (MCP + WS, Holochain off, hardened)"
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
  [ "$code" = "200" ] && { echo "[a5oc] node MCP ready"; break; }
  sleep 2
done

echo "[a5oc] mint a channel perspective (shared by mock, driver, assertions)"
UUID=$(cd "$ROOT" && npx tsx "$HERE/sovereign/waker-ad4m.ts" mint "http://127.0.0.1:${MCP_PORT}" "$ADMIN" "a5oc-channel" | grep '^UUID=' | cut -d= -f2-)
[ -z "$UUID" ] && { echo "[a5oc] FAIL — could not mint perspective"; exit 1; }
echo "[a5oc] perspective=$UUID"

echo "[a5oc] start mock LLM (perceive: read the transcript -> act: reply add_link)"
TRANSCRIPT_ADDR="$CHAN/transcript/1"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 \
  -e MOCK_LLM_TRIGGER="AD4M call transcript" \
  -e MOCK_LLM_MARK="$MARK" \
  -e MOCK_LLM_SCRIPT="[{\"tool_calls\":[{\"name\":\"ad4m__query_links\",\"arguments\":{\"perspective_id\":\"$UUID\",\"source\":\"$TRANSCRIPT_ADDR\"}}]},{\"tool_calls\":[{\"name\":\"ad4m__add_link\",\"arguments\":{\"perspective_id\":\"$UUID\",\"source\":\"$CHAN\",\"predicate\":\"ad4m://has_child\",\"target\":\"$REPLY\"}},{\"name\":\"ad4m__add_link\",\"arguments\":{\"perspective_id\":\"$UUID\",\"source\":\"$REPLY\",\"predicate\":\"ad4m://message_body\",\"target\":\"literal://string:$MARKER\"}}]},{\"text\":\"replied\"}]" \
  "$MOCK_IMG" >/dev/null

echo "[a5oc] start OpenClaw gateway (mock model + mcp.servers.ad4m + real /hooks/wake -> agent)"
docker run -d --name "$OC" --network "$NET" --entrypoint sleep \
  -p 127.0.0.1:${HOOK_PORT}:18789 \
  --cap-drop ALL --security-opt no-new-privileges:true --memory 2g --cpus 2 --pids-limit 1024 \
  "$OC_IMG" infinity >/dev/null
cat >/tmp/a5oc-cfg.json5 <<JSON
{
  gateway: { mode: "local" },
  models: { providers: { mock: { api: "openai-completions", baseUrl: "http://$MOCK:8080/v1", apiKey: "mock", auth: "api-key", contextWindow: 8192, maxTokens: 256, models: [ { id: "mock-model", name: "mock-model", compat: { supportsTools: true } } ] } } },
  mcp: { servers: { ad4m: { url: "http://$EXE:3001/mcp", transport: "streamable-http", headers: { Authorization: "Bearer $ADMIN" } } } },
  hooks: { enabled: true, token: "$WAKE_TOKEN", mappings: [ { id: "wake", match: { path: "wake" }, action: "agent", deliver: false, allowUnsafeExternalContent: true, messageTemplate: "AD4M call transcript wake — read the channel transcript with your ad4m tools, then reply into the channel." } ] }
}
JSON
docker cp /tmp/a5oc-cfg.json5 "$OC":/tmp/cfg.json5 >/dev/null
docker exec "$OC" sh -c 'openclaw config patch --file /tmp/cfg.json5 >/dev/null 2>&1 && openclaw models set mock/mock-model >/dev/null 2>&1'
docker exec -d "$OC" sh -c "openclaw gateway run --auth token --token $GW_TOKEN --bind auto --allow-unconfigured --force >/tmp/gw.log 2>&1"
for i in $(seq 1 30); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 4 -X POST http://127.0.0.1:${HOOK_PORT}/hooks/wake \
    -H "Authorization: Bearer $WAKE_TOKEN" -H 'Content-Type: application/json' -d '{"text":"probe","mode":"now"}' 2>/dev/null || echo 000)
  [ "$code" = "200" ] && { echo "[a5oc] gateway hook ready"; break; }
  sleep 2
done

echo "[a5oc] run A5 driver (mention waker -> real /hooks/wake ingress; wake half)"
cd "$ROOT"
set +e
OUT=$(AD4M_PLUGIN_DIR="$PLUGIN_DIR" A5_WAKE_ONLY=1 \
  A5_WS="http://127.0.0.1:${WS_PORT}" A5_MCP="http://127.0.0.1:${MCP_PORT}" A5_ADMIN="$ADMIN" \
  A5_HOOK="http://127.0.0.1:${HOOK_PORT}/hooks/wake" A5_WAKE_TOKEN="$WAKE_TOKEN" \
  A5_UUID="$UUID" A5_CHAN="$CHAN" A5_TRANSCRIPT="$TRANSCRIPT" A5_MARKER="$MARKER" A5_NAME="Aria" \
  timeout 180 npx tsx "$HERE/a5-openclaw-driver.ts" 2>&1)
RC=$?
set -e
echo "$OUT" | grep -E '\[a5oc\]' | sed 's/^/    /'
if [ $RC -ne 0 ]; then
  echo "[a5oc] FAIL — the mocked-call WAKE loop did not pass (negative control / wake / real hook ingress)"
  exit 1
fi

# The WAKE half passed end-to-end (real waker -> spoken-name wake -> real
# /hooks/wake 2xx, content-gated by the negative control, call-presence readable).
# The perceive->act-via-MCP half is NOT driven on the mock lane: OpenClaw lists
# MCP tools in the system prompt and drives them through its own text / code-bridge
# tool protocol (not an OpenAI `tools` array — model compat.supportsTools does not
# flip its hook turn to native tool_calls), and its `action:agent` hook turn
# surfaces no model-visible user message for content-gated scripting. So the
# deterministic OpenAI-format mock cannot make OpenClaw execute the transcript
# read + reply. That half rides the real-model lane, where the reference
# @coasys/openclaw-ad4m plugin registers ad4m tools natively — out of scope here.
echo "[a5oc] SKIP — mocked-call WAKE loop verified; perceive->act via MCP not driven on the mock lane (OpenClaw text/code-bridge tool protocol + hook turn carries no model-visible prompt). See finding in the PR."
