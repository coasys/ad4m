#!/usr/bin/env bash
#
# A2 (Hermes) — provision & connect via the real Hermes agent
# (nousresearch/hermes-agent). A user instructs Hermes to onboard to a
# pre-existing multi-user ADAM node and create a working agent identity.
#
# Hermes connects to the node's MCP server (streamable HTTP + Bearer JWT, its
# `mcp_servers.ad4m` config), runs ONE headless turn (`hermes -z`) against a mock
# OpenAI-compatible model that drives `mcp__ad4m__get_my_did` + `add_perspective`,
# and we verify — independently, over MCP — that the provisioned user's DID and
# the created perspective both exist on the node. So it proves init/connect + a
# real MCP action round-trip through the actual harness.
#
# `tools.tool_search: off` keeps MCP tools directly callable (Hermes otherwise
# hides them behind a tool_search bridge). Hardened + host-isolated: private
# network, no host mounts (config goes in via `docker cp`), loopback-only node
# port, resource limits, Holochain off. Full teardown.
#
# Env: KEEP=1 leaves the pod up for debugging.
#
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)" # worktree root (for `npx tsx`)
HER_IMG="${HERMES_IMAGE:-nousresearch/hermes-agent:latest}"
EXE_IMG="${A2H_EXEC_IMAGE:-ad4m-test:latest}"
MOCK_IMG="${A2H_MOCK_IMAGE:-a2wt-mockllm}"
NET=a2h-net
EXE=a2h-exec
OC=a2h-hermes
MOCK=a2h-mock
ADMIN=windtunnel-admin
EMAIL="hermes-a2@agent.local"
UPASS="hermes-a2-pass-123"
PNAME="hermes-a2-channel"
MCP_PORT=14420
KEEP="${KEEP:-}"

if ! docker image inspect "$HER_IMG" >/dev/null 2>&1; then
  echo "[a2h] pull $HER_IMG (large, first run only)"
  if ! timeout 600 docker pull "$HER_IMG" >/dev/null 2>&1; then
    echo "[a2h] SKIP — Hermes image $HER_IMG unavailable"
    exit 0
  fi
fi

teardown() { [ -n "$KEEP" ] && { echo "[a2h] KEEP set — leaving pod up"; return; }; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true; }
trap teardown EXIT

echo "[a2h] clean slate"; docker rm -f "$EXE" "$OC" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true
docker network create "$NET" >/dev/null

if ! docker image inspect "$MOCK_IMG" >/dev/null 2>&1; then
  echo "[a2h] build mock-llm image ($MOCK_IMG)"; docker build -q -t "$MOCK_IMG" "$HERE/mock-llm" >/dev/null
fi

echo "[a2h] start multi-user node (MCP, Holochain off, hardened)"
docker run -d --name "$EXE" --network "$NET" \
  -e ADMIN_CREDENTIAL="$ADMIN" -e ENABLE_MULTI_USER=true -e ENABLE_MCP=true -e MCP_PORT=3001 \
  -e RUN_HOLOCHAIN=false -e AGENT_PASSPHRASE=windtunnel-pass \
  -p 127.0.0.1:${MCP_PORT}:3001 \
  --cap-drop ALL --cap-add CHOWN --cap-add SETUID --cap-add SETGID --cap-add DAC_OVERRIDE --cap-add FOWNER \
  --security-opt no-new-privileges:true --memory 2g --cpus 1.5 --pids-limit 512 \
  "$EXE_IMG" >/dev/null
for i in $(seq 1 60); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 -X POST http://127.0.0.1:${MCP_PORT}/mcp \
    -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' -H "Authorization: Bearer $ADMIN" \
    -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"w","version":"1"}}}' 2>/dev/null || echo 000)
  [ "$code" = "200" ] && { echo "[a2h] node MCP ready"; break; }
  sleep 2
done

echo "[a2h] provision Hermes's own user identity (signup + login)"
PROV=$(cd "$ROOT" && A2H_ADMIN="$ADMIN" npx tsx "$HERE/ad4m-user.ts" provision "http://127.0.0.1:${MCP_PORT}" "$EMAIL" "$UPASS")
JWT=$(echo "$PROV" | grep '^JWT=' | cut -d= -f2-)
DID=$(echo "$PROV" | grep '^DID=' | cut -d= -f2-)
if [ -z "$JWT" ] || [ -z "$DID" ]; then echo "[a2h] FAIL — could not provision a user identity"; echo "$PROV" | sed 's/^/    /'; exit 1; fi
echo "[a2h] provisioned DID: $DID"

echo "[a2h] start mock LLM (scripted ad4m tool calls for the turn)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 \
  -e MOCK_LLM_SCRIPT="[{\"tool_calls\":[{\"name\":\"mcp__ad4m__get_my_did\",\"arguments\":{}}]},{\"tool_calls\":[{\"name\":\"mcp__ad4m__add_perspective\",\"arguments\":{\"name\":\"$PNAME\"}}]},{\"text\":\"created channel\"}]" \
  "$MOCK_IMG" >/dev/null

echo "[a2h] configure Hermes (mcp_servers.ad4m + mock model) and run one turn"
cat >/tmp/a2h-config.yaml <<YAML
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
      Authorization: "Bearer $JWT"
    skip_preflight: true
    connect_timeout: 15
    timeout: 30
tools:
  tool_search:
    enabled: "off"
YAML
docker create --name "$OC" --network "$NET" --memory 3g --cpus 2 --pids-limit 1024 \
  "$HER_IMG" -z "Connect to AD4M: call get_my_did, then create a channel perspective named $PNAME." >/dev/null
docker cp /tmp/a2h-config.yaml "$OC":/opt/data/config.yaml >/dev/null
OUT=$(timeout 220 docker start -a "$OC" 2>&1 || true)
echo "$OUT" | grep -iE 'created channel|error|ad4m' | tail -6 | sed 's/^/    /'

echo "[a2h] verify the identity + created perspective on the node"
CHK=$(cd "$ROOT" && A2H_ADMIN="$ADMIN" npx tsx "$HERE/ad4m-user.ts" check "http://127.0.0.1:${MCP_PORT}" "$JWT" "$PNAME")
echo "$CHK" | sed 's/^/    /'
if ! echo "$CHK" | grep -q 'PERSP=found'; then echo "[a2h] FAIL — Hermes turn did not create the perspective via MCP"; exit 1; fi
if ! echo "$CHK" | grep -q "DID=$DID"; then echo "[a2h] FAIL — provisioned DID mismatch"; exit 1; fi

echo "[a2h] PASS — Hermes provisioned a distinct identity ($DID) and drove an ad4m MCP round-trip (created $PNAME)"
