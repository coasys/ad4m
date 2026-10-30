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
# Env: see lib.sh. KEEP=1 leaves the pod up for debugging.
#
set -euo pipefail
TAG=a2h
NET=a2h-net
source "$(dirname "$0")/lib.sh"
EXE=a2h-exec
OC=a2h-hermes
MOCK=a2h-mock
EMAIL="hermes-a2@agent.local"
UPASS="hermes-a2-pass-123"
PNAME="hermes-a2-channel"
MCP_PORT=14420

need_image "$HER_IMG"
echo "[a2h] clean slate"; pod_init "$EXE" "$OC" "$MOCK"
build_mock

echo "[a2h] start multi-user node (MCP, Holochain off, hardened)"
start_node "$EXE" "$MCP_PORT"

echo "[a2h] provision Hermes's own user identity (signup + login)"
PROV=$(A2_ADMIN="$ADMIN" "$TSX" "$AGENTS/ad4m-user.ts" provision "http://127.0.0.1:${MCP_PORT}" "$EMAIL" "$UPASS")
JWT=$(echo "$PROV" | grep '^JWT=' | cut -d= -f2- || true)
DID=$(echo "$PROV" | grep '^DID=' | cut -d= -f2- || true)
if [ -z "$JWT" ] || [ -z "$DID" ]; then echo "[a2h] FAIL — could not provision a user identity"; echo "$PROV" | sed 's/^/    /'; exit 1; fi
echo "[a2h] provisioned DID: $DID"

echo "[a2h] start mock LLM (scripted ad4m tool calls for the turn)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 \
  -e MOCK_LLM_SCRIPT="[{\"tool_calls\":[{\"name\":\"mcp__ad4m__get_my_did\",\"arguments\":{}}]},{\"tool_calls\":[{\"name\":\"mcp__ad4m__add_perspective\",\"arguments\":{\"name\":\"$PNAME\"}}]},{\"text\":\"created channel\"}]" \
  "$MOCK_IMG" >/dev/null

echo "[a2h] configure Hermes (mcp_servers.ad4m + mock model) and run one turn"
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
docker cp "$CFG" "$OC":/opt/data/config.yaml >/dev/null; rm -f "$CFG"
OUT=$(timeout 220 docker start -a "$OC" 2>&1 || true)
echo "$OUT" | grep -iE 'created channel|error|ad4m' | tail -6 | sed 's/^/    /' || true

echo "[a2h] verify the identity + created perspective on the node"
CHK=$("$TSX" "$AGENTS/ad4m-user.ts" check "http://127.0.0.1:${MCP_PORT}" "$JWT" "$PNAME")
echo "$CHK" | sed 's/^/    /'
if ! echo "$CHK" | grep -q 'PERSP=found'; then echo "[a2h] FAIL — Hermes turn did not create the perspective via MCP"; exit 1; fi
if ! echo "$CHK" | grep -q "DID=$DID"; then echo "[a2h] FAIL — provisioned DID mismatch"; exit 1; fi

echo "[a2h] PASS — Hermes provisioned a distinct identity ($DID) and drove an ad4m MCP round-trip (created $PNAME)"
