#!/usr/bin/env bash
#
# A2 — Provision & connect (external / pre-existing multi-user node), OpenClaw.
#
# Self-contained, reproducible from a clean slate: it stands up a hardened,
# Docker-isolated pod (multi-user AD4M node + deterministic mock LLM + real
# OpenClaw), points OpenClaw at the node's MCP server and the mock model, then
# runs one OpenClaw agent turn where the assistant — instructed in natural
# language — creates its own ADAM agent identity (signup -> login -> report DID).
# We then independently verify that identity exists on the node, and tear the
# whole pod down. Nothing touches the host OS: private bridge network, no host
# mounts, loopback-only ports, cap-drop + no-new-privileges + resource limits,
# Holochain off.
#
# Env: see lib.sh. KEEP=1 leaves the pod running for debugging.
#
set -euo pipefail
TAG=a2
NET=a2wt-net
source "$(dirname "$0")/lib.sh"
EXE=a2wt-exec
MOCK=a2wt-mockllm
OC=a2wt-openclaw
EMAIL="a2-openclaw@test.com"
UPASS="a2pass123"
MCP_PORT=14001

need_image "$OC_IMG"
echo "[a2] clean slate"; pod_init "$EXE" "$MOCK" "$OC"
build_mock

echo "[a2] start AD4M node (multi-user, MCP, Holochain off, hardened)"
start_node "$EXE" "$MCP_PORT"

echo "[a2] start mock LLM (scripts the onboarding tool sequence)"
SCRIPT='[{"tool_calls":[{"name":"ad4m__signup","arguments":{"email":"'"$EMAIL"'","password":"'"$UPASS"'"}}]},{"tool_calls":[{"name":"ad4m__login_email","arguments":{"email":"'"$EMAIL"'","password":"'"$UPASS"'"}}]},{"tool_calls":[{"name":"ad4m__get_my_did","arguments":{}}]},{"text":"ADAM identity created and verified."}]'
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 -e MOCK_LLM_SCRIPT="$SCRIPT" "$MOCK_IMG" >/dev/null

echo "[a2] start OpenClaw (idle, hardened) + configure"
docker run -d --name "$OC" --network "$NET" --entrypoint sleep \
  --cap-drop ALL --security-opt no-new-privileges:true --memory 2g --cpus 2 --pids-limit 1024 \
  "$OC_IMG" infinity >/dev/null
# point OpenClaw at the node's MCP server (probes before saving)
docker exec "$OC" openclaw mcp add ad4m --url "http://$EXE:3001/mcp" --transport streamable-http \
  --header "Authorization=Bearer $ADMIN" --connect-timeout 10 --timeout 30 >/dev/null
# point OpenClaw's model at the mock
PATCH="$(mktemp)"
cat >"$PATCH" <<JSON
{ models: { providers: { mock: { api: "openai-completions", baseUrl: "http://$MOCK:8080/v1", apiKey: "mock", auth: "api-key", contextWindow: 8192, maxTokens: 1024, models: [ { id: "mock-model", name: "mock-model" } ] } } } }
JSON
docker cp "$PATCH" "$OC":/tmp/p.json5 >/dev/null; rm -f "$PATCH"
docker exec "$OC" sh -c 'openclaw config patch --file /tmp/p.json5 >/dev/null && openclaw models set mock/mock-model >/dev/null'
echo "[a2] OpenClaw configured: $(docker exec "$OC" openclaw mcp probe ad4m 2>&1 | grep -oE 'ad4m: [0-9]+ tools')"

echo "[a2] run the assistant onboarding turn"
TURN=$(docker exec "$OC" openclaw agent --local --json --timeout 150 --session-key agent:main:a2 \
  --message "Create my ADAM identity: sign up, log in, and report my DID." 2>/dev/null || true)
echo "[a2] tools executed: $(echo "$TURN" | grep -oE '"ad4m__(signup|login_email|get_my_did)"' | sort -u | tr '\n' ' ')"

echo "[a2] verify the created identity exists on the node"
"$TSX" "$AGENTS/openclaw/verify-user.ts" "$EMAIL" "$UPASS" "http://127.0.0.1:$MCP_PORT"

echo "[a2] PASS — OpenClaw assistant provisioned + verified an ADAM agent on a pre-existing multi-user node"
