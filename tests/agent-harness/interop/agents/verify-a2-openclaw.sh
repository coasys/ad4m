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
# whole pod down. Nothing touches the host OS: private bridge network, named
# volume only (no host mounts), loopback-only ports, cap-drop + no-new-privileges
# + resource limits, Holochain off.
#
# Env: KEEP=1 leaves the pod running for debugging (default: full teardown).
#
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)" # worktree root (for `npx tsx`)

NET=a2wt-net
VOL=a2wt-exec-data
EXEC=a2wt-exec
MOCK=a2wt-mockllm
OC=a2wt-openclaw
ADMIN=windtunnel-admin
NODE_PASS=windtunnel-pass
EMAIL="a2-openclaw@test.com"
UPASS="a2pass123"
EXEC_IMG="${EXEC_IMG:-ad4m-test:latest}"
OC_IMG="${OC_IMG:-ghcr.io/openclaw/openclaw:latest}"
KEEP="${KEEP:-}"

nuke() {
  docker rm -f "$EXEC" "$MOCK" "$OC" >/dev/null 2>&1 || true
  docker volume rm "$VOL" >/dev/null 2>&1 || true
  docker network rm "$NET" >/dev/null 2>&1 || true
}
teardown() {
  if [ -n "$KEEP" ]; then echo "[a2] KEEP set — leaving pod up"; return; fi
  echo "[a2] teardown"; nuke
}
trap teardown EXIT

echo "[a2] clean slate"; nuke
docker network create "$NET" >/dev/null
docker volume create "$VOL" >/dev/null

echo "[a2] build mock-llm image"
docker build -q -t a2wt-mockllm "$HERE/mock-llm" >/dev/null

echo "[a2] start AD4M node (multi-user, MCP, Holochain off, hardened)"
docker run -d --name "$EXEC" --network "$NET" \
  -e ADMIN_CREDENTIAL="$ADMIN" -e ENABLE_MULTI_USER=true -e ENABLE_MCP=true -e MCP_PORT=3001 \
  -e RUN_HOLOCHAIN=false -e AGENT_PASSPHRASE="$NODE_PASS" -v "$VOL":/data \
  -p 127.0.0.1:14000:12000 -p 127.0.0.1:14001:3001 \
  --cap-drop ALL --cap-add CHOWN --cap-add SETUID --cap-add SETGID --cap-add DAC_OVERRIDE --cap-add FOWNER \
  --security-opt no-new-privileges:true --memory 2g --cpus 1.5 --pids-limit 512 \
  "$EXEC_IMG" >/dev/null
for i in $(seq 1 60); do
  curl -fsS --max-time 3 http://127.0.0.1:14000/health >/dev/null 2>&1 && break
  [ "$(docker inspect -f '{{.State.Status}}' "$EXEC" 2>/dev/null)" = exited ] && { echo "[a2] node exited early"; docker logs --tail 20 "$EXEC"; exit 1; }
  sleep 2
done
curl -fsS --max-time 3 http://127.0.0.1:14000/health >/dev/null || { echo "[a2] node not healthy"; exit 1; }
echo "[a2] node healthy"

echo "[a2] start mock LLM (scripts the onboarding tool sequence)"
SCRIPT='[{"tool_calls":[{"name":"ad4m__signup","arguments":{"email":"'"$EMAIL"'","password":"'"$UPASS"'"}}]},{"tool_calls":[{"name":"ad4m__login_email","arguments":{"email":"'"$EMAIL"'","password":"'"$UPASS"'"}}]},{"tool_calls":[{"name":"ad4m__get_my_did","arguments":{}}]},{"text":"ADAM identity created and verified."}]'
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_SCRIPT="$SCRIPT" a2wt-mockllm >/dev/null

echo "[a2] start OpenClaw (idle, hardened) + configure"
docker run -d --name "$OC" --network "$NET" --entrypoint sleep \
  --cap-drop ALL --security-opt no-new-privileges:true --memory 2g --cpus 2 --pids-limit 1024 \
  "$OC_IMG" infinity >/dev/null
# point OpenClaw at the node's MCP server (probes before saving)
docker exec "$OC" openclaw mcp add ad4m --url "http://$EXEC:3001/mcp" --transport streamable-http \
  --header "Authorization=Bearer $ADMIN" --connect-timeout 10 --timeout 30 >/dev/null
# point OpenClaw's model at the mock
cat >/tmp/a2patch.json5 <<JSON
{ models: { providers: { mock: { api: "openai-completions", baseUrl: "http://$MOCK:8080/v1", apiKey: "mock", auth: "api-key", contextWindow: 8192, maxTokens: 1024, models: [ { id: "mock-model", name: "mock-model" } ] } } } }
JSON
docker cp /tmp/a2patch.json5 "$OC":/tmp/p.json5 >/dev/null
docker exec "$OC" sh -c 'openclaw config patch --file /tmp/p.json5 >/dev/null && openclaw models set mock/mock-model >/dev/null'
echo "[a2] OpenClaw configured: $(docker exec "$OC" openclaw mcp probe ad4m 2>&1 | grep -oE 'ad4m: [0-9]+ tools')"

echo "[a2] run the assistant onboarding turn"
docker exec "$OC" openclaw agent --local --json --timeout 150 --session-key agent:main:a2 \
  --message "Create my ADAM identity: sign up, log in, and report my DID." >/tmp/a2turn.json 2>/dev/null || true
echo "[a2] tools executed: $(grep -oE '"ad4m__(signup|login_email|get_my_did)"' /tmp/a2turn.json | sort -u | tr '\n' ' ')"

echo "[a2] verify the created identity exists on the node"
( cd "$ROOT" && npx tsx "$HERE/openclaw/verify-user.ts" "$EMAIL" "$UPASS" http://127.0.0.1:14001 )

echo "[a2] PASS — OpenClaw assistant provisioned + verified an ADAM agent on a pre-existing multi-user node"
