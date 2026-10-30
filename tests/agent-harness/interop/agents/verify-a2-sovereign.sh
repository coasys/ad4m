#!/usr/bin/env bash
#
# A2 (Sovereign) — provision & connect via the real Sovereign server, headless.
#
# Sovereign runs on the Claude Agent SDK and injects the node's MCP directly into
# every session (config `ad4m.mcpUrl` + a Bearer token file), giving the agent
# mcp__ad4m__* tools. This stands up a headless test-Sovereign image (never the
# live host service), points it at a containerised multi-user node with a
# provisioned user's JWT, drives one chat turn against a mock Anthropic endpoint
# that calls `mcp__ad4m__add_perspective`, and verifies — over MCP — that the
# perspective landed on the node under the provisioned identity. So it proves
# init/connect + a real MCP action round-trip through the actual harness.
#
# Hardened + host-isolated: private network, no host mounts (config + token go in
# via `docker cp`), loopback-only ports, resource limits, Holochain off. Full
# teardown. Needs a locally built Sovereign image (SOVEREIGN_IMAGE, see
# interop/agents/sovereign/Dockerfile); SKIPs if absent.
#
# Env: see lib.sh. KEEP=1 leaves the pod up for debugging.
#
set -euo pipefail
TAG=a2sv
NET=a2sv-net
source "$(dirname "$0")/lib.sh"
EXE=a2sv-exec
SV=a2sv-sovereign
MOCK=a2sv-mock
EMAIL="sovereign-a2@agent.local"
UPASS="sovereign-a2-pass-123"
PNAME="sovereign-a2-channel"
MCP_PORT=14431
SV_PORT=14432

if ! docker image inspect "$SV_IMG" >/dev/null 2>&1; then
  echo "[a2sv] SKIP — no test-Sovereign image ($SV_IMG). Build interop/agents/sovereign/Dockerfile first."
  exit 0
fi

echo "[a2sv] clean slate"; pod_init "$EXE" "$SV" "$MOCK"
build_mock

echo "[a2sv] start multi-user node (MCP, Holochain off, hardened)"
start_node "$EXE" "$MCP_PORT"

echo "[a2sv] provision Sovereign's own user identity (signup + login)"
PROV=$(A2_ADMIN="$ADMIN" "$TSX" "$AGENTS/ad4m-user.ts" provision "http://127.0.0.1:${MCP_PORT}" "$EMAIL" "$UPASS")
JWT=$(echo "$PROV" | grep '^JWT=' | cut -d= -f2- || true)
DID=$(echo "$PROV" | grep '^DID=' | cut -d= -f2- || true)
if [ -z "$JWT" ] || [ -z "$DID" ]; then echo "[a2sv] FAIL — could not provision a user identity"; echo "$PROV" | sed 's/^/    /'; exit 1; fi
echo "[a2sv] provisioned DID: $DID"

echo "[a2sv] start mock LLM (Anthropic streaming; scripted ad4m tool call)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 \
  -e MOCK_LLM_SCRIPT="[{\"tool_calls\":[{\"name\":\"mcp__ad4m__add_perspective\",\"arguments\":{\"name\":\"$PNAME\"}}]},{\"text\":\"created the channel\"}]" \
  "$MOCK_IMG" >/dev/null

echo "[a2sv] start headless Sovereign (ad4m MCP + mock Anthropic)"
CFG="$(mktemp)"; TOK="$(mktemp)"
cat >"$CFG" <<JSON
{
  "server": { "port": 8080, "host": "0.0.0.0", "tls": { "enabled": false } },
  "identity": { "agentName": "Aria", "agentIcon": "A" },
  "workspace": { "root": "/data/home/ws", "globalPath": "" },
  "personality": { "sourceDir": "", "files": [], "separator": "\n" },
  "ad4m": { "host": "", "mcpUrl": "http://$EXE:3001/mcp" },
  "agentBackend": {
    "enabled": ["claude-code"], "default": "claude-code",
    "claudeCode": { "cwd": "/data/home/ws", "agentDir": "/data/home/.claude", "defaultModel": "claude-opus-4-6", "modelContextWindows": { "opus": 200000 } }
  },
  "seed": { "membraneId": "personal", "membraneName": "Personal", "threadLabel": "Main" }
}
JSON
printf '{"token":"%s"}' "$JWT" >"$TOK"
docker create --name "$SV" --network "$NET" \
  -e ANTHROPIC_BASE_URL="http://$MOCK:8080" -e ANTHROPIC_API_KEY=sk-ant-mock -e ANTHROPIC_AUTH_TOKEN=mock-token -e IS_SANDBOX=1 \
  -p 127.0.0.1:${SV_PORT}:8080 \
  --security-opt no-new-privileges:true --memory 3g --cpus 2 --pids-limit 1024 \
  "$SV_IMG" >/dev/null
docker cp "$CFG" "$SV":/data/config/config.json >/dev/null
docker cp "$TOK" "$SV":/data/data/ad4m-token.json >/dev/null
rm -f "$CFG" "$TOK"
docker start "$SV" >/dev/null
for i in $(seq 1 45); do
  curl -s -o /dev/null -w '%{http_code}' --max-time 3 "http://127.0.0.1:${SV_PORT}/health" 2>/dev/null | grep -q 200 && { echo "[a2sv] Sovereign healthy"; break; }
  sleep 2
done

echo "[a2sv] drive one chat turn (agent calls mcp__ad4m__add_perspective)"
TID=$(curl -s --max-time 5 "http://127.0.0.1:${SV_PORT}/api/threads" | python3 -c '
import sys,json
try: d=json.load(sys.stdin)
except: print(""); sys.exit()
ts = d if isinstance(d,list) else (d.get("threads") or d.get("data") or [])
def idof(t): return t.get("id") or t.get("threadId") or t.get("key") or ""
main=[t for t in ts if "main" in str(t.get("label",t.get("name",""))).lower()]
print(idof(main[0]) if main else (idof(ts[0]) if ts else ""))')
echo "[a2sv] thread: ${TID:-<none>}"
if [ -z "$TID" ]; then echo "[a2sv] FAIL — no thread available to drive"; docker logs "$SV" 2>&1 | tail -20 | sed 's/^/    /'; exit 1; fi
curl -s --max-time 10 -X POST "http://127.0.0.1:${SV_PORT}/api/chat/send" \
  -H 'Content-Type: application/json' \
  -d "{\"threadId\":\"$TID\",\"message\":\"Create an ad4m channel perspective named $PNAME using your ad4m tools.\"}" >/dev/null || true

echo "[a2sv] wait for the ad4m round-trip to land on the node"
FOUND=""
for i in $(seq 1 30); do
  CHK=$("$TSX" "$AGENTS/ad4m-user.ts" check "http://127.0.0.1:${MCP_PORT}" "$JWT" "$PNAME" 2>/dev/null || true)
  if echo "$CHK" | grep -q 'PERSP=found'; then FOUND=1; break; fi
  sleep 3
done
if [ -z "$FOUND" ]; then
  echo "[a2sv] FAIL — Sovereign turn did not create the perspective via ad4m MCP"
  docker logs "$SV" 2>&1 | tail -25 | sed 's/^/    /'
  docker logs "$MOCK" 2>&1 | tail -10 | sed 's/^/    mock: /'
  exit 1
fi

echo "[a2sv] PASS — headless Sovereign connected as $DID and drove an ad4m MCP round-trip (created $PNAME)"
