#!/usr/bin/env bash
#
# A4 (Sovereign) — Waker. A neighbourhood event wakes Sovereign and it acts,
# then writes its reply back into the same ad4m channel. Full closed loop.
#
# Drives Sovereign's OWN in-server native waker (packages/ad4m): the scenario
# stands it up (ad4m.host + user token + agentName Aria), pins a perspective via
# POST /api/ad4m/watch/perspectives, then injects a real @mention message on the
# node (a has_child + message_body link whose body names the agent). The waker's
# SPARQL mention query detects it, resolves the parent channel, and emits an
# ad4m.thread.message wake into Sovereign's presence-internal thread. The presence
# agent then runs a turn and calls presence_reply_ad4m, which posts a child
# message back into the channel.
#
# Asserts the FULL loop: (1) the native waker woke an act-capable presence turn
# — proven by the mock recording, per request, that the mention text reached the
# turn AND the ad4m reply tool was offered; and (2) the reply write-back landed —
# a fresh channel child whose body carries the reply marker.
#
# Two real Sovereign bugs this scenario surfaced and now guards against:
#   * the SDK WS reconnect drops the server-side SPARQL subscription without
#     re-registering it → the waker carries a querySparql polling safety-net;
#   * the waker's parent-channel resolver parsed the wrong querySparql result
#     shape (results.bindings vs the flat array the executor returns), so every
#     wake carried an empty channelAddress and the reply could never post.
#
# Hardened + host-isolated, full teardown. Needs a locally built Sovereign image
# (SOVEREIGN_IMAGE); SKIPs if absent. See lib.sh for env. KEEP=1 leaves the pod.
#
set -euo pipefail
TAG=a4sv
NET=a4sv-net
source "$(dirname "$0")/lib.sh"
EXE=a4sv-exec
SV=a4sv-sovereign
MOCK=a4sv-mock
EMAIL="sovereign-a4@agent.local"
UPASS="sovereign-a4-pass-123"
CHAN="a4sv://channel"
MARKER="A4SV_REPLY_OK"
MCP_PORT=14441
SV_PORT=14442

if ! docker image inspect "$SV_IMG" >/dev/null 2>&1; then
  echo "[a4sv] SKIP — no test-Sovereign image ($SV_IMG). Build interop/agents/sovereign/Dockerfile first."
  exit 0
fi

echo "[a4sv] clean slate"; pod_init "$EXE" "$SV" "$MOCK"
build_mock

echo "[a4sv] start multi-user node (MCP + WS, Holochain off, hardened)"
start_node "$EXE" "$MCP_PORT"

echo "[a4sv] provision a user + mint a channel perspective"
PROV=$(A2_ADMIN="$ADMIN" "$TSX" "$AGENTS/ad4m-user.ts" provision "http://127.0.0.1:${MCP_PORT}" "$EMAIL" "$UPASS")
JWT=$(echo "$PROV" | grep '^JWT=' | cut -d= -f2- || true); DID=$(echo "$PROV" | grep '^DID=' | cut -d= -f2- || true)
[ -z "$JWT" ] && { echo "[a4sv] FAIL — provisioning"; echo "$PROV" | sed -e 's/^JWT=.*/JWT=<redacted>/' -e 's/^/    /'; exit 1; }
UUID=$("$TSX" "$AGENTS/sovereign/waker-ad4m.ts" mint "http://127.0.0.1:${MCP_PORT}" "$JWT" "a4sv-channel" | grep '^UUID=' | cut -d= -f2- || true)
[ -z "$UUID" ] && { echo "[a4sv] FAIL — could not mint perspective"; exit 1; }
echo "[a4sv] DID=$DID perspective=$UUID"

echo "[a4sv] start mock LLM (presence turn replies via presence_reply_ad4m)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 \
  -e MOCK_LLM_TRIGGER="mentioned you" \
  -e MOCK_LLM_SCRIPT="[{\"tool_calls\":[{\"name\":\"mcp__sovereign__presence_reply_ad4m\",\"arguments\":{\"text\":\"$MARKER acknowledged the mention\"}}]},{\"text\":\"done\"}]" \
  "$MOCK_IMG" >/dev/null

echo "[a4sv] start headless Sovereign (ad4m.host waker + agentName Aria + mock Anthropic)"
CFG="$(mktemp)"; TOK="$(mktemp)"
cat >"$CFG" <<JSON
{
  "server": { "port": 8080, "host": "0.0.0.0", "tls": { "enabled": false } },
  "identity": { "agentName": "Aria", "agentIcon": "A" },
  "workspace": { "root": "/data/home/ws", "globalPath": "" },
  "personality": { "sourceDir": "", "files": [], "separator": "\n" },
  "ad4m": { "host": "http://$EXE:12000", "mcpUrl": "http://$EXE:3001/mcp" },
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
  curl -s -o /dev/null -w '%{http_code}' --max-time 3 "http://127.0.0.1:${SV_PORT}/health" 2>/dev/null | grep -q 200 && { echo "[a4sv] Sovereign healthy"; break; }
  sleep 2
done
# Let the waker connect + the agent DID resolve, AND let boot/auto-start turns
# settle to idle before we inject — so the presence turn runs alone and the
# in-server `activeSessionKey` (which presence_reply_ad4m's internal-gate reads)
# is not stomped by a concurrent boot turn.
sleep 6
for i in $(seq 1 20); do
  st=$(curl -s --max-time 4 "http://127.0.0.1:${SV_PORT}/api/threads" 2>/dev/null | python3 -c 'import sys,json
try: d=json.load(sys.stdin)
except: print("busy"); sys.exit()
ts=d if isinstance(d,list) else (d.get("threads") or [])
print("busy" if any(str(t.get("agentStatus","")).lower() in ("working","thinking") for t in ts) else "idle")' 2>/dev/null || echo busy)
  [ "$st" = "idle" ] && break
  sleep 3
done

echo "[a4sv] resolve presence-internal thread"
INT=$(curl -s --max-time 5 "http://127.0.0.1:${SV_PORT}/api/threads/presence" | python3 -c '
import sys,json
try: d=json.load(sys.stdin)
except: print(""); sys.exit()
def idof(t): return (t or {}).get("id") or (t or {}).get("key") or (t or {}).get("threadId") or ""
if isinstance(d,dict) and d.get("internal"): print(idof(d["internal"]));
else:
  items = d if isinstance(d,list) else (d.get("threads") or list(d.values()) if isinstance(d,dict) else [])
  cand=[t for t in items if isinstance(t,dict) and "internal" in str(t.get("label",t.get("role",""))).lower()]
  print(idof(cand[0]) if cand else (idof(items[0]) if items else ""))')
echo "[a4sv] presence-internal thread: ${INT:-<none>}"
if [ -z "$INT" ]; then echo "[a4sv] FAIL — no presence-internal thread"; docker logs "$SV" 2>&1 | tail -15 | sed 's/^/    /'; exit 1; fi

echo "[a4sv] pin the perspective for Sovereign's native waker"
curl -s --max-time 6 -X POST "http://127.0.0.1:${SV_PORT}/api/ad4m/watch/perspectives" \
  -H 'Content-Type: application/json' -d "{\"uuid\":\"$UUID\",\"label\":\"a4sv\"}" | sed 's/^/    /'
for i in $(seq 1 15); do
  docker logs "$SV" 2>&1 | grep -q "mention subscription active for $UUID" && { echo "[a4sv] native waker subscribed"; break; }
  sleep 2
done

echo "[a4sv] inject a mention message ('Hey Aria …') on the node"
"$TSX" "$AGENTS/sovereign/waker-ad4m.ts" mention "http://127.0.0.1:${MCP_PORT}" "$JWT" "$UUID" "$CHAN" "$CHAN/msg/1" "Hey Aria, please acknowledge this" | sed 's/^/    /'

echo "[a4sv] wait for native waker -> presence agent turn (act-capable)"
# Gate = the full loop. WOKE: the mock records, per request, whether the mention
# text reached the turn AND the ad4m reply tool was offered — an unambiguous,
# harness-agnostic "woke an act-capable turn" signal. REPLY: the presence agent's
# presence_reply_ad4m write-back landed as a fresh channel child carrying the
# marker. Keep polling for the reply after the wake — the write-back is async.
WOKE=""; REPLY=""
for i in $(seq 1 45); do
  if [ -z "$WOKE" ] && docker logs "$MOCK" 2>&1 | grep -q 'has_mention=true has_presence_reply_tool=true'; then WOKE=1; fi
  if [ -n "$WOKE" ]; then
    R=$( ("$TSX" "$AGENTS/sovereign/waker-ad4m.ts" child-has "http://127.0.0.1:${MCP_PORT}" "$JWT" "$UUID" "$CHAN" "$MARKER" 2>/dev/null || true) | grep '^REPLY=' | cut -d= -f2- || true)
    [ "$R" = "found" ] && { REPLY=1; break; }
  fi
  sleep 3
done
echo "[a4sv] wokeActCapableTurn=${WOKE:-0} replyLanded=${REPLY:-0}"
if [ -z "$WOKE" ]; then
  echo "[a4sv] FAIL — Sovereign's native waker did not wake a presence agent turn on the ad4m mention"
  docker logs "$SV" 2>&1 | grep -iE 'waker|mention|presence' | grep -viE 'WebSocket error' | tail -20 | sed 's/^/    /'
  exit 1
fi
if [ -z "$REPLY" ]; then
  echo "[a4sv] FAIL — presence agent woke but its ad4m reply write-back did not land in the channel"
  docker logs "$SV" 2>&1 | grep -iE 'waker|mention|presence|reply' | grep -viE 'WebSocket error' | tail -20 | sed 's/^/    /'
  exit 1
fi
echo "[a4sv] PASS — Sovereign's native ad4m waker woke the presence agent on a mention AND its presence_reply_ad4m write-back landed back in the channel (full loop)"
