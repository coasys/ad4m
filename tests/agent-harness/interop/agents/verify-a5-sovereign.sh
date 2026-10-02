#!/usr/bin/env bash
#
# A5 (Sovereign) — A/V action loop (mocked). Sovereign wakes on a mocked call:
# a call-presence entry marks a live call on a channel, a transcript names the
# agent in free speech, and Sovereign reads the transcript and replies into the
# channel. Media transport is mocked; presence + transcript + reply are all real
# AD4M perspective links / Message expressions.
#
# Reuses A4-sovereign's proven full loop — Sovereign's OWN in-server native waker
# (packages/ad4m) -> the presence-internal agent -> presence_reply_ad4m write-back.
# The A5 deltas: (1) a mocked call-presence entry seeded on the channel, (2) a
# transcript Message whose body names the agent in FREE, natural speech ("… hey
# Aria, can you summarise the last point?") — the spoken-name wake, not a
# structured @mention, and (3) a content-gated perceive proof (negative control).
#
# Asserts the FULL loop:
#   (a) WAKE      — the transcript's spoken name wakes an act-capable turn exactly
#                   once (the mock records, per request, that the mention text
#                   reached the turn AND the ad4m reply tool was offered; the
#                   waker logs exactly one mention emit);
#   (b) PERCEIVE  — the wake is gated on the TRANSCRIPT CONTENT: the mocked
#                   call-presence entry alone (which never names the agent)
#                   produces NO wake; only the transcript does. The waker's SPARQL
#                   mention query reads + decodes each body via
#                   ad4m://fn/parse_literal and fires only on a content match.
#   (c) REPLY     — the presence agent's presence_reply_ad4m write-back landed as
#                   a fresh channel child carrying the reply marker.
#   plus the call-presence entry is readable back over MCP.
#
# NOTE (honest scope): the Sovereign presence-internal turn exposes only presence
# tools, not ad4m read tools, and the native waker's inline body resolution does
# not surface in this image — so perceive is proven by content-gating (the wake
# rides the transcript content), not by an explicit agent-issued read. OpenClaw +
# Hermes, whose woken turns DO carry ad4m read tools, prove perceive by an
# explicit transcript read.
#
# True cross-user visibility needs neighbourhood sync, which is OUT of scope — the
# reply is asserted to land + be readable, not to sync to a second agent.
#
# No Sovereign source changes are needed — A5 reuses the A4 waker/reply loop.
#
# Hardened + host-isolated, full teardown. Needs the swwt-sovereign image + a
# plugins/ad4m checkout (AD4M_PLUGIN_DIR); SKIPs if absent. KEEP=1 leaves the pod.
#
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
SV_IMG="${SOVEREIGN_IMAGE:-swwt-sovereign:latest}"
EXE_IMG="${A5SV_EXEC_IMAGE:-ad4m-test:latest}"
MOCK_IMG="${A5SV_MOCK_IMAGE:-a2wt-mockllm}"
NET=a5sv-net
EXE=a5sv-exec
SV=a5sv-sovereign
MOCK=a5sv-mock
ADMIN=windtunnel-admin
EMAIL="sovereign-a5@agent.local"
UPASS="sovereign-a5-pass-123"
CHAN="a5sv://channel"
MARKER="A5SV_REPLY_OK"
TRANSCRIPT="Earlier on the call someone said: hey Aria, can you summarise the last point about the soil trial?"
MARK="summarise the last point"
MCP_PORT=14451
SV_PORT=14452
KEEP="${KEEP:-}"

if ! docker image inspect "$SV_IMG" >/dev/null 2>&1; then
  echo "[a5sv] SKIP — no test-Sovereign image ($SV_IMG). Build interop/agents/sovereign/Dockerfile first."
  exit 0
fi
PLUGIN_DIR="${AD4M_PLUGIN_DIR:-${AD4M_REPO:+$AD4M_REPO/plugins/ad4m}}"
if [ -z "${PLUGIN_DIR:-}" ] || [ ! -f "$PLUGIN_DIR/package.json" ]; then
  echo "[a5sv] SKIP — no ad4m plugin for the waker driver (set AD4M_PLUGIN_DIR to a plugins/ad4m checkout)"
  exit 0
fi
[ -d "$PLUGIN_DIR/node_modules/@coasys/ad4m" ] || ( cd "$PLUGIN_DIR" && NODE_ENV=development npm install --include=dev >/dev/null 2>&1 )

teardown() { [ -n "$KEEP" ] && { echo "[a5sv] KEEP set — leaving pod up"; return; }; docker rm -f "$EXE" "$SV" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true; }
trap teardown EXIT

echo "[a5sv] clean slate"; docker rm -f "$EXE" "$SV" "$MOCK" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true
docker network create "$NET" >/dev/null
# Always (re)build the mock image — A5 relies on the current server.mjs
# (MOCK_LLM_MARK perceive flag). The build is a trivial COPY, so this is cheap.
docker build -q -t "$MOCK_IMG" "$HERE/mock-llm" >/dev/null

echo "[a5sv] start multi-user node (MCP + WS, Holochain off, hardened)"
docker run -d --name "$EXE" --network "$NET" \
  -e ADMIN_CREDENTIAL="$ADMIN" -e ENABLE_MULTI_USER=true -e ENABLE_MCP=true -e MCP_PORT=3001 \
  -e RUN_HOLOCHAIN=false -e AGENT_PASSPHRASE=windtunnel-pass \
  -p 127.0.0.1:${MCP_PORT}:3001 -p 127.0.0.1:14453:12000 \
  --cap-drop ALL --cap-add CHOWN --cap-add SETUID --cap-add SETGID --cap-add DAC_OVERRIDE --cap-add FOWNER \
  --security-opt no-new-privileges:true --memory 4g --cpus 1.5 --pids-limit 512 \
  "$EXE_IMG" >/dev/null
for i in $(seq 1 60); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 -X POST http://127.0.0.1:${MCP_PORT}/mcp \
    -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' -H "Authorization: Bearer $ADMIN" \
    -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"w","version":"1"}}}' 2>/dev/null || echo 000)
  [ "$code" = "200" ] && { echo "[a5sv] node MCP ready"; break; }
  sleep 2
done

echo "[a5sv] provision a user + mint a channel perspective"
PROV=$(cd "$ROOT" && A2_ADMIN="$ADMIN" npx tsx "$HERE/ad4m-user.ts" provision "http://127.0.0.1:${MCP_PORT}" "$EMAIL" "$UPASS")
JWT=$(echo "$PROV" | grep '^JWT=' | cut -d= -f2-); DID=$(echo "$PROV" | grep '^DID=' | cut -d= -f2-)
[ -z "$JWT" ] && { echo "[a5sv] FAIL — provisioning"; echo "$PROV" | sed 's/^/    /'; exit 1; }
UUID=$(cd "$ROOT" && npx tsx "$HERE/sovereign/waker-ad4m.ts" mint "http://127.0.0.1:${MCP_PORT}" "$JWT" "a5sv-channel" | grep '^UUID=' | cut -d= -f2-)
[ -z "$UUID" ] && { echo "[a5sv] FAIL — could not mint perspective"; exit 1; }
echo "[a5sv] DID=$DID perspective=$UUID"

echo "[a5sv] start mock LLM (presence turn replies via presence_reply_ad4m)"
docker run -d --name "$MOCK" --network "$NET" -e MOCK_LLM_LOG=1 \
  -e MOCK_LLM_TRIGGER="mentioned you" \
  -e MOCK_LLM_MARK="$MARK" \
  -e MOCK_LLM_SCRIPT="[{\"tool_calls\":[{\"name\":\"mcp__sovereign__presence_reply_ad4m\",\"arguments\":{\"text\":\"$MARKER — on the call transcript, summarising the last point\"}}]},{\"text\":\"done\"}]" \
  "$MOCK_IMG" >/dev/null

echo "[a5sv] start headless Sovereign (ad4m.host waker + agentName Aria + mock Anthropic)"
cat >/tmp/a5sv-config.json <<JSON
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
printf '{"token":"%s"}' "$JWT" >/tmp/a5sv-token.json
docker create --name "$SV" --network "$NET" \
  -e ANTHROPIC_BASE_URL="http://$MOCK:8080" -e ANTHROPIC_API_KEY=sk-ant-mock -e ANTHROPIC_AUTH_TOKEN=mock-token -e IS_SANDBOX=1 \
  -p 127.0.0.1:${SV_PORT}:8080 \
  --security-opt no-new-privileges:true --memory 3g --cpus 2 --pids-limit 1024 \
  "$SV_IMG" >/dev/null
docker cp /tmp/a5sv-config.json "$SV":/data/config/config.json >/dev/null
docker cp /tmp/a5sv-token.json "$SV":/data/data/ad4m-token.json >/dev/null
docker start "$SV" >/dev/null
for i in $(seq 1 45); do
  curl -s -o /dev/null -w '%{http_code}' --max-time 3 "http://127.0.0.1:${SV_PORT}/health" 2>/dev/null | grep -q 200 && { echo "[a5sv] Sovereign healthy"; break; }
  sleep 2
done
# Let the waker connect + the agent DID resolve, AND let boot/auto-start turns
# settle to idle before we seed — so the presence turn runs alone and the
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

echo "[a5sv] resolve presence-internal thread"
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
echo "[a5sv] presence-internal thread: ${INT:-<none>}"
if [ -z "$INT" ]; then echo "[a5sv] FAIL — no presence-internal thread"; docker logs "$SV" 2>&1 | tail -15 | sed 's/^/    /'; exit 1; fi

echo "[a5sv] pin the perspective for Sovereign's native waker"
curl -s --max-time 6 -X POST "http://127.0.0.1:${SV_PORT}/api/ad4m/watch/perspectives" \
  -H 'Content-Type: application/json' -d "{\"uuid\":\"$UUID\",\"label\":\"a5sv\"}" | sed 's/^/    /'
for i in $(seq 1 15); do
  docker logs "$SV" 2>&1 | grep -q "mention subscription active for $UUID" && { echo "[a5sv] native waker subscribed"; break; }
  sleep 2
done

# ── PERCEIVE / negative control ──────────────────────────────────────────────
# The Sovereign presence-internal turn exposes only presence tools (reply_ad4m,
# reply_text, watch, …) — NOT ad4m read tools — so the agent cannot issue its own
# transcript read; and the native waker's inline body resolution does not surface
# in this image (resolveChildBody returns null over the SDK WS). So A5 proves the
# agent perceived the TRANSCRIPT CONTENT (not mere channel activity) by a negative
# control: the mocked call-presence entry alone — which never names the agent —
# must produce NO wake; only the transcript, whose body names the agent in free
# speech, wakes the presence agent. The waker's SPARQL mention query reads +
# decodes each body via ad4m://fn/parse_literal and fires only on a content match,
# so a wake that appears only after the transcript proves the transcript content
# drove it.
echo "[a5sv] seed ONLY the mocked call-presence entry (negative control)"
cd "$ROOT" && npx tsx "$HERE/mock-av.ts" seed-presence "http://127.0.0.1:${MCP_PORT}" "$JWT" "$UUID" "$CHAN" | sed 's/^/    /'

echo "[a5sv] assert the call-presence entry is readable over MCP"
PRES=$( (cd "$ROOT" && npx tsx "$HERE/mock-av.ts" call-active "http://127.0.0.1:${MCP_PORT}" "$JWT" "$UUID" "$CHAN" 2>/dev/null || true) | grep '^PRESENCE=' | cut -d= -f2- || true)
echo "[a5sv] callPresence=${PRES:-absent}"
if [ "$PRES" != "active" ]; then echo "[a5sv] FAIL — call-presence entry did not land / is not readable"; exit 1; fi

echo "[a5sv] negative control: confirm the call-presence entry alone does NOT wake the agent (~10s)"
sleep 10
NEG_WAKES=$(docker logs "$SV" 2>&1 | grep -c 'new mention' || true)
NEG_TURNS=$(docker logs "$MOCK" 2>&1 | grep -c 'has_mention=true has_presence_reply_tool=true' || true)
echo "[a5sv] after presence-only: wakerMentions=$NEG_WAKES actCapableTurns=$NEG_TURNS (both must be 0)"
if [ "$NEG_WAKES" -ne 0 ] || [ "$NEG_TURNS" -ne 0 ]; then
  echo "[a5sv] FAIL — the call-presence entry alone woke the agent; the wake is not gated on the transcript content"
  docker logs "$SV" 2>&1 | grep -iE 'waker|mention' | grep -viE 'WebSocket error' | tail -10 | sed 's/^/    /'
  exit 1
fi

# ── WAKE + ACT ───────────────────────────────────────────────────────────────
echo "[a5sv] seed the transcript (free-text spoken name) — the wake trigger"
cd "$ROOT" && npx tsx "$HERE/mock-av.ts" seed-transcript "http://127.0.0.1:${MCP_PORT}" "$JWT" "$UUID" "$CHAN" "$TRANSCRIPT" | sed 's/^/    /'

echo "[a5sv] wait for native waker -> presence agent turn -> reply write-back"
# WAKE: the transcript reached an act-capable turn — the mock records, per request,
# that the mention text reached the turn AND the ad4m reply tool was offered.
# REPLY: the presence agent's presence_reply_ad4m write-back landed as a fresh
# channel child carrying the marker (poll — the write-back is async).
WOKE=""; REPLY=""
for i in $(seq 1 45); do
  if [ -z "$WOKE" ] && docker logs "$MOCK" 2>&1 | grep -q 'has_mention=true has_presence_reply_tool=true'; then WOKE=1; fi
  if [ -n "$WOKE" ]; then
    R=$( (cd "$ROOT" && npx tsx "$HERE/sovereign/waker-ad4m.ts" child-has "http://127.0.0.1:${MCP_PORT}" "$JWT" "$UUID" "$CHAN" "$MARKER" 2>/dev/null || true) | grep '^REPLY=' | cut -d= -f2- || true)
    [ "$R" = "found" ] && { REPLY=1; break; }
  fi
  sleep 3
done
ONE_WAKE=$(docker logs "$SV" 2>&1 | grep -c 'new mention' || true)
echo "[a5sv] wokeActCapableTurn=${WOKE:-0} wakerMentions=$ONE_WAKE replyLanded=${REPLY:-0}"
if [ -z "$WOKE" ]; then
  echo "[a5sv] FAIL — Sovereign's native waker did not wake a presence agent turn on the transcript's spoken name"
  docker logs "$SV" 2>&1 | grep -iE 'waker|mention|presence' | grep -viE 'WebSocket error' | tail -20 | sed 's/^/    /'
  exit 1
fi
if [ "$ONE_WAKE" -ne 1 ]; then
  echo "[a5sv] FAIL — expected exactly one wake on the transcript, saw $ONE_WAKE"
  exit 1
fi
if [ -z "$REPLY" ]; then
  echo "[a5sv] FAIL — presence agent woke on the transcript but its ad4m reply write-back did not land in the channel"
  docker logs "$SV" 2>&1 | grep -iE 'waker|mention|presence|reply' | grep -viE 'WebSocket error' | tail -20 | sed 's/^/    /'
  exit 1
fi
echo "[a5sv] PASS — call-presence entry alone did not wake; the transcript's spoken name woke the presence agent exactly once (content-gated perceive), and its presence_reply_ad4m reply landed back in the channel (full mocked-A/V loop)"
