#!/usr/bin/env bash
#
# A2 — Provision & connect (external / pre-existing multi-user node) via the REAL
# @coasys/openclaw-ad4m plugin.
#
# Complements verify-a2-openclaw.sh (which drives OpenClaw's *native* MCP client):
# this exercises the shipped plugin's own onboarding — `openclaw ad4m-setup`
# external, its `mcpClient`, MCP detection, and the multi-user signup/login branch
# — so it regression-guards the plugin fixes (coasys/ad4m#880 findings 2/3/5).
#
# A real OpenClaw runtime + the installed plugin connect to a pre-running
# multi-user node, provision the assistant's own user identity (signup + login),
# and we verify that identity independently. Hardened + host-isolated; full
# teardown. Needs a dist-built plugin (set AD4M_PLUGIN_DIR); SKIPs otherwise.
#
# Env: KEEP=1 leaves the pod up for debugging.
#
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)" # worktree root (for `npx tsx`)
OC_IMG="${OC_IMG:-ghcr.io/openclaw/openclaw:latest}"
EXE_IMG="${A2_EXEC_IMAGE:-ad4m-test:latest}"
NET=a2xp-net
EXE=a2xp-exec
OC=a2xp-oc
ADMIN=windtunnel-admin
EMAIL="assistant@agent.local"
UPASS="a2xp-pass-123"
KEEP="${KEEP:-}"

PLUGIN_DIR="${AD4M_PLUGIN_DIR:-${AD4M_REPO:+$AD4M_REPO/plugins/ad4m}}"
if [ -z "${PLUGIN_DIR:-}" ] || [ ! -f "$PLUGIN_DIR/package.json" ] || ! grep -q '"build"' "$PLUGIN_DIR/package.json" 2>/dev/null; then
  echo "[a2xp] SKIP — no ad4m plugin with a dist build available (set AD4M_PLUGIN_DIR to a plugins/ad4m checkout carrying coasys/ad4m#880)"
  exit 0
fi

teardown() { [ -n "$KEEP" ] && { echo "[a2xp] KEEP set — leaving pod up"; return; }; docker rm -f "$EXE" "$OC" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true; }
trap teardown EXIT

echo "[a2xp] clean slate"; docker rm -f "$EXE" "$OC" >/dev/null 2>&1 || true; docker network rm "$NET" >/dev/null 2>&1 || true
docker network create "$NET" >/dev/null

echo "[a2xp] build ad4m plugin (dist) from $PLUGIN_DIR"
( cd "$PLUGIN_DIR" && NODE_ENV=development npm install --include=dev >/dev/null 2>&1 && NODE_ENV=development npm run build >/dev/null 2>&1 )
TGZ_NAME=$(cd "$PLUGIN_DIR" && NODE_ENV=development npm pack 2>/dev/null | tail -1)
PLUGIN_TGZ="$PLUGIN_DIR/$TGZ_NAME"

echo "[a2xp] start multi-user node (MCP, Holochain off, hardened)"
docker run -d --name "$EXE" --network "$NET" \
  -e ADMIN_CREDENTIAL="$ADMIN" -e ENABLE_MULTI_USER=true -e ENABLE_MCP=true -e MCP_PORT=3001 \
  -e RUN_HOLOCHAIN=false -e AGENT_PASSPHRASE=windtunnel-pass \
  -p 127.0.0.1:14301:3001 \
  --cap-drop ALL --cap-add CHOWN --cap-add SETUID --cap-add SETGID --cap-add DAC_OVERRIDE --cap-add FOWNER \
  --security-opt no-new-privileges:true --memory 2g --cpus 1.5 --pids-limit 512 \
  "$EXE_IMG" >/dev/null
for i in $(seq 1 60); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 3 -X POST http://127.0.0.1:14301/mcp \
    -H 'Content-Type: application/json' -H 'Accept: application/json, text/event-stream' -H "Authorization: Bearer $ADMIN" \
    -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"w","version":"1"}}}' 2>/dev/null || echo 000)
  [ "$code" = "200" ] && { echo "[a2xp] node MCP ready"; break; }
  sleep 2
done

echo "[a2xp] start OpenClaw + install plugin + configure external multi-user"
docker run -d --name "$OC" --network "$NET" --entrypoint sleep \
  --cap-drop ALL --security-opt no-new-privileges:true --memory 2g --cpus 2 --pids-limit 1024 \
  "$OC_IMG" infinity >/dev/null
docker cp "$PLUGIN_TGZ" "$OC":/tmp/plugin.tgz >/dev/null
rm -f "$PLUGIN_TGZ"
docker exec "$OC" openclaw plugins install /tmp/plugin.tgz >/dev/null 2>&1
cat >/tmp/a2xp-cfg.json5 <<JSON
{ plugins: { entries: { ad4m: { config: { multiUser: true, email: "$EMAIL", password: "$UPASS" } } } } }
JSON
docker cp /tmp/a2xp-cfg.json5 "$OC":/tmp/ec.json5 >/dev/null
docker exec "$OC" openclaw config patch --file /tmp/ec.json5 >/dev/null 2>&1

echo "[a2xp] run ad4m-setup external (multi-user) through the plugin"
timeout 120 docker exec "$OC" openclaw ad4m-setup --endpoint "http://$EXE:3001/mcp" --executor-url "http://$EXE:12000" >/tmp/a2xp-setup.log 2>&1 || true
if ! grep -qE 'Logged in as|own user identity ready' /tmp/a2xp-setup.log; then
  echo "[a2xp] FAIL — plugin setup did not provision an identity"; tail -15 /tmp/a2xp-setup.log | sed 's/^/    /'; exit 1
fi

echo "[a2xp] verify the created identity on the node"
( cd "$ROOT" && npx tsx "$HERE/openclaw/verify-user.ts" "$EMAIL" "$UPASS" http://127.0.0.1:14301 )

echo "[a2xp] PASS — OpenClaw plugin (ad4m-setup external) provisioned + verified a distinct multi-user identity"
