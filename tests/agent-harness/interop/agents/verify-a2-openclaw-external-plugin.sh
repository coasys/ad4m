#!/usr/bin/env bash
#
# A2 — Provision & connect (external / pre-existing multi-user node) via the REAL
# @coasys/openclaw-ad4m plugin.
#
# Complements verify-a2-openclaw.sh (which drives OpenClaw's *native* MCP client):
# this exercises the plugin's own onboarding — `openclaw ad4m-setup` external, its
# `mcpClient`, MCP detection, and the multi-user signup/login branch — so it
# regression-guards the plugin.
#
# A real OpenClaw runtime + the installed plugin (packed from this repo's
# plugins/ad4m) connect to a pre-running multi-user node, provision the
# assistant's own user identity (signup + login), and we verify that identity
# independently. Hardened + host-isolated; full teardown.
#
# Env: see lib.sh. KEEP=1 leaves the pod up for debugging.
#
set -euo pipefail
TAG=a2xp
NET=a2xp-net
source "$(dirname "$0")/lib.sh"
EXE=a2xp-exec
OC=a2xp-oc
EMAIL="assistant@agent.local"
UPASS="a2xp-pass-123"
MCP_PORT=14301

need_image "$OC_IMG"
echo "[a2xp] clean slate"; pod_init "$EXE" "$OC"

echo "[a2xp] build + pack the ad4m plugin from $PLUGIN_DIR"
build_plugin
TGZ_NAME=$(cd "$PLUGIN_DIR" && NODE_ENV=development npm pack 2>/dev/null | tail -1)
PLUGIN_TGZ="$PLUGIN_DIR/$TGZ_NAME"

echo "[a2xp] start multi-user node (MCP, Holochain off, hardened)"
start_node "$EXE" "$MCP_PORT"

echo "[a2xp] start OpenClaw + install plugin + configure external multi-user"
docker run -d --name "$OC" --network "$NET" --entrypoint sleep \
  --cap-drop ALL --security-opt no-new-privileges:true --memory 2g --cpus 2 --pids-limit 1024 \
  "$OC_IMG" infinity >/dev/null
docker cp "$PLUGIN_TGZ" "$OC":/tmp/plugin.tgz >/dev/null
rm -f "$PLUGIN_TGZ"
docker exec "$OC" openclaw plugins install /tmp/plugin.tgz >/dev/null 2>&1
CFG="$(mktemp)"
cat >"$CFG" <<JSON
{ plugins: { entries: { ad4m: { config: { multiUser: true, email: "$EMAIL", password: "$UPASS" } } } } }
JSON
docker cp "$CFG" "$OC":/tmp/ec.json5 >/dev/null; rm -f "$CFG"
docker exec "$OC" openclaw config patch --file /tmp/ec.json5 >/dev/null 2>&1

echo "[a2xp] run ad4m-setup external (multi-user) through the plugin"
SETUP=$(timeout 120 docker exec "$OC" openclaw ad4m-setup --endpoint "http://$EXE:3001/mcp" --executor-url "http://$EXE:12000" 2>&1 || true)
if ! echo "$SETUP" | grep -qE 'Logged in as|own user identity ready'; then
  echo "[a2xp] FAIL — plugin setup did not provision an identity"; echo "$SETUP" | tail -15 | sed 's/^/    /'; exit 1
fi

echo "[a2xp] verify the created identity on the node"
"$TSX" "$AGENTS/openclaw/verify-user.ts" "$EMAIL" "$UPASS" "http://127.0.0.1:$MCP_PORT"

echo "[a2xp] PASS — OpenClaw plugin (ad4m-setup external) provisioned + verified a distinct multi-user identity"
