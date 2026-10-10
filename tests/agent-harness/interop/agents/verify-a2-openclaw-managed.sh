#!/usr/bin/env bash
#
# A2 — Provision & connect (managed / download+install a fresh node), OpenClaw.
#
# The "download & install" onboarding path: a real OpenClaw runtime, with the
# ad4m plugin in managed mode, stands up its OWN fresh ADAM node and generates
# its own agent — all inside one hardened, fully-isolated container
# (`--network none`, so zero egress; managed mode is self-contained). We then
# verify the fresh agent + node persisted, and tear the container down.
#
# Needs the ad4m plugin built with compiled output (coasys/ad4m#880). Point
# AD4M_PLUGIN_DIR at a plugins/ad4m checkout that has the `build` script; if none
# is available the run SKIPs (exit 0) rather than failing.
#
# Env: KEEP=1 leaves the container up for debugging.
#
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
IMG="${A2M_IMAGE:-a2wt-openclaw-node}"
BASE_IMG="${A2_EXEC_IMAGE:-ad4m-test:latest}"
OC=a2m-openclaw
# Managed mode runs fully zero-egress: the plugin installs offline (deps bundled
# into dist) and managed mode starts the executor with --run-holochain false, so
# there is no npm fetch and no Holochain bootstrap. The container is host-isolated
# too (no host mounts, cap-drop, limits, non-root).
NET="${A2M_NETWORK:-none}"
KEEP="${KEEP:-}"

# Resolve the plugin source (must have the dist build script — coasys/ad4m#880).
PLUGIN_DIR="${AD4M_PLUGIN_DIR:-${AD4M_REPO:+$AD4M_REPO/plugins/ad4m}}"
if [ -z "${PLUGIN_DIR:-}" ] || [ ! -f "$PLUGIN_DIR/package.json" ] || ! grep -q '"build"' "$PLUGIN_DIR/package.json" 2>/dev/null; then
  echo "[a2m] SKIP — no ad4m plugin with a dist build available (set AD4M_PLUGIN_DIR to a plugins/ad4m checkout carrying coasys/ad4m#880)"
  exit 0
fi

teardown() { [ -n "$KEEP" ] && { echo "[a2m] KEEP set — leaving container up"; return; }; docker rm -f "$OC" >/dev/null 2>&1 || true; }
trap teardown EXIT

echo "[a2m] clean slate"; docker rm -f "$OC" >/dev/null 2>&1 || true

echo "[a2m] ensure combined OpenClaw+executor image"
docker image inspect "$IMG" >/dev/null 2>&1 || docker build -q -t "$IMG" --build-arg BASE="$BASE_IMG" "$HERE/openclaw-node/" >/dev/null

echo "[a2m] build ad4m plugin (dist) from $PLUGIN_DIR"
( cd "$PLUGIN_DIR" && NODE_ENV=development npm install --include=dev >/dev/null 2>&1 && NODE_ENV=development npm run build >/dev/null 2>&1 )
TGZ_NAME=$(cd "$PLUGIN_DIR" && NODE_ENV=development npm pack 2>/dev/null | tail -1)
PLUGIN_TGZ="$PLUGIN_DIR/$TGZ_NAME"

echo "[a2m] start combined container (network=$NET, hardened)"
docker run -d --name "$OC" --network "$NET" --entrypoint sleep \
  --cap-drop ALL --cap-add CHOWN --cap-add SETUID --cap-add SETGID --cap-add DAC_OVERRIDE --cap-add FOWNER \
  --security-opt no-new-privileges:true --memory 3g --cpus 2 --pids-limit 1024 \
  "$IMG" infinity >/dev/null
docker cp "$PLUGIN_TGZ" "$OC":/tmp/plugin.tgz >/dev/null
rm -f "$PLUGIN_TGZ"

echo "[a2m] install plugin + configure managed mode"
docker exec "$OC" openclaw plugins install /tmp/plugin.tgz >/dev/null 2>&1
docker exec "$OC" sh -c 'cat >/tmp/mc.json5 <<JSON
{ plugins: { entries: { ad4m: { config: { mode: "managed", ad4mBinaryPath: "/usr/local/bin/ad4m-executor", runHolochain: false } } } } }
JSON
openclaw config patch --file /tmp/mc.json5 >/dev/null 2>&1'

echo "[a2m] run managed onboarding (ad4m-setup: start fresh node + generate agent)"
OUT=$(timeout 240 docker exec "$OC" openclaw ad4m-setup 2>&1 || true)
DID=$(echo "$OUT" | grep -oE 'Agent ready\. DID: did:key:[A-Za-z0-9]+' | grep -oE 'did:key:[A-Za-z0-9]+' | head -1)
docker exec "$OC" sh -c 'test -f "$HOME/.ad4m/ad4m_db.sqlite"' && DB=yes || DB=no
echo "[a2m] generated DID: ${DID:-NONE} | node DB persisted: $DB"

if [ -n "$DID" ] && [ "$DB" = yes ]; then
  echo "[a2m] PASS — OpenClaw assistant installed a fresh node and generated agent did=$DID"
else
  echo "[a2m] FAIL — DID=${DID:-none} DB=$DB"
  echo "$OUT" | tail -25
  exit 1
fi
