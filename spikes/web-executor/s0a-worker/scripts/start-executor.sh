#!/bin/bash
# Native executor for S0a: temp data dir, all egress through the deny proxy, no Holochain.
set -e
# Env: S (s0a-worker dir, default: parent of this script), EXECUTOR_BIN (default: `ad4m-executor` on PATH),
# SEED (local seed dir from bootstrap-languages/local/generate-seed.mjs, default: $S/seed), D (data dir).
S=${S:-$(cd "$(dirname "$0")/.." && pwd)}
X=${EXECUTOR_BIN:-ad4m-executor}
SEED=${SEED:-$S/seed}
mkdir -p "$S/logs"
D=${D:-$(mktemp -d /tmp/s0a-exec-XXXX)}
echo "$D" > "$S/logs/executor-datadir"
if [ ! -d "$D/ad4m" ]; then
  "$X" init --data-path "$D" --network-bootstrap-seed "$SEED/docker_seed.json"
  for d in "$SEED"/languages/*/; do h=$(basename "$d"); mkdir -p "$D/ad4m/languages/$h"; cp "$d/bundle.js" "$D/ad4m/languages/$h/"; done
fi
export HTTP_PROXY=http://127.0.0.1:47459 HTTPS_PROXY=http://127.0.0.1:47459 ALL_PROXY=http://127.0.0.1:47459
export http_proxy=$HTTP_PROXY https_proxy=$HTTPS_PROXY all_proxy=$ALL_PROXY
export NO_PROXY=127.0.0.1,localhost no_proxy=127.0.0.1,localhost
export HF_HUB_OFFLINE=1 RUST_LOG=${RUST_LOG:-info}
cd "$D"
exec "$X" run --app-data-path "$D" --port 47460 --language-language-only false --run-dapp-server false \
  --run-holochain false --hc-bootstrap-url http://127.0.0.1:47499 --hc-relay-url http://127.0.0.1:47499 \
  --hc-use-bootstrap false --hc-use-proxy false --hc-use-mdns false \
  --local-ai-autostart false --insecure-no-admin-credential
