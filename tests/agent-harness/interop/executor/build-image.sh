#!/usr/bin/env bash
#
# Build the executor image the verify scripts run (default tag
# ad4m-agent-harness-executor:local, override with AD4M_EXEC_IMAGE) from binaries
# already built in this checkout:
#
#   cargo build --release -p ad4m --bin ad4m-executor --bin ad4m
#   tests/agent-harness/interop/executor/build-image.sh
#
# AD4M_BIN_DIR points at another directory holding ad4m-executor and ad4m
# (default: <repo>/target/release). The binaries are copied into a private build
# context first, so a concurrent cargo build cannot swap them mid-build.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../../../.." && pwd)"
IMG="${AD4M_EXEC_IMAGE:-ad4m-agent-harness-executor:local}"
BIN_DIR="${AD4M_BIN_DIR:-$REPO/target/release}"

for b in ad4m-executor ad4m; do
  [ -x "$BIN_DIR/$b" ] || { echo "[exec-image] missing $BIN_DIR/$b — build it first (cargo build --release -p ad4m --bin ad4m-executor --bin ad4m)"; exit 1; }
done

CTX="$(mktemp -d)"
trap 'rm -rf "$CTX"' EXIT
cp "$BIN_DIR/ad4m-executor" "$BIN_DIR/ad4m" "$CTX/"
cp "$REPO/docker-entrypoint.sh" "$HERE/Dockerfile" "$CTX/"
node "$REPO/docker/generate-seed.mjs" "$REPO/bootstrap-languages/local" "$CTX/seed" >/dev/null
SOURCE="$(git -C "$REPO" rev-parse HEAD 2>/dev/null || echo unknown)"
git -C "$REPO" diff --quiet HEAD -- rust-executor cli rust-client 2>/dev/null || SOURCE="$SOURCE+dirty"
echo "$SOURCE" >"$CTX/SOURCE"

docker build -q -t "$IMG" --label "org.opencontainers.image.revision=$SOURCE" "$CTX" >/dev/null
echo "[exec-image] built $IMG from $BIN_DIR ($SOURCE)"
