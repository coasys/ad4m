#!/usr/bin/env bash
# An offline AD4M executor for trying out a Language.
#
# It boots on the local bootstrap Languages (bootstrap-languages/local), runs no
# Holochain conductor and publishes nothing outside this machine: Languages you
# publish go into the local language-language's store under .sandbox/.
#
#   ./sandbox.sh init        create .sandbox/ (seed, data directory, admin credential)
#   ./sandbox.sh run         start the executor in the foreground
#   ./sandbox.sh cli ...     run the `ad4m` CLI against it, e.g. ./sandbox.sh cli agent me
#
# Environment: AD4M_EXECUTOR and AD4M_CLI (binaries, default: on PATH), AD4M_REPO
# (an ad4m checkout, default: the one this example lives in), AD4M_PORT (default 12100),
# SANDBOX (default: .sandbox next to this script).
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="${AD4M_REPO:-$HERE/../../../..}"
SANDBOX="${SANDBOX:-$HERE/.sandbox}"
PORT="${AD4M_PORT:-12100}"
EXECUTOR="${AD4M_EXECUTOR:-ad4m-executor}"
CLI="${AD4M_CLI:-ad4m}"

case "${1:-}" in
init)
    if [ -e "$SANDBOX" ]; then
        echo "$SANDBOX already exists; remove it to start over" >&2
        exit 1
    fi
    node "$REPO/bootstrap-languages/local/generate-seed.mjs" \
        "$REPO/bootstrap-languages/local" "$SANDBOX/seed"
    "$EXECUTOR" init --data-path "$SANDBOX/data" \
        --network-bootstrap-seed "$SANDBOX/seed/docker_seed.json"

    # The executor would fetch the bootstrap Languages from the language-language;
    # put their bundles where it looks first, and the language-language's store
    # where it can serve them, so nothing has to be fetched.
    for dir in "$SANDBOX"/seed/languages/*/; do
        target="$SANDBOX/data/ad4m/languages/$(basename "$dir")"
        mkdir -p "$target"
        cp "$dir/bundle.js" "$target/bundle.js"
    done
    language_language="$(cat "$SANDBOX/seed/language-language-kv/address.txt")"
    mkdir -p "$SANDBOX/data/ad4m/languages/$language_language"
    cp "$SANDBOX/seed/language-language-kv/ad4m-language-kv.json" \
        "$SANDBOX/data/ad4m/languages/$language_language/"

    # Whoever holds this credential controls the executor.
    (umask 077 && head -c 32 /dev/urandom | base64 > "$SANDBOX/admin-credential")
    echo "Sandbox ready in $SANDBOX. Start it with: ./sandbox.sh run"
    ;;
run)
    AD4M_ADMIN_CREDENTIAL_FILE="$SANDBOX/admin-credential" exec "$EXECUTOR" run \
        --app-data-path "$SANDBOX/data" \
        --port "$PORT" \
        --run-holochain false \
        --run-dapp-server false \
        --enable-mcp false
    ;;
cli)
    shift
    exec "$CLI" -e "http://127.0.0.1:$PORT" -a "$(cat "$SANDBOX/admin-credential")" "$@"
    ;;
*)
    sed -n '2,14p' "$0" | sed 's/^# \{0,1\}//'
    exit 1
    ;;
esac
