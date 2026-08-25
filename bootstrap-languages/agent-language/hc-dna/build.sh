#!/bin/bash
# Build only the zome packages for WASM (exclude sweettest which has native-only dependencies)
#
# Prefer the workspace-local, version-pinned `hc` installed by
# scripts/install-hc-toolchain.sh over the host's global one. See that script
# for the rationale (Cargo.lock-pinned `holochain_cli_bundle` must match the
# `hc` used to pack DNAs and hApps, otherwise the executor rejects the
# bundle at install time — e.g. `unknown field 'signal_url'`).
set -euo pipefail
REPO_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
LOCAL_HC="$REPO_ROOT/.hc-toolchain/bin/hc"
if [ -x "$LOCAL_HC" ]; then
    HC="$LOCAL_HC"
else
    HC="hc"
fi
echo "agent-language build.sh: using hc=$HC ($("$HC" --version))"

CARGO_TARGET_DIR=target RUSTFLAGS='--cfg getrandom_backend="custom"' cargo build --release --target wasm32-unknown-unknown -p agent_store -p agent_store_integrity
"$HC" dna pack workdir
"$HC" app pack workdir
