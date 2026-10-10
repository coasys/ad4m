#!/bin/bash
# Shared build script for `bootstrap-languages/*/hc-dna/`. Called from a
# per-language `build.sh` shim that only declares the crates that need
# building. See any language's `hc-dna/build.sh` for the pattern.
#
# Prefer the workspace-local, version-pinned `hc` installed by
# scripts/install-hc-toolchain.sh over the host's global one. See that
# script for the rationale (Cargo.lock-pinned `holochain_cli_bundle` must
# match the `hc` used to pack DNAs and hApps, otherwise the executor
# rejects the bundle at install time — e.g. `unknown field 'signal_url'`).
#
# Env / args contract:
#   HC_DNA_NAME     — language display name for log lines (required)
#   $@              — cargo `-p <crate>` package flags for the wasm build
#                     (e.g. `-p perspective_diff_sync -p perspective_diff_sync_integrity`)
#
# The script assumes it's called from the `hc-dna/` directory of the
# language (so `workdir` here refers to `hc-dna/workdir`), which is where
# the language's own `build.sh` sits.

set -euo pipefail

if [[ -z "${HC_DNA_NAME:-}" ]]; then
    echo "hc-dna-build.sh: HC_DNA_NAME env var is required" >&2
    exit 2
fi

if [[ $# -eq 0 ]]; then
    echo "hc-dna-build.sh: no cargo package flags supplied" >&2
    exit 2
fi

# Resolve the repo root relative to THIS shared script, then locate the
# workspace-local `hc` if present. The shared script lives at
# `bootstrap-languages/hc-dna-build.sh`; the repo root is one level up.
SHARED_SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SHARED_SCRIPT_DIR/.." && pwd)"
LOCAL_HC="$REPO_ROOT/.hc-toolchain/bin/hc"
if [[ -x "$LOCAL_HC" ]]; then
    HC="$LOCAL_HC"
else
    HC="hc"
fi
echo "$HC_DNA_NAME build.sh: using hc=$HC ($("$HC" --version))"

CARGO_TARGET_DIR=target \
    RUSTFLAGS='--cfg getrandom_backend="custom"' \
    cargo build --release --target wasm32-unknown-unknown "$@"

"$HC" dna pack workdir
"$HC" app pack workdir
